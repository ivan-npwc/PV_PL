# LRG_3d_Measurements_v2.r
# 3D measurements of seals (length, width, height, spine, girth, volume, indicative mass) from mesh crops.
# Version 2 (2026-09) of LRG_3D_Measurenents.r; functions in FUNCTIONS_SEAL_3D_MEASUREMENTS_v2.r,
# parameters set a priori as in seal3d (HF1): the outline as it is, ground ring 20-45 cm (no tuning to observers' lines).
#
# Input : <labelInput>/Predict/MeshCrop_wide/*.ply   (LRG_3d_CROP_wide.py, preferred: 55 cm of ground around
#                                                     the seal) or, if absent, Predict/MeshCrop/*.ply (LRG_3d_CROP.r)
#         <labelInput>/Predict/SpP_LRG_level_2_level#2_<date>.kml   (LRG_PredsToPolygons_level_2.r) - the body
#                                                     footprint; without it the body is found from the heights
#                                                     alone (mode "mesh", much less accurate)
# Output: <OUTDIR>/<date>_sealVolume_v2.csv and .kml   (OUTDIR default <labelInput>/Predict; nothing is deleted)
#
# Usage:  Rscript LRG_3d_Measurements_v2.r <labelInput> [OUTDIR] [s_vert]
#   or:   labelInput <- "D:\\PV_DB\\2023_H0052A_OPP\\20230615_103330\\20230615_103330_MAVIC2PRO_40m"
#         s_vert <- 1.05        # optional
#         source(".../Modules/LRG/LRG_3d_Measurements_v2.r")
# s_vert = vertical scale factor of the model (F / F_ref from the Metashape report, see README_3D_morphometry.md).
#          When it is not 1, the CSV gets a second set of columns (suffix _s) with heights, girth, spine, volume and
#          mass divided by it; the columns without suffix are always the values as reconstructed.
#
# Steps: 1. one raster (1 cm) per crop in the frame of its outline: DSM = highest surface over every cell;
#        2. registration of the outlines to the meshes, one XY shift per flight (as seal3d, applied if >= 2 cm);
#        3. ground plane per seal from the ring 20-45 cm outside the outline (sector-weighted, Tukey biweight);
#        4. measurements inside the outline: L along the curved midline, W across it, H above ground,
#           spine (3D dorsal ridge), girth of cross-sections, envelope "chest" of the old code, V above ground,
#           V_ell (elliptic sections), M_est = 1020 kg/m3 * V; quality flag instead of discarding.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) labelInput <- args[1]
OUTDIR <- if (length(args) >= 2 && nchar(args[2])) args[2] else file.path(labelInput, "Predict")
S_VERT <- if (length(args) >= 3) as.numeric(args[3]) else if (exists("s_vert")) s_vert else 1
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
this_dir <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa))) else
  tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)), error = function(e) getwd())
options(rgl.useNULL = TRUE, warn = -1)
suppressPackageStartupMessages({ library(Rvcg); library(geometry); library(sf) })
source(file.path(this_dir, "FUNCTIONS_SEAL_3D_MEASUREMENTS_v2.r"))

p <- v2_defaults
bs <- basename(labelInput); date1 <- substr(bs, 1, 15)
MeshDir <- file.path(labelInput, "Predict", "MeshCrop_wide")
if (!dir.exists(MeshDir) || !length(list.files(MeshDir, pattern = "\\.ply$"))) MeshDir <- file.path(labelInput, "Predict", "MeshCrop")
if (!dir.exists(MeshDir)) MeshDir <- file.path(labelInput, "MeshCrop")
MeshLst <- list.files(MeshDir, pattern = "\\.ply$", full.names = TRUE)
kml <- list.files(file.path(labelInput, "Predict"), pattern = "^SpP_LRG_level_2.*\\.kml$", full.names = TRUE)
if (!length(kml)) kml <- list.files(labelInput, pattern = "^SpP_LRG_level_2.*\\.kml$", full.names = TRUE)
OL <- if (length(kml)) read_outlines(kml[1], p) else NULL
if (is.null(OL)) message("No SpP_LRG_level_2 outlines: mode 'mesh' (body from heights only, less accurate)")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
saveCSVPth <- file.path(OUTDIR, paste0(date1, "_sealVolume_v2.csv"))
saveKMLpth <- file.path(OUTDIR, paste0(date1, "_sealVolume_v2.kml"))
message(bs, ": ", length(MeshLst), " crops from ", basename(MeshDir), if (!is.null(OL)) paste0(", ", length(OL$rings), " outlines") else "")

t00 <- proc.time()[3]
# 1. rasters
ols <- vector("list", length(MeshLst)); grids <- vector("list", length(MeshLst)); note <- character(length(MeshLst))
for (y in seq_along(MeshLst)) {
  ols[y] <- list(if (!is.null(OL)) match_outline(MeshLst[y], OL, p) else NULL)      # ols[y] <- list(): keeps NULLs in place
  m <- tryCatch(read_crop(MeshLst[y], p), error = function(e) NULL)
  if (is.null(m)) { note[y] <- "too few faces"; next }
  grids[y] <- list(tryCatch(seal_grid(m, ols[[y]], p), error = function(e) { note[y] <<- paste("grid error:", conditionMessage(e)); NULL }))
}
message("  rasters ", round(proc.time()[3] - t00), " s")
# 2. registration (outlines vs meshes) for the flight
shift <- c(0, 0); reg <- list(dx = 0, dy = 0, used = 0, gain = NA, applied = FALSE)
if (!is.null(OL)) {
  ok <- which(!vapply(grids, is.null, TRUE) & !vapply(ols, is.null, TRUE))
  set.seed(1); ok <- ok[sample(length(ok), min(60, length(ok)))]
  reg <- tryCatch(register_flight(grids[ok], ols[ok], p), error = function(e) reg)
  if (isTRUE(reg$applied)) shift <- c(reg$dx, reg$dy)
  message(sprintf("  registration: dx %.3f dy %.3f m, gain %.3f, n %d, applied %s (%.0f s)", reg$dx, reg$dy, reg$gain, reg$used, reg$applied, proc.time()[3] - t00))
}
# 3-4. ground, measurements
num <- function(x, k) if (is.null(x[[k]])) NA_real_ else as.numeric(x[[k]])
fin <- vector("list", length(MeshLst))
for (y in seq_along(MeshLst)) {
  nme <- basename(MeshLst[y]); t0 <- proc.time()[3]
  x <- list(status = note[y])
  if (!is.null(grids[[y]])) {
    x <- tryCatch({
      r <- finish_raster(grids[[y]], ols[[y]], p, shift)
      if (is.null(r)) list(status = "no ground around the outline") else {
        out <- measure_raster(r, p)
        if (S_VERT != 1 && identical(out$status, "ok")) {         # the same raster with the heights divided by s
          rs <- r; rs$H <- r$H / S_VERT; xs <- measure_raster(rs, p)
          out$H_max_s <- xs$H_max; out$H_core_s <- xs$H_core; out$spine_s <- xs$spine; out$G_max_s <- xs$G_max
          out$chest_env_s <- xs$chest_env; out$V_s <- xs$V; out$V_ell_s <- xs$V_ell; out$M_est_s <- xs$M_est
        }
        c(out, list(mode = if (is.null(ols[[y]])) "mesh" else "outline", n_face = r$n_face, n_ring = r$n_ring, gap_used = r$gap_used,
                    ground_mad = r$ground_mad, ground_sectors = r$ground_sectors, slope_deg = r$slope_deg))
      }
    }, error = function(e) list(status = paste0("error: ", conditionMessage(e))))
  }
  if (identical(x$status, "ok") && !is.null(OL) && is.null(ols[[y]])) x$mode <- "mesh (no outline matched)"
  name2 <- sub("\\.ply$", "", strsplit(nme, "#")[[1]][2])
  East1 <- sub("^E([0-9.]+)N.*$", "\\1", name2); Nord <- sub("^E[0-9.]+N([0-9.]+)(_.*)?$", "\\1", name2)
  fin[[y]] <- data.frame(Nord = Nord, East = East1,
    weightMesh = num(x, "V"), spine_length = num(x, "spine"), chest_length = num(x, "G_max"),   # names of the original CSV
    L_plan = num(x, "L_plan"), W_max = num(x, "W_max"), W_pos = num(x, "W_pos"), H_max = num(x, "H_max"), H_core = num(x, "H_core"),
    G_max = num(x, "G_max"), G_pos = num(x, "G_pos"), chest_env = num(x, "chest_env"), V = num(x, "V"), V_ell = num(x, "V_ell"),
    M_est = num(x, "M_est"), area = num(x, "area"), cover = num(x, "cover"),
    H_max_s = num(x, "H_max_s"), H_core_s = num(x, "H_core_s"), spine_s = num(x, "spine_s"), G_max_s = num(x, "G_max_s"),
    chest_env_s = num(x, "chest_env_s"), V_s = num(x, "V_s"), V_ell_s = num(x, "V_ell_s"), M_est_s = num(x, "M_est_s"),
    ground_mad = num(x, "ground_mad"), ground_sectors = num(x, "ground_sectors"), slope_deg = num(x, "slope_deg"),
    ring_gap = num(x, "gap_used"), n_face = num(x, "n_face"), s_vert = S_VERT, reg_dx = shift[1], reg_dy = shift[2],
    mode = if (is.null(x$mode)) NA else x$mode, quality = quality_v2(x), status = x$status, crop = basename(MeshDir), file = nme,
    secs = round(proc.time()[3] - t0, 2))
}
fin <- do.call(rbind, fin)
if (S_VERT == 1) fin <- fin[, !grepl("_s$", names(fin)) | names(fin) == "s_vert"]
write.csv(fin, saveCSVPth, row.names = FALSE)

ok <- fin[fin$status == "ok", ]
if (nrow(ok) > 0) {
  ok$label <- sprintf("L-%.0f W-%.0f H-%.0f Spine-%.0f Chest-%.0f Weight-%.0f", 100 * ok$L_plan, 100 * ok$W_max, 100 * ok$H_max,
                      100 * ok$spine_length, 100 * ok$chest_length, ok$M_est)
  ok$East <- as.numeric(ok$East); ok$Nord <- as.numeric(ok$Nord)
  seals_sf <- st_as_sf(ok[, c("East", "Nord", "label", "L_plan", "W_max", "H_max", "spine_length", "chest_length", "V", "M_est", "quality")],
                       coords = c("East", "Nord"), crs = p$crs)
  suppressWarnings(st_write(seals_sf, saveKMLpth, driver = "KML", delete_dsn = TRUE, quiet = TRUE, dataset_options = c("NameField=label")))
}
message("DONE ", bs, ": ", sum(fin$status == "ok"), " of ", nrow(fin), " measured, ", sum(fin$quality == "good"), " good  (",
        round(proc.time()[3] - t00), " s)")
