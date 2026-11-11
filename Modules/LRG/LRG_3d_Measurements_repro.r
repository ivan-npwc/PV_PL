# LRG_3d_Measurements_repro.r
# Restored, runnable version of Ivan Usatov's 3D seal measurements (spine, chest, mesh volume, weight).
# Written 2026-09 by Claude from the code in this repository; the processing lines are copied verbatim.
#
#   variant "2025" (default) = LRG_3D_Measurenents.r + FUNCTIONS_SEAL_3D_MEASUREMENTS.r from BackUp/20251101,
#                              the last version in which the measurement block was active.
#   variant "2026"           = Modules/LRG/LRG_3d_Measurements.r + Modules/LRG/FUNCTIONS_SEAL_3D_MEASUREMENTS.r
#                              (Feb 2026) with its commented-out measurement block restored.
#
# Changes against the originals (control flow and I/O only):
#   1. 2025: the watertight loop inside tryCatch() returns the mesh. In the original the block ends with the
#      for-loop, which returns NULL in R, so "if (is.null(sl_m34)) next" skipped every seal.
#   2. 2025: vcgSample(..., geodes = TRUE) is replaced by geodes = FALSE (as in the Feb 2026 functions).
#      Rvcg's Rsample() never sets the normals of the Monte-Carlo samples, and the geodesic distance uses
#      them: with zeroed memory it equals the Euclidean distance, with leftover memory it is arbitrary, and
#      a NaN normal makes PoissonDiskPruning loop forever (the process grows in memory and never returns).
#   3. Each seal runs inside tryCatch(): an error in one mesh does not stop the flight; column "status" says
#      why a seal was skipped. The input MeshCrop folder is only read (nothing is deleted or moved).
#   4. Outputs keep the original columns (Nord, East, weightMesh, spine_length, chest_length) plus
#      file, status, n_loop, n_orient_iter, x/y/z_range and area of the final mesh (PCA frame), secs.
#      KML labels as in the original: "Spine-<cm> Chest-<cm> Weight-<3000*V>".
#
# Usage (command line):
#   Rscript LRG_3d_Measurements_repro.r <labelInput> [variant 2025|2026] [OUTDIR] [FUNCTIONS file]
# or, in the style of the other scripts:
#   labelInput <- "D:\\PV_DB\\2023_H0052A_OPP\\20230615_103330\\20230615_103330_MAVIC2PRO_40m"
#   source("<repo>/Modules/LRG/LRG_3d_Measurements_repro.r")
# Input : <labelInput>/Predict/MeshCrop/*.ply   (LRG_3d_CROP.r)
# Output: <OUTDIR>/<date>_sealVolume_repro<variant>.csv and .kml  (OUTDIR default <labelInput>/Predict;
#         the original <date>_sealVolume.csv is not overwritten)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) labelInput <- args[1]
VARIANT <- if (length(args) >= 2) args[2] else if (exists("variant")) variant else "2025"
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
this_dir <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa))) else
  tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)), error = function(e) getwd())
repo <- normalizePath(file.path(this_dir, "..", ".."), mustWork = FALSE)
OUTDIR <- if (length(args) >= 3) args[3] else file.path(labelInput, "Predict")
FUNFILE <- if (length(args) >= 4) args[4] else if (VARIANT == "2025")
  file.path(repo, "BackUp", "20251101", "FUNCTIONS_SEAL_3D_MEASUREMENTS.r") else
  file.path(repo, "Modules", "LRG", "FUNCTIONS_SEAL_3D_MEASUREMENTS.r")
SAVE_MESH <- FALSE
options(rgl.useNULL = TRUE, warn = -1)
suppressPackageStartupMessages({
  library(Rvcg); library(rgl); library(sf); library(Morpho); library(XML); library(dplyr)
  library(dbscan); library(alphashape3d); library(plotly); library(RANN); library(geometry); library(igraph)
})
crs <- 32610         # UTM zone 10N coordinate system
E <- 435000          # Easting offset
N <- 5451000         # Northing offset
A <- -100            # Altitude offset (not currently used)

bs <- basename(labelInput)
date1 <- substr(bs, 1, 15)
MeshDir <- file.path(labelInput, "Predict", "MeshCrop")
if (!dir.exists(MeshDir)) MeshDir <- file.path(labelInput, "MeshCrop")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
MeshSaveDir <- file.path(OUTDIR, "MeshCrop_proceed_repro")
MeshLst <- list.files(MeshDir, pattern = "\\.ply$", full.names = TRUE)
source(FUNFILE)
if (VARIANT == "2025")
  vcgSample <- function(mesh, SampleNum = 100, type = "km", MCsamp = 20, geodes = FALSE, ...)
    Rvcg::vcgSample(mesh, SampleNum = SampleNum, type = type, MCsamp = MCsamp, geodes = FALSE, ...)
saveCSVPth <- file.path(OUTDIR, paste0(date1, "_sealVolume_repro", VARIANT, ".csv"))
saveKMLpth <- file.path(OUTDIR, paste0(date1, "_sealVolume_repro", VARIANT, ".kml"))
bs <- paste0(date1, "_repro", VARIANT)

process_2025 <- function(slpth, saveMeshPth) {
  flsize <- file.size(slpth)
  if (flsize < 90000) return(list(status = "skip: file < 90 kB"))
  sl = vcgImport(slpth, silent = TRUE)
  sl1= mesh_reconstract(sl)
  sl2 = close_small_holes(sl1)
  sl3= mesh_reconstract(sl2)
  sl4=frontalis_slice_filter(sl3)
  sl5=axial_slice_filter(sl4)
  sl6=saggital_slice_filter(sl5)
  pca_mesh=seal_pca(sl6)
  check=vcgSmooth (pca_mesh,type = "laplace", iteration = 30)
  vec= t(check$vb)[,3]
  altrng=diff(range(vec))
  if (altrng < 0.21){ return(list(status = "skip: flat mesh (z range < 0.21 m)")) }
  ########################## check and fix reconstract
  orientation=determine_mesh_orientation(pca_mesh)
  iter=0;max_iter=10
  while(orientation == "midi up" && iter < max_iter) {
    iter <- iter + 1
    pca_mesh=seal_pca(sl6)
    orientation=determine_mesh_orientation(pca_mesh)
  }
  ##############################
  sl8=frontalis_slice_filter(pca_mesh)
  sl9 = axial_slice_filter(sl8)
  sl10 = saggital_slice_filter(sl9)
  sl11= mesh_reconstract(sl10)
  sl12= mesh_reconstract(sl11)
  ################################################ cut 1
  bottom = Get_bottom(sl12, alt_correct=0.017)
  trimmed_seal = trim_seal_by_ground (seal_mesh=sl12, ground_mesh=bottom, above = T)
  ################################################ cut 2
  trimmed_seal= mesh_reconstract(trimmed_seal)
  trimmed_seal = close_small_holes(trimmed_seal)
  sl13=frontalis_slice_filter(trimmed_seal)
  sl14 <- tryCatch({ axial_slice_filter(sl13) }, error = function(e) { NULL })
  if (is.null(sl14)) return(list(status = "skip: axial_slice_filter failed"))
  sl15 <- tryCatch({ saggital_slice_filter(sl14) }, error = function(e) { NULL })
  if (is.null(sl15)) return(list(status = "skip: saggital_slice_filter failed"))
  seal_mesh = mesh_reconstract(sl15)
  altmsh=median(t(seal_mesh$vb)[,3])
  if (altmsh<0){ return(list(status = "skip: median z < 0")) }
  ground_mesh = Get_bottom(seal_mesh, alt_correct=0)
  inside <- Rvcg::vcgClost(ground_mesh, seal_mesh, sign = TRUE)$quality > 0
  trimmed_ground_pts <- vert2points(ground_mesh)[inside, ]
  trimmed_ground_mesh <- vcgBallPivoting(trimmed_ground_pts,radius=0.02)
  sl_m=merg_seal_ground(seal_mesh,trimmed_ground_mesh)
  sl_m1 = mesh_reconstract(sl_m)
  sl_m2 = vcgSmooth(sl_m1,type = "laplace", iteration = 300)
  sl_m3=sl_m2
  n_loop <- NA
  sl_m34 <- tryCatch({
    for (i in 1:100){
      sl_m3=mesh_reconstract(sl_m3)
      sl_m3=close_small_holes(sl_m3)
      if(is_watertight(sl_m3)==T){break}
    }
    n_loop <- i
    sl_m3                                   # <- added: return the mesh from the block
  }, error = function(e) { NULL })
  if (is.null(sl_m34)) return(list(status = "skip: close holes failed"))
  if (is_watertight(sl_m34)==F) return(list(status = "skip: not watertight after 100 loops", n_loop = n_loop))
  sl_m3 <- sl_m34
  sl_m4=sl_m34
  scale_x <-  1.22
  scale_y <-  1.22
  scale_z <-  1.33
  # Масштабирование (only the saved mesh is scaled, measurements use sl_m3)
  sl_m4$vb[1, ] <- sl_m3$vb[1, ] * scale_x  # X
  sl_m4$vb[2, ] <- sl_m3$vb[2, ] * scale_y  # Y
  sl_m4$vb[3, ] <- sl_m3$vb[3, ] * scale_z  # Z
  weightMesh = vcgVolume(sl_m3)
  spine= get_spine(sl_m3)
  chest = get_chest(sl_m3)
  spine_orig = measurement_to_orig(meshBase=sl3, measurement=spine)
  chest_orig =  measurement_to_orig(meshBase=sl3, measurement=chest)
  spine_length <- sum(sqrt(rowSums(diff(spine_orig)^2)))
  chest_length <- sum(sqrt(rowSums(diff(chest_orig)^2)))
  if (SAVE_MESH) vcgPlyWrite(sl_m4, saveMeshPth)
  # extra geometry of the final mesh (PCA frame: x = length, y = width, z = height)
  vb <- t(sl_m3$vb[1:3, ])
  list(status = "ok", weightMesh = weightMesh, spine_length = spine_length, chest_length = chest_length,
       n_loop = n_loop, x_range = diff(range(vb[,1])), y_range = diff(range(vb[,2])), z_range = diff(range(vb[,3])),
       area = vcgArea(sl_m3), n_orient_iter = iter)
}


process_2026 <- function(slpth, saveMeshPth) {
  # Modules/LRG/LRG_3d_Measurements.r (Feb 2026) with its commented-out measurement block restored
  sl = vcgImport(slpth, silent = TRUE)
  sl2= mesh_reconstract(sl)
  sl3 = close_small_holes(sl2)
  sl4 =frontalis_slice_filter(sl3)
  if (is.null(sl4)) return(list(status = "skip: frontalis_slice_filter returned NULL"))
  sl5=axial_slice_filter(sl4)
  sl6=saggital_slice_filter(sl5)
  pca_mesh=seal_pca(sl6)
  check=vcgSmooth (pca_mesh,type = "laplace", iteration = 30)
  vec= t(check$vb)[,3]
  altrng=diff(range(vec))
  if (altrng < 0.21){ return(list(status = "skip: flat mesh (z range < 0.21 m)")) }
  ########################## check and fix reconstract
  orientation=determine_mesh_orientation(pca_mesh)
  iter=0;max_iter=10
  while(orientation == "midi up" && iter < max_iter) {
    iter <- iter + 1
    pca_mesh=seal_pca(sl6)
    orientation=determine_mesh_orientation(pca_mesh)
  }
  ##############################
  sl8=frontalis_slice_filter(pca_mesh)
  sl9 = axial_slice_filter(sl8)
  sl10 = saggital_slice_filter(sl9)
  sl12= mesh_reconstract(sl10)
  ###
  orientation=determine_mesh_orientation(sl12)
  iter2=0;max_iter=10
  while(orientation == "midi up" && iter2 < max_iter) {
    iter2 <- iter2 + 1
    sl12=seal_pca(sl12)
    orientation=determine_mesh_orientation(sl12)
  }
  ################################################ cut 1
  bottom = Get_bottom(sl12, alt_correct=0.017)
  trimmed_seal = trim_seal_by_ground (seal_mesh=sl12, ground_mesh=bottom, above = T)
  ################################################ cut 2
  trimmed_seal= mesh_reconstract(trimmed_seal)
  trimmed_seal = close_small_holes(trimmed_seal)
  sl13=frontalis_slice_filter(trimmed_seal)
  sl14 = axial_slice_filter(sl13)
  sl15 = saggital_slice_filter(sl14)
  sl16 = mesh_reconstract(sl15)
  sl17 = close_small_holes(sl16)
  sl18 = mesh_reconstract(sl17)
  altmsh =median(t(sl18$vb)[,3])
  if (altmsh<0){ return(list(status = "skip: median z < 0")) }
  seal_mesh = clip_mesh_bottom(sl18,percent=12)
  ground_mesh = Get_bottom(trimmed_seal, alt_correct=0)
  sl_m = merg_seal_ground(sl18, ground_mesh)
  ########## below: the block that is commented out in the Feb 2026 script
  sl_m1 = mesh_reconstract(sl_m)
  sl_m2 = vcgSmooth(sl_m1,type = "laplace", iteration = 300)
  sl_m3=sl_m2
  n_loop <- NA
  for (i in 1:100){
    sl_m3=mesh_reconstract(sl_m3)
    sl_m3=close_small_holes(sl_m3)
    if(is_watertight(sl_m3)==T){break}
  }
  n_loop <- i
  if(is_watertight(sl_m3)==F) return(list(status = "skip: not watertight after 100 loops", n_loop = n_loop))
  sl_m4=sl_m3
  scale_x <-  1.22
  scale_y <-  1.22
  scale_z <-  1.33
  sl_m4$vb[1, ] <- sl_m3$vb[1, ] * scale_x  # X
  sl_m4$vb[2, ] <- sl_m3$vb[2, ] * scale_y  # Y
  sl_m4$vb[3, ] <- sl_m3$vb[3, ] * scale_z  # Z
  weightMesh = vcgVolume(sl_m3)
  spine= get_spine(sl_m3)
  chest = get_chest(sl_m3)
  spine_orig = measurement_to_orig(meshBase=sl3, measurement=spine)
  chest_orig =  measurement_to_orig(meshBase=sl3, measurement=chest)
  spine_length <- sum(sqrt(rowSums(diff(spine_orig)^2)))
  chest_length <- sum(sqrt(rowSums(diff(chest_orig)^2)))
  if (SAVE_MESH) vcgPlyWrite(sl_m4, saveMeshPth)
  vb <- t(sl_m3$vb[1:3, ])
  list(status = "ok", weightMesh = weightMesh, spine_length = spine_length, chest_length = chest_length,
       n_loop = n_loop, x_range = diff(range(vb[,1])), y_range = diff(range(vb[,2])), z_range = diff(range(vb[,3])),
       area = vcgArea(sl_m3), n_orient_iter = iter)
}


PROCESS <- if (VARIANT == "2025") process_2025 else process_2026
hangf <- file.path(OUTDIR, paste0(bs, "_skip.txt"))   # optional: file names to skip ("name<TAB>reason"), e.g. a seal that crashes R
curf <- file.path(tempdir(), paste0(bs, "_current.txt"))
hl <- if (file.exists(hangf)) strsplit(readLines(hangf), "\t") else list()
hung <- vapply(hl, `[`, "", 1)
hung_why <- vapply(hl, function(x) if (length(x) > 1) x[2] else "hang (killed by watchdog)", "")
fin <- NULL
if (file.exists(saveCSVPth)) fin <- read.csv(saveCSVPth, colClasses = c(Nord = "character", East = "character"))
for (y in seq_along(MeshLst)) {
  slpth <- MeshLst[y]; nme <- basename(slpth)
  if (!is.null(fin) && nme %in% fin$file) next
  t0 <- Sys.time()
  writeLines(nme, curf)
  r <- if (nme %in% hung) list(status = paste0("skip: ", hung_why[match(nme, hung)])) else tryCatch(PROCESS(slpth, file.path(MeshSaveDir, nme)),
                error = function(e) list(status = paste0("error: ", conditionMessage(e))))
  name1=strsplit(nme,"#")[[1]][2]
  name2=gsub(".ply","",name1)
  East= strsplit(name2,"N")[[1]][1]
  East1 = gsub("E","",East)
  Nord = strsplit(name2,"N")[[1]][2]
  g <- function(k) if (is.null(r[[k]])) NA else r[[k]]
  info <- data.frame(Nord = Nord, East = East1, weightMesh = g("weightMesh"), spine_length = g("spine_length"),
                     chest_length = g("chest_length"), file = nme, status = r$status, n_loop = g("n_loop"),
                     n_orient_iter = g("n_orient_iter"), x_range = g("x_range"), y_range = g("y_range"),
                     z_range = g("z_range"), area = g("area"),
                     secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
  fin <- rbind(fin, info)
  write.csv(fin, saveCSVPth, row.names = FALSE)
  message(bs, " ", y, "/", length(MeshLst), " ", r$status, " ", info$secs, " s")
}

################################################################ KML as in the original
ok <- fin[fin$status == "ok", ]
if (nrow(ok) > 0) {
  ok$weight <- round(ok$weightMesh * 3000)
  ok$spine_cm <- round(ok$spine_length * 100)
  ok$chest_cm <- round(ok$chest_length * 100)
  ok$spine_chest_volume <- paste0("Spine-", ok$spine_cm, " Chest-", ok$chest_cm, " Weight-", ok$weight)
  ok$East <- as.numeric(ok$East); ok$Nord <- as.numeric(ok$Nord)
  seals_sf <- st_as_sf(ok[, c("East", "Nord", "spine_chest_volume", "weightMesh", "spine_cm", "chest_cm", "weight")],
                       coords = c("East", "Nord"), crs = crs)
  suppressWarnings(st_write(seals_sf, saveKMLpth, driver = "KML", delete_dsn = TRUE, quiet = TRUE,
                            dataset_options = c("NameField=spine_chest_volume")))
}
message("DONE ", bs, ": ", sum(fin$status == "ok"), " of ", nrow(fin))
