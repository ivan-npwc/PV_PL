   # Agisoft_create_model_fixed_f.r  -  fixed focal length test, one source() does everything:
   #   source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/Agisoft_create_model_fixed_f.r")
   #
   # For every flight in `flights` (section SETTINGS below):
   #   1. rebuilds the final (no_water) project with the focal length FIXED (no self-calibration of f);
   #      same images (Aerial_Images_For_Model), same water masks, same matching / depth-map / mesh settings
   #      as Agisoft_create_model_with_masks.r - only the camera model differs;
   #   2. exports the mesh tiles (Agisoft_export_model.r);
   #   3. re-measures the same outlines with HF1 (seal3d.py), after registering the outlines to the new mesh:
#      a re-processed block sits in a slightly different place (metres for the consumer UAVs), so the offset of
#      each new tile to the original tile is measured by DSM cross-correlation (mesh_offset.py) and passed to
#      seal3d as the initial shift, which seal3d then refines per tile (reg_init / reg_search, seal3d 1.2.1);
   #   4. compares with the original project: s, heights, H/W of large seals -> fixed_f_test_compare.csv
   # Nothing in the original flight folders is modified. Everything goes to a sibling folder
   #     <flight>_fixf/<flight>_fixf_Model_no_water.psx  (+ _Report.pdf + _calib.json + _Model/*.ply + Predict/)
   # Steps already done (psx / tiles / measurements present) are skipped unless overwrite = TRUE,
   # so the script can be re-run after an interruption.
   #
   # f_mode:  "ref"    - factory focal length from the image metadata (f_ref of the paper; s = 1 by construction)
   #          "factor" - f = f_factor[drone] * f_ref (the stable self-calibration values of the paper)
   #          "xml"    - calibration of the camera from a calibration flight with vertical control
   #                     (Metashape: Tools > Camera Calibration > Adjusted > Save as XML); F, Cx, Cy fixed
   # Metashape Python API: sensor.user_calib + sensor.fixed_params (names are case-sensitive: "F", "Cx", "Cy").
   # The focal lengths used and the result are logged to <flight>_fixf_Model_no_water_calib.json
   # (f_ref, f_fix, f_final, s_final must equal s_fix; aligned/cameras).

##### SETTINGS ###################################################################################
PV_UI     = "C:/Users/usato/SSL_UI/PV_UI"
PV_PL_dir = "C:/Users/usato/SSL_UI/GIT/PV_PL"
py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
py_pth_unx = "/home/ivan/anaconda3/bin/python"

f_mode    = "ref"
f_factor  = c(M3T = 1.05, MINI2 = 1.16, MINI3PRO = 1.00, MAVIC2PRO = 1.00)      # for f_mode = "factor"
calib_xml = c(M3T = "", MINI2 = "", MINI3PRO = "", MAVIC2PRO = "")               # for f_mode = "xml"
variant_suffix = "_fixf"
overwrite = FALSE          # TRUE: rebuild the variant project and tiles even if they exist
redo_hf1  = TRUE           # TRUE: always re-measure (HF1 is minutes per flight; the registration may have changed)
reg_search = 1.5           # half-width (m) of the registration refinement around the tile offsets
do_export = TRUE; do_hf1 = TRUE; do_compare = TRUE

# test set (vertical scale s and median H/W of large seals in the original final projects, Table S1):
flights = c("20240918_123329_M3T_20M",      # H0063, f refined, s 1.056, H/W 0.70   } same haulout, 24 min apart:
            "20240918_125737_M3T_20M",      # H0063, f factory, s 1.000, H/W 0.65   } with one fixed f they should agree
            "20240917_094533_MINI3PRO_20M", # s 1.35, H/W 1.02  (too high)
            "20240918_095054_MINI3PRO_20M", # s 1.46, H/W 1.00  (too high)
            "20240916_112614_MINI3PRO_20M", # s 0.80, H/W 0.61  (too low)
            "20240917_091131_MINI3PRO_20M", # s 0.83, H/W 0.60  (too low)
            "20230518_104633_MINI2_20m",    # s 1.18, H/W 0.78
            "20230718_111424_MINI2_20m")    # s 1.00 (factory f), H/W 0.59
# Expected if the instability is the self-calibration: all rebuilt models within the stable corridor
# of the paper (about 0.65-0.80) and the two H0063 models within a few per cent of each other.
##################################################################################################

library(reticulate)
sys_info <- Sys.info()
py_pth = if (sys_info["sysname"] == "Windows") py_pth_win else py_pth_unx
use_python(py_pth, required = TRUE)
if (!"package:magrittr" %in% search() && !"package:dplyr" %in% search()) suppressMessages(library(magrittr))  # %>% in Agisoft_export_model.r

mod_dir  = file.path(PV_PL_dir, "Modules", "LRG")
log_file = file.path(PV_UI, "fixed_f_test_log.txt")
logit <- function(...) {msg = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...); message(msg); cat(msg, "\n", file = log_file, append = TRUE)}

find_flight <- function(name) {                       # PV_UI/<year_site_OPP>/<date_time>/<flight>  or  PV_UI/<year_site_OPP>/<flight>
  for (site in list.dirs(PV_UI, recursive = FALSE)) {
    p = file.path(site, name); if (dir.exists(p) && dir.exists(file.path(p, "Predict"))) return(normalizePath(p, winslash = "/"))
    for (d in list.dirs(site, recursive = FALSE)) {
      p = file.path(d, name); if (dir.exists(p)) return(normalizePath(p, winslash = "/"))
    }
  }
  NA
}
seal3d <- import_from_path("seal3d", path = mod_dir)
mesh_offset <- import_from_path("mesh_offset", path = mod_dir)

# ---------------------------------------------------------------- 1. Metashape project with fixed f
build_fixed_f <- function(labelInput) {
  bsname  = basename(labelInput)
  drone   = toupper(strsplit(bsname, "_")[[1]][3])
  factor_used = if (f_mode == "factor") unname(f_factor[drone]) else 1.0
  if (is.na(factor_used)) stop(paste("no f_factor for drone", drone))
  xml_used = if (f_mode == "xml") unname(calib_xml[drone]) else ""
  if (f_mode == "xml" && (is.na(xml_used) || !file.exists(xml_used))) stop(paste("calibration XML not found for", drone, ":", xml_used))
  imagefolder = file.path(labelInput, "Aerial_Images_For_Model")
  mskdir      = file.path(labelInput, "Predict", "WATER_MASK")
  variant_dir = paste0(labelInput, variant_suffix)
  bsname_v    = paste0(bsname, variant_suffix)
  outputpath  = file.path(variant_dir, paste0(bsname_v, "_Model_no_water.psx"))
  reportpath  = file.path(variant_dir, paste0(bsname_v, "_Model_no_water_Report.pdf"))
  logpath     = file.path(variant_dir, paste0(bsname_v, "_Model_no_water_calib.json"))
  if (length(list.files(imagefolder)) == 0) {logit("SKIP  no aerial images in ", imagefolder); return(invisible(FALSE))}
  dir.create(variant_dir, showWarnings = FALSE)
  # inputs of the downstream scripts (small files only; images and masks are read from the original folder)
  dir.create(file.path(variant_dir, "Polygons", "Predict"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(variant_dir, "Predict"), showWarnings = FALSE)
  file.copy(list.files(file.path(labelInput, "Polygons", "Predict"), full.names = TRUE), file.path(variant_dir, "Polygons", "Predict"), overwrite = TRUE)
  file.copy(list.files(file.path(labelInput, "Predict"), pattern = "^SpP_LRG_level_2.*\\.kml$", full.names = TRUE), file.path(variant_dir, "Predict"), overwrite = TRUE)
  if (file.exists(outputpath) && !overwrite) {logit("exists  ", outputpath); return(invisible(TRUE))}
  if (file.exists(outputpath)) {unlink(outputpath); unlink(sub("\\.psx$", ".files", outputpath), recursive = TRUE)}
  image_folder = normalizePath(imagefolder, winslash = "/", mustWork = FALSE)
  mask_dir     = normalizePath(mskdir,      winslash = "/", mustWork = FALSE)
  output_path  = normalizePath(outputpath,  winslash = "/", mustWork = FALSE)
  report_path  = normalizePath(reportpath,  winslash = "/", mustWork = FALSE)
  log_path     = normalizePath(logpath,     winslash = "/", mustWork = FALSE)
  xml_path     = if (xml_used != "") normalizePath(xml_used, winslash = "/", mustWork = FALSE) else ""
  downscale_photo = if (drone == "M3T") 2 else 1       # alignment accuracy as in the final projects of the paper
  py_run_string(paste0('
import Metashape
import os, json
print("Metashape", Metashape.app.version)

doc = Metashape.Document()
chunk = doc.addChunk()
chunk.crs = Metashape.CoordinateSystem("EPSG:4326")

image_folder = r"', image_folder, '"
mask_dir = r"', mask_dir, '"
image_files = [f for f in os.listdir(image_folder) if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))]
chunk.addPhotos([os.path.join(image_folder, f) for f in image_files])

n_mask = 0
for camera in chunk.cameras:
    filename_base = os.path.splitext(os.path.basename(camera.photo.path))[0]
    mask_path = os.path.join(mask_dir, f"{filename_base}_mask.png")
    if os.path.exists(mask_path):
        try:
            mask = Metashape.Mask()
            mask.load(mask_path)
            camera.mask = mask
            n_mask += 1
        except Exception as e:
            print(f"mask error {mask_path}: {e}")
print("masks loaded:", n_mask, "of", len(chunk.cameras))

# ---------------- fixed focal length (set after addPhotos: the sensors exist only now) ----------------
f_mode = "', f_mode, '"
f_factor = ', format(factor_used, nsmall = 4), '
xml_path = r"', xml_path, '"
calib_log = {"f_mode": f_mode, "f_factor": f_factor, "xml": xml_path, "sensors": {}}
for sensor in chunk.sensors:
    c0 = sensor.calibration                     # initial calibration from the image metadata (f_ref of the paper)
    f_ref = c0.f
    c = Metashape.Calibration()                 # a separate object, assigned to the sensor as a whole
    c.width, c.height = c0.width, c0.height
    if f_mode == "xml":
        c.load(xml_path, format=Metashape.CalibrationFormatXML)
        fixed = ["F", "Cx", "Cy"]               # names are case-sensitive in the Metashape API
    else:
        c.f = f_ref * f_factor
        fixed = ["F"]
    sensor.user_calib = c
    sensor.fixed_params = fixed                 # F kept fixed; cx, cy, k1-k3, p1, p2 (b1, b2) are refined
    calib_log["sensors"][sensor.label] = {"width": c0.width, "height": c0.height, "f_ref": f_ref, "f_fix": c.f,
                                          "s_fix": c.f / f_ref, "fixed": fixed}
    print("sensor", sensor.label, "f_ref", round(f_ref, 2), "f_fix", round(c.f, 2), "fixed", fixed)

chunk.matchPhotos(
    downscale=', downscale_photo, ',
    downscale_3d=1,
    generic_preselection=True,
    reference_preselection=True,
    keypoint_limit=200000,
    keypoint_limit_3d=900000,
    tiepoint_limit=50000,
    keypoint_limit_per_mpx=5000,
    guided_matching=True,
    mask_tiepoints=True
)

chunk.alignCameras(adaptive_fitting=False)

chunk.optimizeCameras(
    fit_f=False,
    fit_cx=(f_mode != "xml"),
    fit_cy=(f_mode != "xml"),
    fit_b1=True,
    fit_b2=True,
    fit_k1=True,
    fit_k2=True,
    fit_k3=True,
    fit_p1=True,
    fit_p2=True,
    adaptive_fitting=False,
    tiepoint_covariance=True
)

for sensor in chunk.sensors:
    d = calib_log["sensors"][sensor.label]
    d["f_final"] = sensor.calibration.f
    d["s_final"] = sensor.calibration.f / d["f_ref"]
    print("sensor", sensor.label, "f_final", round(d["f_final"], 2), "s_final", round(d["s_final"], 4), "(must equal s_fix", round(d["s_fix"], 4), ")")
calib_log["cameras"] = len(chunk.cameras)
calib_log["aligned"] = sum(1 for cam in chunk.cameras if cam.transform is not None)
json.dump(calib_log, open(r"', log_path, '", "w"), indent=1)

chunk.buildDepthMaps(
    downscale=2,
    filter_mode=Metashape.MildFiltering,
    max_neighbors=16
)

chunk.buildModel(
    source_data=Metashape.DepthMapsData,
    surface_type=Metashape.Arbitrary,
    interpolation=Metashape.EnabledInterpolation,
    face_count=Metashape.CustomFaceCount,
    face_count_custom=50000000,
    vertex_colors=True,
    volumetric_masks=False
)

chunk.crs = Metashape.CoordinateSystem("EPSG:4326")
doc.save("', output_path, '")
chunk.exportReport(path=r"', report_path, '", title="Full Processing Report (fixed focal length)")
del doc
'))
  invisible(TRUE)
}

# ---------------------------------------------------------------- 4. comparison original vs fixed f
num <- function(x) suppressWarnings(as.numeric(x))
hf1_stats <- function(f) {
  d = read.csv(f, stringsAsFactors = FALSE)
  d = d[!is.na(d$status) & d$status == "ok" & d$quality %in% c("good", "fair"), ]
  big = d[num(d$L_2D_m) > 0.93, ]
  c(n_ok = nrow(d), n_large = nrow(big), H = median(num(big$H_max_m)), W = median(num(big$W_max_m)),
    HW = median(num(big$H_max_m) / num(big$W_max_m)), V = median(num(big$V_ell_m3)))
}
compare_fixed_f <- function(labs) {
  out = NULL
  for (lab in labs) {
    bs = basename(lab); date1 = substr(bs, 1, 15); vdir = paste0(lab, variant_suffix)
    f0 = file.path(lab,  "Predict", paste0("Measurements_3D_", date1, ".csv"))
    f1 = file.path(vdir, "Predict", paste0("Measurements_3D_", date1, ".csv"))
    fj = file.path(vdir, paste0(bs, variant_suffix, "_Model_no_water_calib.json"))
    if (!file.exists(f0) || !file.exists(f1)) {logit("compare: missing measurements for ", bs); next}
    s0 = hf1_stats(f0); s1 = hf1_stats(f1); s_fix = NA; s_fin = NA; aligned = NA
    d0 = read.csv(f0, stringsAsFactors = FALSE); d1 = read.csv(f1, stringsAsFactors = FALSE)   # per-seal height ratio (same seals)
    m = merge(d0[d0$status == "ok", c("seal_id", "H_max_m")], d1[d1$status == "ok", c("seal_id", "H_max_m")], by = "seal_id")
    m = m[num(m$H_max_m.x) > 0.08 & num(m$H_max_m.y) > 0, ]
    pseal = if (nrow(m)) round(median(num(m$H_max_m.y) / num(m$H_max_m.x)), 3) else NA
    if (file.exists(fj) && requireNamespace("jsonlite", quietly = TRUE)) {
      j = jsonlite::fromJSON(fj); s_fix = j$sensors[[1]]$s_fix; s_fin = j$sensors[[1]]$s_final; aligned = paste(j$aligned, j$cameras, sep = "/")
    }
    out = rbind(out, data.frame(flight = bs, s_fix = round(s_fix, 3), s_final = round(s_fin, 3), aligned = aligned,
                                n_large_old = s0[["n_large"]], n_large_new = s1[["n_large"]],
                                H_old = round(s0[["H"]], 3), H_new = round(s1[["H"]], 3), H_ratio = round(s1[["H"]] / s0[["H"]], 3),
                                W_ratio = round(s1[["W"]] / s0[["W"]], 3),
                                HW_old = round(s0[["HW"]], 3), HW_new = round(s1[["HW"]], 3), HW_ratio = round(s1[["HW"]] / s0[["HW"]], 3),
                                V_ratio = round(s1[["V"]] / s0[["V"]], 3), H_ratio_per_seal = pseal, n_seals_both = nrow(m),
                                ok_old = s0[["n_ok"]], ok_new = s1[["n_ok"]], row.names = NULL))
  }
  out
}

# ---------------------------------------------------------------- main loop
logit("==== fixed focal length test: f_mode = ", f_mode, ", ", length(flights), " flights")
done = c()
for (fl in flights) {
  lab = find_flight(fl)
  if (is.na(lab)) {logit("NOT FOUND  ", fl); next}
  t0 = Sys.time(); logit("---- ", lab)
  ok = tryCatch({
    build_fixed_f(lab)
    labelInput = paste0(lab, variant_suffix)                                # the downstream scripts use this global
    bs_v = basename(labelInput)
    tiles = list.files(file.path(labelInput, paste0(bs_v, "_Model")), pattern = "\\.ply$")
    if (do_export && (length(tiles) == 0 || overwrite)) source(file.path(mod_dir, "Agisoft_export_model.r"))   # mesh tiles
    m3d = file.path(labelInput, "Predict", paste0("Measurements_3D_", substr(bs_v, 1, 15), ".csv"))
    if (do_hf1 && (!file.exists(m3d) || overwrite || redo_hf1)) {
      # outlines were drawn on the orthomosaic of the ORIGINAL project: measure the offset of every new tile
      # to the original tile and let seal3d start its registration from there (refined per tile)
      offs = mesh_offset$tile_offsets(file.path(lab, paste0(basename(lab), "_Model")), file.path(labelInput, paste0(bs_v, "_Model")))
      logit("tile offsets (new vs original, m): ", paste(names(offs), sapply(offs, function(v) sprintf("(%+.2f, %+.2f)", v[[1]], v[[2]])), collapse = "; "))
      unlink(list.files(file.path(labelInput, "Predict"), pattern = "^_seal3d_(cache|reg)_", full.names = TRUE))   # stale registration / cache
      st3d = seal3d$run_flight(normalizePath(labelInput), res = 0.01, plots = TRUE, reg_init = offs, reg_search = reg_search)
      logit("HF1: ", st3d)
    }
    TRUE
  }, error = function(e) {logit("ERROR  ", fl, ": ", conditionMessage(e)); FALSE})
  logit(if (ok) "done  " else "failed  ", fl, "  ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
  if (ok) done = c(done, lab)
}
if (do_compare && length(done)) {
  cmp = compare_fixed_f(done)
  if (!is.null(cmp)) {
    print(cmp, row.names = FALSE)
    write.csv(cmp, file.path(PV_UI, "fixed_f_test_compare.csv"), row.names = FALSE)
    logit("comparison written to ", file.path(PV_UI, "fixed_f_test_compare.csv"))
  }
}
