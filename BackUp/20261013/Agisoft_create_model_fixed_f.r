   # source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/Agisoft_create_model_fixed_f.r")
   # labelInput = "C:\\Users\\usato\\SSL_UI\\PV_UI\\2024_H0063_OPP\\20240918_125737\\20240918_125737_M3T_20M"
   #
   # Variant of Agisoft_create_model_with_masks.r with a FIXED focal length (no self-calibration of f).
   # Everything else is identical to the final (no_water) projects of the paper: same images
   # (Aerial_Images_For_Model), same water masks, same matching / depth-map / mesh settings.
   # Nothing of the original flight folder is modified: the project is written to a sibling folder
   #     <labelInput>_fixf/<bsname>_fixf_Model_no_water.psx      (+ _Report.pdf, + _calib.json)
   # together with copies of Polygons/Predict and Predict/SpP_LRG_level_2*.kml, so that the rest of
   # the pipeline runs unchanged on the variant folder:
   #     labelInput = paste0(labelInput, "_fixf")
   #     source(".../Agisoft_export_model.r")      # mesh tiles  -> <bsname>_fixf_Model/*.ply
   #     source(".../LRG_3d_Morphometry.r")        # HF1         -> Predict/Measurements_3D_<date>.csv
   #     Rscript compare_fixed_f.R <labelInput>    # original vs fixed-f: s, heights, H/W of large seals
   #
   # f_mode (set before sourcing, default "ref"):
   #   "ref"    - factory focal length from the image metadata (f_ref of the paper; s = 1 by construction)
   #   "factor" - f = f_factor[drone] * f_ref, e.g. the stable self-calibration values of the paper
   #              (M3T 1.05, MINI2 1.16, MINI3PRO 1.00, MAVIC2PRO 1.00): all models of one camera get the
   #              same vertical scale, but the value itself is still unverified
   #   "xml"    - calibration of the camera from a calibration flight with vertical control
   #              (Metashape Tools > Camera Calibration > Save, XML); f, cx, cy fixed, distortion refined
   # The focal lengths used are logged to <bsname>_fixf_Model_no_water_calib.json (f_ref, f_fix, f_final).

    library(reticulate)
    bsname = basename(labelInput)
    drone <<- strsplit(bsname, "_")[[1]][3]
    print(drone)
    if (!exists("f_mode"))   f_mode   = "ref"
    if (!exists("f_factor")) f_factor = c(M3T = 1.05, MINI2 = 1.16, MINI3PRO = 1.00, MAVIC2PRO = 1.00)
    if (!exists("calib_xml")) calib_xml = c(M3T = "", MINI2 = "", MINI3PRO = "", MAVIC2PRO = "")  # paths for f_mode = "xml"
    if (!exists("variant_suffix")) variant_suffix = "_fixf"
    factor_used = if (f_mode == "factor") unname(f_factor[toupper(drone)]) else 1.0
    if (is.na(factor_used)) stop(paste("no f_factor for drone", drone))
    xml_used = if (f_mode == "xml") unname(calib_xml[toupper(drone)]) else ""
    if (f_mode == "xml" && !file.exists(xml_used)) stop(paste("calibration XML not found for", drone, ":", xml_used))

    py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    py_pth_unx = "/home/ivan/anaconda3/bin/python"
    sys_info <- Sys.info()
    if (sys_info["sysname"] == "Windows") {py_pth = py_pth_win}
    if (sys_info["sysname"] == "Linux")   {py_pth = py_pth_unx}
    use_python(py_pth, required = TRUE)
    #####################################
    crs = "EPSG:4326"
    imagefolder = file.path(labelInput, "Aerial_Images_For_Model")
    mskdir      = file.path(labelInput, "Predict", "WATER_MASK")
    variant_dir = paste0(labelInput, variant_suffix)
    bsname_v    = paste0(bsname, variant_suffix)
    outputpath  = file.path(variant_dir, paste0(bsname_v, "_Model_no_water.psx"))
    reportpath  = file.path(variant_dir, paste0(bsname_v, "_Model_no_water_Report.pdf"))
    logpath     = file.path(variant_dir, paste0(bsname_v, "_Model_no_water_calib.json"))
##############################################################
if (file.exists(list.files(imagefolder, full.names = T)[1]) == F) {print(paste0("SKIP    ", labelInput, "   NO AERIAL IMAGES FOUND")); next}
    dir.create(variant_dir, showWarnings = FALSE)
    # inputs of the downstream scripts (small files only; images and masks are read from the original folder)
    dir.create(file.path(variant_dir, "Polygons", "Predict"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(variant_dir, "Predict"), showWarnings = FALSE)
    file.copy(list.files(file.path(labelInput, "Polygons", "Predict"), full.names = TRUE), file.path(variant_dir, "Polygons", "Predict"), overwrite = TRUE)
    file.copy(list.files(file.path(labelInput, "Predict"), pattern = "^SpP_LRG_level_2.*\\.kml$", full.names = TRUE), file.path(variant_dir, "Predict"), overwrite = TRUE)
    if (file.exists(outputpath)) {print(paste0("SKIP    ", outputpath, "   already built")); next}
    image_folder = normalizePath(imagefolder, winslash = "/", mustWork = F)
    output_path  = normalizePath(outputpath,  winslash = "/", mustWork = F)
    report_path  = normalizePath(reportpath,  winslash = "/", mustWork = F)
    log_path     = normalizePath(logpath,     winslash = "/", mustWork = F)
    mskdir       = normalizePath(mskdir,      winslash = "/", mustWork = F)
    xml_path     = if (xml_used != "") normalizePath(xml_used, winslash = "/", mustWork = F) else ""
####################################################################
    build_model <- function(image_folder, output_path, crs, downscale_photo) {
      py_run_string(paste0('
import Metashape
import os, json
print("Metashape", Metashape.app.version)

doc = Metashape.Document()
chunk = doc.addChunk()
chunk.crs = Metashape.CoordinateSystem("', crs, '")

image_folder = r"', image_folder, '"
mask_dir = r"', mskdir, '"
image_files = [f for f in os.listdir(image_folder) if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))]
photo_paths = [os.path.join(image_folder, f) for f in image_files]
chunk.addPhotos(photo_paths)

for camera in chunk.cameras:
    filename_base = os.path.splitext(os.path.basename(camera.photo.path))[0]
    mask_path = os.path.join(mask_dir, f"{filename_base}_mask.png")
    if os.path.exists(mask_path):
        try:
            mask = Metashape.Mask()
            mask.load(mask_path)
            camera.mask = mask
        except Exception as e:
            print(f"mask error {mask_path}: {e}")
    else:
        print(f"no mask for {filename_base}")

# ---------------- fixed focal length ----------------------------------------------------------
f_mode = "', f_mode, '"
f_factor = ', format(factor_used, nsmall = 4), '
xml_path = r"', xml_path, '"
calib_log = {"f_mode": f_mode, "f_factor": f_factor, "xml": xml_path, "sensors": {}}
for sensor in chunk.sensors:
    c0 = sensor.calibration                     # initial calibration from the image metadata (f_ref of the paper)
    f_ref = c0.f
    c = Metashape.Calibration()
    c.width, c.height = c0.width, c0.height
    if f_mode == "xml":
        c.load(xml_path, format=Metashape.CalibrationFormatXML)
        fixed = ["F", "Cx", "Cy"]      # names are case-sensitive in the Metashape API
    else:
        c.f = f_ref * f_factor
        fixed = ["F"]
    sensor.user_calib = c
    sensor.fixed_params = fixed                 # parameters kept fixed in alignment and optimization (sensor.fixed would fix all)
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
    print("sensor", sensor.label, "f_final", round(d["f_final"], 2), "s_final", round(d["s_final"], 4))
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

chunk.crs = Metashape.CoordinateSystem("', crs, '")
doc.save("', output_path, '")
chunk.exportReport(path=r"', report_path, '", title="Full Processing Report (fixed focal length)")
del doc
'))
}

# alignment accuracy as in the final projects of the paper (M3T 2024: Medium, others: High)
if (drone == "M3T") {build_model(image_folder, output_path, crs, downscale_photo = 2)}
if (drone != "M3T") {build_model(image_folder, output_path, crs, downscale_photo = 1)}
###################################################################
