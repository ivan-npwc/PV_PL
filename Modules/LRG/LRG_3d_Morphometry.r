
   # source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/LRG_3d_Morphometry.r")
   # labelInput = "C:\\Users\\usato\\SSL_UI\\PV_UI\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
   #
   # 3D morphometry of seals (Python module seal3d.py in the same folder; needs only numpy + matplotlib).
   # Replaces LRG_3d_CROP.r + LRG_3d_Measurements.r: works directly on the exported model tiles,
   # no MeshCrop step.
   #   IN : <labelInput>/<bsname>_Model/<bsname>_Model_N.ply          (Agisoft_export_model.r)
   #        <labelInput>/Predict/SpP_LRG_level_2_level#2_<date>.kml     (LRG_PredsToPolygons_level_2.r)
   #   OUT: <labelInput>/Predict/Measurements_3D_<date>.csv            one row per seal outline
   #        <labelInput>/Predict/Measurements_3D_<date>_profiles.csv   width/height/section area along the body
   #        <labelInput>/Predict/Measurements_3D_<date>.kml            outlines named "L.. W.. H.. V..L", coloured by quality
   #        <labelInput>/Predict/Measurements_3D_<date>_overview.png   quick look
   #   Resumable: per-seal cache Predict/_seal3d_cache_<bsname>.jsonl (delete it to recompute).
   #   Batch over the whole archive from a shell:
   #        python seal3d.py --root C:\Users\usato\SSL_UI\PV_UI --plots

    library(reticulate)

    py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    py_pth_unx = "/home/ivan/anaconda3/bin/python"

    sys_info <- Sys.info()
    if (sys_info["sysname"] == "Windows") {py_pth = py_pth_win}
    if (sys_info["sysname"] == "Linux")   {py_pth = py_pth_unx}
    use_python(py_pth, required = TRUE)

    # folder with seal3d.py (first existing candidate)
    mod_dir_candidates = c(if (exists("PV_PL_dir")) file.path(PV_PL_dir, "Modules", "LRG"),
                           "C:\\Users\\usato\\SSL_UI\\GIT\\PV_PL\\Modules\\LRG",
                           "/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG",
                           "/home/ivan/GIT_HUB/PV_PL/Modules/LRG")
    mod_dir = mod_dir_candidates[file.exists(file.path(mod_dir_candidates, "seal3d.py"))][1]
    if (is.na(mod_dir)) stop("seal3d.py not found - set PV_PL_dir")

    for (m in c("numpy", "matplotlib")) {
      if (!py_module_available(m)) py_install(m)
    }
    seal3d <- import_from_path("seal3d", path = mod_dir)

    ###########################################################################
    # res = 0.01  -> 1 cm raster of the mesh around each seal (default)
    # plots = TRUE -> Measurements_3D_<date>_overview.png
    status3d <- seal3d$run_flight(normalizePath(labelInput), res = 0.01, plots = TRUE)
    message("3D morphometry: ", status3d, "   ", labelInput)

    # results back into R
    date1 <- substr(basename(labelInput), 1, 15)
    m3d_pth <- file.path(labelInput, "Predict", paste0("Measurements_3D_", date1, ".csv"))
    if (file.exists(m3d_pth)) {
      m3d <- read.csv(m3d_pth)
      print(table(m3d$quality))
    }
