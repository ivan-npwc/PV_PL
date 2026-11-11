
   # source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/LRG_3d_Close.r")
   # labelInput = "C:\\Users\\usato\\SSL_UI\\PV_UI\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
   #
   # Closed (watertight) seal meshes for volume and mass (Python module seal_close.py in the same folder;
   # needs seal3d.py next to it, numpy + scipy; trimesh optional for an independent mesh check).
   # Runs after LRG_3d_CROP_wide.py (Predict/MeshCrop_wide) and LRG_PredsToPolygons_level_2.r.
   #   IN : <labelInput>/Predict/MeshCrop_wide/*.ply  (or Predict/MeshCrop)        wide crops of the model around each seal
   #        <labelInput>/Predict/SpP_LRG_level_2_level#2_<date>.kml                 outlines (level 2)
   #        <labelInput>/Observer_count/Length.kml, Width.kml                       observers' lines (optional: size fit)
   #        <labelInput>/Predict/Measurements_3D_<date>.csv                         seal3d output (optional: outline-mesh registration)
   #   OUT: <labelInput>/Predict/MeshClosed/<crop>.ply                              one closed mesh per seal (variant 'super')
   #        <labelInput>/Predict/<date>_sealClosed.csv                              volumes of the 4 closure variants, size fit, mass
   #        <labelInput>/Predict/<date>_sealClosed.kml                              points named "V .. L, M .. kg"
   #   Vertical scale of the flight (s_vert = f / f_ref, see README_3D_morphometry.md): 1.0 unless known.
   #   Batch from a shell:  python seal_close.py <labelInput> --s-vert 1.0

    library(reticulate)

    py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    py_pth_unx = "/home/ivan/anaconda3/bin/python"

    sys_info <- Sys.info()
    if (sys_info["sysname"] == "Windows") {py_pth = py_pth_win}
    if (sys_info["sysname"] == "Linux")   {py_pth = py_pth_unx}
    use_python(py_pth, required = TRUE)

    # folder with seal_close.py + seal3d.py (first existing candidate)
    mod_dir_candidates = c(if (exists("PV_PL_dir")) file.path(PV_PL_dir, "Modules", "LRG"),
                           "C:\\Users\\usato\\SSL_UI\\GIT\\PV_PL\\Modules\\LRG",
                           "/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG",
                           "/home/ivan/GIT_HUB/PV_PL/Modules/LRG")
    mod_dir = mod_dir_candidates[file.exists(file.path(mod_dir_candidates, "seal_close.py"))][1]
    if (is.na(mod_dir)) stop("seal_close.py not found - set PV_PL_dir")

    for (m in c("numpy", "scipy", "matplotlib")) {
      if (!py_module_available(m)) py_install(m)
    }
    seal_close <- import_from_path("seal_close", path = mod_dir)

    ###########################################################################
    # variant  : "super" (recommended), "mirror", "ell", "flat" or "all" (writes <crop>_<variant>.ply for each)
    # s_vert   : vertical scale factor of the flight (heights are divided by it), 1.0 if unknown
    # scale_mode: "seal" - per-seal size fit from the observers' lines where both lines exist, flight medians otherwise;
    #             "flight" - flight medians for every seal
    if (!exists("s_vert")) s_vert = 1.0
    rows <- seal_close$run_flight(normalizePath(labelInput), variant = "super", s_vert = s_vert, scale_mode = "seal")
    message("closed meshes: ", length(rows), " outlines   ", labelInput)

    # results back into R
    date1 <- substr(basename(labelInput), 1, 15)
    cl_pth <- file.path(labelInput, "Predict", paste0(date1, "_sealClosed.csv"))
    if (file.exists(cl_pth)) {
      sealClosed <- read.csv(cl_pth)
      print(table(sealClosed$status))
      print(summary(sealClosed$M_fit_1000_kg))
    }
