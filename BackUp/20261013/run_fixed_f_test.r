# run_fixed_f_test.r - does a fixed focal length remove the between-project instability of the vertical scale?
#   source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/run_fixed_f_test.r")
# For each test flight: rebuild the final (no_water) project with the focal length fixed
# (Agisoft_create_model_fixed_f.r -> <flight>_fixf/), export the mesh tiles (Agisoft_export_model.r),
# re-measure the same outlines with HF1 (LRG_3d_Morphometry.r), then compare with the original project:
#   Rscript compare_fixed_f.R <original flight folders>   (also run at the end of this script)
# Original projects, meshes and measurements are not touched.
#
# Test set (vertical scale s and median H/W of large seals in the original final projects, Table S1):
#   20240918_123329_M3T_20M / 20240918_125737_M3T_20M   same haulout H0063, 24 min apart: f refined (s 1.056, H/W 0.70)
#                                                       vs factory f (s 1, H/W 0.65) - with one fixed f they should agree
#   20240917_094533 / 20240918_095054 MINI3PRO_20M       s 1.35 and 1.46 (H/W 1.02 and 1.00 - too high)
#   20240916_112614 / 20240917_091131 MINI3PRO_20M       s 0.80 and 0.83 (H/W 0.61 and 0.60 - too low)
#   20230518_104633 / 20230718_111424 MINI2_20m          s 1.18 (H/W 0.78) vs factory f (H/W 0.59)
# Expected if the instability is the self-calibration: H/W of all rebuilt models within the stable
# corridor of the paper (about 0.65-0.80) and the two H0063 models within a few per cent of each other.

PV_UI     = "C:/Users/usato/SSL_UI/PV_UI"
PV_PL_dir = "C:/Users/usato/SSL_UI/GIT/PV_PL"
f_mode    = "ref"        # "ref": factory f for every camera (no assumption); "factor": stable self-calibration values
f_factor  = c(M3T = 1.05, MINI2 = 1.16, MINI3PRO = 1.00, MAVIC2PRO = 1.00)
flights   = c("20240918_123329_M3T_20M", "20240918_125737_M3T_20M",
              "20240917_094533_MINI3PRO_20M", "20240918_095054_MINI3PRO_20M",
              "20240916_112614_MINI3PRO_20M", "20240917_091131_MINI3PRO_20M",
              "20230518_104633_MINI2_20m", "20230718_111424_MINI2_20m")

mod = file.path(PV_PL_dir, "Modules", "LRG")
find_flight <- function(name) {                       # PV_UI/<year_site_OPP>/<date_time>/<flight>
  for (site in list.dirs(PV_UI, recursive = FALSE)) for (d in list.dirs(site, recursive = FALSE)) {
    p = file.path(d, name); if (dir.exists(p)) return(normalizePath(p, winslash = "\\"))
  }
  NA
}
done = c()
for (fl in flights) {
  labelInput = find_flight(fl)
  if (is.na(labelInput)) {message("not found: ", fl); next}
  message("==== ", labelInput)
  t0 = Sys.time()
  source(file.path(mod, "Agisoft_create_model_fixed_f.r"))        # -> <labelInput>_fixf/..._Model_no_water.psx
  labelInput = paste0(labelInput, "_fixf")
  source(file.path(mod, "Agisoft_export_model.r"))                 # mesh tiles -> <bs>_fixf_Model/*.ply
  source(file.path(mod, "LRG_3d_Morphometry.r"))                   # HF1 -> Predict/Measurements_3D_<date>.csv
  message("     ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
  done = c(done, sub("_fixf$", "", labelInput))
}
if (length(done)) system2("Rscript", c(shQuote(file.path(mod, "compare_fixed_f.R")), shQuote(done)))
