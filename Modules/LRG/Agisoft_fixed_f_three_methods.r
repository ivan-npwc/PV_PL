# Agisoft_fixed_f_three_methods.r  -  the fixed focal length test measured by all three methods (HF1, HF2, CB).
#   source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/Agisoft_fixed_f_three_methods.r")
#
# Runs after Agisoft_create_model_fixed_f.r (the <flight>_fixf projects, their tiles and the HF1 run with the
# outline-to-mesh registration). For every <flight>_fixf folder:
#   1. register_outlines.py: the level-2 outline KML is rewritten with the per-tile shifts found by that HF1 run
#      (Predict/_seal3d_reg_<bs>.json); the original KML is kept as *.unregistered.bak. Done once (marker file
#      Predict/_outline_registration_<bs>.json).
#   2. HF1 again on the registered outlines (seal3d.run_flight without reg_init; the old cache is renamed old3_*.bak)
#   3. LRG_3d_CROP_wide.py  -> Predict/MeshCrop_wide/*.ply          (crops around the registered outlines)
#   4. HF2: LRG_3d_Measurements_v2.r -> Predict/<date>_sealVolume_v2.csv
#   5. CB:  seal_close.py (variant super, s_vert 1) -> Predict/<date>_sealClosed.csv
#   6. collects the three methods of the fixed-f models into PV_UI/S4_fixed_f_3methods.csv (seal3d columns + v2_* + sc_*,
#      as in Dataset S1) and compares them, seal by seal, with the three methods of the original (self-calibrated) models
#      -> PV_UI/fixed_f_3methods_summary.csv (flight x method: height / volume ratios, H/W of large seals, quality counts)
# Everything in the original flight folders is only read. Resumable: steps with existing outputs are skipped unless the
# redo_* switches are TRUE. Log: PV_UI/fixed_f_3methods_log.txt. Cost: a few minutes per model for HF1 and CB, 5-15 min for HF2.

##### SETTINGS ###################################################################################
PV_UI     = "C:/Users/usato/SSL_UI/PV_UI"
PV_PL_dir = "C:/Users/usato/SSL_UI/GIT/PV_PL"
py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
py_pth_unx = "/home/ivan/anaconda3/bin/python"
variant_suffix = "_fixf"
flights = c("20240918_123329_M3T_20M", "20240918_125737_M3T_20M",
            "20240917_094533_MINI3PRO_20M", "20240918_095054_MINI3PRO_20M", "20240917_091131_MINI3PRO_20M",
            "20230518_104633_MINI2_20m", "20230718_111424_MINI2_20m",
            "20240916_112614_MINI3PRO_20M")          # skipped automatically while its _fixf project has no tiles
redo_hf1 = FALSE; redo_crops = FALSE; redo_hf2 = FALSE; redo_cb = FALSE
##################################################################################################

library(reticulate)
sys_info <- Sys.info()
py_pth = if (sys_info["sysname"] == "Windows") py_pth_win else py_pth_unx
use_python(py_pth, required = TRUE)
mod_dir = file.path(PV_PL_dir, "Modules", "LRG")
seal3d     <- import_from_path("seal3d", path = mod_dir)
reg_out    <- import_from_path("register_outlines", path = mod_dir)
crop_wide  <- import_from_path("LRG_3d_CROP_wide", path = mod_dir)
seal_close <- import_from_path("seal_close", path = mod_dir)
log_file = file.path(PV_UI, "fixed_f_3methods_log.txt")
logit <- function(...) {msg = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...); message(msg); cat(msg, "\n", file = log_file, append = TRUE)}

find_flight <- function(name) {
  for (site in list.dirs(PV_UI, recursive = FALSE)) {
    p = file.path(site, name); if (dir.exists(p) && dir.exists(file.path(p, "Predict"))) return(normalizePath(p, winslash = "/"))
    for (d in list.dirs(site, recursive = FALSE)) { p = file.path(d, name); if (dir.exists(p)) return(normalizePath(p, winslash = "/")) }
  }
  NA
}
rename_aside <- function(dir, pattern, prefix = "old3_") {
  for (f in list.files(dir, pattern = pattern, full.names = TRUE)) {
    to = file.path(dir, paste0(prefix, basename(f), ".bak")); k = 0
    while (file.exists(to)) {k = k + 1; to = file.path(dir, paste0(prefix, basename(f), ".bak", k))}
    file.rename(f, to)
  }
}

# ---------------------------------------------------------------- readers of the three outputs (prefixes as in Dataset S1)
to_num <- function(x) suppressWarnings(as.numeric(x))
rbind_fill <- function(a, b) {                       # rbind with the union of the columns
  if (is.null(a)) return(b); if (is.null(b)) return(a)
  for (n in setdiff(names(b), names(a))) a[[n]] = NA
  for (n in setdiff(names(a), names(b))) b[[n]] = NA
  rbind(a, b[, names(a)])
}
read_hf1 <- function(folder) {
  f = file.path(folder, "Predict", paste0("Measurements_3D_", substr(basename(folder), 1, 15), ".csv"))
  if (!file.exists(f)) return(NULL); read.csv(f, stringsAsFactors = FALSE)
}
read_hf2 <- function(folder) {
  date1 = substr(basename(folder), 1, 15); f = file.path(folder, "Predict", paste0(date1, "_sealVolume_v2.csv"))
  if (!file.exists(f)) return(NULL)
  d = read.csv(f, stringsAsFactors = FALSE, colClasses = c(Nord = "character", East = "character"))
  id = suppressWarnings(as.integer(sub(".*_id(\\d+)\\.ply$", "\\1", d$file)))
  d = d[!is.na(id), ]; id = id[!is.na(id)]
  keep = c("L_plan", "W_max", "H_max", "H_core", "spine_length", "G_max", "chest_env", "V", "V_ell", "M_est", "area", "cover",
           "ground_mad", "slope_deg", "quality", "status", "mode", "ring_gap", "reg_dx", "reg_dy")
  keep = keep[keep %in% names(d)]
  out = d[, keep]; names(out) = paste0("v2_", sub("^spine_length$", "spine", keep))
  cbind(seal_id = sprintf("%s_%03d", date1, id), out, stringsAsFactors = FALSE)
}
read_cb <- function(folder) {
  date1 = substr(basename(folder), 1, 15); f = file.path(folder, "Predict", paste0(date1, "_sealClosed.csv"))
  if (!file.exists(f)) return(NULL)
  d = read.csv(f, stringsAsFactors = FALSE)
  keep = c("status", "flags", "variant", "L_axis_m", "L_mid_m", "W_max_m", "H_max_m", "V_mesh_m3", "V_flat_raw", "V_ell_raw", "V_mirror_raw",
           "V_super_raw", "V_super_c0_raw", "V_super_c85_raw", "super_n", "super_k", "super_rms_m", "edge_frac", "edge_asym", "n_sections",
           "n_multi_chord", "mesh_cover", "ground_mad_m", "ground_sectors", "slope_deg", "frac_below_ground", "watertight", "n_faces")
  keep = keep[keep %in% names(d)]
  out = d[, keep]; names(out) = paste0("sc_", keep)
  out$sc_H = to_num(d$H_max_m); out$sc_W = to_num(d$W_max_m); out$sc_L = to_num(d$L_mid_m)
  out$sc_V = to_num(d$V_super_raw); out$sc_Vell = to_num(d$V_ell_raw); out$sc_Vcol = to_num(d$V_flat_raw)
  out$sc_clean = d$status == "ok" & (is.na(d$flags) | d$flags == "")
  cbind(seal_id = d$seal_id, out, stringsAsFactors = FALSE)
}
three_methods <- function(folder) {
  a = read_hf1(folder); if (is.null(a)) return(NULL)
  b = read_hf2(folder); c3 = read_cb(folder)
  if (!is.null(b)) a = merge(a, b, by = "seal_id", all.x = TRUE, sort = FALSE)
  if (!is.null(c3)) a = merge(a, c3, by = "seal_id", all.x = TRUE, sort = FALSE)
  a[order(a$seal_id), ]
}

# ---------------------------------------------------------------- main loop
logit("==== fixed focal length, three methods: ", length(flights), " flights")
done = list()
for (fl in flights) {
  lab = find_flight(fl); if (is.na(lab)) {logit("NOT FOUND  ", fl); next}
  vd = paste0(lab, variant_suffix); bs = basename(vd); date1 = substr(fl, 1, 15); pred = file.path(vd, "Predict")
  if (!length(list.files(file.path(vd, paste0(bs, "_Model")), pattern = "\\.ply$"))) {logit("no tiles (project not built): ", bs); next}
  t0 = Sys.time(); logit("---- ", vd)
  ok = tryCatch({
    # 1. outlines registered onto the fixed-f mesh (once)
    info = reg_out$register_kml(normalizePath(vd))
    logit("outlines: median shift ", paste(round(unlist(info$median), 2), collapse = ", "), " m (", info$outlines, " outlines)")
    # 2. HF1 on the registered outlines
    m3d = file.path(pred, paste0("Measurements_3D_", date1, ".csv"))
    if (redo_hf1 || !file.exists(m3d) || file.mtime(m3d) < file.mtime(file.path(pred, paste0("_outline_registration_", bs, ".json")))) {
      rename_aside(pred, "^_seal3d_(cache|reg)_")
      st = seal3d$run_flight(normalizePath(vd), res = 0.01, plots = FALSE)
      h1 = read_hf1(vd); logit("HF1: ", st, "; registration dx=", h1$reg_dx_m[1], " dy=", h1$reg_dy_m[1], " m; ok ", sum(h1$status == "ok"), " of ", nrow(h1))
    } else logit("HF1: exists")
    # 3. wide crops around the registered outlines
    crops = file.path(pred, "MeshCrop_wide")
    if (redo_crops || !length(list.files(crops, pattern = "\\.ply$"))) {
      n = crop_wide$crop_flight(normalizePath(vd), 0.55); logit("crops: ", n)
    } else logit("crops: exist")
    # 4. HF2 (R)
    hf2 = file.path(pred, paste0(date1, "_sealVolume_v2.csv"))
    if (redo_hf2 || !file.exists(hf2)) {
      e = new.env(); assign("labelInput", normalizePath(vd), envir = e)
      source(file.path(mod_dir, "LRG_3d_Measurements_v2.r"), local = e)
      h2 = read_hf2(vd); logit("HF2: ", sum(h2$v2_status == "ok"), " ok, ", sum(h2$v2_quality == "good"), " good of ", nrow(h2))
    } else logit("HF2: exists")
    # 5. CB (Python)
    cb = file.path(pred, paste0(date1, "_sealClosed.csv"))
    if (redo_cb || !file.exists(cb)) {
      rows = seal_close$run_flight(normalizePath(vd), variant = "super", s_vert = 1.0, write_meshes = FALSE)
      h3 = read_cb(vd); logit("CB: ", sum(h3$sc_status == "ok"), " closed, ", sum(h3$sc_clean), " clean of ", nrow(h3))
    } else logit("CB: exists")
    TRUE
  }, error = function(e) {logit("ERROR  ", fl, ": ", conditionMessage(e)); FALSE})
  logit(if (ok) "done  " else "failed  ", fl, "  ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  if (ok) done[[fl]] = lab
}

# ---------------------------------------------------------------- 6. collect and compare with the original (self-calibrated) models
if (length(done)) {
  info_file = file.path(PV_UI, "S4_models_info.json")
  minfo = if (file.exists(info_file)) jsonlite::fromJSON(info_file, simplifyVector = FALSE) else list()
  minfo = setNames(minfo, vapply(minfo, function(x) x$flight, ""))
  S4 = NULL; SUM = NULL
  med = function(x) round(median(x, na.rm = TRUE), 3)
  for (fl in names(done)) {
    lab = done[[fl]]; vd = paste0(lab, variant_suffix)
    A = three_methods(lab); B = three_methods(vd)
    if (is.null(A) || is.null(B)) {logit("collect: missing outputs for ", fl); next}
    plat = strsplit(fl, "_")[[1]][3]; plat = c(M3T = "M3T", MINI3PRO = "MINI3PRO", MINI2 = "MINI2", MAVIC2PRO = "MAVIC2PRO")[plat]
    mi = minfo[[fl]]
    B = cbind(flight = fl, platform = if (!is.null(mi)) mi$platform else unname(plat),
              s_orig = if (!is.null(mi)) mi$s_orig else NA, s_fix = if (!is.null(mi)) mi$s_fix else NA, B, stringsAsFactors = FALSE)
    S4 = rbind_fill(S4, B)
    m = merge(A, B, by = "seal_id", suffixes = c("_o", "_f"))
    big = !is.na(to_num(m$L_2D_m_o)) & to_num(m$L_2D_m_o) > 0.93
    methods = list(
      HF1 = list(H = c("H_max_m_o", "H_max_m_f"), Hc = c("H_core_max_m_o", "H_core_max_m_f"), W = c("W_max_m_o", "W_max_m_f"), V = c("V_ell_m3_o", "V_ell_m3_f"),
                 ok_o = m$status_o == "ok" & m$quality_o %in% c("good", "fair"), ok_f = m$status_f == "ok" & m$quality_f %in% c("good", "fair")),
      HF2 = list(H = c("v2_H_max_o", "v2_H_max_f"), Hc = c("v2_H_core_o", "v2_H_core_f"), W = c("v2_W_max_o", "v2_W_max_f"), V = c("v2_V_ell_o", "v2_V_ell_f"),
                 ok_o = !is.na(m$v2_quality_o) & m$v2_quality_o == "good", ok_f = !is.na(m$v2_quality_f) & m$v2_quality_f == "good"),
      CB  = list(H = c("sc_H_o", "sc_H_f"), Hc = c("sc_H_o", "sc_H_f"), W = c("sc_W_o", "sc_W_f"), V = c("sc_V_o", "sc_V_f"),
                 ok_o = !is.na(m$sc_clean_o) & m$sc_clean_o, ok_f = !is.na(m$sc_clean_f) & m$sc_clean_f))
    for (mn in names(methods)) {
      q = methods[[mn]]; if (!all(c(q$H, q$W, q$V) %in% names(m))) {logit("collect: no ", mn, " columns for ", fl); next}
      Ho = to_num(m[[q$H[1]]]); Hf = to_num(m[[q$H[2]]]); Vo = to_num(m[[q$V[1]]]); Vf = to_num(m[[q$V[2]]])
      HWo = to_num(m[[q$Hc[1]]]) / to_num(m[[q$W[1]]]); HWf = to_num(m[[q$Hc[2]]]) / to_num(m[[q$W[2]]])
      both = !is.na(Ho) & !is.na(Hf) & Ho > 0.08
      SUM = rbind(SUM, data.frame(flight = fl, platform = B$platform[1], method = mn, s_orig = B$s_orig[1], s_fix = B$s_fix[1],
                                  expected_H_ratio = round(B$s_fix[1] / B$s_orig[1], 3),
                                  n_both = sum(both), H_ratio = med(Hf[both] / Ho[both]), V_ratio = med(Vf[both] / Vo[both]),
                                  ok_orig = sum(q$ok_o), ok_fixed = sum(q$ok_f),
                                  n_large_orig = sum(q$ok_o & big), n_large_fixed = sum(q$ok_f & big),
                                  HW_large_orig = med(HWo[q$ok_o & big]), HW_large_fixed = med(HWf[q$ok_f & big]), row.names = NULL))
    }
  }
  write.csv(S4, file.path(PV_UI, "S4_fixed_f_3methods.csv"), row.names = FALSE)
  write.csv(SUM, file.path(PV_UI, "fixed_f_3methods_summary.csv"), row.names = FALSE)
  print(SUM, row.names = FALSE)
  cv = function(x) round(100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), 1)
  for (mn in unique(SUM$method)) {
    s = SUM[SUM$method == mn, ]
    logit(mn, ": CV of median H/W of large seals across models ", cv(s$HW_large_orig), "% (self-calibration) -> ", cv(s$HW_large_fixed), "% (fixed f)")
  }
  logit("written: ", file.path(PV_UI, "S4_fixed_f_3methods.csv"), " and fixed_f_3methods_summary.csv")
}
