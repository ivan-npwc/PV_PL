# compare_fixed_f.R - original final project vs the fixed-focal-length variant of the same flight(s).
# Usage: Rscript compare_fixed_f.R <labelInput> [<labelInput> ...]      (original flight folders)
# Needs Predict/Measurements_3D_<date>.csv in <labelInput> and in <labelInput>_fixf (seal3d.py / LRG_3d_Morphometry.r)
# and <labelInput>_fixf/<bs>_fixf_Model_no_water_calib.json (Agisoft_create_model_fixed_f.r).
# Reports, for seals of good or fair quality longer than 0.93 m: n, median height, width, H/W, and the ratios.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("give one or more flight folders")
suffix <- Sys.getenv("FIXF_SUFFIX", "_fixf")
num <- function(x) suppressWarnings(as.numeric(x))
stats <- function(d) {
  d <- d[!is.na(d$status) & d$status == "ok" & d$quality %in% c("good", "fair"), ]
  big <- d[num(d$L_2D_m) > 0.93, ]
  c(n_ok = nrow(d), n_large = nrow(big), H = median(num(big$H_max_m)), W = median(num(big$W_max_m)),
    HW = median(num(big$H_max_m) / num(big$W_max_m)), V = median(num(big$V_ell_m3)))
}
out <- NULL
for (lab in args) {
  bs <- basename(lab); date1 <- substr(bs, 1, 15)
  f0 <- file.path(lab, "Predict", paste0("Measurements_3D_", date1, ".csv"))
  f1 <- file.path(paste0(lab, suffix), "Predict", paste0("Measurements_3D_", date1, ".csv"))
  fj <- file.path(paste0(lab, suffix), paste0(bs, suffix, "_Model_no_water_calib.json"))
  if (!file.exists(f0) || !file.exists(f1)) {message("missing measurements for ", bs); next}
  s0 <- stats(read.csv(f0, stringsAsFactors = FALSE)); s1 <- stats(read.csv(f1, stringsAsFactors = FALSE))
  s_fix <- NA; aligned <- NA
  if (file.exists(fj) && requireNamespace("jsonlite", quietly = TRUE)) {
    j <- jsonlite::fromJSON(fj); s_fix <- j$sensors[[1]]$s_final; aligned <- paste(j$aligned, j$cameras, sep = "/")
  }
  out <- rbind(out, data.frame(flight = bs, s_fix = round(s_fix, 3), aligned = aligned,
                               n_large_old = s0["n_large"], n_large_new = s1["n_large"],
                               H_old = round(s0["H"], 3), H_new = round(s1["H"], 3), H_ratio = round(s1["H"] / s0["H"], 3),
                               W_ratio = round(s1["W"] / s0["W"], 3),
                               HW_old = round(s0["HW"], 3), HW_new = round(s1["HW"], 3), HW_ratio = round(s1["HW"] / s0["HW"], 3),
                               V_ratio = round(s1["V"] / s0["V"], 3), ok_old = s0["n_ok"], ok_new = s1["n_ok"], row.names = NULL))
}
print(out, row.names = FALSE)
if (!is.null(out)) write.csv(out, "compare_fixed_f.csv", row.names = FALSE)
