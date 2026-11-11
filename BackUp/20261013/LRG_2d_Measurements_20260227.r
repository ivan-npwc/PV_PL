
 
	  #  source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/LRG_2d_Measurements.r")
      #labelInput= "/mnt/c/Users/usato/SSL_UI/PV_UI/2024_H0590_OPP/20240916_133629/20240916_133629_MINI3PRO_20M


library(sf)
library(geosphere)
library(sp)

step_cm  <- 5
Species  <- "LRG_level_2"
PRJ      <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# === ПОРОГИ ФИЛЬТРАЦИИ FP ===
FP_L_THR  <- 0.55   # минимальная длина (м)
FP_LW_THR <- 2.0    # минимальное соотношение L/W

date1   <- substr(basename(labelInput), 1, 15)
kmlPath <- file.path(labelInput, "Predict",
                     paste0("SpP_", Species, "_level#2_", date1, ".kml"))
savePth <- file.path(labelInput, "Predict",
                     paste0("Measurements_2D_", date1, ".kml"))

plgns <- st_read(kmlPath)
fin   <- NULL
n_filtered <- 0

for (i in 1:nrow(plgns)) {
  cat("Processing seal", i, "of", nrow(plgns), "\n")
  poly <- plgns[i, ]

  centroid <- st_centroid(poly)
  lon      <- st_coordinates(centroid)[1]
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_crs  <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")

  poly_utm <- st_transform(poly, utm_crs)
  coords   <- st_coordinates(poly_utm)[, 1:2]

  center          <- colMeans(coords)
  coords_centered <- sweep(coords, 2, center)
  pca             <- prcomp(coords_centered)
  main_axis       <- pca$rotation[, 1]

  proj_lengths <- coords_centered %*% main_axis
  min_proj     <- min(proj_lengths)
  max_proj     <- max(proj_lengths)
  length_m     <- max_proj - min_proj
  length_cm    <- round(length_m * 100)

  step_m   <- step_cm / 100
  n_steps  <- floor(length_m / step_m)
  axis_points <- seq(min_proj, max_proj, length.out = n_steps + 1)

  widths <- data.frame(
    position_cm = round(seq(0, length_cm, length.out = n_steps + 1)),
    width_cm    = NA
  )

  for (k in 1:length(axis_points)) {
    perp_vector    <- c(-main_axis[2], main_axis[1])
    point_on_axis  <- center + axis_points[k] * main_axis
    perp_length    <- max(dist(coords))
    perp_line_start <- point_on_axis - perp_vector * perp_length
    perp_line_end   <- point_on_axis + perp_vector * perp_length

    perp_line        <- st_sfc(st_linestring(rbind(perp_line_start, perp_line_end)),
                               crs = utm_crs)
    poly_simplified  <- st_simplify(poly_utm, dTolerance = 0.03)
    if (!st_is_valid(poly_simplified)) {
      poly_simplified <- st_make_valid(poly_simplified)
    }

    intersection <- st_intersection(perp_line, poly_simplified)
    pts          <- st_coordinates(intersection)

    if (nrow(pts) >= 2) {
      dist_matrix  <- as.matrix(dist(pts))
      max_dist_idx <- which(dist_matrix == max(dist_matrix), arr.ind=TRUE)[1, ]
      width_m      <- dist_matrix[max_dist_idx[1], max_dist_idx[2]]
      widths$width_cm[k] <- round(width_m * 100)
    }
  }

  max_width_cm <- round(max(widths$width_cm, na.rm=TRUE))
  max_width_m  <- max_width_cm / 100
  LW_ratio     <- ifelse(max_width_m > 0, length_m / max_width_m, 0)

  # === ФИЛЬТР FP ===
  if (length_m < FP_L_THR & LW_ratio < FP_LW_THR) {
    n_filtered <- n_filtered + 1
    cat(sprintf("  -> ОТФИЛЬТРОВАН (L=%.2fm, L/W=%.2f)\n", length_m, LW_ratio))
    next
  }

  poly_name    <- paste0("Seal__L", length_cm, "cm_W", max_width_cm, "cm")
  poly$poly_name <- poly_name
  fin          <- rbind(poly, fin)
}

cat(sprintf("\nОтфильтровано полигонов: %d из %d (%.1f%%)\n",
            n_filtered, nrow(plgns), n_filtered/nrow(plgns)*100))

st_write(fin, savePth,
         driver         = "KML",
         delete_dsn     = TRUE,
         dataset_options = c("NameField=poly_name"))