

      # source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_2d_Measurements.r")
	   labelInput  = "/home/ivan/adata8tb/PV_DB/2023_H0052A_OPP/20230615_103330/20230615_103330_MAVIC2PRO_40m"




 library(sf)
 library(geosphere)
 library(sp)
  step_cm =5
  Species = "LRG_level_2"
  PRJ=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  date1= substr(basename(labelInput),1,15)
  kmlPath = file.path(labelInput, "Predict",  paste0("SpP_",Species,"_","level#2_",date1,".kml"))
  savePth= file.path(labelInput, "Predict",  paste0("Measurements_2D_",date1,".kml"))
  ######################################
  plgns = st_read(kmlPath)
  fin=NULL
#####################################
  for (i in 1:nrow(plgns)) {
  cat("Processing seal", i, "of", nrow(plgns), "\n")
  poly <- plgns[i, ]

  centroid <- st_centroid(poly)
  lon <- st_coordinates(centroid)[1]
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m")
  # Проецируем в UTM
  poly_utm <- st_transform(poly, utm_crs)
  # 2. Получаем координаты полигона в UTM
  coords <- st_coordinates(poly_utm)[, 1:2]
    # 3. Находим главную ось через анализ главных компонент
  # Центрируем координаты
  center <- colMeans(coords)
  coords_centered <- sweep(coords, 2, center)
   # PCA для определения главной оси
  pca <- prcomp(coords_centered)
  main_axis <- pca$rotation[, 1]  # главная ось (направление длины)
   # 4. Проецируем точки на главную ось
  proj_lengths <- coords_centered %*% main_axis
  min_proj <- min(proj_lengths)
  max_proj <- max(proj_lengths)
  # Длина тюленя (в метрах)
  length_m <- max_proj - min_proj
  length_cm <- round(length_m * 100)

   # 5. Создаем точки вдоль главной оси с шагом 5 см
  step_m <- step_cm / 100
  n_steps <- floor(length_m / step_m)
  axis_points <- seq(min_proj, max_proj, length.out = n_steps + 1)
  # 6. Для каждой точки на главной оси находим ширину
  widths <- data.frame(
    position_cm = round(seq(0, length_cm, length.out = n_steps + 1)),
    width_cm = NA
  )
 ###########
   for (k in 1:length(axis_points)) {
    # Перпендикулярный вектор
    perp_vector <- c(-main_axis[2], main_axis[1])

    # Точка на главной оси
    point_on_axis <- center + axis_points[k] * main_axis

    # Создаем линию, перпендикулярную главной оси
    perp_length <- max(dist(coords))  # достаточно длинная
    perp_line_start <- point_on_axis - perp_vector * perp_length
    perp_line_end <- point_on_axis + perp_vector * perp_length



    # Создаем sf объекты для пересечения
    perp_line <- st_sfc(st_linestring(rbind(perp_line_start, perp_line_end)),
                        crs = utm_crs)


    #  poly_buffer <- st_buffer(poly_utm, 0.01)
      poly_simplified <- st_simplify(poly_utm, dTolerance = 0.03)
     if (!st_is_valid(poly_simplified)) {poly_simplified <- st_make_valid(poly_simplified)}

    # Находим пересечение с полигоном
    intersection = st_intersection(perp_line, poly_simplified)


  # plot(perp_line)
  # plot(poly_utm[1])
  # plot(intersection, add=T,col=5)

        # Берем две крайние точки
        pts <- st_coordinates(intersection)
        if (nrow(pts) >= 2) {
          # Находим две точки с максимальным расстоянием
          dist_matrix <- as.matrix(dist(pts))
          max_dist_idx <- which(dist_matrix == max(dist_matrix), arr.ind = TRUE)[1,]
          width_m <- dist_matrix[max_dist_idx[1], max_dist_idx[2]]
          widths$width_cm[k] <- round(width_m * 100)
        }

  }
 ########
 max_width_cm = round(max(widths$width_cm, na.rm=T))
 poly_name <- paste0("Seal_", "_L", length_cm, "cm_W", max_width_cm, "cm")
 poly$poly_name=  poly_name
#########################
#fn <- poly  # ваш полигон
#fn$poly_name <- poly_name
#fn$length_cm <- length_cm
#fn$max_width_cm <- max_width_cm

# Добавляем транспонированные ширины как атрибуты (опционально)
#for(v in 1:nrow(widths)) {
#  col_name <- paste0("pos_", round(widths$position_cm[v]), "cm")
#  fn[[col_name]] <- widths$width_cm[v]
#}
#####################
 fin=rbind(poly,fin)
 }

 #############################
st_write(fin, savePth,
         driver = "KML",
         delete_dsn = TRUE,
         dataset_options = c(
        "NameField=poly_name"
         ))

