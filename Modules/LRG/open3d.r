
    library(reticulate)
   open3d <- import("open3d")
   np <- import("numpy")

######################################
# Конвертер R mesh3d -> Open3D
rmesh_to_open3d <- function(r_mesh) {
  vertices <- t(r_mesh$vb[1:3, ])
  faces <- t(r_mesh$it - 1)  # Open3D использует 0-based индексацию
  
  o3d_mesh <- open3d$geometry$TriangleMesh()
  o3d_mesh$vertices <- open3d$utility$Vector3dVector(vertices)
  o3d_mesh$triangles <- open3d$utility$Vector3iVector(faces)
  
  return(o3d_mesh)
}
# Конвертер Open3D -> R mesh3d

 open3d_to_rmesh  <- function(o3d_mesh) {
  vertices <- np$array(o3d_mesh$vertices)
  faces <- np$array(o3d_mesh$triangles) + 1  # Возвращаем к 1-based
  
  r_mesh <- list(
    vb = rbind(t(vertices), 1),
    it = t(faces)
  )
  class(r_mesh) <- "mesh3d"
  
  return(r_mesh)
}

#############################################################

connect_seal_to_ground <- function(seal_mesh, ground_mesh, gap_threshold = 0.1) {
  cat("=== Объединение тюленя и грунта ===\n")
  
  
  seal_mesh =trimmed_seal
  ground_mesh =ground_mesh
  
  # Конвертируем оба меша в Open3D
  seal_o3d <- rmesh_to_open3d(seal_mesh)
  ground_o3d <- rmesh_to_open3d(ground_mesh)
  
  cat("Тюлень: вершин =", nrow(np$array(seal_o3d$vertices)), 
      ", граней =", nrow(np$array(seal_o3d$triangles)), "\n")
  cat("Грунт: вершин =", nrow(np$array(ground_o3d$vertices)), 
      ", граней =", nrow(np$array(ground_o3d$triangles)), "\n")
  
  # 1. Объединяем меши в Open3D
  combined_mesh <- combine_meshes_with_bridge(seal_o3d, ground_o3d, gap_threshold)
  
  # 2. Создаем водонепроницаемую оболочку
  cat("Создание водонепроницаемой оболочки...\n")
  watertight_mesh <- create_watertight_envelope(combined_mesh)
  
  # 3. Конвертируем обратно в R
  final_mesh_r <- open3d_to_rmesh(watertight_mesh)
  
  # 4. Проверяем результат
  cat("Итоговый меш: вершин =", ncol(final_mesh_r$vb), 
      ", граней =", ncol(final_mesh_r$it), "\n")
  
  return(final_mesh_r)
}

# Функция объединения с созданием моста между мешами
combine_meshes_with_bridge <- function(mesh1_o3d, mesh2_o3d, gap_threshold) {
  cat("Создание моста между мешами...\n")
  
  mesh1_o3d = seal_o3d
  mesh2_o3d =ground_o3d
  
  # Получаем вершины обоих мешей
  vertices1 <- np$array(mesh1_o3d$vertices)
  vertices2 <- np$array(mesh2_o3d$vertices)
  
  # Находим ближайшие точки между мешами
  cat("Поиск ближайших точек...\n")
  closest_pairs <- find_closest_point_pairs(vertices1, vertices2, gap_threshold)
  
  if (nrow(closest_pairs) == 0) {
    cat("Близких точек не найдено, создаю соединение искусственно...\n")
    closest_pairs <- create_artificial_connection(vertices1, vertices2)
  }
  
  cat("Найдено пар для соединения:", nrow(closest_pairs), "\n")
  
  # Создаем треугольники для соединения промежутка
  bridge_triangles <- create_bridge_triangles(closest_pairs, vertices1, vertices2)
  
  # Объединяем все треугольники
  triangles1 <- np$array(mesh1_o3d$triangles)
  triangles2 <- np$array(mesh2_o3d$triangles)
  
  # Смещаем индексы второго меша
  triangles2 <- triangles2 + nrow(vertices1)
  
  # Объединяем все треугольники
  all_triangles <- rbind(triangles1, triangles2, bridge_triangles)
  
  # Объединяем все вершины
  all_vertices <- rbind(vertices1, vertices2)
  
  # Создаем объединенный меш
  combined_mesh <- open3d$geometry$TriangleMesh()
  combined_mesh$vertices <- open3d$utility$Vector3dVector(all_vertices)
  combined_mesh$triangles <- open3d$utility$Vector3iVector(all_triangles)
  
  return(combined_mesh)
}

# Находим ближайшие пары точек между двумя мешами
find_closest_point_pairs <- function(vertices1, vertices2, max_distance) {

vertices1
vertices2
max_distance

  # Используем KDTree для быстрого поиска ближайших соседей
  pcd1 <- open3d$geometry$PointCloud()
  pcd1$points <- open3d$utility$Vector3dVector(vertices1)
  
  pcd2 <- open3d$geometry$PointCloud()
  pcd2$points <- open3d$utility$Vector3dVector(vertices2)
  
  # Создаем KDTree для второго меша
  kdtree <- open3d$geometry$KDTreeFlann(pcd2)
  
  pairs <- matrix(0, nrow = 0, ncol = 2)
  
  # Для каждой точки первого меша ищем ближайшую во втором
  for (i in 1:nrow(vertices1)) {
  
    result <- kdtree$search_knn_vector_3d(vertices1[i, ], 1L)
	 distances_squared <- np$array(result[[2]])
    distances <- sqrt(distances_squared)
    
    if (dist <= 1) {
      pairs <- rbind(pairs, c(i, result[[1]][1]))
    }
  }
  
  return(pairs)
}

# Создаем искусственное соединение если точек близко нет
create_artificial_connection <- function(vertices1, vertices2) {
  cat("Создание искусственного соединения...\n")
  
  # Находим центры обоих мешей
  center1 <- colMeans(vertices1)
  center2 <- colMeans(vertices2)
  
  # Находим ближайшие точки к линии между центрами
  # Для меша 1: точка ближайшая к center2
  distances1 <- apply(vertices1, 1, function(p) sqrt(sum((p - center2)^2)))
  idx1 <- which.min(distances1)
  
  # Для меша 2: точка ближайшая к center1
  distances2 <- apply(vertices2, 1, function(p) sqrt(sum((p - center1)^2)))
  idx2 <- which.min(distances2)
  
  # Создаем несколько пар для лучшего соединения
  pairs <- matrix(0, nrow = 5, ncol = 2)
  
  # Основная пара
  pairs[1, ] <- c(idx1, idx2)
  
  # Добавляем дополнительные пары вокруг
  for (i in 2:5) {
    # Находим другие точки рядом
    if (i <= nrow(vertices1) && i <= nrow(vertices2)) {
      pairs[i, ] <- c(i, i)
    }
  }
  
  return(pairs)
}

# Создаем треугольники-мостики между мешами
create_bridge_triangles <- function(pairs, vertices1, vertices2) {
  if (nrow(pairs) < 2) {
    cat("Недостаточно пар для создания моста\n")
    return(matrix(0, nrow = 0, ncol = 3))
  }
  
  triangles <- matrix(0, nrow = 0, ncol = 3)
  n1 <- nrow(vertices1)
  
  # Создаем треугольники между соседними парами
  for (i in 1:(nrow(pairs)-1)) {
    # Индексы в объединенном массиве вершин
    v1 <- pairs[i, 1] - 1      # Из первого меша (0-based)
    v2 <- pairs[i, 2] - 1 + n1 # Из второго меша со смещением (0-based)
    v3 <- pairs[i+1, 1] - 1    # Следующая точка из первого меша
    v4 <- pairs[i+1, 2] - 1 + n1 # Следующая точка из второго меша
    
    # Создаем два треугольника для четырехугольника
    triangles <- rbind(triangles, c(v1, v2, v3))
    triangles <- rbind(triangles, c(v2, v4, v3))
  }
  
  cat("Создано треугольников-мостиков:", nrow(triangles), "\n")
  return(triangles)
}