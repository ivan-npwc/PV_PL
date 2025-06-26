   
     # Load required libraries
library(Rvcg)        # For 3D mesh operations
library(rgl)         # For 3D visualization
library(sf)          # For spatial data operations
library(Morpho)      # For morphological operations on 3D data
library(XML)         # For XML parsing
library(dplyr)       # For data manipulation
library(dbscan)      # For density-based clustering
library(alphashape3d) # For alpha shape calculations
#library(EBImage)     # For image processing
# Set coordinate reference system (CRS) and offset values
crs <- 32610         # UTM zone 10N coordinate system
E <- 430000          # Easting offset
N <- 5451000         # Northing offset
A <- -100            # Altitude offset (not currently used)

# Define input directory path
labelInput <- "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"

# Set species and extract date from directory name
Species <- "LRG"
date1 <- substr(basename(labelInput), 1, 15)
MeshDir = paste0(labelInput, "\\Predict\\MeshCrop")
MeshLst=list.files(MeshDir, full.names=T)
##############################################
pl=function(mesh,col=1,alpha = 0.5){shade3d(mesh, color=col, alpha=alpha); bbox_lines <- rgl::bbox3d(color = "gray")}
##############################################################################  mesh_surface_reconstract
   mesh_surface_reconstract_alpha=function(mesh){
   vertices <- vert2points(mesh)
   bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты
   # Генерация случайных точек в bounding box
   n_points <- 100000  # Желаемое количество точек
  random_points <- matrix(
  runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
  ncol = 3,
  byrow = TRUE
   )
 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
  closest <- vcgClostKD(
  x = random_points,
  mesh = mesh,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения
  )
 
       inside_points <- random_points[closest$quality > -0.01 &  closest$quality < 0.01 , ]
       reconstructed_mesh <- ashape3d(inside_points, alpha = 0.1)
	   reconstructed_mesh <- as.mesh3d(reconstructed_mesh) 
	   return(reconstructed_mesh)
}
###############################################################################  CREATE ALPHA SHAPE 
   # Extract boundary edges and vertices
   mesh_to_alpha_botton=function(mesh, alpha = 0.4,filter = T){
   boundary_edges <- Rvcg::vcgBorder(mesh)[[1]]
   border_vertices <- vert2points(mesh)[boundary_edges, ]
   clusters <- dbscan(border_vertices, eps = 0.1, minPts = 3)$cluster
   # Считаем размер каждого кластера
   cluster_sizes <- table(clusters)
   # Исключаем самый большой кластер (внешний край)
    main_cluster <- as.integer(names(which.max(cluster_sizes)))
     small_holes <- clusters != main_cluster & clusters != 0  # 0 — шум в DBSCAN
if (filter==T){br_vr=border_vertices[!small_holes,]} else {br_vr=border_vertices}
   # Create alpha shape from boundary vertices
   alpha_shape <- ashape3d(br_vr, alpha =alpha)
   alpha_shape <- as.mesh3d(alpha_shape)
return(alpha_shape)
}
######################################################################################   ALPHA SHAPE TO SURFACE
  alfa_to_surface=function(alpha_shape){
vertices <- t(alpha_shape$vb[1:3, ])  # Nx3 матрица координат
### 2. Находим минимальные Z для каждой (X, Y)
# Округляем X и Y до 3 знаков, чтобы избежать шума
rounded_xy <- round(vertices[, 1:2], digits = 3)
# Группируем по (X, Y) и находим минимальный Z в каждой группе
unique_xy <- unique(rounded_xy)
min_z_points <- matrix(nrow = nrow(unique_xy), ncol = 3)
for (i in 1:nrow(unique_xy)) {
  x <- unique_xy[i, 1]
  y <- unique_xy[i, 2]
  mask <- (rounded_xy[, 1] == x) & (rounded_xy[, 2] == y)
  min_z <- min(vertices[mask, 3])
  min_z_points[i, ] <- c(x, y, min_z)
}
### 3. Строим новую поверхность (триангуляция Делоне)
delaunay_tri <- delaunayn(min_z_points[, 1:2])  # Триангуляция в 2D
# Преобразуем в формат mesh3d
new_mesh <- list(
  vb = rbind(t(min_z_points), 1),  # Вершины (в однородных координатах)
  it = t(delaunay_tri),            # Грани (триангуляция)
  normals = NULL
)
class(new_mesh) <- "mesh3d"
#####
# Получаем вершины (X, Y, Z)
vertices <- t(new_mesh$vb[1:3, ])
x <- vertices[, 1]
y <- vertices[, 2]
z_original <- vertices[, 3]  # Исходные высоты
#####
# Функция для скользящего среднего (можно заменить на сплайн или гауссово сглаживание)
smooth_z <- function(z, window_size = 5) {
  n <- length(z)
  smoothed_z <- numeric(n)
  for (i in 1:n) {
    start <- max(1, i - window_size)
    end <- min(n, i + window_size)
    smoothed_z[i] <- mean(z[start:end])
  }
  return(smoothed_z)
}
# Сглаживаем Z
z_smoothed <- smooth_z(z_original, window_size = 15)
###
# Создаём новый меш (X и Y остаются прежними)
smoothed_surface <- new_mesh
smoothed_surface$vb[3, ] <- z_smoothed  # Обновляем только Z
return(smoothed_surface)
}
####################################################################### mesh_cut_by_mesh
  mesh_cut_by_mesh=function(mesh_1, mesh_2){
 # Вычисляем расстояние от каждой вершины mesh1 до mesh2
 
 #  mesh_1=SlUp1
  # mesh_2= SlDown3

   distances <- vcgClost(mesh_1, mesh_2,  sign=TRUE, borderchk = T)$quality
   q025=quantile(distances,0.25)
   q075=quantile(distances,0.75)
  # Определяем вершины для удаления (ниже поверхности mesh2)
   keep_vertices <- which(distances < q075 & distances > q025)  # Оставляем только внешние точки
   trimmed_mesh <- rmVertex(mesh_1, keep_vertices)
   trimmed_mesh=vcgIsolated(trimmed_mesh)
   # pl(trimmed_mesh)

   return(trimmed_mesh)
  
}
############################################
 mesh_cut_by_convhulln=function(mesh_1){
  vertices <- t(mesh_1$vb[1:3,])# Получаем вершины меша
  hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
  convex_mesh <- list(vb=mesh_1$vb, it=t(hull))# Создаем новый меш
  class(convex_mesh) <- "mesh3d"
  cleanMwsh <- vcgClean(convex_mesh, sel = c(1,2,3,4,5,6,7), iterate = TRUE)
  mesh_cut <- vcgClost(mesh_1,cleanMwsh, sign = TRUE)
  return(mesh_cut)
}
############################################################################# изьятия боков ларги и построение выпуклой оболочки на этой основе.
   surface_to_solid_mesh = function(mesh){
      normals <- mesh$normals  # Nx3
     # Вычисляем угол между нормалью и вертикалью (ось Z)
      angles <- acos(abs(normals[3,]))  # угол в радианах
    # Оставляем только те точки, где нормаль почти вертикальна (т.е. не пол)
     threshold_angle <- pi / 5#6 # 4  # 45 градусов
     valid_indices <- which(angles < threshold_angle)
     filtered_mesh <- rmVertex(mesh, index = valid_indices)
     tm <- vcgIsolated(filtered_mesh) 
  vertices <- t(tm$vb[1:3,])# Получаем вершины меша
  hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
  solid_mesh <- list(vb=tm$vb, it=t(hull))# Создаем новый меш
  class(solid_mesh) <- "mesh3d"
  solid_mesh = vcgUpdateNormals(solid_mesh)
  return(solid_mesh)
  }
 #######################################################################
   Get_Top = function(mesh){
      
	#  mesh=SlUp1
      normals <- mesh$normals  # Nx3
     # Вычисляем угол между нормалью и вертикалью (ось Z)
      angles <- acos(abs(normals[3,]))  # угол в радианах
    # Оставляем только те точки, где нормаль почти вертикальна (т.е. не пол)
     threshold_angle <- pi / 5#6 # 4  # 45 градусов
     valid_indices <- which(angles < threshold_angle)
     filtered_mesh <- rmVertex(mesh, index = valid_indices)
     tm <- vcgIsolated(filtered_mesh) 
	 distances <- vcgClost( mesh, tm,  sign=TRUE, borderchk = T)$quality
  # Определяем вершины для удаления 
   keep_vertices <- which(distances == 0)
   trimmed_mesh <- rmVertex(mesh, keep_vertices)
   trimmed_mesh=vcgIsolated(trimmed_mesh)
 #  pl(trimmed_mesh)
   return(trimmed_mesh)
  }
 #####################################################################
   mesh_reconstract = function(mesh,SampleNum = 10000, type = "pd",radius = 0, clustering = 0.6, angle = 180){
    # 1. Генерируем плотное облако точек на поверхности
    samples <- vcgSample(mesh, SampleNum = SampleNum, type = type)
    msh <- vcgBallPivoting(
                        x=samples,
                        radius = radius,
                        clustering = clustering,
                        angle = angle,
                       deleteFaces = FALSE
                       )
	
		return(msh)
				   
	}	
######################################################################### 
is_watertight <- function(mesh) {
  # Находим все граничные ребра
  edges <- vcgGetEdge(mesh, unique = FALSE)
  border_edges <- edges[edges$border == 1, ]
  
  # Если нет граничных ребер - меш водонепроницаем
  if (nrow(border_edges) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
##########################################################
mesh_dilate=function(mesh,factor=0.4, iter=1){
# Сначала увеличиваем, потом сглаживаем
inflated_mesh <- mesh
inflated_mesh$vb[1:2,] <- mesh$vb[1:2,] + mesh$normals[1:2,] * factor

# Сглаживаем для устранения артефактов
smoothed_mesh <- vcgSmooth(inflated_mesh, type = "laplace", iteration = 3)
return(smoothed_mesh)
}
#################################################################
adjust_mesh_elevation <- function(base_mesh,      # Базовый меш (эталон)
                                 mesh_to_adjust,  # Меш для корректировки
                                 crs=32610,             # Система координат
                                 smooth_iter = 65,# Число итераций сглаживания
                                 radius = 0.05,   # Радиус для Ball Pivoting
                                 z_scale = 1) { # Коэффициент коррекции высоты
  

    # base_mesh=SlUp2
   #  mesh_to_adjust = mrg4
  
  # 1. Предварительное сглаживание исходного меша
  #smoothed_mesh <- vcgSmooth(mesh_to_adjust, type = "laplace", iteration = 10)
  
  # 2. Извлекаем вершины всех мешей
 # base_verts <- vert2points(base_mesh)     # Вершины базового меша (эталон)
  adjust_verts <- vert2points(mesh_to_adjust) # Вершины корректируемого меша

  # 4. Создаем пространственные объекты для анализа пересечений
  # Преобразуем базовый меш в пространственные точки (только X,Y)
  

  
  base_sf <- st_as_sf(data.frame(x = base_mesh$vb[1,],  y = base_mesh$vb[2,],  z=  base_mesh$vb[3,]),
                      coords = c("x", "y","z"), crs= 32610)
  
  # Преобразуем корректируемый меш в пространственные точки (X,Y,Z)
  adjust_sf <- st_as_sf(data.frame(x = mesh_to_adjust$vb[1,],
                                   y = mesh_to_adjust$vb[2,],
                                   z = mesh_to_adjust$vb[3,]),
                        coords = c("x", "y","z"),
                        crs = 32610)
  
  # 5. Создаем выпуклую оболочку базового меша
  base_hull <- st_convex_hull(st_union(base_sf))
  buffered <- st_buffer(base_hull, dist = 0.05)
  
  # 6. Находим точки корректируемого меша внутри выпуклой оболочки
  points_inside <- st_intersects(adjust_sf, buffered, sparse = FALSE)
  points_inside1 = adjust_verts[points_inside,]
  meanalt = mean(points_inside1[,3])
  
  UPpoints_inside1 = points_inside1[points_inside1[,3]>meanalt,]
 # DOWNpoints_inside1 = points_inside1[points_inside1[,3]<meanalt,]
  
  # Upmesh <- vcgBallPivoting(UPpoints_inside1, radius = 0.05)
  #  Dpmesh <- vcgBallPivoting(DOWNpoints_inside1, radius = 0.05)
 # conectM = connect_meshes(Upmesh, Dpmesh, n_samples=100)

 # points3d(UPpoints_inside1,col=2)
 # pl(sl,4,0.5)
  
  
  AltitudeUp = mean(base_mesh$vb[3,]) 
  AltitudeDown= mean(UPpoints_inside1[,3])
  
  z_diff <- abs(abs(AltitudeUp)-abs(AltitudeDown))
  
  
  # 7. Корректируем высоту только для внутренних точек
  adjust_verts[points_inside, 3] <- adjust_verts[points_inside, 3] + (z_diff * z_scale)
  
  # 8. Создаем скорректированный меш
  adjusted_mesh <- mesh_to_adjust
  adjusted_mesh$vb[1:3,] <- t(adjust_verts)  # Обновляем вершины
  
  # 9. Обновляем нормали и применяем постобработку
  adjusted_mesh <- vcgUpdateNormals(adjusted_mesh)
  adjusted_mesh <- vcgSmooth(adjusted_mesh, type = "laplace", iteration = smooth_iter)
  
  # 10. Очищаем меш от артефактов
  adjusted_mesh <- vcgClean(adjusted_mesh, sel = 1:7, iterate = TRUE)
  
  # 11. Создаем окончательную поверхность (опционально)
  final_mesh <- vcgBallPivoting(adjusted_mesh, radius = radius)
  
  return(final_mesh)
}
####################################################################
mesh_reconstract_by_mesh <- function(base_mesh,      # Базовый меш (эталон)
                                     mesh_to_adjust,  # Меш для корректировки
                                     crs=32610) {        # Система координат
 
    base_mesh=SlUp2
   mesh_to_adjust = mrg4
  

  adjust_verts <- vert2points(mesh_to_adjust) # Вершины корректируемого меша

  base_sf <- st_as_sf(data.frame(x = base_mesh$vb[1,],  y = base_mesh$vb[2,],  z=  base_mesh$vb[3,]),
                      coords = c("x", "y","z"), crs= 32610)
  
  # Преобразуем корректируемый меш в пространственные точки (X,Y,Z)
  adjust_sf <- st_as_sf(data.frame(x = mesh_to_adjust$vb[1,],
                                   y = mesh_to_adjust$vb[2,],
                                   z = mesh_to_adjust$vb[3,]),
                        coords = c("x", "y","z"),
                        crs = 32610)
  
  # 5. Создаем выпуклую оболочку базового меша
  base_hull <- st_convex_hull(st_union(base_sf))
  buffered <- st_buffer(base_hull, dist = 0.07)
  
  # 6. Находим точки корректируемого меша внутри выпуклой оболочки
  points_inside <- st_intersects(adjust_sf, buffered, sparse = FALSE)
  points_inside1 = adjust_verts[points_inside,]
    pointsmesh <- vcgBallPivoting(points_inside1, radius = 0.05)   
 #pl(sl,2,0.4); points3d(points_inside1)
  
  alpha_shape <- ashape3d(points_inside1, alpha =0.5)
  alpha_shape <- as.mesh3d(alpha_shape)  
 
    # 1. Генерируем плотное облако точек на поверхности
    samples <- vcgSample(alpha_shape, SampleNum = 10000, type =  "pd")
    
	msh <- vcgBallPivoting(
                        x=samples,
                        radius = 0,
                        clustering =  0.6,
                        angle = 180,
                       deleteFaces = FALSE
                       )
	
		return(msh)
}
#####################################################################
sl = NULL
sl2 = NULL
sl3 = NULL
sl4 = NULL
alpha_botton = NULL
SlDown = NULL
SlDown1 = NULL
SlDown2 = NULL
SlDown3 = NULL
SlUp1 = NULL
SlUp2 = NULL
SlUp3 = NULL
mrg = NULL
mrg1 = NULL
mrg2 = NULL
mrg3 = NULL
mrg4 = NULL
mrg5 = NULL
 
 for (i in 1:length(MeshLst)){
  slpth=  MeshLst[i]
 flsize= file.size(MeshLst[i])
  if (flsize < 90000) {unlink(slpth)} else {
  sl = vcgImport(slpth)
  
  sl2=vcgIsolated(sl)
  sl3 <- vcgUpdateNormals(sl2)
  
   open3d()
   shade3d(mesh)
   writePLY(slpth)
   close3d()
  #sl4 <- vcgSmooth(sl3)
  }
  }
  alpha_botton = mesh_to_alpha_botton(sl4)
  
  SlDown = vcgClost(alpha_botton, sl4)
  SlDown1 = alfa_to_surface(SlDown)
  SlDown2 <- vcgClean(SlDown1, sel = c(1,2,3,4,5,6,7), iterate = TRUE)
  SlDown3 = vcgSmooth(SlDown2)
  
  
   SlUp1 =  mesh_cut_by_convhulln(sl4)
   SlUp2= Get_Top(SlUp1)
   SlUp3= mesh_dilate(SlUp2)

   mrg = mergeMeshes (SlUp2,SlDown3)
   mrg = vcgUpdateNormals(mrg)
   mrg1 = mesh_to_alpha_botton(mrg,alpha = 0.6,filter = F)
   

   mrg2  = mesh_reconstract(mrg1)
   mrg3  = vcgClean(mrg2, sel = c(1,2,3,4,5,6,7), iterate = TRUE)
   mrg4 = vcgSmooth(mrg3, "laplace", iteration = 75) #HClaplace
   mrg5 = mesh_reconstract_by_mesh(base_mesh=SlUp2, mesh_to_adjust = mrg4)

###########################################################################################	

 
  if (is_watertight(mrg5)==T){ vcgVolume(seal1)}

  pl(sl,2,0.5)
  pl(mrg5,4,0.5)
 
 pl(mrg,4,0.5)
 


draft=function(){
    mrg6 = ashape3d(t(mrg4_adjusted$vb[1:3,]),alpha=0.9)
   volume_ashape3d (mrg5)

boundary_points <- Rvcg::vcgNonBorderEdge(combined_mesh)[[1]]
border_vertices <- vert2points(combined_mesh)[boundary_points, ]
# Create alpha shape from boundary vertices
alpha_shape <- ashape3d(border_vertices, alpha = 0.4)
closed_mesh <- as.mesh3d(alpha_shape)


 nmsh= vcgBallPivoting(border_vertices, radius = 0.2)


     shade3d(trimmed_mesh, color = "gray", alpha = 0.8)
	 shade3d(mesh1, col = "red", size = 7, alpha = 0.3)
	  points3d(border_vertices, col = "red", size = 7, alpha = 0.8)
	 
	 
	 
	 
	shade3d(shell_mesh, color = "gray", alpha = 0.4)
	
	 bbox_lines <- rgl::bbox3d(color = "gray")
	 
	 
    shade3d(cleanMwsh, color = "gray", alpha = 0.5)
    
 
	 shade3d(cleanMwsh, color = "lightblue", alpha = 0.5)
	    points3d(insidepoints, col = "red", size = 7, alpha = 0.2)
 vertices <- t(trimmed_mesh$vb[1:3,])# Получаем вершины меша
  hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
  convex_mesh <- list(vb=trimmed_mesh$vb, it=t(hull))# Создаем новый меш
  class(convex_mesh) <- "mesh3d"
  cleanMwsh <- vcgClean(convex_mesh, sel = c(1,2,3,4,5,6,7), iterate = TRUE)




 vertices <- vert2points(cleanMwsh)
   bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты
# Генерация случайных точек в bounding box
n_points <- 1000000  # Желаемое количество точек
random_points <- matrix(
  runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
  ncol = 3,
  byrow = TRUE
)
 
 kdtree <- vcgCreateKDtree(cleanMwsh)
 
 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
closest <- vcgClostKD(
  x = random_points,
  mesh = cleanMwsh,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения

)
 
 inside_points <- random_points[closest$quality < -0.001 ,]  #&  closest$quality < 0.001 , ]

 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
closest <- vcgClostKD(
  x = inside_points,
  mesh = ground,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения

)

 insidepoints <- inside_points[closest$quality < -0.001 ,]  #&  closest$quality < 0.001 , ]

#################################################
normals <- mesh5$normals  # Nx3
# Вычисляем угол между нормалью и вертикалью (ось Z)
angles <- acos(abs(normals[3,]))  # угол в радианах
# Оставляем только те точки, где нормаль почти вертикальна (т.е. не пол)
threshold_angle <- pi / 5#6 # 4  # 45 градусов
valid_indices <- which(angles < threshold_angle)
filtered_mesh <- rmVertex(mesh5, index = valid_indices)
mesh6 <- vcgIsolated(filtered_mesh) 
##################################################### 
  vertices <- t(tm$vb[1:3,])# Получаем вершины меша
  hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
  convex_mesh <- list(vb=tm$vb, it=t(hull))# Создаем новый меш
  class(convex_mesh) <- "mesh3d"
  cleanMwsh <- vcgClean(convex_mesh, sel = c(1,2,3,4,5,6,7), iterate = TRUE)



     shade3d(mesh12, color = "lightblue", alpha = 0.8)
	 shade3d(trimmed_mesh, color = "red", alpha = 0.3)
	 
	 bbox_lines <- rgl::bbox3d(color = "gray")
     vcgVolume(mesh12)
     vcgArea(cleanMwsh)
  


boundary_points <- Rvcg::vcgBorder(mesh5)[[1]]
border_vertices <- vert2points(mesh5)[boundary_points, ]

# PCA для анализа структуры границ
#pca <- prcomp(border_vertices)

# Применяем DBSCAN (подбираем eps в зависимости от масштаба меша)
clusters <- dbscan(border_vertices, eps = 0.1, minPts = 3)$cluster
# Визуализация кластеров (каждый кластер — разный цвет)
plot3d(border_vertices, col = clusters + 1, size = 5)

# Считаем размер каждого кластера
cluster_sizes <- table(clusters)
# Исключаем самый большой кластер (внешний край)
main_cluster <- as.integer(names(which.max(cluster_sizes)))
small_holes <- clusters != main_cluster & clusters != 0  # 0 — шум в DBSCAN


valid_indices=small_holes
filtered_mesh <- rmVertex(mesh5, index = valid_indices)
mesh6 <- vcgIsolated(filtered_mesh) 



 shade3d(closed_mesh , color = "lightblue", alpha = 0.9)
   points3d(pca$x, col = "red", size = 7)
   bbox_lines <- rgl::bbox3d(color = "gray")

#mesh5 <- vcgUpdateNormals(mesh4)
#mesh12 <- vcgBallPivoting(filtered_mesh, radius = 0.05)
#mesh13 <- vcgSmooth(mesh12, "HClaplace", iteration = 5)
#mesh14 <- vcgClean(mesh13, sel = c(1,2,3,4,5,6,7), iterate = TRUE)
#mesh15 <- vcgUpdateNormals(mesh14)
 # cleanMwsh <- vcgClean(convex_mesh, sel = c(1,2,3,4,5,6,7), iterate = TRUE)
 # cleanMwsh <- vcgUpdateNormals(cleanMwsh)
###############################################
# mrg <- vcgClost(mesh1, mesh14, sign=TRUE, borderchk = T)
# distances = mrg$quality
 

  # Определяем вершины для удаления (ниже поверхности mesh2)
   keep_vertices <- which(distances > 0)  # Оставляем только внешние точки
   trimmed_mesh <- rmVertex(mesh1, keep_vertices)
   

  # Удаляем артефакты
 #  seal4 <- vcgClean(trimmed_mesh, sel=0)  # Удаление несвязных компонент
   seal5= vcgSmooth(seal4)


   vcgVolume(cleanMwsh)
   vcgArea(cleanMwsh)
  


     vcgWrlWrite(mesh1,"mesh1.obj")
	 
#mesh2=mesh1# <- vcgUpdateNormals(mesh1)  # Update mesh normals

# Extract boundary edges and vertices
boundary_edges <- Rvcg::vcgBorder(mesh14)[[1]]
border_vertices <- vert2points(mesh14)[boundary_edges, ]

# Create alpha shape from boundary vertices
alpha_shape <- ashape3d(border_vertices, alpha = 0.4)
closed_mesh <- as.mesh3d(alpha_shape)

# Calculate signed distances from mesh to alpha shape
ground <- vcgClost(mesh14, closed_mesh, sign = TRUE)
botton <- vcgSmooth(ground, "HClaplace", iteration = 30)  # Smooth the bottom surface


# Convert bottom mesh back to CRS coordinates
x <- botton$vb[1,] + E
y <- botton$vb[2,] + N
botton_crs <- st_as_sf(data.frame(x = x, y = y, z = 1), 
                      coords = c("x", "y", "z"), crs = crs)

# Create convex hull of bottom points and find points inside
polygon <- st_convex_hull(st_union(botton_crs))
inside <- st_intersects(points_inside, polygon, sparse = FALSE)
pointsinside <- points_inside[inside,]

# Convert to local coordinates again
coords <- st_coordinates(pointsinside)
coords[,1] <- coords[,1] - E
coords[,2] <- coords[,2] - N

# Create seal mesh from filtered points
seal <- vcgBallPivoting(coords, radius = 0.05)
seal2 <- vcgUpdateNormals(seal)

 # Вычисляем расстояние от каждой вершины mesh1 до mesh2
  distances <- vcgClost(seal2, botton, sign=TRUE, borderchk = T)$quality
  # Определяем вершины для удаления (ниже поверхности mesh2)
   keep_vertices <- which(distances > 0)  # Оставляем только внешние точки
   trimmed_mesh <- rmVertex(seal2,keep_vertices)
  # Удаляем артефакты
 #  seal4 <- vcgClean(trimmed_mesh, sel=0)  # Удаление несвязных компонент
   seal5= vcgSmooth(seal4)

 vcgWrlWrite(cleanMwsh,"cleanMwsh.obj")
################################################################################
# Проверяем, какие вершины mesh1 попадают внутрь проекции mesh2
		 inside_points <- vcgClost(seal,botton,borderchk=T,barycentric=T) #
         combined_mesh <- mergeMeshes(seal, inside_points)
 
 
       # Создаем облако точек из объединенного меша
       combined_points <- t(combined_mesh$vb[1:3, ])

    # Вычисляем выпуклую оболочку или реконструируем поверхность
   # Увеличьте точность реконструкции
     watertight_mesh <- vcgBallPivoting(combined_points, 
                                 radius =0.09,  # Несколько радиусов
                                 clustering = 0.1)
	 Smoor=vcgSmooth(watertight_mesh)	
 
	
   
		     smMwsh=vcgSmooth(cleanMwsh, iteration = 5)							 
watertight_mesh <- vcgQEdecim(watertight_mesh, percent = 0.3)  # Simplify by 70%


normals <- mesh2$normals  # Nx3
# Вычисляем угол между нормалью и вертикалью (ось Z)
angles <- acos(abs(normals[3,]))  # угол в радианах
# Оставляем только те точки, где нормаль почти вертикальна (т.е. не пол)
threshold_angle <- pi / 6 # 4  # 45 градусов
valid_indices <- which(angles < threshold_angle)

filtered_mesh <- rmVertex(mesh2, index = valid_indices)
mesh12 <- vcgBallPivoting(filtered_mesh, radius = 0.05)
shade3d(mesh12, color = "gray", alpha = 0.5)

vcgPlyWrite(mesh2, "mesh2.ply")




watertight_mesh <- vcgQEdecim(closed_seal, percent = 0.3)  # Simplify by 70%
smoorfic <- vcgSmooth(watertight_mesh, "HClaplace", iteration = 10)  # Smooth the bottom surface
shade3d(smoorfic, color = "gray", alpha = 0.5)





shade3d(uniform, color = "gray", alpha = 0.5)
shade3d(seal2, color = "red", alpha = 0.5)

vcgVolume(uniform_points)  # Calculate volume of the mesh






mesh3 <- vcgClean(closed_seal, sel = c(1,2,3,4,5,6,7), iterate = TRUE)

# Simplify mesh and calculate volume
watertight_mesh <- vcgQEdecim(mesh3, percent = 0.3)  # Simplify by 70%
vcgVolume(watertight_mesh)  # Calculate volume of the mesh

shade3d(watertight_mesh, color = "gray", alpha = 0.9)
shade3d(botton, color = "red", alpha = 0.9)
# Additional visualization (commented out)
# shade3d(watertight_mesh, color = "gray", alpha = 0.4)
# bbox_lines <- rgl::bbox3d(color = "gray")
# points3d(inside_points, col = "red", size = 2)
   
 
	
	 vcgVolume(watertight_mesh) 


	
	#нужно теперь вершины перенести на округлённого с оригиального при этом брать только те вершины, что в пределах округлённого
	mesh = watertight_mesh
	# Используем PCA для определения ориентации меша
    pca <- prcomp(t(mesh$vb[1:3,]))
    # Основная ось (допустим, Z соответствует вертикали)
    main_axis <- pca$rotation[,3]
    # Проекция всех вершин на основную ось
    projections <- t(mesh$vb[1:3,]) %*% main_axis
    # Автоматическое определение точки обрезки
    library(diptest)
    dip_test <- dip.test(projections)
    cutoff <- quantile(projections, probs = 0.5) # или используйте более сложные методы

    # Обрезка
    keep_vertices <- projections > cutoff

    kp=data.frame(keep_vertices)
    index = as.numeric(c(row.names(kp)[kp$keep_vertices==F]))
 
   clean_mesh <- rmVertex(mesh,index)
   shade3d(clean_mesh, color = "gray", alpha = 0.6)
	




 clean_mesh <- vcgClean(clean_mesh, sel = c(1,2,3,4,5,6,7), iterate = TRUE)

shade3d(clean_mesh, color = "gray", alpha = 0.6)
vcgIsolated(clean_mesh)
	
	
	
shade3d(test, col="gray", alpha=0.7)
shade3d(mesh3, col="red", alpha=0.7)
	
	
	
	
	shade3d(closed_seal, color = "red", alpha = 0.4)
	points3d(border_vertices, col = "red", size = 7)
	bbox_lines <- rgl::bbox3d(color = "gray")
	
volume_ashape3d(alpha_shape) # volume of ground
 vcgVolume(closed_seal)            # volume seal with ground
	
	# Объединение вершин и граней
   combined_mesh <- list(
  vb = cbind(closed_mesh$vb, mesh2$vb), # Объединение вершин
  it = cbind(closed_mesh$it, mesh2$it + ncol(closed_mesh$vb)) # Объединение граней с коррекцией индексов
)
   # Преобразование в класс mesh3d
    class(combined_mesh) <- "mesh3d"
    # Очистка меша (удаление дубликатов и нереферентных вершин)
    mesh4 <- vcgClean(combined_mesh, sel = c(1:7), iterate = TRUE)
	shade3d(mesh4 , color = "lightblue", alpha = 0.5)
		bbox_lines <- rgl::bbox3d(color = "gray")
	vcgVolume(mesh4) 

	
	
    shade3d(mesh4 , color = "lightblue", alpha = 0.5)
   vertices <- vert2points(mesh3)
   bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты

# Генерация случайных точек в bounding box
n_points <- 1000000  # Желаемое количество точек
random_points <- matrix(
  runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
  ncol = 3,
  byrow = TRUE
)
 
 kdtree <- vcgCreateKDtree(mesh3)
 
 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
closest <- vcgClostKD(
  x = random_points,
  mesh = mesh3,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения

)
 
 inside_points <- random_points[closest$quality > -0.001 &  closest$quality < 0.001 , ]

 shade3d(mesh4, color = "lightblue", alpha = 0.9)


# Меш (прозрачный)
shade3d(mesh3, color = "lightblue", alpha = 0.4)
# Ограничивающий бокс (для проверки)
bbox_lines <- rgl::bbox3d(color = "gray")
# Точки внутри меша
points3d(inside_points, col = "red", size = 2)

 
 
 
# Определяем размеры ограничивающего бокса
verts <- t(mesh3$vb[1:3,])
bbox <- apply(verts, 2, range)  # min и max по X,Y,Z

# Задаем разрешение сетки (например, 100x100x100)
grid_res <- 100

# Преобразуем меш в логическую 3D-матрицу (TRUE = внутри меша)
grid <- meshToGrid(
  mesh3,
  nx = grid_res, ny = grid_res, nz = grid_res,
  xmin = bbox[1,1], xmax = bbox[2,1],
  ymin = bbox[1,2], ymax = bbox[2,2],
  zmin = bbox[1,3], zmax = bbox[2,3]
)
 

 
 library(misc3d)

# Вокселизация меша
grid <- meshToGrid(mesh, nx = 50, ny = 50, nz = 50)

# Находим внешние воксели (через морфологическую эрозию)
library(EBImage)
grid_bin <- as.Image(grid)
grid_eroded <- erode(grid_bin, kern = makeBrush(3, shape = "box"))
outer_shell <- grid & !grid_eroded

# Преобразуем обратно в меш
verts <- which(outer_shell, arr.ind = TRUE)
mesh_shell <- vcgDelaunay(verts) # Триангуляция

# Визуализация
shade3d(mesh_shell, col = "red", alpha = 0.5)
wire3d(mesh, col = "gray") # Исходный меш для сравнения
 

 
 
 uniform_points <- vcgUniformRemesh(
  mesh3,
  voxelSize = 0.01,  # Размер вокселя (меньше = плотнее сетка)
  discretize = TRUE  # Создавать дискретные точки
)
 # Визуализация
shade3d(mesh3, color = "blue", alpha = 0.4)
points3d(t(uniform_points$vb[1:3,]), col = "green", size = 3, alpha = 0.4)
 bbox_lines <- rgl::bbox3d(color = "gray")
 
 
 
 
 
 
 
 
 
# Находим ближайшие точки на меше и их нормали
closest <- vcgClost(random_points, mesh3, sign = TRUE)

# Точки с отрицательным знаком лежат внутри меша
internal_points <- random_points[closest$quality > 0, ]

# Визуализация
library(rgl)
shade3d(mesh3, col = "lightblue", alpha = 0.2)
points3d(internal_points, col = "red", size = 2)



shade3d(random_points, col = "lightblue", alpha = 0.2)
 hade3d(NewShape1 , color = "lightblue")
volume_ashape3d(alpha_shape)
 vcgVolume(mesh3)
  
  

  mesh2 <- vcgUpdateNormals(seal_mesh)
 mesh1 <- vcgBallPivoting(seal_mesh, radius = 0.05)
  shade3d(seal_mesh , color = "lightblue")


  
  mesh4 <- vcgIsolated(mesh3)
  mesh5 <- vcgUpdateNormals(mesh4)

  mesh4=vcgClean(mesh3,sel=0:6,iterate=TRUE)
  
   mesh7 <- vcgUpdateNormals(mesh6)
   
  shade3d(mesh7 , color = "lightblue")
  
  
  vcgVolume(mesh3)
  
 ############################################################# 
  
#  open3d()
  
####
vcgBallPivoting
mesh=Seal_mesh
# Построение графа смежности
adj_matrix <- vcgVertexNeighbors(mesh) |> 
  as_adjacency_matrix()


graph_cut <- min_cut(
  graph_from_adjacency_matrix(adj_matrix),
  source = which.min(coords[,3]), # Нижние точки как "источник"
  target = which.max(coords[,3])  # Верхние точки как "сток"
)




################  
vertices <- t(Seal_mesh$vb[1:3,])# Получаем вершины меша
hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
convex_mesh <- list(vb=Seal_mesh$vb, it=t(hull))# Создаем новый меш
class(convex_mesh) <- "mesh3d"
shade3d(convex_mesh,  color = "lightblue")# Визуализация
   vcgVolume(convex_mesh)
   vcgArea(convex_mesh)
#####################################   

   
 ############################################################## 


#  doc <- htmlParse(doc_pth)
 # leftOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//left")))
 # topOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//top")))
 # rightOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//right")))
 # bottomOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//bottom")))
  
  # crs= xmlValue(getNodeSet(doc, "//crs"))
  
#  Width_mesh=  abs(min(site_3d_local$vb[1,])) + abs(max(site_3d_local$vb[1,]))
#   Hight_mesh=  abs(min(site_3d_local$vb[2,]))+ abs(max(site_3d_local$vb[2,]))
   
   #Width_mesh_gps=  max(site_3d_gps$vb[1,]) - min(site_3d_gps$vb[1,])
  # Hight_mesh_gps=  max(site_3d_gps$vb[2,]) - min(site_3d_gps$vb[2,])
   
 #  Width_OPP = rightOPP - leftOPP
 #  Hight_OPP= topOPP -bottomOPP
   
  # LagCRS1 = 430437.7+79.5   #  430517.2
 #  LagCRS2 = 5451033+35.2    # 5451068
   
   # widthNorm = site_3d_local$vb[1,] + abs(min(site_3d_local$vb[1,]))
	#hightNorm = site_3d_local$vb[2,] + abs(min(site_3d_local$vb[2,]))
	
   # width_proportion = widthNorm/Width_mesh
  #  hight_proportion = hightNorm/Hight_mesh

	 
	#width_convert= min(site_3d_gps$vb[1,]) +  (Width_mesh_gps * width_proportion)
	#hight_convert=  min(site_3d_gps$vb[2,]) +  (Hight_mesh_gps * hight_proportion)
	
#	width_convert= leftOPP +  (Width_OPP * width_proportion)
#	hight_convert=  bottomOPP +  (Hight_OPP * hight_proportion)
	
  # LagCRS1 = leftOPP+Width_mesh + 0.7627411   #  430437.7 Width_mesh
  # LagCRS2 = bottomOPP+Hight_mesh +  12.01926    #5451033 Hight_mesh
   
 #  site_3d_crs=site_3d_local
#   site_3d_crs$vb[1,] =width_convert #site_3d_crs$vb[1,] + LagCRS1
 #  site_3d_crs$vb[2,] =hight_convert #site_3d_crs$vb[2,] + LagCRS2
 #  site_3d_crs$vb[3,] =site_3d_crs$vb[3,] + 17.7
   
   	# 3. Пространственная кластеризация
	#coords <- t(mesh$vb[1:3,])
	dbscan_res <- dbscan(st_coordinates(points_inside), eps = 0.3, minPts = 50)  # параметры кластеризации
	# 4. Определение основного кластера (предположительно тюлень)
	clusters <- data.frame(cluster = dbscan_res$cluster)
	#main_cluster <- which.max(table(clusters))
	points_inside$cluster=clusters
	cluster_points <- points_inside[points_inside$cluster==0,]
   
	#cutoff_z <- median(st_coordinates(points_inside)[,3]) + 0.01*sd(st_coordinates(points_inside)[,3]) # вычисление уровня земли
   # filtered_points <- points_inside[st_coordinates(points_inside)[,3] > cutoff_z, ]                   #обрезка по уровню земли
	
	# 7. Геометрическая фильтрация
# Вычисляем расстояние до центра масс

  vertices <- Seal_mesh$vb[1:3,]  # Вершины (x,y,z)
  faces <- Seal_mesh$it           # Грани (индексы вершин)

# Создание rgl меша
rgl_mesh <- tmesh3d(
  vertices = vertices,
  indices = faces,
  homogeneous = T
)

   shade3d(rgl_mesh , color = "lightblue")
   
   
   clean_mesh <- vcgClean(
  Seal_mesh,
  sel = c(1, 1, 1, 1, 1), # Включить все виды очистки
  tol = 0.01 # Допуск в метрах
)

# Сглаживание (без потери деталей)
  smooth_mesh <- vcgSmooth(
  clean_mesh,
  type = "HC", # Алгоритм Taubin
  lambda = 0.5,
  mu = -0.5,
  iteration = 3
)



cln_mesh=vcgClean(smooth_mesh, sel=0:6) 

# Hole filling
filled_mesh <- vcgClean(cln_mesh, sel = 7)  # Option 7 fills holes
repaired <- vcgClean(filled_mesh, sel = 0) # Repair non-manifold edges
corrected <- vcgUpdateNormals(repaired) # Fix flipped normals
components <- vcgIsolated(corrected) # Handle disconnected components
###################################

vcgVolume(corrected)
vcgArea(mesh)
# Compute geodesic distances
dist_matrix <- vcgGeodist(mesh)





	  shade3d(smooth_mesh, color = "lightblue")
	 
st_write(points_inside, "points_inside_local.kml", driver = "KML",delete_dsn = T)
   
   ###########
    vertices_gps <- st_as_sf(data.frame(x = site_3d_gps$vb[1,], y = site_3d_gps$vb[2,], z = site_3d_gps$vb[3,]+17.7), 
                     coords = c("x", "y","z"), crs = 32610)	
					 
	thinned_points_gps <- vertices %>% 
     slice_sample(prop = 0.1)
	 # slice(1:10)	
	 	 
st_write(thinned_points_gps, "thinned_points_gps.kml", driver = "KML",delete_dsn = T)
   
   
   #NewRef=data.frame(meshvb1=mesh$vb[1,],meshvb2=mesh$vb[2,])
  # NewRef$meshvb1New = signif(Width_OPP*(abs( NewRef$meshvb1)/Width_mesh) + leftOPP,digits = 16)
  # NewRef$meshvb2New =   signif(Length_OPP*(abs( NewRef$meshvb2)/Length_mesh) + bottomOPP,digits = 16)
  
 #  mesh$vb[1,] = NewRef$meshvb1New
  # mesh$vb[2,] = NewRef$meshvb2New
    
  # mesh$vb[1,]=as.numeric(mesh$vb[1,])
  # mesh$vb[2,]=as.numeric(mesh$vb[2,])
 
 


 #  for (i in 1:length(vb1)){
 #  seq = vb1[i]
 #  seqabs=abs(seq) 
 #  seqProportion =  seqabs/Width_mesh
 #  CRSvalue = Width_OPP*seqProportion+ leftOPP
 #  vb1New=c(vb1New,CRSvalue) 
 #  }



MeshConvertToMeters=function(mesh,crs){
meshM=mesh
crs = 32610

vertices <- st_as_sf(data.frame(x = meshM$vb[1,], y = meshM$vb[2,]), 
                     coords = c("x", "y"), crs = 32610)				 
	thinned_points <- vertices %>% 
     slice_sample(prop = 0.9) 	
st_write(thinned_points, "thinned_points.kml", driver = "KML")

 # Преобразуем в UTM Zone 18N (EPSG:32618) для восточной Канады
 #UTM Zone 10N (Ванкувер): EPSG:32610
 
vertices_local <- st_transform(vertices, 32610)
# Извлекаем координаты в метрах
coords_local <- st_coordinates(vertices_local)
# Обновляем вершины меша
mesh$vb[1,] <- coords_local[,1]
mesh$vb[2,] <- coords_local[,2]


center_1 <- mean(mesh$vb[1,])
center_2 <- mean(mesh$vb[2,])


#centr=apply(mesh$vb[1:3,], 1, mean)
# Смещаем меш к началу координат
mesh$vb[1,] <- mesh$vb[1,] - center_1
mesh$vb[2,] <- mesh$vb[2,] - center_2

return(mesh)
}
shade3d(mesh, color="lightblue")

###



##################################################################################
#insite1=unlist(filtered_points)
 #a=c(1: 2808035)
 



min(site_3d_local$vb[1,]); max(site_3d_local$vb[1,])
min(site_3d_local$vb[2,]); max(site_3d_local$vb[2,])
min(site_3d_local$vb[3,]); max(site_3d_local$vb[3,])

min(site_3d_CONVERT$vb[1,]); max(site_3d_CONVERT$vb[1,])
min(site_3d_CONVERT$vb[2,]); max(site_3d_CONVERT$vb[2,])
min(site_3d_CONVERT$vb[3,]); max(site_3d_CONVERT$vb[3,])



open3d()
shade3d(cropped, color="lightblue")

min(site_3d_nativ$vb[1,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[2,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[3,]);max(site_3d_nativ$vb[1,])

min(site_3d_local$vb[1,]);max(site_3d_local$vb[1,])
min(site_3d_local$vb[2,]);max(site_3d_local$vb[1,])
min(site_3d_local$vb[3,]);max(site_3d_local$vb[1,])



min(site_3d_nativ$vb[1,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[2,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[3,]);max(site_3d_nativ$vb[1,])

length(site_3d_nativ$vb[1,])
length(site_3d_local$vb[1,])


length(site_3d_nativ$vb[2,])
length(site_3d_local$vb[2,])

length(site_3d_nativ$vb[3,])
length(site_3d_local$vb[3,])

length(unique(site_3d_local$vb[1,]))
length(unique(site_3d_local$vb[2,]))
length(unique(site_3d_local$vb[3,]))

 #site_3d_gps = vcgImport(site_3d_gps_pth)
length(unique(site_3d_nativ$vb[1,]))
length(unique(site_3d_nativ$vb[2,]))
length(unique(site_3d_nativ$vb[3,]))

length(site_3d_nativ$vb[1,])
length(site_3d_nativ$vb[2,])
length(site_3d_nativ$vb[3,])



open3d()
cropped <- rmVertex(site_3d, !points_id)


verts=data.frame(lat=site_3d_gps$vb[1,], lon=site_3d_gps$vb[2,])
verts$uniqverts=paste0(verts$lat,"_",verts$lon)
length(unique(verts$uniqverts))



########################################################################
# Crop vertices first before boolean op
#st_crs(current_poly)=crs
#st_transform(current_poly,crs)

poly_sf=st_transform(poly_sf,crs)

#current_poly <- poly_sf[i,]$geometry

verts <- t(site_3d_gps$vb[1:3,])
points=st_as_sf(data.frame(verts), coords = 1:2)
st_crs(points)=crs

points$points_id=c(1:length(points$X3))
singlePol = poly_sf$geometry

singlePol=st_transform(singlePol,crs)
points=st_transform(points,crs)

insite <- st_intersects(singlePol, points, sparse = FALSE)
ins=points[insite,]
points_id=ins$points_id
cropped <- rmVertex(site_3d, !points_id)





grid <- st_make_grid(points, n = c(10, 10))
grid <- st_as_sf(grid) 
grid$grid_id = c(1:length(grid$x))


grid_intersects <- grid[st_intersects(grid, poly_sf, sparse = FALSE),]
points_with_grid <- st_join(points, grid, join = st_intersects)

filtered_points <- points_with_grid %>% 
  filter(grid_id %in% grid_intersects$grid_id)
  

#plot(poly_sf)
#plot(filtered_points, add=T)
#plot(poly_sf, add=T)

plot(singlePol)

insite <- st_intersects(singlePol,filtered_points, sparse = FALSE)
ins=filtered_points[insite,]
points_id=ins$points_id
#inside <- st_intersection(poly_sf,verts1)

site_3d_CONVERT = MeshConvertToMeters(site_3d_gps)
open3d()
shade3d(site_3d_CONVERT, color="lightblue")






#################################################
MeshConvertToSnmetrs=function(mesh){
 lat=mesh$vb[1,]
 lon=mesh$vb[2,]
 alt=mesh$vb[3,]

latmin=min(lat)
latmax=max(lat)

lonmin=min(lon)
lonmax=max(lon)

points1=c(latmin,lonmin)
points2=c(latmax,lonmax)

p1 <- st_sfc(st_point(points1), crs = crs) 
p2 <- st_sfc(st_point(points2), crs = crs) 

dist = st_distance(p1, p2)
step=length(mesh$vb[1,])



}
##########

#############################################################################
########################################################################
min_z <- min(obj_mesh$vb[3,])
max_z <- max(obj_mesh$vb[3,])
z_range <- range(obj_mesh$vb[3,])

# Function to extrude 2D polygon to 3D volume
extrude_polygon <- function(poly, zmin, zmax) {
  # Create bottom face
  bottom <- cbind(st_coordinates(poly)[,1:2], zmin)
  # Create top face
  top <- cbind(st_coordinates(poly)[,1:2], zmax)
  # Combine to create prism
  list(
    vb = t(cbind(bottom, top)),
    it = NULL # Will generate faces later
  )
}
#------------------
# Process each polygon
results <- lapply(1:nrow(poly_sf), function(i) {
  # Get current polygon
  current_poly <- poly_sf[i,]$geometry
  
  # Create 3D clipping volume
  clip_volume <- extrude_polygon(current_poly, z_range[1], z_range[2])
  
  # Convert to proper mesh (using Morpho)
  clip_mesh <- list(
    vb = clip_volume$vb,
    it = matrix(1:ncol(clip_volume$vb), nrow = 3)
  )
  
  # Crop original mesh (using Rvcg)
  cropped <- vcgIntersect(obj_mesh, clip_mesh)
  
  # Make solid (close holes and ensure watertight)
  solid_mesh <- vcgClean(cropped, sel = c(0, 7)) # 0=clean, 7=fill holes
  
  # Add original colors if they exist
  if(!is.null(obj_mesh$material)) {
    solid_mesh$material <- obj_mesh$material
  }
  
  return(solid_mesh)
}
###########################
bbox <- st_bbox(poly_sf)

# Filter vertices within bounding box
vert <- t(obj_mesh$vb[1:3,])

keep <- vert[,1] >= bbox$xmin & vert[,1] <= bbox$xmax &
         vert[,2] >= bbox$ymin & vert[,2] <= bbox$ymax &
         vert[,3] >= min_z & vert[,3] <= max_z


cropped_mesh <- rmVertex(obj_mesh, !keep)




###################################
cln_mesh=vcgClean(obj_mesh, sel=0:6) 
smooth_mesh <- vcgSmooth(cln_mesh, iteration = 3)
# Hole filling
filled_mesh <- vcgClean(smooth_mesh, sel = 7)  # Option 7 fills holes
repaired <- vcgClean(filled_mesh, sel = 0) # Repair non-manifold edges
corrected <- vcgUpdateNormals(repaired) # Fix flipped normals
components <- vcgIsolated(corrected, type = "vert") # Handle disconnected components
###################################

vcgVolume(corrected)
vcgArea(mesh)
# Compute geodesic distances
dist_matrix <- vcgGeodist(mesh)



poly_3d <- vcgBorder(poly_sf)  # Convert to 3D border



#vcgMeshres(cln_mesh)  


# Transform to match OBJ coordinates if needed
# OBJs typically use local coordinates or UTM
#cropping_polygons <- st_transform(cropping_polygons, target_crs)


# Add elevation range (Z-values) to your polygons
min_z <- min(obj_mesh$vb[3,])
max_z <- max(obj_mesh$vb[3,])

# Create 3D extrusion from 2D polygons
polygons_3d <- extrude3d(cropping_polygons), zmin = min_z, zmax = max_z)
}
