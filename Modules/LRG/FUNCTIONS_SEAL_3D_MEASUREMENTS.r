

vcgSample_type = "pd"

mesh_reconstract=function(mesh,ball_radius=0.02,num_samples=10000){

   #mesh=sl; ball_radius=0.02;num_samples=10000
 # 

   #
    mesh <- vcgQEdecim(mesh, tarface = 2000)
    mesh <- vcgUpdateNormals(mesh)
	#mesh <- vcgSmooth (mesh,type = "laplace", iteration = 20)
    #mesh <- vcgUpdateNormals(mesh)
#	vcgPlyWrite(mesh, "mesh.ply")
	
	
    smpls <- vcgSample(mesh, SampleNum = num_samples, type = vcgSample_type, geodes = FALSE) #km, mc pd


  reconstract <- vcgBallPivoting(smpls, radius = ball_radius)
 
  reconstract=vcgIsolated(reconstract)
  reconstract <- vcgUpdateNormals(reconstract)
 # vcgPlyWrite(reconstract, "mesh.ply")

 return(reconstract)

}
##############################################
pl=function(mesh,col=1,alpha = 0.5){shade3d(mesh, color=col, alpha=alpha) } #bbox_lines <- rgl::bbox3d(color = "gray")
##############################################################################
 determine_mesh_orientation=function(mesh){

    #mesh=pca_mesh
    verts <- vert2points(mesh)
    z_coords <- verts[,3]
	rng=range(z_coords)
	meansealalt=mean(rng)

	boundary_edges <- Rvcg::vcgBorder(mesh)[[1]]
   border_vertices <- vert2points(mesh)[boundary_edges, ]
   clusters <- dbscan(border_vertices, eps = 0.1, minPts = 3)$cluster
   # Считаем размер каждого кластера
   cluster_sizes <- table(clusters)
   # Исключаем самый большой кластер (внешний край)
     main_cluster <- as.integer(names(which.max(cluster_sizes)))
     small_holes <- clusters != main_cluster & clusters != 0  # 0 — шум в DBSCAN
     br_vr=border_vertices[!small_holes,]
	 BottonAlt=median(br_vr[,3])

    if(BottonAlt < meansealalt) {
      orientation <- "midi down"
    } else {
      orientation <- "midi up"
    }
  #}  
    return(orientation) 
}
############################################################################
seal_pca=function(mesh){
  # mesh=  sl6
   smpls <- vcgSample(mesh, SampleNum = 10000, type = vcgSample_type,geodes = FALSE)
   pca <- prcomp(smpls, center = TRUE, scale. = FALSE)
   pca_mesh <- vcgBallPivoting(x = pca$x, radius=0.02)
   return(pca_mesh)
   
#######
#if (returnOrig==T){
 #  pca_center=pca$center
#   pca_rotation=   pca$rotation
 #  pca_vertices <- vert2points(sl_m4) 
#  # Обратное преобразование PCA
#  original_vertices <- t(pca_rotation %*% t(pca_vertices)) + matrix(pca_center, 
#                                                                  nrow = nrow(pca_vertices), 
#                                                                  ncol = ncol(pca_vertices), 
 #                                                                 byrow = TRUE)
#																  
# finmesh <- vcgBallPivoting(x = original_vertices, radius = 0.02)																  
#}

}
################################################################################
###################################################
close_small_holes <- function(mesh, max_hole_size = 50) {
options(warn = -1) 
 # Проверка входных данных
  if (!inherits(mesh, "mesh3d")) stop("Input must be a mesh3d object")
  
  # Получаем координаты вершин
  pnts <- t(mesh$vb)[,1:3]
  
  # Находим граничные рёбра
   borders <- Rvcg::vcgGetEdge(mesh)
   border_edges =  as.matrix(borders[borders$border==1,][,1:2])

  
  # Если нет граничных рёбер - возвращаем исходный меш
  if (nrow(border_edges) == 0) {
    warning("No border edges found - mesh may be watertight")
    return(mesh)
  }
  
  # Создаём граф для поиска компонент связности (отдельных отверстий)
  require(igraph)
  g <- graph_from_edgelist(border_edges, directed = FALSE)
  components <- components(g)
    print(which(components$csize>2))
  # Обрабатываем каждое отверстие
  for (i in 1:components$no) {
    verts_in_hole <- which(components$membership == i)
    hole_size <- length(verts_in_hole)
    
    # Проверяем размер отверстия
    if (hole_size > 2 && hole_size <= max_hole_size) {
      hole_coords <- pnts[verts_in_hole,]
      
      # Для треугольных отверстий - просто создаём грань
      if (hole_size == 3) {
        patch <- list(
          vb = t(cbind(hole_coords, 1)),
          it = matrix(1:3, ncol = 1)
          #normals = t(rep(1, 4))
        )
        class(patch) <- "mesh3d"
      } else {
        # Для более сложных отверстий используем триангуляцию
        tryCatch({
          # Проецируем на 2D плоскость
          pca <- prcomp(hole_coords, center = TRUE)
          hole_2d <- hole_coords %*% pca$rotation[,1:2]
          
          # Триангуляция Делоне
          tri <- geometry::delaunayn(hole_2d, options = "Qz")
          
          # Создаём патч
          patch <- list(
            vb = t(cbind(hole_coords, 1)),
            it = t(tri)
            #normals = t(rep(1, 4))
          )
          class(patch) <- "mesh3d"
        }, error = function(e) {
          warning(paste("Failed to triangulate hole", i, ":", e$message))
          return(NULL)
        })
      }
      
      # Объединяем патч с исходным мешем
      if (exists("patch") && !is.null(patch)) {
	  

        mesh <- mergeMeshes(mesh, patch)
      }
    }
  }
  
  
  
  return(mesh)
  options(warn = 0)
}
#############################################################################
 frontalis_slice_filter <- function(mesh, ball_radius = 0.02, num_samples = 10000, num_slices = 10) { # up to down

#mesh=pca_mesh ; ball_radius = 0.02; num_samples = 10000; num_slices = 10
    #  Сэмплирование точек на поверхности меша
  smpls <- vcgSample(mesh, SampleNum = num_samples, type = vcgSample_type,geodes = FALSE)
  # 4. Рассчитываем параметры для срезов
  z_range <- range(smpls[,3])
  alt_range <- diff(z_range)
  step_size <- alt_range / num_slices
  slice_alts <- seq(z_range[1], z_range[2], step_size)
  
  # 5. Обрабатываем каждый слой
 finmesh=NULL
  for (e in 1:length(slice_alts)) {
       if (e == 1){lower_bound=slice_alts[e]}
	   if (e != 1){lower_bound=slice_alts[e-1]}
	   
	   if (e == length(slice_alts)){upper_bound=slice_alts[e]}
	   if (e != length(slice_alts)){upper_bound=slice_alts[e+1]}

     # Выбираем точки в текущем слое
     slice_points <- smpls[smpls[,3] > lower_bound & smpls[,3] < upper_bound, ]
     # Создаем меш для слоя
     if (nrow(slice_points) < 3) next  # Проверка на достаточное количество точек
      slice_mesh <- vcgBallPivoting(x = slice_points, radius = ball_radius)
	  #if (ncol(slice$it) < 1) next
	 # if (e>3){slice_isolated <- vcgIsolated(slice_mesh)} else{slice_isolated=slice_mesh}
       slice_isolated <- vcgIsolated(slice_mesh)
	 # vcgPlyWrite(slice_isolated, paste0(e,"_slice.ply"))
	   
        if (is.null(finmesh)) {
          finmesh <- slice_isolated
        } else {
          finmesh <- mergeMeshes(finmesh, slice_isolated)
        }
		
  
    }

	return(finmesh)
  }
###############################################################
axial_slice_filter =function(mesh,num_samples=10000, num_slices=20,ball_radius = 0.02){   # from head to tail
   #mesh=pca_mesh ; ball_radius = 0.02; num_samples = 10000; num_slices = 10
   #
    smpls <- vcgSample(mesh, SampleNum = num_samples, type = vcgSample_type,geodes = FALSE)
    x_range <- range(smpls[,1])
    	#z_r = range(smpls[,3])
	  #  alt_r = abs(z_r[1]) + abs(z_r[2])
   lngth_range <-  diff(x_range)
   step_size <- lngth_range / num_slices
   slice_lngths <- seq(x_range[1], x_range[2], step_size)
  mesh_out=NULL
#########
 for (e in 1:length(slice_lngths)) {
       if (e == 1){lower_bound=slice_lngths[e]}
	   if (e != 1){lower_bound=slice_lngths[e-1]}
	   
	   if (e == length(slice_lngths)){upper_bound=slice_lngths[e]}
	   if (e != length(slice_lngths)){upper_bound=slice_lngths[e+1]}

    # Выбираем точки в текущем слое
      slice_points <- smpls[smpls[,1] > lower_bound & smpls[,1] < upper_bound, ]
	   if (nrow(slice_points) < 5) next
	  slice=vcgBallPivoting(slice_points,radius=ball_radius)
    # if (ncol(slice$it) < 1) next
	  
	  slice=vcgIsolated(slice)
	#vcgPlyWrite(slice, paste0(e,"_slice.ply"))
	    # z_range <- range(slice_points[,3])
        # alt_range <- abs(z_range[1]) + abs(z_range[2])
		 
	 if (is.null(mesh_out)) {
          mesh_out <- slice
        } else {
          mesh_out <- mergeMeshes(mesh_out, slice)
        }
	
    }
 

return ( mesh_out)
}
#####################################################
saggital_slice_filter =function(mesh,num_samples=10000, num_slices=20, ball_radius = 0.02){ # from side to side
  #mesh=pca_mesh ; ball_radius = 0.02; num_samples = 10000; num_slices = 10
    smpls <- vcgSample(mesh, SampleNum = num_samples, type = vcgSample_type,geodes = FALSE)
    y_range <- range(smpls[,2])
    #	z_r = range(smpls[,3])
	  #  alt_r = abs(z_r[1]) + abs(z_r[2])
   width_range <- diff(y_range)
   step_size <- width_range / num_slices
   slice_width <- seq(y_range[1], y_range[2], step_size)
  mesh_out=NULL
#########
 for (e in 1:length(slice_width)) {
       if (e == 1){lower_bound=slice_width[e]}
	   if (e != 1){lower_bound=slice_width[e-1]}
	   
	   if (e == length(slice_width)){upper_bound=slice_width[e]}
	   if (e != length(slice_width)){upper_bound=slice_width[e+1]}

    # Выбираем точки в текущем слое
      slice_points <- smpls[smpls[,2] > lower_bound & smpls[,2] < upper_bound, ]
	  if (nrow(slice_points) < 5) next
	  slice=vcgBallPivoting(slice_points,radius=ball_radius)
	  #if (ncol(slice$it) < 1) next
	  slice=vcgIsolated(slice)
	 # 	vcgPlyWrite(slice, paste0(e,"_slice.ply"))
	     z_range <- range(slice_points[,3])
         alt_range <- abs(z_range[1]) + abs(z_range[2])
		 
	 if (is.null(mesh_out)) {
          mesh_out <- slice
        } else {
          mesh_out <- mergeMeshes(mesh_out, slice)
        }
	
    }
 

return ( mesh_out)

}
#####################################################
generate_precise_contours <- function(mesh, num_slices = 10, image_size = 1024) {

  # mesh=pca_mesh
 #  num_slices = 50
 #  image_size = 1024
  # 1. Подготовка данных
  vertices <- vert2points(mesh)
  z <- vertices[,3]
  z_levels <- seq(min(z), max(z), length.out = num_slices)
  
  # 2. Создаем пустое изображение
  x_range <- range(vertices[,1])
  y_range <- range(vertices[,2])
  plot(x_range, y_range, type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")
  
  # 3. Для каждого уровня высты
  contour_list <- list()
  for(i in 1:length(z_levels)) {
    # Находим все грани, пересекающие текущий уровень
    faces <- t(mesh$it)
    z_face <- matrix(z[faces], ncol = 3)
    crosses <- apply(z_face, 1, function(fz) {
      sum(fz >= z_levels[i]) != 3 && sum(fz >= z_levels[i]) != 0
    })
    
    # Вычисляем линии пересечения
    contour_lines <- list()
    for(f in which(crosses)) {
      tri <- vertices[faces[f,],]
      edges <- list(tri[c(1,2),], tri[c(2,3),], tri[c(3,1),])
      
      for(edge in edges) {
        if(diff(edge[,3] >= z_levels[i]) != 0) {
          t <- (z_levels[i] - edge[1,3]) / (edge[2,3] - edge[1,3])
          x <- edge[1,1] + t*(edge[2,1] - edge[1,1])
          y <- edge[1,2] + t*(edge[2,2] - edge[1,2])
          contour_lines[[length(contour_lines)+1]] <- c(x,y)
        }
      }
    }
  	
    # Сохраняем контуры
    if(length(contour_lines) > 0) {
      contour_coords <- do.call(rbind, contour_lines)
      contour_list[[i]] <- contour_coords
      
      # Рисуем контур
      if(nrow(contour_coords) > 1) {
        k <- chull(contour_coords)
        polygon(contour_coords[k,], border = hcl.colors(num_slices, "Plasma")[i], lwd = 2)
      }
    }
 }
 }
 #########################################################
get_spine=function(mesh,num_slices=50){

  # mesh=sl_m6
  # num_slices=20
   
  smpls <- vcgSample(mesh, SampleNum = 10000, type = vcgSample_type,geodes = FALSE)
    x_range <- range(smpls[,1])
    	#z_r = range(smpls[,3])
	  #  alt_r = abs(z_r[1]) + abs(z_r[2])
   lngth_range <-  diff(x_range)
   step_size <- lngth_range / num_slices
   slice_lngths <- seq(x_range[1], x_range[2], step_size)
   verts=NULL
 for (e in 1:length(slice_lngths)) {
       if (e == 1){lower_bound=slice_lngths[e]}
	   if (e != 1){lower_bound=slice_lngths[e-1]}
	   
	   if (e == length(slice_lngths)){upper_bound=slice_lngths[e]}
	   if (e != length(slice_lngths)){upper_bound=slice_lngths[e+1]}
    # Выбираем точки в текущем слое
      slice_points <- smpls[smpls[,1] > lower_bound & smpls[,1] < upper_bound, ]
	  DorsalHight= slice_points[which.max(slice_points[, 3]), ][3]
	  pca <- prcomp(slice_points, center = TRUE, scale. = FALSE)
      vertebra <- pca$center
	  vertebra[3]=DorsalHight
	  verts=rbind(vertebra,verts)
    }
  return(verts)
}
#####################################################
get_chest=function(mesh,num_slices=50){

   # mesh=sl_m3
  # num_slices=50

 smpls <- vcgSample(mesh, SampleNum = 10000, type = vcgSample_type,geodes = FALSE)
    y_range <- range(smpls[,2])
    #	z_r = range(smpls[,3])
	  #  alt_r = abs(z_r[1]) + abs(z_r[2])
   width_range <- diff(y_range)
   step_size <- width_range / num_slices
   slice_width <- seq(y_range[1], y_range[2], step_size)
   chest = NULL
   chestUp=NULL
   chestDown=NULL
########
 for (e in 1:length(slice_width)) {
       if (e == 1){lower_bound=slice_width[e]}
	   if (e != 1){lower_bound=slice_width[e-1]}
	   
	   if (e == length(slice_width)){upper_bound=slice_width[e]}
	   if (e != length(slice_width)){upper_bound=slice_width[e+1]}

    # Выбираем точки в текущем слое
      slice_points <- smpls[smpls[,2] > lower_bound & smpls[,2] < upper_bound, ]
	 
	 Up= slice_points[which.max(slice_points[, 3]), ][3]
	 Down = slice_points[which.min(slice_points[, 3]), ][3]
	  pca <- prcomp(slice_points, center = TRUE, scale. = FALSE)
      chestUp1 <- pca$center
	  chestUp1[3]=Up
	  chestDown1=chestUp1
	  chestDown1[3]=Down

	  chestUp=rbind(chestUp,chestUp1)
	  chestDown=rbind(chestDown1,chestDown)

    }
  chest=rbind(chestUp,chestDown)
	chest[,1]=median(chest[,1])
	return(chest)
}
#####################################################################
 surface_to_solid_mesh = function(mesh){
	  mesh=vcgUpdateNormals(mesh)
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
 ###############################
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
################################
 Get_bottom=function(mesh, alpha = 0.4, alt_correct=0){
 #mesh=sl12
   mesh=mesh_reconstract(mesh)
  # mesh=vcgSmooth (mesh,type = "laplace", iteration = 30)
      #mesh=mesh_reconstract(mesh)
   boundary_edges <- Rvcg::vcgBorder(mesh)[[1]]
   border_vertices <- vert2points(mesh)[boundary_edges, ]
   
 
# Настройки
k_sor <- 20       # Количество соседей для анализа
multiplier <- 1.5  # Множитель для порога (чем выше, тем агрессивнее фильтрация)

# Вычисляем средние расстояния до k соседей
nn <- vcgKDtree(border_vertices, border_vertices, k = k_sor)
mean_dist <- rowMeans(nn$distance)

# Определяем порог (медиана + множитель * SD)
threshold <- median(mean_dist) + multiplier * sd(mean_dist)
is_noise <- mean_dist > threshold

filtered_points <- border_vertices[!is_noise, ]

   # Create alpha shape from boundary vertices
   alpha_shape <- ashape3d(filtered_points, alpha =alpha)
   alpha_mesh <- as.mesh3d(alpha_shape)
   
   vertices <- t(alpha_mesh$vb[1:3, ])  # Nx3 матрица координат
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

  smpls <- vcgSample(new_mesh, SampleNum = 5000, type = vcgSample_type,geodes = FALSE)
 
  smpls[,3] =  smpls[,3] + alt_correct
  reconstract <- vcgBallPivoting(smpls, radius = 0.02)
  reconstract <- vcgUpdateNormals(reconstract)
   return(reconstract)
} 
############################################
 trim_seal_by_ground <- function(seal_mesh, ground_mesh, above = T) {
  
 #seal_mesh = sl12
 # ground_mesh = bottom
 # above = T
 # ground_mesh=vcgSmooth (ground_mesh,type = "laplace", iteration = 30)
  # Вычисляем расстояние от каждой вершины тюленя до поверхности
    clost <- vcgClost(seal_mesh, ground_mesh, sign = TRUE)
	
	inside = clost$quality > 0
	outside=clost$quality < 0
	
    trimmed_seal_pts1 <- vert2points(seal_mesh)[inside, ]
	trimmed_seal_pts2 <- vert2points(seal_mesh)[outside, ]
	
	if (length(trimmed_seal_pts2[,1])> length(trimmed_seal_pts1[,1])){trimmed_seal_pts=trimmed_seal_pts2}else{trimmed_seal_pts=trimmed_seal_pts1}
   # trimmed_seal_mesh <- vcgBallPivoting(trimmed_seal_pts,radius=0.02)
  
  # Определяем вершины для сохранения
 # keep_verts1 <-  which(dist < 0)
 # keep_verts12= which(dist > 0)
  

#  vertices <- vert2points(seal_mesh)
#  filter_verts = vertices[keep_verts,]
  
   # clusters <- dbscan(trimmed_seal_pts, eps = 0.01, minPts = 3,)$cluster
    # Считаем размер каждого кластера
  #  cluster_sizes <- table(clusters)
    # Исключаем самый большой кластер (внешний край)
  #   main_cluster <- as.integer(names(which.max(cluster_sizes)))
  #   small_holes <- clusters != main_cluster & clusters != 0  # 0 — шум в DBSCAN
   #  br_vr=trimmed_seal_pts # [!small_holes,]
  

  
 #  filter_mesh <- vcgBallPivoting(br_vr, radius = 0.02)
 #  filter_mesh <- vcgUpdateNormals(filter_mesh)
   
   
   trimmed_seal_mesh <- vcgBallPivoting(trimmed_seal_pts,radius=0.02)
  # vcgPlyWrite(trimmed_seal_mesh, "mesh.ply")
 
 
   #smpls <- vcgSample(trimmed_seal_mesh, SampleNum = num_samples, type = vcgSample_type, geodes = FALSE) #km, mc pd


 # reconstract <- vcgBallPivoting(smpls, radius = ball_radius)
 
   #	vcgPlyWrite(reconstract, "mesh.ply") 
 #  trimmed_seal2 = trim_seal_by_ground (seal_mesh=sl12, surface_mesh=bottom, above = F)
#		     # check and fix  
#			range1= diff(range(vert2points(trimmed_seal1)[,3]))
#			range2= diff(range(vert2points(trimmed_seal2)[,3]))
#			 if(range1>range2){trimmed_seal=trimmed_seal1} else{trimmed_seal=trimmed_seal2}
   
   
   return(trimmed_seal_mesh)
   

   
 #  pl(surface_mesh)
 #  pl(seal_mesh,4)
   
}   
	
###################################################
 PointySize = function(
                     mesh,
                     initial_points=5000,
					 length.out=100,
					 type="km",
                     depth	=0.8				 #pd mc
					){
 #  mesh=sl_m
 # length.out=10
 # initial_points=9000
 #  type="km"
  
   
  #  i=1

   result_points <- NULL
   scales <- seq(1, depth, length.out = length.out)
   

  for (i in 1:length.out) {
    # Масштабирование меша
    scaled_mesh <- mesh
    scale_factor = scales[i]
	n_points <- ifelse(i > 11, round(initial_points *  exp(-i/20)), initial_points) 
    scaled_mesh$vb[1:3, ] <- scale_factor * mesh$vb[1:3, ]
    new_points <- vcgSample(scaled_mesh, SampleNum =n_points, type = vcgSample_type, geodes = FALSE) # mc pd
    result_points <- rbind(result_points, new_points)
    print(paste0(i,"     scale_factor   ",scale_factor ))

	#points3d(result_points,col=2,alpha=0.2)
 
  }
  
  return(result_points)
 

 }

######################################################
  VoxeliSize = function(PointsVolume, voxel_size=0.01){
 # PointsVolume=pnts
  bbox <- apply(PointsVolume, 2, range)  # min/max по X, Y, Z
  grid_size <- ceiling((bbox[2,] - bbox[1,]) / voxel_size)
# Создаём регулярный грид
x_seq <<- seq(bbox[1, 1], bbox[2, 1], length.out = grid_size[1])
y_seq <<- seq(bbox[1, 2], bbox[2, 2], length.out = grid_size[2])
z_seq <<- seq(bbox[1, 3], bbox[2, 3], length.out = grid_size[3])

# Инициализируем пустой 3D-массив (0 = пусто, 1 = воксель)
voxel_array <- array(0, dim = grid_size)

 # Привязываем точки к ближайшим вокселям
for (i in 1:nrow(PointsVolume)) {
  x_idx <- which.min(abs(x_seq - PointsVolume[i, 1]))
  y_idx <- which.min(abs(y_seq - PointsVolume[i, 2]))
  z_idx <- which.min(abs(z_seq - PointsVolume[i, 3]))
  voxel_array[x_idx, y_idx, z_idx] <- 1  # помечаем воксель как заполненный
} 
  return(voxel_array)
  

  }
  
######################################
vpl=function(voxel_array){


 #voxel_array=voxel
# Преобразуем массив в координаты вокселей
voxel_coords <- which(voxel_array == 1, arr.ind = TRUE)

plot_ly(
  x = x_seq[voxel_coords[, 1]],
  y = y_seq[voxel_coords[, 2]],
  z = z_seq[voxel_coords[, 3]],
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = "red", opacity = 0.2)
 
) 
}

voxel_to_mesh=function(voxel_array){
#voxel_array=voxel
# Создаем координатную сетку (автоматически по размеру массива)
#x <- 1:dim(voxel_array)[1]
#y <- 1:dim(voxel_array)[2]
#z <- 1:dim(voxel_array)[3]


# Применяем Marching Cubes
  voxel_size=0.01
  mesh <- vcgIsosurface(voxel_array, threshold = 1)
  mesh$vb[1:3, ] <- voxel_size * mesh$vb[1:3, ]
  points <- vcgSample(mesh, SampleNum =10000, type = vcgSample_type, geodes = FALSE) #,MCsamp=100,strict=T,geodes=T,iter.max=10


 ashape <- ashape3d(points, alpha = 0.45)
 alpha_mesh <- as.mesh3d(ashape)
  smpls <- vcgSample(alpha_mesh, SampleNum = 10000, type = vcgSample_type, geodes = FALSE)
  reconstract <- vcgBallPivoting(smpls, radius = 0.04)
  reconstract=vcgIsolated(reconstract)
  reconstract <- vcgUpdateNormals(reconstract)
  
  	alpha_shape <- ashape3d(vert2points(reconstract), alpha =0.04)
	volume_ashape3d(alpha_shape)
  
  
  
  return(reconstract)


}
###################################################################
clip_mesh_bottom=function(mesh,percent=15){
 #mesh=sl16
 #percent=15
 
 
    vertices <- vert2points(mesh)
    z <- vertices[,3]
    cut_height <- quantile(z, probs = percent/100)
	keep_verts <- which(z >= cut_height)
	verticesCut=vertices[keep_verts,]
	meshCut=vcgBallPivoting(verticesCut,radius=0.02)
	return(meshCut)
   # pl(meshCut,2,0.3);pl(mesh,5,0.4)

}
#####################################################################
merg_seal_ground <- function(seal_mesh, ground_mesh) {

 #  seal_mesh =sl16 
#   ground_mesh = ground_mesh

   ground_pts=vert2points(ground_mesh)
   #seal_pts=vert2points(seal_mesh)
   #merg_pts = rbind(ground_pts,seal_pts)
   #merg_mesh <- vcgBallPivoting(merg_pts,radius=0.02)
   #vcgPlyWrite(merg_mesh, "merg_mesh.ply") 
   inside <- Rvcg::vcgClost(ground_mesh, seal_mesh, sign = TRUE)$quality > 0
   trimmed_ground_pts <- ground_pts[inside, ]
   trimmed_ground_mesh <- vcgBallPivoting(trimmed_ground_pts,radius=0.02)
   combined_mesh <- mergeMeshes(seal_mesh, trimmed_ground_mesh)

  return(combined_mesh)
}
#####################################################################
Smooth_points=function(points ,k = 100,sigma = 10){
   
   points=smoothed
   k = 1000 
   sigma = 10

  n <- nrow(points)
  smoothed <- matrix(0, nrow = n, ncol = 3)
  
  for (i in 1:n) {
    # Находим k ближайших соседей
    neighbors <- nn2(points, query = points[i, , drop = FALSE], k = k)$nn.idx[1, ]
    dists <- sqrt(rowSums((points[neighbors, ] - points[i, ])^2))
    
    # Гауссовы веса (чем дальше, тем меньше вес)
    weights <- exp(-dists^2 / (2 * sigma^2))
    weights <- weights / sum(weights)  # нормировка
    
    # Взвешенное усреднение
    smoothed[i, ] <- colSums(points[neighbors, ] * weights)
  }
  
  return(smoothed)
}
#############################################################
################################################################
get_ground=function(mesh){

    mesh=pca_mesh
    ground_mesh=Get_bottom(mesh)

      clost <- vcgClost(mesh, ground_mesh, sign = TRUE)
	
	inside = clost$quality > 0
	outside=clost$quality < 0
	
    trimmed_pts1 <- vert2points(mesh)[inside, ]
	trimmed_pts2 <- vert2points(mesh)[outside, ]
	
	if (length(trimmed_pts1[,1])> length(trimmed_pts2[,1])){trimmed_pts=trimmed_pts2}else{trimmed_pts=trimmed_pts1}


    trimmed_pts
	
 clusters <- dbscan(trimmed_pts, eps = 0.01, minPts = 3,)$cluster
    # Считаем размер каждого кластера
    cluster_sizes <- table(clusters)
    # Исключаем самый большой кластер (внешний край)
     main_cluster <- as.integer(names(which.max(cluster_sizes)))
     small_holes <- clusters != main_cluster & clusters != 0  # 0 — шум в DBSCAN
     br_vr=trimmed_pts[!small_holes,]
  points3d(br_vr)
  
  
	ground_pts_in=vert2points(ground_mesh)
	ground=rbind(ground_pts_in,br_vr)



}
###################################################################
pca_to_orig=function(meshBase,meshPCA){
     #meshBase=sl6
   smpls <- vcgSample(meshBase, SampleNum = 10000, type = vcgSample_type, geodes = FALSE)
   pca <- prcomp(smpls, center = TRUE, scale. = FALSE)

   pca_center=pca$center
   pca_rotation=   pca$rotation
   pca_vertices <- vert2points(meshPCA) 
#  # Обратное преобразование PCA
  original_vertices <- t(pca_rotation %*% t(pca_vertices)) + matrix(pca_center, 
                                                                  nrow = nrow(pca_vertices), 
                                                                  ncol = ncol(pca_vertices), 
                                                                  byrow = TRUE)
																  
  mesh <- vcgBallPivoting(x = original_vertices, radius = 0.02)	
  return(mesh) 




}

###########################################################
measurement_to_orig=function(meshBase,measurement){
   
  # meshBase=sl6
  #measurement=spine
   smpls <- vcgSample(meshBase, SampleNum = 10000, type = vcgSample_type, geodes = FALSE)
   pca <- prcomp(smpls, center = TRUE, scale. = FALSE)

   pca_center=pca$center
   pca_rotation=   pca$rotation
   pca_vertices <-measurement
#  # Обратное преобразование PCA
  original_vertices <- t(pca_rotation %*% t(pca_vertices)) + matrix(pca_center, 
                                                                  nrow = nrow(pca_vertices), 
                                                                  ncol = ncol(pca_vertices), 
                                                                  byrow = TRUE)
																   
  return(original_vertices) 
  }
#####################################################
draft=function(){

##########################################################
PointsSizedraft = function(mesh){

   # mesh=sl_m
	for (i in 1:100){
         if (i==1){scaled_mesh <- mesh ; scaled_factor=1;fin=NULL; n_points <- 300000; minquality= -0.001; maxquality=0.001 }
     
     scaled_mesh$vb[1:3, ] <- scaled_factor * scaled_mesh$vb[1:3, ]  # масштабирование
     vertices <- vert2points(scaled_mesh)
     bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты
	  
        # Генерация случайных точек в bounding box
      points <- matrix(
      runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
      ncol = 3,
      byrow = TRUE
      )
 
 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
closest <- vcgClostKD(
  x = points,
  mesh = scaled_mesh,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения
)
 
 inside_points <- points[closest$quality > minquality  &  closest$quality < maxquality , ]
 fin=rbind(fin,inside_points)



  scaled_factor=scaled_factor - 0.05
  log_scale <- log(n_points) - log(i + 1)
  n_points=  exp(log_scale)*2  # Возвращаем к исходной шкале

 minquality = minquality*1.2
 maxquality = maxquality*1.2
 
 if (i>2){ minquality = minquality*3
            maxquality = maxquality*3}
 
 
}
return(fin)
 # points3d(fin,col=2,alpha=0.2 )
}
##########################################################
PointsSizedraft2 = function(mesh, 
                     initial_scale = 1.0, 
                     scale_step = 0.05,
                     initial_points = 500000,
                     min_dist_factor = 0.001,
                     max_iter = 100,
                     threads = 4) {
  
  # Инициализация
  result_points <- NULL
  current_scale <- initial_scale
  quality_range <- c(-min_dist_factor, min_dist_factor)
  
  for (i in 1:max_iter) {
    # Масштабирование меша
    scaled_mesh <- mesh
    scaled_mesh$vb[1:3, ] <- current_scale * mesh$vb[1:3, ]
    
    # Получаем вершины и bounding box
    vertices <-vert2points(scaled_mesh)
    bbox <- apply(vertices, 2, range)
    
    # Адаптивное количество точек (логарифмически уменьшается)
   if (i>11){ n_points <- round(initial_points * exp(-i/1.5))} else {n_points=initial_points}
    
    # Генерация случайных точек в bounding box
    points <- matrix(
      runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
      ncol = 3,
      byrow = TRUE
    )
    
    # Находим ближайшие точки с учетом знака расстояния
    closest <- Rvcg::vcgClostKD(
      x = points,
      mesh = scaled_mesh,
      sign = TRUE,
      threads = threads
    )
    
    # Фильтрация точек внутри меша
    inside <- which(closest$quality > quality_range[1] & 
                    closest$quality < quality_range[2])
					
				
    
    if (length(inside) > 0) {
      new_points <- points[inside, ]
      result_points <- rbind(result_points, new_points)
    }
    
    # Адаптивно изменяем параметры
    current_scale <- current_scale - scale_step
    quality_range <- quality_range * 1.1  # Постепенно увеличиваем диапазон
    
    # Ранняя остановка если меш стал слишком маленьким
    if (current_scale < 0.2) break
  }
  
  return(result_points)
}
############################################################
 mesh=sl_m
   vertices <- vert2points(mesh)
   bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты
# Генерация случайных точек в bounding box
n_points <- 100000 # Желаемое количество точек
 
 points <- matrix(
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
 
 


 # Создаем плотную выборку точек на поверхности меша
  mesh_samples <- vcgSample(mesh, SampleNum = 10000, geodes = FALSE)
  
  # Строим KD-дерево для меша
  nn <- nn2(mesh_samples, points, k = k)
  
  # Эвристика: точка внутри, если среднее расстояние меньше порога
  mean_dists <- rowMeans(nn$nn.dists)
  threshold <- quantile(mean_dists, 0.1)
  inside_points <- points[mean_dists < threshold, ]
 
 
 
 
 
 
 
 #inside_points <- random_points[closest$quality > -0.001  &  closest$quality < 0.001 , ]


 
  inside_points <- random_points[closest$quality > 0.01, ]

  points3d(inside_points,col=2,alpha=0.2 )
  pl(sl_m,alpha=0.2 )

}





















