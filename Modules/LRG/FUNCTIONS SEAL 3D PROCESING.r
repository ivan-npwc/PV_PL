



mesh_reconstract=function(mesh,ball_radius=0.02,num_samples=10000){
 smpls <- vcgSample(mesh, SampleNum = num_samples, type = "pd")
  reconstract <- vcgBallPivoting(smpls, radius = ball_radius)
  reconstract=vcgIsolated(reconstract)
  reconstract <- vcgUpdateNormals(reconstract)
  return(reconstract)

}
##############################################
pl=function(mesh,col=1,alpha = 0.5){shade3d(mesh, color=col, alpha=alpha); bbox_lines <- rgl::bbox3d(color = "gray")}
##############################################################################  
seal_pca=function(mesh,ball_radius=0.02,num_samples=10000){

   smpls <- vcgSample(mesh, SampleNum = num_samples, type = "pd")
   pca <- prcomp(smpls, center = TRUE, scale. = FALSE)
   pca_mesh <- vcgBallPivoting(x = pca$x, radius = ball_radius)
   return(pca_mesh)
#######
#if (returnOrig==T){
#  pca_vertices <- vert2points(finmesh) 
#  # Обратное преобразование PCA
#  original_vertices <- t(pca_rotation %*% t(pca_vertices)) + matrix(pca_center, 
#                                                                  nrow = nrow(pca_vertices), 
#                                                                  ncol = ncol(pca_vertices), 
#                                                                  byrow = TRUE)
# finmesh <- vcgBallPivoting(x = original_vertices, radius = ball_radius)																  
#}

}
################################################################################
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

    #  Сэмплирование точек на поверхности меша
  smpls <- vcgSample(mesh, SampleNum = num_samples, type = "pd")
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
     if (nrow(slice_points) > 3) {  # Проверка на достаточное количество точек
      slice_mesh <- vcgBallPivoting(x = slice_points, radius = ball_radius)
	 # if (e>3){slice_isolated <- vcgIsolated(slice_mesh)} else{slice_isolated=slice_mesh}
       slice_isolated <- vcgIsolated(slice_mesh)
	   
        if (is.null(finmesh)) {
          finmesh <- slice_isolated
        } else {
          finmesh <- mergeMeshes(finmesh, slice_isolated)
        }
		
    }
    }

	return(finmesh)
  }
###############################################################
axial_slice_filter =function(mesh,num_samples=10000, num_slices=20,ball_radius = 0.02){   # from head to tail
   #mesh=sl8
    smpls <- vcgSample(mesh, SampleNum = num_samples, type = "pd")
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
	  slice=vcgBallPivoting(slice_points,radius=ball_radius)
	  slice=vcgIsolated(slice)
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
 #mesh=sl8
    smpls <- vcgSample(mesh, SampleNum = num_samples, type = "pd")
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
	  slice=vcgBallPivoting(slice_points,radius=ball_radius)
	  slice=vcgIsolated(slice)
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
  
  return(list(
    contours = contour_list,
    levels = z_levels,
    x_range = x_range,
    y_range = y_range
  ))
}
#######################################################################
get_vertebras=function(mesh,num_slices=20){
  # mesh=sl8
  # num_slices=20
   
   smpls <- vert2points(mesh)
  # 1. Рассчитываем параметры для срезов
  x_range <- range(smpls[,1])
  lngth_range <- abs(x_range[1]) + abs(x_range[2])
  step_size <- lngth_range / num_slices
  slice_lngths <- seq(x_range[1], x_range[2], step_size)
verts=NULL
###################################################
 for (e in 1:length(slice_lngths)) {
       if (e == 1){lower_bound=slice_alts[e]}
	   if (e != 1){lower_bound=slice_alts[e-1]}
	   
	   if (e == length(slice_alts)){upper_bound=slice_alts[e]}
	   if (e != length(slice_alts)){upper_bound=slice_alts[e+1]}

    # Выбираем точки в текущем слое
      slice_points <- smpls[smpls[,1] > lower_bound & smpls[,1] < upper_bound, ]
	  pca <- prcomp(slice_points, center = TRUE, scale. = FALSE)
      vertebra <- pca$center
	  verts=rbind(vertebra,verts)
    }
  return(verts)
}
############################################################################## 
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