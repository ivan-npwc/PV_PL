 # Load required libraries
library(Rvcg)        # For 3D mesh operations
library(rgl)         # For 3D visualization
library(sf)          # For spatial data operations
library(Morpho)      # For morphological operations on 3D data
library(XML)         # For XML parsing
library(dplyr)       # For data manipulation
#library(dbscan)      # For density-based clustering
library(alphashape3d) # For alpha shape calculations
#library(onion) # Для работы с кватернионами
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
source("C:\\Users\\usato\\SSL_DB\\PV_PL\\Modules\\LRG\\FUNCTIONS SEAL 3D PROCESING.r")# upload function to processing
############################################################
 #for (y in 1:length(MeshLst)){
  y=25
  slpth=  MeshLst[y]
  flsize= file.size(MeshLst[y])
 # if (flsize < 90000) {unlink(slpth)} else {
  sl = vcgImport(slpth)
  sl1= mesh_reconstract(sl)
  sl2 = close_small_holes(sl1)
  sl3= mesh_reconstract(sl2)
  
  sl4=frontalis_slice_filter(sl3)
  sl5=axial_slice_filter(sl4)
  sl6=saggital_slice_filter(sl5)
 
   sl7=seal_pca(sl6)
   sl8= surface_to_solid_mesh(sl7)
   
   sl9 = mesh_reconstract(sl8)
     sl9 = close_small_holes(sl9)
   sl9 = mesh_reconstract(sl9)
	   is_watertight(sl9)

############################################################################
#need check  library(bioimagetools) for segmentation
	
	pl(finmesh,col=4)
	pl(sl6,col=3)
	points3d(center_of_mass,col=2,size=10)


	
	
	
}
}





draft=function(){

  ball_radius = 0.02
  num_samples = 10000
  num_slices = 10
  mesh_in=sl5
 
 
   vertices <- vert2points(sl3)
   pca <- prcomp(vertices, center = T, scale. = FALSE)
   mesh <- vcgBallPivoting( x=pca$x,
                        radius = 0.05)
  smpls <- vcgSample(mesh, SampleNum = 10000, type = "pd")
 
  NumberSlices = 10
  Alt=abs(min(smpls[,3]))+abs(max(smpls[,3]))
  step=  Alt/10
  slisesAlt=seq(min(smpls[,3]),max(smpls[,3]),step)
  
  for (i in 1:length(slisesAlt)){
  
      # alt=slisesAlt[i]
	#  i=3
	   if (i >1){minalt=slisesAlt[i-1]}else{minalt=slisesAlt[i]}
       if (i < length(slisesAlt)){maxalt=slisesAlt[i+1]} else {maxalt=slisesAlt[i]}              
   
      SlidPoints= smpls[smpls[,3] > minalt &  smpls[,3]< maxalt ,]
	  
	   SlideMesh <- vcgBallPivoting( x=SlidPoints,
                        radius = 0.05)
	   SlideIsolated = vcgIsolated(SlideMesh)
	   if (i ==1){finmesh=SlideIsolated} else {
	   
	   finmesh=mergeMeshes(finmesh,SlideIsolated)
  
}	
}	
	
finmesh= vcgBallPivoting(finmesh,radius = 0.05)



	

 
	 # 2. Создаём плоскость по трём точкам
  # Векторы в плоскости
  v1 <- hole_coords[2,] - hole_coords[1,]
  v2 <- hole_coords[3,] - hole_coords[1,]
	 
	 
	  # Нормаль к плоскости
  normal <- crossProduct(v1, v2)
  normal <- normal / sqrt(sum(normal^2))  # Нормализуем
	 
	 # 3. Генерируем случайные точки в плоскости
  # Базис плоскости
  basis1 <- v1 / sqrt(sum(v1^2))
  basis2 <- crossProduct(normal, basis1)



  # Случайные координаты в базисе плоскости
  set.seed(42)  # Для воспроизводимости
  random_coords <- cbind(
    runif(150, min = -0.5, max = 0.5),
    runif(150, min = -0.5, max = 0.5)
  )


  # Преобразуем в 3D координаты
  random_points <- t(hole_coords[1,] + 
                     random_coords[,1] %*% t(basis1) + 
                     random_coords[,2] %*% t(basis2))
Frpn=NULL

iterCount=length(random_points[1,])-2

for (y in 1:round(iterCount/3)){
  if (y==1){i=1}
  rpn=random_points[,i:(i+2)] 
  Frpn=rbind(rpn,Frpn)
  i=i+3
Frpn
}
	pl(finmesh,col=2,alpha=0.2)
	pl(mesh,col=4,alpha=0.3)
	
	
	
	
	
	
	
	
	pl(SlideIsolated,col=3,alpha=0.8)
     points3d(smpls,col=2,,alpha=0.2)
   
   bbox_lines <- rgl::bbox3d(color = "gray")
	
	
	points3d(SlidPoints,col=2)

   points3d(smpls,col=4,alpha=0.2)
   points3d(SlidPoints,col=2)
   
   bbox_lines <- rgl::bbox3d(color = "gray")
  
  }
  
  
  
  
  