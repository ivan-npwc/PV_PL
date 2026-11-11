   
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

 Shift_E =  435000 
 Shift_N =   5451000 


E <-  as.numeric(Shift_E)         #
N <- as.numeric(Shift_N)         #   
#A <- 18.06            # Altitude offset   

# Define input directory path
labelInput #<-  "D:\\PV_DB\\2023_H0052A_OPP\\20230615_103330\\20230615_103330_MAVIC2PRO_40m"

# Set species and extract date from directory name
Species <- "LRG"
date1 <- substr(basename(labelInput), 1, 15)
bsname=basename(labelInput)
# Create prediction directory if it doesn't exist
predDir <- paste0(labelInput, "\\Predict");dir.create(predDir, showWarnings = FALSE)
DirSave=paste0(predDir,"\\MeshCrop");unlink(DirSave, recursive=T); dir.create(DirSave)

# Define file paths
PolPth <- paste0(labelInput, "\\Predict\\SpP_LRG_", date1, ".kml")
site_3d_gps_pth = paste0(labelInput,"\\",bsname,"_Model.ply")
dir_site_model =  paste0(labelInput,"\\",bsname,"_Model")
 if (dir.exists(dir_site_model)){site_3d_gps_pth=list.files(dir_site_model, full.names=T)} 
  if (file.exists(PolPth)==T){
  for (y in 1: length(site_3d_gps_pth)){

if (file.exists(site_3d_gps_pth[y])==T){

########################################################################
# Read polygon from KML file and transform to target CRS
poly_sf <- st_read(PolPth)
poly_sf <- poly_sf %>% st_transform(crs)

# Import 3D GPS point cloud data
site_3d_local <- vcgImport(site_3d_gps_pth[y])

# Convert 3D points to CRS coordinates by adding offsets
site_3d_crs <- site_3d_local
site_3d_crs$vb[1,] <- site_3d_local$vb[1,] + E  # Add Easting offset
site_3d_crs$vb[2,] <- site_3d_local$vb[2,] + N  # Add Northing offset
#site_3d_crs$vb[3,] <- site_3d_crs$vb[3,] + A  # Altitude offset

# Convert vertices to spatial points
vertices_crs <- st_as_sf(data.frame(x = site_3d_crs$vb[1,], 
                                  y = site_3d_crs$vb[2,], 
                                  z = site_3d_crs$vb[3,]), 
                        coords = c("x", "y", "z"), crs = crs)

# Find points inside the polygon boundary
SealCount = length(poly_sf$geometry)
   for (i in 1:SealCount){
  singlePol <- poly_sf$geometry[i]
  centr = data.frame(st_coordinates(st_centroid(singlePol)))
  centrName=paste0("E",centr$X,"N",centr$Y)
inside <- st_intersects(vertices_crs, singlePol, sparse = FALSE)
points_inside <- vertices_crs[inside,]
#st_write(points_inside,"points_inside.kml",delete_dsn = T)
#########
# Process points inside polygon
# Extract coordinates and convert to local system
coords <- st_coordinates(points_inside)
 if (length(coords[,1])<70){message(paste0("The quality of the model does not allow building the seal mesh   ", i)); next}
coords[,1] <- coords[,1] - E  # Convert to local Easting
coords[,2] <- coords[,2] - N  # Convert to local Northing

# Calculate median height level and define upper/lower bounds
MedianLevel <- median(coords[,3]) 
UpLevel <- MedianLevel + 0.5
DownLevel <- MedianLevel - 0.5

# Filter points within height range
points_cut <- coords[coords[,3] > DownLevel, ]  # Remove points below lower bound
points_cut <- points_cut[points_cut[,3] < UpLevel, ]  # Remove points above upper bound

# Create mesh using ball pivoting algorithm
 mesh <- vcgBallPivoting(points_cut, radius = 0.05)
 #########################################################################
 # Создание строковых идентификаторов через paste0
detailed_keys <- paste0(site_3d_local$vb[1,], "_", 
                        site_3d_local$vb[2,], "_", 
                       site_3d_local$vb[3,])
					   
schematic_keys <- paste0(mesh$vb[1,], "_", 
                        mesh$vb[2,], "_", 
                       mesh$vb[3,])
keep_vertices <- detailed_keys %in% schematic_keys
valid_indices <- which(keep_vertices)					   
filtered_mesh <- site_3d_local
filtered_mesh$vb <- filtered_mesh$vb[, valid_indices, drop = FALSE]

# Обновляем индексы граней
new_indices <- rep(0, ncol(site_3d_local$vb))
new_indices[valid_indices] <- seq_along(valid_indices)
filtered_mesh$it <- matrix(new_indices[filtered_mesh$it], nrow = 3)
filtered_mesh$it <- filtered_mesh$it[, colSums(filtered_mesh$it == 0) == 0, drop = FALSE]

filtered_mesh$normals <- site_3d_local$normals[, valid_indices, drop = FALSE]

 
################################################################### 
 # sl2=vcgIsolated(mesh)
#  sl3 <- vcgUpdateNormals(sl2)
 
 
 name=paste0(date1,"#",centrName,".ply")
 pthSave=paste0(DirSave,"\\",name) 
 
 open3d()
 shade3d(filtered_mesh)
 writePLY(pthSave)
close3d()
}
}
}
###########















}