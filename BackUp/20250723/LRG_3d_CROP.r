   
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

# Create prediction directory if it doesn't exist
predDir <- paste0(labelInput, "\\Predict");dir.create(predDir, showWarnings = FALSE)
DirSave=paste0(predDir,"\\MeshCrop");dir.create(DirSave, showWarnings = FALSE)

# Define file paths
PolPth <- paste0(labelInput, "\\Predict\\SpP_LRG_", date1, ".kml")
site_3d_gps_pth <- paste0(labelInput, "\\", date1, "_3D_gps.ply")
########################################################################
# Read polygon from KML file and transform to target CRS
poly_sf <- st_read(PolPth)
poly_sf <- poly_sf %>% st_transform(crs)

# Import 3D GPS point cloud data
site_3d_gps <- vcgImport(site_3d_gps_pth)

# Convert 3D points to CRS coordinates by adding offsets
site_3d_crs <- site_3d_gps
site_3d_crs$vb[1,] <- site_3d_crs$vb[1,] + E  # Add Easting offset
site_3d_crs$vb[2,] <- site_3d_crs$vb[2,] + N  # Add Northing offset

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
#########
# Process points inside polygon
# Extract coordinates and convert to local system
coords <- st_coordinates(points_inside)
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
  sl2=vcgIsolated(mesh)
  sl3 <- vcgUpdateNormals(sl2)
 
 
 name=paste0(date1,"#",centrName,".ply")
 pthSave=paste0(DirSave,"\\",name) 
 
 open3d()
 shade3d(sl3)
 writePLY(pthSave)
close3d()
}
#####################################################################
