# PV_PL
# 3D Seal Body Processing Code
# This code processes 3D point cloud data of seal rookeries to:

*Isolate individual seal bodies using polygon boundaries
*Reconstruct 3D meshes of seal bodies
*Calculate seal body volumes
#Create watertight 3D models for analysis

#Input Requirements

#Polygon Data (.kml files):
#Boundary polygons outlining individual seals
#Path: labelInput\\Predict\\SpP_LRG_{date}.kml
#3D Point Cloud (.ply files):

#OPP scan of entire rookery
#Created in Agisoft Metashape

#Path: labelInput\\{date}_3D_gps.ply

#Coordinate Parameters:
#UTM CRS (32610 = Zone 10N)
#Local offsets (E, N, A) for coordinate conversion

#Processing Pipeline
#1. Setup & Initialization

#r
library(Rvcg)        # 3D mesh operations
library(rgl)         # 3D visualization
library(sf)          # Spatial data handling
# ... other libraries ...

crs <- 32610         # UTM Zone 10N
E <- 430000; N <- 5451000 # Coordinate offsets
labelInput <- "D:\\PV_DB\\2023_H0052A_OPP\\..." 

#Loads required geospatial/3D processing libraries
#Defines coordinate system and translation offsets

2. Data Preparation
#r
poly_sf <- st_read(PolPth) %>% st_transform(crs)
site_3d_gps <- vcgImport(site_3d_gps_pth)

#Reads seal boundary polygon (KML → SF object)
#Imports 3D point cloud (PLY format)
#Transforms both to common UTM coordinate system

#3. Coordinate Conversion
#r
site_3d_crs$vb[1,] <- site_3d_crs$vb[1,] + E  # Easting
site_3d_crs$vb[2,] <- site_3d_crs$vb[2,] + N  # Northing
vertices_crs <- st_as_sf(...)  # Convert to spatial points

#Converts GPS coordinates to local grid system
#Creates spatial points for geometric operations

#4. Point Filtering
#r
inside <- st_intersects(vertices_crs, singlePol)
points_inside <- vertices_crs[inside,]

#Filters 3D points to keep only those inside seal polygon
#Uses spatial intersection between points and polygon

#5. Height-Based Filtering
#r

#MedianLevel <- median(coords[,3])
#points_cut <- coords[coords[,3] > (MedianLevel-1) & ...]
#Calculates median elevation of seal body
#Removes ground points (±1 meter from median elevation)
#6. Mesh Reconstruction
#r

mesh1 <- vcgBallPivoting(points_cut, radius = 0.05)
mesh2 <- vcgUpdateNormals(mesh1)

#Creates initial mesh using ball pivoting algorithm
#Updates vertex normals for proper lighting/shading
#7. Boundary Processing
#r

boundary_edges <- vcgBorder(mesh2)[[1]]
alpha_shape <- ashape3d(border_vertices, alpha = 0.4)

#Extracts mesh boundary edges
#Creates alpha shape (concave hull) to define seal base
#8. Bottom Surface Creation
#r

distances <- vcgClost(mesh2, closed_mesh, sign=TRUE)
botton <- vcgSmooth(distances, "HClaplace", iteration=10)

#Computes signed distances to alpha shape
#Generates smooth bottom surface using Laplacian smoothing
#9. Seal Body Extraction
#r

polygon <- st_convex_hull(st_union(botton_crs))
pointsinside <- points_inside[st_intersects(...),]
seal <- vcgBallPivoting(coords, radius=0.05)

#Creates convex hull of bottom surface
#Filters points inside convex hull
#Generates top mesh of seal body

#10. Watertight Model Creation
#r

closed_seal <- mergeMeshes(botton, seal2)
watertight_mesh <- vcgQEdecim(mesh3, percent=0.3)
volume <- vcgVolume(watertight_mesh)

#Merges top and bottom mesh components
#Simplifies mesh (70% reduction) while preserving form
#Calculates seal body volume in cubic meters
#Outputs
#3D Mesh Models:
#Visualizable in RGL viewer
#Watertight seal body reconstruction
#Volume Measurement:
#Quantitative seal body volume
#Accessible via vcgVolume() output
#Intermediate Visualizations:

# Red bottom surface +  seal body
#Height-filtered point cloud
#Mesh boundaries and alpha shapes

#Key Parameters for Tuning

#radius = 0.05 - Ball pivoting radius (adjust for point density)
#alpha = 0.4 - Alpha shape concavity (higher = smoother)
#percent = 0.3 - Mesh simplification percentage
#iteration = 10 - Smoothing intensity

#Dependencies
#r
#install.packages(c("Rvcg", "rgl", "sf", "Morpho", 
                   "XML", "dplyr", "dbscan", 
                   "alphashape3d", "EBImage"))
#Usage Example
#r
# Set input directory and offsets
labelInput <- "path/to/your/rookery_data"
E <- 432100; N <- 5453000  # Site-specific offsets

# Run processing pipeline
source("seal_processing.R")

# View results
shade3d(watertight_mesh, col="gray")
print(paste("Seal volume:", volume, "m³"))


#Notes
#Coordinate offsets (E, N) must match your drone survey data
#Polygon files should be created prior to processing
#Optimal parameters vary with point cloud density (adjust radius/alpha)
#Visual inspection of intermediate results is recommended
#Output volumes are in cubic meters (convert as needed)
