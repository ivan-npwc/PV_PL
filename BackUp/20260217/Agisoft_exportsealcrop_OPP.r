
   #source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_exportsealcrop_OPP.r")
   # labelInput = "/home/ivan/adata8tb/PV_DB/2023_H0052A_OPP/20230615_103330/20230615_103330_MAVIC2PRO_40m"
    ####################################3
    library(reticulate)
    library(EBImage)
	bsname=basename(labelInput)
    ####################################3

    py_pth_win =  "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
	py_pth_unx = "/home/ivan/anaconda3/bin/python"

	sys_info <- Sys.info()
	if (sys_info["sysname"] == "Windows") {py_pth=py_pth_win}
	if (sys_info["sysname"] == "Linux")     {py_pth=py_pth_unx}

    use_python(py_pth, required = TRUE)
	#####################################
	Species = "LRG"
    date1= substr(basename(labelInput),1,15)
    crs = "EPSG:32610"   #32610 4326
    outputpath = file.path(labelInput, paste0(bsname,"_Model_no_water.psx"))
    pthPol = file.path(labelInput, "Predict",  paste0("SpP_",Species,"_",date1,".kml"))
    outputDir = file.path(labelInput, "Predict", "SealCrop"); unlink(outputDir, recursive=T); dir.create(outputDir)

    psx_path = normalizePath(outputpath,winslash = "/")
    pthPol = normalizePath(pthPol,winslash = "/")
    outputDir = normalizePath(outputDir,winslash = "/")

if (file.exists(outputpath)==F) {break(paste0("No    ", psx_path, "   found"))}
####################################################################
   # Функция для построения модели
    export_crops <- function(psx_path, crs, pthPol, outputDir) {
      py_run_string(paste0('
import Metashape
import os
import sys
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
############################################
if chunk.shapes:
    existing_shapes = list(chunk.shapes.shapes)
    chunk.shapes.remove(existing_shapes)


chunk.importShapes(
path=r"', pthPol, '",
format=Metashape.ShapesFormatKML)
#crs=Metashape.CoordinateSystem("', crs, '")

polygons = list(chunk.shapes.shapes)
print(f"Импортировано полигонов: {len(polygons)}")
#####################################################################
for y, point in enumerate(polygons):
    polygon = polygons[y]
    сoords = polygon.geometry.coordinates[0]
    #print(f"Number of vertices: {len(сoords)}")
##################################################
    min_x = min_y = min_z=  float("inf")
    max_x = max_y = max_z= float("-inf")

    all_lat = []
    all_lon = []
    for i, point in enumerate(сoords):
        point = сoords[i]
        lat = point[1]
        lon = point[0]
        all_lat.append(lat)
        all_lon.append(lon)

    min_lat = min(all_lat)
    max_lat = max(all_lat)
    min_lon = min(all_lon)
    max_lon = max(all_lon)

    filename_coords = f"{min_lat:.14f}_{max_lat:.14f}_{min_lon:.14f}_{max_lon:.14f}"

#######################################################
    bbox = Metashape.BBox()
    bbox.min = Metashape.Vector([min_lon, min_lat])
    bbox.max = Metashape.Vector([max_lon, max_lat])
#############################################################checking bbox
    #    vertices = [
    #    Metashape.Vector([bbox.min.x, bbox.min.y, bbox.min.z]),
    #    Metashape.Vector([bbox.max.x, bbox.min.y, bbox.min.z]),
    #    Metashape.Vector([bbox.max.x, bbox.max.y, bbox.max.z]),
    #    Metashape.Vector([bbox.min.x, bbox.max.y, bbox.max.z]),
    #    Metashape.Vector([bbox.min.x, bbox.min.y, bbox.min.z])  # закрыть полигон
    #    ]
    #existing_shapes = list(chunk.shapes.shapes)
    #chunk.shapes.remove(existing_shapes)
    #shape = chunk.shapes.addShape()
    #shape.geometry = Metashape.Geometry.Polygon(vertices)
    #chunk.exportShapes(
    #path = "test.kml",
    #format=Metashape.ShapesFormatKML)#
    #
    #   crs = target_crs)
###################################################################
    output_file = os.path.join(r"', outputDir, '", f"{filename_coords}.tif")
    #print(f"  Exporting to: {output_file}")
######################################
    compression = Metashape.ImageCompression()
    compression.tiff_tiled = True
    compression.tiff_overviews = True
    compression.tiff_compression = Metashape.ImageCompression.TiffCompressionJPEG
    compression.jpeg_quality = 100
#############################################
    chunk.exportRaster(path=output_file,
                       source_data=Metashape.OrthomosaicData,
                       image_compression=compression,
                       clip_to_boundary=True,
                       region=bbox)

del doc
'))
}

export_crops(psx_path, crs, pthPol, outputDir)
###################################################################
#lstimgs = list.files(outputDir, full.names=T, pattern="tif")
# for (i in 1:length(lstimgs)){
#     pth =  lstimgs[i]
#     img = readImage(pth)
#     newName = gsub("tif","JPG", pth)
#     writeImage(img, newName, quality = 100, type = "jpeg")
#     }
###############################################################



