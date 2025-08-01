
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
	#####################################
    crs = "EPSG:4326"   #32610 4326
    imagefolder = paste0(labelInput,"\\Aerial_Images_For_Model")
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    outputply = paste0(labelInput,"\\",bsname,"_Model.ply")
##############################################################
#if (dir.exists(imagefolder)==F){stop("No Aerial_Images_For_Model found")}
   image_folder = normalizePath(imagefolder,winslash = "/")
   output_path = normalizePath(outputpath,winslash = "/")
   output_ply = normalizePath(outputply,winslash = "/")
  
   #py_run_string("import Metashape")
####################################################################
   # Функция для построения модели
    build_model <- function(image_folder, output_path,crs) {
      py_run_string(paste0('
import Metashape
import os
doc = Metashape.Document()
chunk = doc.addChunk()
chunk.crs = Metashape.CoordinateSystem("', crs, '")  
chunk.addPhotos([os.path.join(r"', image_folder, '", f) for f in os.listdir(r"', image_folder, '") if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))])
chunk.matchPhotos(downscale=1, generic_preselection=True, reference_preselection=True)
chunk.alignCameras()
chunk.buildDepthMaps(downscale=1, filter_mode=Metashape.MildFiltering)
chunk.buildModel(source_data=Metashape.DepthMapsData, surface_type=Metashape.Arbitrary, interpolation=Metashape.DisabledInterpolation) #EnabledInterpolation
chunk.crs = Metashape.CoordinateSystem("', crs, '")  
print(chunk.crs)
doc.save("', output_path, '")
del doc
'))
}
# Использование функции
build_model(image_folder, output_path, crs)

###################################################################




