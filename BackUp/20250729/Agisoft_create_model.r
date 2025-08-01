
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)

    imagefolder = paste0(labelInput,"\\Aerial_Images_For_Model")
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    outputply = paste0(labelInput,"\\",bsname,"_Model.ply")
##############################################################
   image_folder = normalizePath(imagefolder,winslash = "/")
   output_path = normalizePath(outputpath,winslash = "/")
   output_ply = normalizePath(outputply,winslash = "/")

   # Проверьте доступность модуля Metashape
   #py_run_string("import Metashape")
####################################################################
   # Функция для построения модели
    build_model <- function(image_folder, output_path) {
      py_run_string(paste0('
import Metashape
import os
doc = Metashape.Document()
################################################
#doc.open("', output_path, '")
#chunk = doc.chunk
############################################
chunk = doc.addChunk()
chunk.addPhotos([os.path.join(r"', image_folder, '", f) for f in os.listdir(r"', image_folder, '") if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))])
chunk.matchPhotos(downscale=1, generic_preselection=True, reference_preselection=True)
chunk.alignCameras()
chunk.buildDepthMaps(downscale=1, filter_mode=Metashape.MildFiltering)
chunk.buildModel(source_data=Metashape.DepthMapsData, surface_type=Metashape.Arbitrary, interpolation=Metashape.EnabledInterpolation)
chunk.crs = Metashape.CoordinateSystem("EPSG:32610")  
print(chunk.crs)
doc.save("', output_path, '")


export_settings = {
    "format": Metashape.ModelFormatPLY,
    "binary": True,
    "precision": 6,
    "texture_format": Metashape.ImageFormatPNG,
    "save_texture": True,
    "save_uv": True,
    "strip_extensions": False,
	"shift": Metashape.Vector( (435000, 5451000, 0) )
}



chunk.exportModel(
    r"', output_ply, '",
    **export_settings
)
doc.close()
'))
}
# Использование функции
build_model(image_folder, output_path)

###################################################################




