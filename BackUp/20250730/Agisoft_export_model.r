
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    crs = "EPSG:32610"   #32610 4326
  
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    outputply = paste0(labelInput,"\\",bsname,"_Model.ply")
##############################################################
   psx_path = normalizePath(outputpath,winslash = "/")
   output_ply = normalizePath(outputply,winslash = "/", mustWork=F)
####################################################################
   # Функция для построения модели
    export_model <- function(psx_path, output_ply, crs) {
      py_run_string(paste0('
import Metashape
import os
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
############################################
print(chunk.crs)

export_settings = {
    "format": Metashape.ModelFormatPLY,
    "binary": True,
    "precision": 6,
    "texture_format": Metashape.ImageFormatPNG,
    "save_texture": True,
    "save_uv": True,
    "strip_extensions": False,
	"crs": Metashape.CoordinateSystem("EPSG::32610"),
	"shift": Metashape.Vector( (435000, 5451000,-100) )
}



chunk.exportModel(
    r"', output_ply, '",
    **export_settings
)

del doc
'))
}
# Использование функции
export_model(psx_path, output_ply,crs)

###################################################################




