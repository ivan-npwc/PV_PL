
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    crs = "EPSG:32610"   #32610 4326
  
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    psx_path = normalizePath(outputpath,winslash = "/")
####################################################################
   # Функция для построения модели
    create_opp <- function(psx_path, crs) {
      py_run_string(paste0('
import Metashape
import os
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
############################################
print(chunk.crs)

  chunk.buildOrthomosaic(
            surface_data=Metashape.ModelData,
            blending_mode=Metashape.MosaicBlending,
            resolution=0.01
        )
        #chunk.exportOrthomosaic(r"', output_path, '")


del doc
'))
}
# Использование функции
export_model(psx_path, output_ply,crs)

###################################################################




