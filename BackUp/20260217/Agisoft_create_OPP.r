
   #source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_create_OPP.r")
  #   labelInput = "/mnt/adata8tb/PV_DB/2023_H0055_OPP/20230503_111132/20230503_111132_MINI3PRO_20m"
    ####################################3
    library(reticulate)
	bsname=basename(labelInput)
    ####################################3

    py_pth_win =  "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
	py_pth_unx = "/home/ivan/anaconda3/bin/python"
	
	sys_info <- Sys.info()
	if (sys_info["sysname"] == "Windows") {py_pth=py_pth_win}
	if (sys_info["sysname"] == "Linux")     {py_pth=py_pth_unx}
	
    use_python(py_pth, required = TRUE)
	#####################################
	
	
    crs = "EPSG:32610"   #32610 4326
    outputpath = file.path(labelInput, paste0(bsname,"_Model_no_water.psx"))
    psx_path = normalizePath(outputpath,winslash = "/")
	
if (file.exists(outputpath)==T){	
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
       
doc.save("', psx_path, '")
del doc
'))
}
# Использование функции
create_opp(psx_path, crs)
}
###################################################################




