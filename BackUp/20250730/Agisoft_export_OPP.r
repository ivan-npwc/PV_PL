
  library(reticulate)
    labelInput = "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
	bsname=basename(labelInput)
	outputkmz=paste0(labelInput,"\\",bsname,".kmz")
	psx = paste0(labelInput,"\\",bsname,"_Model.psx")
	
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    crs = "EPSG:32610"   #32610 4326
  
    ###########################################
    psx_path = normalizePath(psx,winslash = "/")
	output_kmz = normalizePath(outputkmz,winslash = "/",mustWork=F)
####################################################################
   # Функция для построения модели
    export_opp <- function(psx_path,output_kmz) {
      py_run_string(paste0('
import Metashape
import os
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
#################################################
compression = Metashape.ImageCompression()
compression.tiff_tiled = True
compression.tiff_overviews = True
compression.tiff_compression = Metashape.ImageCompression.TiffCompressionJPEG
compression.jpeg_quality = 95
#############################################
chunk.exportRaster(r"', output_kmz, '",, format=Metashape.RasterFormatKMZ, source_data=Metashape.OrthomosaicData, image_compression=compression)
############################################

del doc
'))
}
# Использование функции
export_opp(psx_path,output_kmz)

###################################################################













# Параметры экспорта KMZ
    