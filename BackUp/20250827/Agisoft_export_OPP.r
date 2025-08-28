
    library(reticulate)
    labelInput# = "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
	bsname=basename(labelInput)
	date_time=substr(basename(labelInput),1,15)
	outputkmz=paste0(labelInput,"\\",date_time,".kmz")
	psxM = paste0(labelInput,"\\",bsname,"_Model.psx")
	
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    crs = "EPSG:32610"   #32610 4326
	resolution_x = 3.221493e-08
	resolution_y = 1.751848e-08
  
    ###########################################
    psx_path = normalizePath(psxM,winslash = "/")
	output_kmz = normalizePath(outputkmz,winslash = "/",mustWork=F)
if (file.exists(psxM)==T){	
####################################################################
    export_opp <- function(psx_path,output_kmz, resolution_x, resolution_y) {
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
compression.jpeg_quality = 100
#############################################
chunk.exportRaster(r"', output_kmz, '",
                  format=Metashape.RasterFormatKMZ, 
				  source_data=Metashape.OrthomosaicData,
				  image_compression=compression,
				  tile_width=512, tile_height=512,
				  resolution_x=', resolution_x, ',
				  resolution_y=', resolution_y, ',
				  save_kml=True)
############################################

del doc
'))
}
export_opp(psx_path,output_kmz, resolution_x, resolution_y)
}
    