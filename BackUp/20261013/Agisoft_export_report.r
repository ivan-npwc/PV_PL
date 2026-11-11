
   ####################################3
    library(reticulate)
   # labelInput = "/media/ivan/pv/PV_DB/2023_H0052A_OPP/20230518_104633/20230518_104633_MINI2_20m"
	bsname=basename(labelInput)

    py_pth_win =  "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
	py_pth_unx = "/home/ivan/anaconda3/bin/python"
	
	sys_info <- Sys.info()
	if (sys_info["sysname"] == "Windows") {py_pth=py_pth_win}
	if (sys_info["sysname"] == "Linux")     {py_pth=py_pth_unx}
	
    use_python(py_pth, required = TRUE)
	#####################################

  
    outputpath = file.path(labelInput,paste0(bsname,"_Model.psx"))
    outputReport1 = file.path(labelInput,paste0(bsname,"_Report.pdf"))
if (file.exists(outputpath)==T){		
##############################################################
   psx_path = normalizePath(outputpath,winslash = "/")
   outputReport = normalizePath(outputReport1,winslash = "/", mustWork=F)
####################################################################
    export_model <- function(psx_path, outputReport) {
      py_run_string(paste0('
import Metashape
import os
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
############################################
chunk.exportReport(
path= r"', outputReport, '",
title="Full Processing Report"
)

del doc
'))
}

export_model(psx_path, outputReport1)
Sys.sleep(3)
}
###################################################################




