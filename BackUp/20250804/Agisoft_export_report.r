
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)

  
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    outputReport1 = paste0(labelInput,"\\",bsname,"_Report.pdf")
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
}
###################################################################
Sys.sleep(360)



