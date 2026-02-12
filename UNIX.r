 
   library(reticulate)
  library("tensorflow")
  library("abind")
  library("doParallel")
  library("foreach")
  library("tfdatasets")
  library(tools)
  library(doMC) 
  library(EBImage)
  library(keras)
  library(parallel)
  
  
  
   #############################################
#num_physical_cores <- detectCores(logical = FALSE)
#num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
#batch_size <- num_cores_to_registr 
#batch_size_global = batch_size*10 
#registerDoMC(cores = num_cores_to_registr)
#computer_name <- Sys.info()["nodename"]		
####################################
  

  
PVdir = "/mnt/adata8tb/PV_DB"
logpth =  file.path(PVdir,"log.csv")
if (file.exists(logpth)==F) {a= data.frame(opp="", action="", fun="",dtime=""); write.csv(a,logpth, row.names=F)}
log1 =read.csv(logpth)


lstopp = list.files(PVdir, recursive=T, pattern="psx", full.names=T)
lstDirInput = unique(sapply(lstopp, dirname))
length(lstDirInput)

#######################


for (ig in 1:length(lstDirInput)){

  labelInput = lstDirInput[ig]
   bsname=basename(labelInput)
  print(labelInput)

  check = length(list.files(file.path(labelInput,"Aerial_Images_For_Model")))
   if (check==0) next

 outputDir = file.path(labelInput, "Predict", "SealCrop")
 lstimgs = list.files(outputDir, full.names=T, pattern="tif")
 if (length(lstimgs)==0) next



   row1=data.frame(opp=labelInput, action="start",fun = "LRG_PredsToPolygons", dtime=format(Sys.time(), "%Y%m%d %H%M%S"))
   log1=rbind(row1,log1)
   write.csv(log1,logpth, row.names=F)

  source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3_level_2.r")

   row1=data.frame(opp=labelInput, action="stop",fun = "LRG_PredsToPolygons", dtime=format(Sys.time(), "%Y%m%d %H%M%S"))
   log1=rbind(row1,log1)
   write.csv(log1,logpth, row.names=F)
  }





 source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_create_model_with_masks.r")
   source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_create_OPP.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_export_OPP.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_export_model.r")
   source("/home/ivan/GIT_HUB/PV_PL/Modules/Unzip.R")
   source("/home/ivan/GIT_HUB/PV_PL/Modules/KMLprepare.R")
   source("/home/ivan/GIT_HUB/PV_PL/Modules/Image_prepare.R")
   #neeed restart
   source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_PredsToPolygons.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_3d_CROP.r")

  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_exportsealcrop_OPP.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/Utilits/tif_to_JPG.r")
  source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3_level_2.r")

 # source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_3d_Measurements.r") IT WORKS GOOD ONLY ON WINDOWS

}






#   exists = length(list.files(file.path(labelInput,"Predict","WATER_MASK")))
#   if (check == exists) next
 # source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/WATER PRED.r")

#  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_create_model.r")
#
 #  }
  # source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_export_OPP.r")
 #  source("/home/ivan/GIT_HUB/PV_PL/Modules/Unzip.R")
 #  source("/home/ivan/GIT_HUB/PV_PL/Modules/KMLprepare.R")
   #source("/home/ivan/GIT_HUB/PV_PL/Modules/Image_prepare.R")
 #  source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3.r")
# source(/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_PredsToPolygons.r")

  #########pred
 #check = length(list.files(file.path(labelInput,"Predict", "TilesOverlap")))
  #if (check==0) next
  #source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3.r")
########## pred to pol
 #  thermalcheck  <- grepl("THERMAL", labelInput)
 #  if(thermalcheck==T) next
 # check = length(list.files(file.path(labelInput,"Predict","Preds")))
 # if (check==0) next
 #  source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_PredsToPolygons.r")   
##############################################

#date1 <- substr(basename(labelInput), 1, 15)
#PolPth <- file.path(labelInput, "Predict",paste0("SpP_LRG_",  date1, ".kml"))
#if (file.exists(PolPth)==F) next

#	MeshDir = file.path(labelInput, "Predict","MeshCrop")
#	MeshLst=list.files(MeshDir, full.names=T)
#  if (length(MeshLst)==0) next
#source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_3d_Measurements.r")
#print(labelInput)
###########################################

  date_time=substr(basename(labelInput),1,15);  kmz =  file.path(labelInput, paste0(date_time,".kmz"))
  expkmz= file.path(labelInput,date_time)
  reftbl = file.path(labelInput, paste0(date_time,".csv"))
  tilesoverlap = file.path(labelInput, "Predict", "TilesOverlap")
  preds = file.path(labelInput,"Predict","Preds")
  polpreds = file.path(labelInput, "Predict",  paste0("SpP_",Species,"_",date_time,".kml"))
  meshcrop = file.path(labelInput, "Predict","MeshCrop")
 unlink(c(kmz,expkmz,reftbl,tilesoverlap,preds,polpreds,meshcrop), recursive=T)
