 
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

lstopp = list.files(PVdir, recursive=T, pattern="psx", full.names=T)
lstDirInput = unique(sapply(lstopp, dirname))
length(lstDirInput)

for (ig in 1:length(lstDirInput)){

  labelInput =  lstDirInput[ig]
  print(labelInput)
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

date1 <- substr(basename(labelInput), 1, 15)
PolPth <- file.path(labelInput, "Predict",paste0("SpP_LRG_",  date1, ".kml"))
if (file.exists(PolPth)==F) next

	MeshDir = file.path(labelInput, "Predict","MeshCrop")
	MeshLst=list.files(MeshDir, full.names=T)
  if (length(MeshLst)==0) next 


source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_3d_Measurements.r")
print(labelInput)
}


