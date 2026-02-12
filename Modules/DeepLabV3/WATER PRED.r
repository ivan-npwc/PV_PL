  
  #source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/WATER PRED.r")
   #labelInput  = "/mnt/adata8tb/PV_DB/2023_H0099_OPP/20230502_135032/20230502_135032_MINI3PRO_20m"
 
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
  library(magick)
 PTHweight =  "/home/ivan/GIT_HUB/PV_PL_system_data/DeepLabV3/whaterremove"
 modelcreatepth =  "/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/DeepLabV3_CREATE.r"
 


 Species = "WATER"
 batch_size = 8
 batch_size_global =  batch_size*5
 vision_dimensions=256L
 file_size = 10000


  if (file.exists(PTHweight)==F){stop("Weight path is invalid")}
  if(exists("model")==F)   {source(modelcreatepth)}
   weight <- readRDS(PTHweight)
   set_weights(model,weight)
 ##############################################
#num_physical_cores <- detectCores(logical = FALSE)
#num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
#batch_size <- num_cores_to_registr 
#batch_size_global = batch_size*10 
#registerDoMC(cores = num_cores_to_registr)
#computer_name <- Sys.info()["nodename"]		
####################################
MskDir = file.path(labelInput,"Predict","WATER_MASK")
#unlink(MskDir, recursive=T)
dir.create(file.path(labelInput,"Predict"))
dir.create(MskDir)
exists = list.files(MskDir)

date1=substr(basename(paste0(labelInput)),1,15)
#predict_dir = file.path(labelInput,"Aerial_Images_For_Model")
##############
bsnmOPP=basename(labelInput)
gendirOPP = gsub(bsnmOPP,"",labelInput)
dirAerial= gsub("OPP","Aerial",gendirOPP)
predict_dir = dirAerial
#############
predsDir = file.path(labelInput,"Predict","Preds") 
unlink(predsDir,recursive=T)
dir.create(predsDir, showWarnings=F)
listImage_glob = list.files(predict_dir, full.names = TRUE, recursive=T)
listImage_glob <- grep("THERMAL", listImage_glob, invert = TRUE, value = TRUE)

###############################################################
if (length(exists) == length(listImage_glob)) {next} else {

 exist = tools::file_path_sans_ext(exists)
 listImage_glob=listImage_glob[!file_path_sans_ext(basename(listImage_glob))%in% exist]

}
#################################################################3
  imgorig = readImage(listImage_glob[1])
  dim1=dim(imgorig)

###############################################################################################################
global_steps <- round(length(listImage_glob)/batch_size_global)  #+1 
if(global_steps==0){global_steps=1}
##############################################################################							
 create_dataset <- function(data1, batch_size = batch_size, vision_dimensions) {  
  dataset <- data1 %>% 
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_image(tf$io$read_file(.x$img), channels = 3, expand_animations = FALSE)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = c(vision_dimensions, vision_dimensions))
    )) 
#	 dataset_map(~.x %>% list_modify(
#    image <-tf$keras$applications$resnet$preprocess_input(.x$img) 	
#	 ))
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  dataset %>% 
    dataset_map(unname) # Keras needs an unnamed output.
}
#################################################################################
for (e in 1:global_steps) {
  batch_ind_global <- c(1:length(listImage_glob))[1:batch_size_global]
  listImage <- listImage_glob[batch_ind_global]
  listImage=listImage[is.na(listImage)==F]
  if (length(listImage_glob) > length(listImage)) {
    listImage_glob <<- listImage_glob[-batch_ind_global] 
  }
  data1 <<- tibble::tibble(img = listImage)
  #######################################################################################
  pred_dataset <- create_dataset(data1, batch_size=batch_size, vision_dimensions=vision_dimensions)
  preds=keras:::predict.keras.engine.training.Model(object=model,
                                                    x=pred_dataset)
 print(paste0("Done  ", e, "  pred from  " ,global_steps)) 													
 ########################################################################################### 
pthsaveRDS= file.path(labelInput,"Predict","Preds",paste0(Species,"_",e))
  saveRDS(preds,pthsaveRDS)
 preds=readRDS(pthsaveRDS)

# mclapply(listImage, function(imgpth) {
for (i in 1:length(listImage)){
     imgpth= listImage[i]
	 pred_order <- which(listImage == imgpth)
     name=basename(imgpth)

     mask0=preds[pred_order, , , ]
     img0 <- t(mask0)
	 img0 <- t(img0)
     dim(img0) <- c(vision_dimensions, vision_dimensions, 1)
     img = getFrame(img0, 1)
     img_d=dilate(img, makeBrush(7, shape='disc'))

	   

	             nmask5 <- t(img_d)
	            # imgorig = readImage(imgpth)
	            # dim1=dim(imgorig)
                 msk=resize(nmask5,dim1[1],dim1[2])
                 y1 = channel(msk, 'asred')

                 name1 = tools::file_path_sans_ext(name)
                 name2=paste0(name1,"_mask.png")

				 MaskPth=file.path(MskDir, name2) #{filename}_mask.png

                 writeImage(y1, MaskPth, type ="png")

}



}
#############################
