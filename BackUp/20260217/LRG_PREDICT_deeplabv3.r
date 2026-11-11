  
  #source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/LRG_PREDICT_deeplabv3.r") 
  
 
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
  
 PTHweight =  "/home/ivan/GIT_HUB/PV_PL_system_data/DeepLabV3/LRG20251028_val93"
 modelcreatepth =  "/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/DeepLabV3_CREATE.r"
 
#labelInput  = "/media/ivan/CRUISE/PV_DB/2023_H0054_OPP/20230503_082013/20230503_082013_MINI3PRO_20m_DUMMY"

 Species = "LRG"
 batch_size = 32
 batch_size_global =  batch_size*10
 vision_dimensions=256L
 resultBlob=NULL
 file_size = 10000
 NotUseEmptyImgs = T
 IncludeBlobAnalis = F
 Brush=9
 thresh=0.009
 MaskCreate = F
 ImageMaskConcetence = F
 
  

  
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
 if (MaskCreate == T){
                      IncludeBlobAnalis = T
                      MskDir = file.path(labelInput,"Predict","MaskPredicted")
					  unlink(MskDir, recursive=T);dir.create(MskDir)}

date1=substr(basename(paste0(labelInput)),1,15)
predict_dir = file.path(labelInput,"Predict", "TilesOverlap")
predsDir = file.path(labelInput,"Predict","Preds") 
unlink(predsDir,recursive=T)
dir.create(predsDir, showWarnings=F)
kmlPathSave1 = file.path(labelInput,"Predict",paste0("#",basename(PTHweight),"#",date1,".kml"))
listImage_glob = list.files(predict_dir, full.names = TRUE)
pth_resultBlob <<- file.path(labelInput,"Predict", paste0(Species,"_BlobTable_", date1, ".csv"))
###############################################################################################################
################################################
if (NotUseEmptyImgs==T) {
                         inf=data.frame(listImage_glob=listImage_glob,file_size=as.numeric(file.size(listImage_glob)))
						 listImage_glob=inf$listImage_glob[inf$file_size > file_size]
						 exl=length(inf$listImage_glob[inf$file_size < file_size])
						 print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImage_glob) , "   Images"))
}  
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
 # batch_ind_global=batch_ind_global[is.na(batch_ind_global)==F]
    listImage_glob <<- listImage_glob[-batch_ind_global] 
  }
  data1 <<- tibble::tibble(img = listImage)
  #######################################################################################
  pred_dataset <- create_dataset(data1, batch_size=batch_size, vision_dimensions=vision_dimensions)
  preds=keras:::predict.keras.engine.training.Model(object=model,
                                                    x=pred_dataset)
 print(paste0("Done  ", e, "  pred from  " ,global_steps)) 													
 ########################################################################################### 
  
  
   if (IncludeBlobAnalis==T){
   resultBlob_tmp=NULL
 result_batch <- mclapply(listImage, function(imgpth) {
#########################################
#resultBlob_tmp <- foreach(i = 1:length(listImage),.combine=rbind) %dopar% {
#   for (i in 1: length (listImage)) {
    # imgpth= listImage[1]
	 pred_order <- which(listImage == imgpth)
     name=basename(imgpth)
     #img_pth=listImage[i]
    
     mask0=preds[pred_order, , , ]
     img0 <- t(mask0)
	 img0 <- t(img0)
     dim(img0) <- c(vision_dimensions, vision_dimensions, 1)
     img = getFrame(img0, 1)
     nmask = thresh(img, 18, 18, thresh)  
     nmask1 <- fillHull(nmask)
     nmask2 = opening(nmask1, makeBrush(Brush,shape='disc') ) # shape='Gaussian', sigma=50   
     nmask3 = fillHull(nmask2)
     nmask4 = bwlabel(nmask3)
	 display(nmask4)  
	   
	       if (MaskCreate==T){
	             nmask5 <- t(nmask4)
	             nmask5=resize(nmask5,1024,1024)
				 MaskPth=file.path(MskDir, gsub("jpg","png",name))
                 writeImage(nmask5,MaskPth)	   	   	   
	   } 
       
         if (max(nmask4)!=0) {   
            fts = computeFeatures.moment(nmask4)  # coordinat
            shapeFeatures <- computeFeatures.shape(nmask4) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
            BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=imgpth) 
           # resultBlob_tmp=rbind(resultBlob_tmp,BlobTable)			

} 
})

result_clean  <- result_batch[!sapply(result_batch, is.null)]
resultBlob_tmp  <- do.call(rbind, result_clean)

######################
if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)}
print(paste0("Done  ", e, "  blobs analisis from  " ,global_steps))		   
   } 
 ###########################################################################
   if (IncludeBlobAnalis==F){
         pthSavePreds = file.path(predsDir,paste0("Preds_",Species,"_",e))
         Preds1=list(preds=preds,DimPreds=dim(preds),dimModel=vision_dimensions,listImageBl=listImage)
         saveRDS(Preds1,pthSavePreds)
		 print(paste0(e," in  ",global_steps))
		 }
		 
}
#############################
 if (IncludeBlobAnalis==T){write.csv(resultBlob,pth_resultBlob,row.names = F)}
#}


if (ImageMaskConcetence==T){source("/home/ivan/GIT_HUB/PV_PL/Modules/Utilits/Image_Mask.r")}