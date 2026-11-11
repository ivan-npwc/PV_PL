#https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
#https://www.kaggle.com/code/bigironsphere/loss-function-library-keras-pytorch#Combo-Loss    SEE FOR LOS
#################################################################

  library(reticulate)
  library("tensorflow")
# Настройка Python
py_pth <- "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\tf_2_10_env/python.exe"
use_python(py_pth, required = TRUE)
use_condaenv("tf_2_10_env", required = TRUE)
tf$config$list_physical_devices('GPU')


  
library(tfdatasets)
library(tidyverse)
library(rsample)
library(raster)
library(unet)
library(keras)
##########################################
# Parameters -----------------------------------------------------
# List all available Conda environments	
	trainDir =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero"
	
	
	epochs =5
	batch_size=4L	
	vision_dimensions=512L
	ValTrainSplit = 0.9 #FIRST TRAIN 0.7 TO LOOK OVERFIT. SECOND TRAIN WITH EPOCH ACORDING FIRSt TRAIN
	NotUseEmptyImgs = T
	limitImgsSize=10000
	Species=basename(trainDir)
    dateTrain=format(Sys.time(),  "%Y%m%d") 
	images_dir=paste0(trainDir,"\\Image")
	masks_dir=paste0(trainDir,"\\Mask")

##########################################################################
source(file.path("C:\\Users\\usato\\SSL_DB\\PV_PL\\Modules\\UNET\\DeepLabV3_create.r"))
##################################################################################
 CheckImgMskDir=function (dirs) {  
    images_dir=paste0(dirs,"/Image")
masks_dir=paste0(dirs,"/Mask")
listImgS <<- list.files(images_dir)
ImgsExten=extension(listImgS)[1]
listImgSNoExt=substr(listImgS,1,nchar(listImgS)-nchar(ImgsExten))
                  if (length(listImgS)==0) {stop ("No Images found")}
     ListMskS<<- list.files(masks_dir)
MskExten=extension(ListMskS)[1]
MskNoExt=substr(ListMskS,1,nchar(ListMskS)-nchar(MskExten))
     deleteListImgs<<- listImgSNoExt[!(listImgSNoExt %in% MskNoExt)] # here is IMGS DELETE without msk
     deleteListMsks<<- MskNoExt[!(MskNoExt %in% listImgSNoExt)]
     deleteListImgs1=paste0(images_dir,"/",deleteListImgs,".jpg")
deleteListImgs2=paste0(images_dir,"/",deleteListImgs,".png")
     deleteListMsk2=paste0(masks_dir,"/",deleteListMsks,".png")
    if (file.exists(deleteListImgs1[1])){unlink(deleteListImgs1, recursive=T)}
if (file.exists(deleteListImgs2[1])){unlink(deleteListImgs2, recursive=T) }
if (file.exists(deleteListMsk2[1])){unlink(deleteListMsk2)}
print(paste0("Found   ",length(list.files(masks_dir)), "  Masks"))
print(paste0("Found   ",length(list.files(images_dir)), "  Image"))
if (length(list.files(masks_dir)) != length(list.files(images_dir))) stop()
}
  # CheckImgMskDir(trainDir)
   ################################################
	 print(paste0("Found   ",length(list.files(images_dir))))
	 print(paste0("Found   ",length(list.files(masks_dir))))
#################
	
	  data_tibl <- tibble::tibble(
	  img = list.files(images_dir, full.names = TRUE),
	  mask = list.files(masks_dir, full.names = TRUE),
	  imgSize=file.size(list.files(images_dir, full.names = TRUE)))
	  
	  
     if (NotUseEmptyImgs==T){data_tibl=data_tibl[data_tibl$imgSize > limitImgsSize,];print (paste0("Found   ",length(data_tibl$img), "  Imgs to TRAIN with size more than  ", limitImgsSize))}
	  data_train <- rsample::initial_split(data_tibl, prop = ValTrainSplit)
   
############################################
	random_bsh <- function(img) {                                           
	img %>% 
	tf$image$random_brightness(max_delta = 0.6) %>% 
	tf$image$random_contrast(lower = 0.1, upper = 1.1) %>% 
	tf$image$random_saturation(lower = 0.1, upper = 1.1) %>% 
	tf$clip_by_value(0, 1) # clip the values into [0,1] range.
	}
############
left_right  <- function(img) {img %>% tf$image$flip_left_right()}
up_down <- function(img) {img %>% tf$image$flip_up_down()}
###################################################################################################################	
	create_dataset <- function(data, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
		
		
	    dataset_map(~.x %>% list_modify(
		  img =tf$io$read_file(.x$img),
		  mask =tf$io$read_file(.x$mask)
		)) %>%
		
		
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_image(.x$img,  channels = 3,  expand_animations = FALSE),
		  mask = tf$image$decode_image(.x$mask, channels = 1, expand_animations = FALSE)
		))  %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = c(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = c(vision_dimensions, vision_dimensions))
		))
		
		dataset <- dataset %>%
		   dataset_map(~.x %>% list_modify(
		   img = tf$ensure_shape(.x$img, shape=list(vision_dimensions, vision_dimensions, 3L)),
		   mask = tf$ensure_shape(.x$mask, shape=list(vision_dimensions, vision_dimensions, 1L))
	    ))
		
	  if (rnorm(1) > 0) {
		dataset <- dataset %>% 
		            dataset_map(~.x %>% list_modify(
			        img = random_bsh(.x$img)
		  ))}

	  

	if ( rnorm(1)>0) {	  
	  dataset <- dataset %>% dataset_map(~.x %>% list_modify(img = left_right(.x$img))) 
	  dataset <- dataset %>% dataset_map(~.x %>% list_modify(mask = left_right(.x$mask))) 
	 } 
	  
	if ( rnorm(1)>0) {	  
	  dataset <- dataset %>% dataset_map(~.x %>% list_modify(img = up_down(.x$img))) 
	  dataset <- dataset %>% dataset_map(~.x %>% list_modify(mask = up_down(.x$mask))) 
	 }
	 
	 
	 	# mask <- tf$round(mask)  # Бинаризация масок
		  dataset <- dataset %>%
		   dataset_map(~.x %>% list_modify(
		                                   mask = tf$round(.x$mask)))
		   
	
	
	 
	 
	 
#		dataset <- dataset %>% 
#	  dataset_shuffle(buffer_size = batch_size*vision_dimensions)	  
	 
	 dataset3 <- dataset %>% 
		dataset_batch(batch_size)
	
	dataset3 %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
	training_dataset <- create_dataset(training(data_train), batch_size=batch_size, vision_dimensions=vision_dimensions)
########################################################################################################################################################################	
	create_val <- function(data, batch_size = batch_size, vision_dimensions) {  
	  dataset <- data %>% 
		tensor_slices_dataset() %>% 
	

	
	    dataset_map(~.x %>% list_modify(
		  img =tf$io$read_file(.x$img),
		  mask =tf$io$read_file(.x$mask)
		)) %>%
		
		
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_image(.x$img,  channels = 3,  expand_animations = FALSE),
		  mask = tf$image$decode_image(.x$mask, channels = 1, expand_animations = FALSE)
		))  %>% 



	
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
		  mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		  mask = tf$image$resize(.x$mask, size = shape(vision_dimensions, vision_dimensions))
		))
		
	  dataset <- dataset %>%
		   dataset_map(~.x %>% list_modify(
		   img = tf$ensure_shape(.x$img, shape=list(vision_dimensions, vision_dimensions, 3L)),
		   mask = tf$ensure_shape(.x$mask, shape=list(vision_dimensions, vision_dimensions, 1L))
	    ))
		
		
		  dataset <- dataset %>%
		   dataset_map(~.x %>% list_modify(
		                                   mask = tf$round(.x$mask)))
	#	dataset2 <- dataset %>% 
	#	  dataset_shuffle(buffer_size = batch_size*vision_dimensions)	
		  
	  dataset3 <- dataset %>% 
		dataset_batch(batch_size)
		
	  dataset3 %>% 
		dataset_map(unname) # Keras needs an unnamed output.
	}
 validation_dataset <- create_val(testing(data_train), batch_size=batch_size, vision_dimensions=vision_dimensions)

#####################################################################################################################
# Training -----------------------------------------------------
checkpoint_dir=paste0(trainDir,"\\Checkpoints");dir.create(checkpoint_dir,showWarnings = F)
BaseName <<- basename(file.path(checkpoint_dir, "val_{val_dice_coef:.2f}_epoch_{epoch:02d}.h5"))
filepath <<- paste0(checkpoint_dir,"\\",Species,"_",dateTrain,"_",BaseName)
output_name="train"

callbacks_list <- list(
  callback_tensorboard(paste0(checkpoint_dir,output_name,"_",vision_dimensions[1])),
#  callback_early_stopping(monitor = "dice_coef",
#                          min_delta = 1e-4,
#                          patience = 4,
#                          verbose = 1,
#                          mode = "max"),
# callback_reduce_lr_on_plateau(monitor = "dice_coef",
#                                factor = 0.1,
#                                patience = 4,
#                                verbose = 1,
#                                mode = "max"),
  callback_model_checkpoint(filepath = filepath,
                            monitor = "dice_coef",
							 period = 4)
                           )
#############################################################################
model %>% fit(
  training_dataset,  
  epochs = epochs,
  callbacks = callbacks_list, 
   validation_data = validation_dataset)
     


