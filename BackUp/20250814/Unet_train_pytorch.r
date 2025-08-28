 
   trainDir =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero"
 
    img_dir = paste0(trainDir,"\\Image")
    mask_dir = paste0(trainDir,"\\Mask")
 
   library(reticulate)
   library(fs) 
   library(EBImage) 
 #  library(torchdatasets)
  #  library(torchvision)
  # use_condaenv("r-pytorch-gpu")
   use_python("C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-pytorch-gpu/python.exe")
    #py_config()
	#py_list_packages()  
   smp <- import("segmentation_models_pytorch")
   torch <- import("torch")
   #####################    
   torch$version$cuda
   torch$cuda$get_device_name(0L)
   smp$encoders$get_encoder_names()
   #################
   AMP=T
   batch_intens=1
   globalsize=640
   batch_size=16
   target_size=c(256, 256)
   augment = TRUE
   epochs = 20
   lr = 0.0001
   imgsCount=length(list.files(img_dir))
   steps = round(globalsize/batch_size*batch_intens)
  ######################################################################### 
   # Создание модели U-Net с ResNet34 в качестве энкодера
   unet <- smp$Unet(
     encoder_name = "resnet34",       # Или "efficientnet-b0", "mobilenet_v2"
     encoder_weights = "imagenet",    # Предобученные веса ImageNet
     in_channels = 3L,                # 3 канала для RGB
     classes = 1L                     # 1 класс для бинарной сегментации
    )

   unet$to("cuda")
   summary(unet)
   
#####################################################################################
augment_image <- function(img, mask) {
  # Яркость/контрастность
  if (runif(1) > 0.5) {
    img <- img * runif(1, 0.7, 1.3)           # Яркость
    img <- (img - mean(img)) * runif(1, 0.7, 1.5) + mean(img)  # Контрастность
  }
  
  # Отражения
  if (runif(1) > 0.5) img <- flop(img)        # По горизонтали
  if (runif(1) > 0.5) img <- flip(img)        # По вертикали
  
  # Нормализация
  img <- (img - min(img)) / (max(img) - min(img))
  
  list(img = img, mask = mask)
}
##################################################################################

load_dataset <- function(img_dir, mask_dir, augment, index) {
  img_paths <- dir_ls(img_dir)[index]
  mask_paths <- dir_ls(mask_dir)[index]
  
  images <- list()
  masks <- list()
  
  for (i in seq_along(img_paths)) {
    img <- EBImage::readImage(img_paths[i])
    mask <- EBImage::readImage(mask_paths[i])
    
	img <-    EBImage::resize(img, 256, 256)
	mask <-    EBImage::resize(mask, 256, 256)
	

    if (augment) {
      augmented <- augment_image(img, mask)
      img <- augmented$img
      mask <- augmented$mask
    }
    
	 # Преобразуем в правильный формат [channels, height, width]
  img_array <- aperm(as.array(img), c(3, 1, 2))  # Из [H,W,C] в [C,H,W]
  mask_array <- as.array(mask)  # Для масок берем только первый канал
	
	
	
    # Приведение к тензорам
    images[[i]] <- torch$tensor(img_array, dtype = torch$float32)
    masks[[i]] <- torch$tensor(mask_array, dtype = torch$float32)
  }
  
  list(images = images, masks = masks)
}
###############################################################
train_unet <- function(epochs = epochs, batch_size = batch_size) {
  # Загрузка данных

  # Оптимизатор и функция потерь
  optimizer <- torch$optim$Adam(unet$parameters(), lr = lr)
  criterion <- torch$nn$BCEWithLogitsLoss()
  
  for (y in 1:epochs) {
      unet$train()
	  epoch_loss <- 0
	  
    for (i in 1:steps) {
  
      index_btch=sample(1:imgsCount)[1:batch_size]
	  data <- load_dataset(img_dir, mask_dir, augment = T, index=index_btch)
	  
      batch_images <- torch$stack(data$images)$to("cuda")
      batch_masks <- torch$stack(data$masks)$to("cuda")
      
      # Forward pass
      optimizer$zero_grad()
	  #######################################  
      outputs <- unet(batch_images)
	  batch_masks <- batch_masks$unsqueeze(1L)
      loss <- criterion(outputs, batch_masks)
      
      # Backward pass
      loss$backward()
      optimizer$step()
	 # epoch_loss <- epoch_loss + loss$item()

      cat(sprintf("Epoch: %d, Batch: %d, Loss: %.4f\n", 
                 y, ceiling(i/batch_size), loss$item()))
				 
	torch$save(unet$state_dict(), sprintf("checkpoint_%d.pth", y))			 
    }
  }
  
}
#################################################################
# Запуск обучения
train_unet(epochs = epochs, batch_size = batch_size)








old=function(){
########################################


###########################################################
# Генератор данных (не загружает все файлы сразу)
image_generator <- function(img_dir, mask_dir, batch_size, augment = TRUE) {
  img_paths <- dir_ls(img_dir, glob = "*.jpg")
  mask_paths <- dir_ls(mask_dir, glob = "*.png")
  
  function() {
    indices <- sample(1:length(img_paths), batch_size)
    batch <- list(images = list(), masks = list())
    
    for (i in indices) {
      img <- EBImage::readImage(img_paths[i]) 
      img <-    EBImage::resize(img, 256, 256)
      
      mask <- EBImage::readImage(mask_paths[i])  
      mask <- EBImage::resize(mask, 256, 256)
	
	if (augment) {
      augmented <- augment_image(img, mask)
      img <- augmented$img
      mask <- augmented$mask
    }		  
      
	  
	    # Преобразуем в правильный формат [channels, height, width]
         img_array <- aperm(as.array(img), c(3, 1, 2))  # Из [H,W,C] в [C,H,W]
         mask_array <- as.array(mask)  # Для масок берем только первый канал
	  
          # Приведение к тензорам
         batch$images[[i]] <- torch$tensor(img_array, dtype = torch$float32)
         batch$masks[[i]] <- torch$tensor(mask_array, dtype = torch$float32)
    }
    
    list(
      images = batch$images,
      masks = batch$masks
    )
  }
}
#################################################################
train_large_dataset <- function(epochs = epochs, batch_size = batch_size) {

  gen <- image_generator(img_dir, mask_dir, batch_size = batch_size, augment = TRUE)
  optimizer <- torch$optim$Adam(unet$parameters(), lr = lr)
  criterion <- torch$nn$BCEWithLogitsLoss()
  
  for (epoch in 1:epochs) {
  seq(1, length(data$images), by = batch_size)
    for (batch_num in 1:seq(1, length(data$images), by = batch_size)) {  
      batch <- gen()
	  
	    batch_images <- torch$stack(batch$images)$to("cuda")
        batch_masks <- torch$stack(batch$images)$to("cuda")
	  
      batch$images <- batch$images$to("cuda")
      batch$masks <- batch$masks$to("cuda")
      
      optimizer$zero_grad()
      outputs <- unet(batch$images)
      loss <- criterion(outputs, batch$masks)
      loss$backward()
      optimizer$step()
      
      if (batch_num %% 50 == 0) {
        cat(sprintf("Epoch: %d, Batch: %d, Loss: %.4f\n", epoch, batch_num, loss$item()))
      }
    }
  }
}

##################################################################################
train_large_dataset()
}










