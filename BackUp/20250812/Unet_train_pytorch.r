 
   trainDir =   "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero"
 
    img_dir = paste0(trainDir,"\\Image")
    mask_dir = paste0(trainDir,"\\Mask")
 
   library(reticulate)
   library(fs) 
   library(EBImage) 
   #use_condaenv("r-pytorch-gpu")
   use_python("C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-pytorch-gpu/python.exe")
   smp <- import("segmentation_models_pytorch")
   torch <- import("torch")
   smp$encoders$get_encoder_names()
   #################
   batch_size=8
   target_size=c(256, 256)
   augment = TRUE
   epochs = 2
    lr = 0.0001
   imgsCount=length(list.files(img_dir))
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
   ##############################################################
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
###########################################################
# Генератор данных (не загружает все файлы сразу)
image_generator <- function(img_dir, mask_dir, batch_size, target_size, augment = TRUE) {
  img_paths <- dir_ls(img_dir, glob = "*.jpg")
  mask_paths <- dir_ls(mask_dir, glob = "*.png")
  
  function() {
    indices <- sample(1:length(img_paths), batch_size)
    batch <- list(images = list(), masks = list())
    
    for (i in indices) {
      img <- EBImage::readImage(img_paths[i]) |> 
             EBImage::resize(target_size[1], target_size[2])
      
      mask <- EBImage::readImage(mask_paths[i]) |> 
              EBImage::resize(target_size[1], target_size[2])
	
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
      images = torch$stack(batch$images),
      masks = torch$stack(batch$masks)
    )
  }
}
#################################################################
train_large_dataset <- function(epochs = epochs, batch_size = batch_size) {
  device <- if (torch$cuda$is_available()) "cuda" else "cpu"
  unet <- init_unet_model(device)  # Ваша функция инициализации модели
  
  gen <- image_generator("Image", "Mask", batch_size = batch_size)
  optimizer <- torch$optim$Adam(unet$parameters(), lr = lr)
  criterion <- torch$nn$BCEWithLogitsLoss()
  
  for (epoch in 1:epochs) {
    for (batch_num in 1:ceiling(imgsCount/batch_size)) {  # Пример для 100K изображений
      batch <- gen()
      batch$images <- batch$images$to(device)
      batch$masks <- batch$masks$to(device)
      
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

######################################################################################
#####################################################################################
old=function(){


load_dataset <- function(img_dir, mask_dir, augment = TRUE) {
  img_paths <- dir_ls(img_dir)
  mask_paths <- dir_ls(mask_dir)
  
  images <- list()
  masks <- list()
  
  for (i in seq_along(img_paths)) {
    img <- EBImage::readImage(img_paths[i])
    mask <- EBImage::readImage(mask_paths[i])
    
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
train_unet <- function(epochs = 10, batch_size = 8) {
  # Загрузка данных
  data <- load_dataset(img_dir, mask_dir)
  
  # Оптимизатор и функция потерь
  optimizer <- torch$optim$Adam(unet$parameters(), lr = 0.001)
  criterion <- torch$nn$BCEWithLogitsLoss()
  
  for (epoch in 1:epochs) {
    for (i in seq(1, length(data$images), by = batch_size)) {
      # Формирование батча
	if(i==1){start_idx=1} 
	  end_idx <- min(start_idx + batch_size - 1, length(data$images))
	  
      batch_images <- torch$stack(data$images[start_idx:end_idx])$to("cuda")
      batch_masks <- torch$stack(data$masks[start_idx:end_idx])$to("cuda")
      
      # Forward pass
      optimizer$zero_grad()
      outputs <- unet(batch_images)
	   batch_masks <- batch_masks$unsqueeze(1L)
      loss <- criterion(outputs, batch_masks)
      
      # Backward pass
      loss$backward()
      optimizer$step()
      
      cat(sprintf("Epoch: %d, Batch: %d, Loss: %.4f\n", 
                 epoch, ceiling(i/batch_size), loss$item()))
	start_idx = end_idx
    }
  }
}
#################################################################
# Запуск обучения
train_unet(epochs = 20)
}











