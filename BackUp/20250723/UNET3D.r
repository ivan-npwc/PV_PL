
 library(keras)
   use_condaenv("base",required = TRUE)
   library("reticulate")
   conda_list <- conda_list()
   print(conda_list)
   Sys.sleep(5)
   py_config() 
  
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)
library(raster)
library(unet)
library(tensorflow)
 

# 3. Создание модели 3D U-Net
create_unet_3d <- function(input_shape, num_classes) {
  inputs <- layer_input(shape = input_shape)
  
  # Encoder
  conv1 <- inputs %>%
    layer_conv_3d(filters = 32, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_conv_3d(filters = 32, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2))
  
  conv2 <- conv1 %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_max_pooling_3d(pool_size = c(2, 2, 2))
  
  # Bottleneck
  conv3 <- conv2 %>%
    layer_conv_3d(filters = 128, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_conv_3d(filters = 128, kernel_size = c(3, 3, 3), activation = "relu", padding = "same")
  
  # Decoder
  up4 <- layer_concatenate(list(
    layer_upsampling_3d(size = c(2, 2, 2))(conv3),
    conv2
  ))
  conv4 <- up4 %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_conv_3d(filters = 64, kernel_size = c(3, 3, 3), activation = "relu", padding = "same")
  
  up5 <- layer_concatenate(list(
    layer_upsampling_3d(size = c(2, 2, 2))(conv4),
    conv1
  ))
  conv5 <- up5 %>%
    layer_conv_3d(filters = 32, kernel_size = c(3, 3, 3), activation = "relu", padding = "same") %>%
    layer_conv_3d(filters = 32, kernel_size = c(3, 3, 3), activation = "relu", padding = "same")
  
  # Output
  outputs <- conv5 %>%
    layer_conv_3d(filters = num_classes, kernel_size = c(1, 1, 1), activation = "softmax")
  
  model <- keras_model(inputs = inputs, outputs = outputs)
  return(model)
}

# Параметры модели
input_shape <- c(128, 128, 64, 1)  # пример размера входного объема (может потребоваться изменить)
num_classes <- 3  # количество классов для сегментации

model <- create_unet_3d(input_shape, num_classes)

# Компиляция модели
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)
