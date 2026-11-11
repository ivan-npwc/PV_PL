 #  source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/DeepLabV3//DeepLabV3_CREATE_rtx5080.r") 

########################################################
library(reticulate)

py_run_string("
import ctypes, os
conda_lib = '/home/ivan/miniconda3/envs/tf_gpu/lib'
wsl_lib = '/usr/lib/wsl/lib'

# Драйвер (из WSL)
ctypes.CDLL(f'{wsl_lib}/libcuda.so.1', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{wsl_lib}/libnvidia-ml.so.1', mode=ctypes.RTLD_GLOBAL)

# CUDA библиотеки (из conda — правильные версии)
ctypes.CDLL(f'{conda_lib}/libcublasLt.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcublas.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcudnn.so.9', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcudart.so.12', mode=ctypes.RTLD_GLOBAL)
print('All libs preloaded OK')
")

library(tensorflow)
print(tf$config$list_physical_devices("GPU"))

#############################################################
library(reticulate)
library(tensorflow)
library(keras3)  # Keras 3 для TF 2.20+

img_height <- 128L
img_width  <- 128L

# ============================================================
# Утилита: извлечь числовое значение из shape-тензора
# $shape возвращает Python TensorShape, нужно достать int
# ============================================================
get_shape_dim <- function(tensor, axis) {
  as.integer(tensor$shape[[axis]])
}
# ============================================================
# ASPP модуль для сегментации клеток
# ============================================================
aspp_module_cell <- function(input_tensor, filters = 256L) {
  
  h <- get_shape_dim(input_tensor, 2)
  w <- get_shape_dim(input_tensor, 3)
  ch <- get_shape_dim(input_tensor, 4)
  
  # Branch 1: 1x1 convolution
  branch1 <- input_tensor %>%
    layer_conv_2d(filters = filters, kernel_size = 1L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 2: 3x3 convolution with rate = 6
  branch2 <- input_tensor %>%
    layer_conv_2d(filters = filters, kernel_size = 3L, dilation_rate = 6L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 3: 3x3 convolution with rate = 12
  branch3 <- input_tensor %>%
    layer_conv_2d(filters = filters, kernel_size = 3L, dilation_rate = 12L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 4: 3x3 convolution with rate = 18
  branch4 <- input_tensor %>%
    layer_conv_2d(filters = filters, kernel_size = 3L, dilation_rate = 18L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 5: Image Pooling (Global Average Pooling -> 1x1 -> upsample back)
  branch5 <- input_tensor %>%
    layer_global_average_pooling_2d(keepdims = TRUE) %>%
    layer_conv_2d(filters = filters, kernel_size = 1L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = c(h, w), interpolation = "bilinear")
  
  # Concatenate all branches
  concatenated <- layer_concatenate(list(branch1, branch2, branch3, branch4, branch5))
  
  # Final 1x1 convolution
 output <- concatenated %>%
    layer_conv_2d(filters = filters, kernel_size = 1L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(rate = 0.1)
  
  return(output)
}

# ============================================================
# Декодер DeepLabV3+
# ============================================================
deeplab_decoder_cell <- function(aspp_output, low_level_feat, num_classes = 1L) {
  
  # Обработка low-level features: уменьшаем каналы до 48
  low_level_feat_processed <- low_level_feat %>%
    layer_conv_2d(filters = 48L, kernel_size = 1L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Вычисляем коэффициент апсэмплинга
  aspp_h <- get_shape_dim(aspp_output, 2)
  low_h  <- get_shape_dim(low_level_feat_processed, 2)
  upsample_factor <- as.integer(low_h / aspp_h)
  
  cat("Decoder upsample factor:", upsample_factor, "\n")
  
  # Апсэмплинг ASPP output до размера low-level features
  aspp_upsampled <- aspp_output %>%
    layer_upsampling_2d(size = c(upsample_factor, upsample_factor),
                        interpolation = "bilinear")
  
  cat("  ASPP upsampled shape:", as.character(aspp_upsampled$shape), "\n")
  cat("  Low-level shape:     ", as.character(low_level_feat_processed$shape), "\n")
  
  # Конкатенация
  concatenated <- layer_concatenate(list(aspp_upsampled, low_level_feat_processed))
  
  # Декодерные свертки
  x <- concatenated %>%
    layer_conv_2d(filters = 256L, kernel_size = 3L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(rate = 0.1) %>%
    layer_conv_2d(filters = 256L, kernel_size = 3L, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(rate = 0.1)
  
  # Финальная классификация
  output <- x %>%
    layer_conv_2d(filters = num_classes, kernel_size = 1L, padding = "same", activation = "sigmoid")
  
  return(output)
}

# ============================================================
# Основная функция создания модели DeepLabV3+
# ============================================================
create_deeplabv3_plus_cell <- function(input_shape = c(512L, 512L, 3L), num_classes = 1L) {
  
  inputs <- layer_input(shape = input_shape)
  
  # Загрузка предобученной ResNet50
  base_model <- application_resnet50(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = inputs
  )
  
  # Выходы энкодера
  aspp_input    <- base_model$output                                # (None, H/32, W/32, 2048)
  low_level_feat <- base_model$get_layer("conv2_block3_out")$output  # (None, H/4,  W/4,  256)
  
  cat("ASPP input shape:      ", as.character(aspp_input$shape), "\n")
  cat("Low-level features shape:", as.character(low_level_feat$shape), "\n")
  
  # ASPP модуль
  aspp_output <- aspp_module_cell(aspp_input)
  
  # Декодер
  decoder_output <- deeplab_decoder_cell(aspp_output, low_level_feat, num_classes)
  
  # Финальный апсэмплинг до исходного размера
  decoder_h <- get_shape_dim(decoder_output, 2)
  final_upsample_factor <- as.integer(input_shape[1] / decoder_h)
  
  cat("Final upsample factor:", final_upsample_factor, "\n")
  
  final_output <- decoder_output %>%
    layer_upsampling_2d(size = c(final_upsample_factor, final_upsample_factor),
                        interpolation = "bilinear")
  
  model <- keras_model(inputs = inputs, outputs = final_output)
  return(model)
}

# ============================================================
# Создание модели
# ============================================================
model <- create_deeplabv3_plus_cell(input_shape = c(img_height, img_width, 3L))

cat("\n=== Model Summary ===\n")
print(model)

# ============================================================
# Метрики и функция потерь
# ============================================================
py_run_string("
import tensorflow as tf

@tf.function
def dice_coef(y_true, y_pred):
    smooth = 1.0
    y_true_f = tf.reshape(y_true, [-1])
    y_pred_f = tf.reshape(y_pred, [-1])
    intersection = tf.reduce_sum(y_true_f * y_pred_f)
    return (2.0 * intersection + smooth) / (tf.reduce_sum(y_true_f) + tf.reduce_sum(y_pred_f) + smooth)

def dice_loss(y_true, y_pred):
    return 1.0 - dice_coef(y_true, y_pred)
")

dice_coef <- py$dice_coef
dice_loss <- py$dice_loss

# ============================================================
# ============================================================
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss = dice_loss,
  metrics = list(dice_coef)
)


