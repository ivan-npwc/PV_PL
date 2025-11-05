

# DeepLabV3+ Модель для сегментации клеток
create_deeplabv3_plus_cell <- function(input_shape = c(512, 512, 3), num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  
  # Загрузка предобученной ResNet50
  base_model <- application_resnet50(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = inputs
  )
  
  # Получаем нужные слои для ASPP и декодера
  aspp_input <- base_model$output
  low_level_feat <- base_model$get_layer("conv2_block3_out")$output
  
  cat("ASPP input shape:", dim(aspp_input), "\n")
  cat("Low-level features shape:", dim(low_level_feat), "\n")
  
  # ASPP модуль
  aspp_output <- aspp_module_cell(aspp_input)
  
  # Декодер с правильным выравниванием размеров
  decoder_output <- deeplab_decoder_cell(aspp_output, low_level_feat, num_classes)
  
  # Финальный апсэмплинг до исходного размера
  final_upsample_factor <- input_shape[1] / dim(decoder_output)[2]
  final_output <- decoder_output %>%
    layer_upsampling_2d(size = c(final_upsample_factor, final_upsample_factor), 
                       interpolation = "bilinear")
  
  model <- keras_model(inputs = inputs, outputs = final_output)
  return(model)
}
#############
# ASPP модуль для сегментации клеток
aspp_module_cell <- function(input_tensor, filters = 256) {
  
  # Branch 1: 1x1 convolution
  branch1 <- input_tensor %>%
    layer_conv_2d(filters, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 2: 3x3 convolution with rate = 6
  branch2 <- input_tensor %>%
    layer_conv_2d(filters, 3, dilation_rate = 6, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 3: 3x3 convolution with rate = 12
  branch3 <- input_tensor %>%
    layer_conv_2d(filters, 3, dilation_rate = 12, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 4: 3x3 convolution with rate = 18
  branch4 <- input_tensor %>%
    layer_conv_2d(filters, 3, dilation_rate = 18, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 5: Image Pooling
  input_shape <- dim(input_tensor)
  branch5 <- input_tensor %>%
    layer_global_average_pooling_2d() %>%
    layer_reshape(c(1, 1, input_shape[4])) %>%
    layer_conv_2d(filters, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = c(input_shape[2], input_shape[3]), interpolation = "bilinear")
  
  # Concatenate all branches
  concatenated <- layer_concatenate(list(branch1, branch2, branch3, branch4, branch5))
  
  # Final 1x1 convolution
  output <- concatenated %>%
    layer_conv_2d(filters, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1)
  
  return(output)
}
#############
# Декодер DeepLabV3+ для сегментации клеток
deeplab_decoder_cell <- function(aspp_output, low_level_feat, num_classes) {
  
  # Обработка low-level features
  low_level_feat_processed <- low_level_feat %>%
    layer_conv_2d(48, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Вычисляем коэффициент апсэмплинга
  aspp_shape <- dim(aspp_output)
  low_level_shape <- dim(low_level_feat_processed)
  upsample_factor <- low_level_shape[2] / aspp_shape[2]
  
  cat("Upsampling ASPP by factor:", upsample_factor, "\n")
  
  # Апсэмплинг ASPP output
  aspp_upsampled <- aspp_output %>%
    layer_upsampling_2d(size = c(upsample_factor, upsample_factor), 
                       interpolation = "bilinear")
  
  cat("After upsampling:\n")
  cat("  ASPP shape:", dim(aspp_upsampled), "\n")
  cat("  Low-level shape:", dim(low_level_feat_processed), "\n")
  
  # Конкатенация
  concatenated <- layer_concatenate(list(aspp_upsampled, low_level_feat_processed))
  
  # Декодерные свертки
  x <- concatenated %>%
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1) %>%
    
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1)
  
  # Финальная классификация
  output <- x %>%
    layer_conv_2d(num_classes, 1, padding = "same", activation = "sigmoid")
  
  return(output)
}

# Создание модели DeepLabV3+
cat("Creating DeepLabV3+ model for cell segmentation...\n")
model_DeepLabV3 <<- create_deeplabv3_plus_cell(input_shape = c(img_height, img_width, 3))

# Функции потерь и метрик для сегментации клеток
dice_coef <- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

iou_metric <- custom_metric("iou", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  union <- k_sum(y_true_f) + k_sum(y_pred_f) - intersection
  (intersection + smooth) / (union + smooth)
})

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)

# Компиляция модели
cat("Compiling DeepLabV3+ model...\n")
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss = dice_coef_loss,
  metrics = list(dice_coef, iou_metric, "binary_accuracy")
)

