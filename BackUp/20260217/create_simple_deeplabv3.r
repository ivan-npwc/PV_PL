create_simple_deeplabv3 <- function(input_shape = c(512, 512, 3), num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  
  # Используем предобученный MobileNetV2 с правильным input_shape
  base_model <- application_mobilenet_v2(
    input_tensor = inputs,
    weights = "imagenet", 
    include_top = FALSE)
#    alpha = 0.35  # Упрощенная версия
#  )
  
  # Получаем нужные слои для ASPP (правильные имена для MobileNetV2)
  aspp_input <- base_model$output  # output_shape: (None, 16, 16, 128)
  low_level_feat <- base_model$get_layer("block_2_expand_relu")$output  # output_shape: (None, 128, 128, 48)
  
  cat("ASPP input shape:", dim(aspp_input), "\n")
  cat("Low-level features shape:", dim(low_level_feat), "\n")
  
  # Упрощенный ASPP модуль
  aspp_output <- simple_aspp_module(aspp_input)
  
  # Упрощенный декодер с правильными размерами
  decoder_output <- simple_deeplab_decoder(aspp_output, low_level_feat, num_classes)
  
  # Финальный выход
  outputs <- layer_conv_2d(decoder_output, num_classes, 1, activation = "sigmoid")
  
  model <- keras_model(inputs = inputs, outputs = outputs)
  return(model)
}

# Упрощенный ASPP
simple_aspp_module <- function(input_tensor, filters = 128) {
  
  # Branch 1: 1x1 convolution
  branch1 <- input_tensor %>%
    layer_conv_2d(filters, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 2: 3x3 dilation rate 6
  branch2 <- input_tensor %>%
    layer_conv_2d(filters, 3, dilation_rate = 6, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 3: 3x3 dilation rate 12
  branch3 <- input_tensor %>%
    layer_conv_2d(filters, 3, dilation_rate = 12, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Global pooling branch
  input_shape <- dim(input_tensor)
  branch4 <- input_tensor %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(filters) %>%
    layer_activation("relu") %>%
    layer_reshape(c(1, 1, filters)) %>%
    layer_upsampling_2d(size = c(input_shape[2], input_shape[3]))
  
  # Concatenate
  concatenated <- layer_concatenate(list(branch1, branch2, branch3, branch4))
  
  output <- concatenated %>%
    layer_conv_2d(filters, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  return(output)
}

# ИСПРАВЛЕННЫЙ декодер с правильными размерами
simple_deeplab_decoder <- function(aspp_output, low_level_feat, num_classes) {
  
  # Обработка low-level features
  low_level_feat_processed <- low_level_feat %>%
    layer_conv_2d(48, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Вычисляем правильный коэффициент апсэмплинга
  aspp_shape <- dim(aspp_output)  # (None, 16, 16, 128)
  low_level_shape <- dim(low_level_feat_processed)  # (None, 128, 128, 48)
  
  upsample_factor <- low_level_shape[2] / aspp_shape[2]  # 128 / 16 = 8
  
  cat("Upsampling factor:", upsample_factor, "\n")
  cat("ASPP shape before upsample:", dim(aspp_output), "\n")
  cat("Low-level shape:", dim(low_level_feat_processed), "\n")
  
  # Апсэмплинг ASPP output до размера low-level features
  aspp_upsampled <- aspp_output %>%
    layer_upsampling_2d(size = c(upsample_factor, upsample_factor), 
                       interpolation = "bilinear")
  
  cat("ASPP shape after upsample:", dim(aspp_upsampled), "\n")
  
  # Конкатенация (теперь размеры должны совпадать)
  concatenated <- layer_concatenate(list(aspp_upsampled, low_level_feat_processed))
  
  cat("After concatenation:", dim(concatenated), "\n")
  
  # Несколько сверточных слоев
  x <- concatenated %>%
    layer_conv_2d(128, 3, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(128, 3, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Финальный апсэмплинг до исходного размера (512x512)
  # Текущий размер: 128x128, нужно увеличить в 4 раза до 512x512
  output <- x %>%
    layer_upsampling_2d(size = c(4, 4), interpolation = "bilinear")
  
  cat("Final output shape:", dim(output), "\n")
  
  return(output)
}


dice_coef <- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
model = create_simple_deeplabv3()

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss = dice_coef_loss,
  metrics = list(dice_coef)) #list(dice_coef, iou_metric, "binary_accuracy")

##################################################################################################


