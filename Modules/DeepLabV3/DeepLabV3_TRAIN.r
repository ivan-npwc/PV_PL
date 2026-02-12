

#source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/DeepLabV3_TRAIN.r")


library(reticulate)
library(tensorflow)
# Настройка Python
#py_pth <- "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\tf_2_10_env/python.exe"
#use_python(py_pth, required = TRUE)
#use_condaenv("tf_2_10_env", required = TRUE)
#tf$config$list_physical_devices('GPU')

library(keras)
library(tfdatasets)
library(tidyverse)
library(tensorflow)

# Параметры
trainDir <-  "/home/ivan/TRAIN/LRG/Crops"
images_dir <- file.path(trainDir, "Image")
masks_dir <- file.path(trainDir, "Mask")

epoch1 <- 50
epoch2=50
dropout = 0.5 #0.1
batch_size <- 32L
img_height <- 128L
img_width <- 128L
validation_split <- 0.2

# Проверка данных
cat("Images found:", length(list.files(images_dir)), "\n")
cat("Masks found:", length(list.files(masks_dir)), "\n")

# Создание датафрейма с путями
image_files <- list.files(images_dir, full.names = TRUE, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
mask_files <- list.files(masks_dir, full.names = TRUE, pattern = "\\.(png|jpg|jpeg)$", ignore.case = TRUE)

# Проверка соответствия имен
get_base_name <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

image_names <- sapply(image_files, get_base_name)
mask_names <- sapply(mask_files, get_base_name)

# Находим общие имена
common_names <- intersect(image_names, mask_names)
cat("Common image-mask pairs:", length(common_names), "\n")

if (length(common_names) == 0) {
  stop("No matching image-mask pairs found!")
}

# Фильтруем файлы по общим именам
image_files <- image_files[image_names %in% common_names]
mask_files <- mask_files[mask_names %in% common_names]

# Упорядочиваем по именам
image_files <- image_files[order(image_names[image_names %in% common_names])]
mask_files <- mask_files[order(mask_names[mask_names %in% common_names])]

data_df <- data.frame(
  image = image_files,
  mask = mask_files,
  stringsAsFactors = FALSE
)

# Предобработка для DeepLabV3+ (ResNet50 backbone)
preprocess_image <- function(image_path) {
  image <- tf$io$read_file(image_path)
  image <- tf$image$decode_image(image, channels = 3, expand_animations = FALSE)
  image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  image <- tf$image$resize(image, size = c(img_height, img_width))
  # ResNet50 предобработка
  #image <- tf$keras$applications$resnet$preprocess_input(image)
  return(image)
}

preprocess_mask <- function(mask_path) {
  mask <- tf$io$read_file(mask_path)
  mask <- tf$image$decode_image(mask, channels = 1, expand_animations = FALSE)
  mask <- tf$image$convert_image_dtype(mask, dtype = tf$float32)
  mask <- tf$image$resize(mask, size = c(img_height, img_width))
  mask <- tf$round(mask)  # Бинаризация масок
  return(mask)
}

# Создание tf.data.Dataset
create_dataset <- function(df, batch_size, shuffle = FALSE, augment = FALSE) {
  
  dataset <- tensor_slices_dataset(list(df$image, df$mask)) %>%
    dataset_map(function(image_path, mask_path) {
      image <- preprocess_image(image_path)
      mask <- preprocess_mask(mask_path)
      list(image, mask)
    }) %>%
    dataset_map(function(image, mask) {
      # Гарантируем правильную форму
      image <- tf$ensure_shape(image, list(img_height, img_width, 3L))
      mask <- tf$ensure_shape(mask, list(img_height, img_width, 1L))
      list(image, mask)
    })
  
  if (shuffle) {
    dataset <- dataset %>% dataset_shuffle(buffer_size = nrow(df))
  }
  
  if (augment) {
    dataset <- dataset %>% 
      dataset_map(function(image, mask) {
        # Случайное отражение по горизонтали
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            list(tf$image$flip_left_right(image), tf$image$flip_left_right(mask))
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Flip up-down
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            list(tf$image$flip_up_down(image), tf$image$flip_up_down(mask))
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Random zoom
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            scale_factor <- runif(1, 0.3, 1.9)  # Меньший диапазон для сохранения деталей
            new_height <- as.integer(img_height * scale_factor)
            new_width <- as.integer(img_width * scale_factor)
            downscaled_image <- tf$image$resize(image, size = c(new_height, new_width))
            downscaled_mask <- tf$image$resize(mask, size = c(new_height, new_width))
            list(
              tf$image$resize(downscaled_image, size = c(img_height, img_width)),
              tf$image$resize(downscaled_mask, size = c(img_height, img_width))
            )
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Яркость (только для изображения)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_brightness(image, max_delta = 0.5),
          false_fn = function() image
        )
        
        # Контраст (только для изображения)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_contrast(image, lower = 0.5, upper = 1.5),
          false_fn = function() image
        )
        
        list(image, mask)
      })
  }
  
  dataset <- dataset %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)
  
  return(dataset)
}

# Разделение на train/validation
set.seed(123)
train_indices <- sample(1:nrow(data_df), size = round((1 - validation_split) * nrow(data_df)))
train_df <- data_df[train_indices, ]
val_df <- data_df[-train_indices, ]

cat("Training samples:", nrow(train_df), "\n")
cat("Validation samples:", nrow(val_df), "\n")

# Создание датасетов
train_dataset <- create_dataset(train_df, batch_size, shuffle = FALSE, augment = TRUE)
val_dataset <- create_dataset(val_df, batch_size, shuffle = FALSE, augment = FALSE)

# Проверка одного батча
check_batch <- function(dataset) {
  iterator <- as_iterator(dataset)
  batch <- iter_next(iterator)
  cat("Batch image shape:", batch[[1]]$shape$as_list(), "\n")
  cat("Batch mask shape:", batch[[2]]$shape$as_list(), "\n")
  cat("Image range:", as.numeric(tf$reduce_min(batch[[1]])), "to", as.numeric(tf$reduce_max(batch[[1]])), "\n")
  cat("Mask range:", as.numeric(tf$reduce_min(batch[[2]])), "to", as.numeric(tf$reduce_max(batch[[2]])), "\n")
}

cat("=== Training batch check ===\n")
check_batch(train_dataset)
cat("=== Validation batch check ===\n")
check_batch(val_dataset)

################################################################
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
model <- create_deeplabv3_plus_cell(input_shape = c(img_height, img_width, 3))

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
  optimizer = optimizer_adam(learning_rate = 1e-6),
  loss = dice_coef_loss,
  metrics = dice_coef)#list(dice_coef, iou_metric, "binary_accuracy")


# Колбэки
checkpoint_dir <- file.path(trainDir, "checkpoints_deeplabv3")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
BaseName <- basename(file.path(checkpoint_dir, "Val_{val_dice_coef:.3f}_epoch_{epoch:02d}.h5"))
filepath <- paste0(checkpoint_dir, "\\CellSegmentation_DeepLabV3_", BaseName)

callbacks <- list(
  callback_model_checkpoint(
    filepath = filepath,
    monitor = "val_dice_coef",
    save_best_only = TRUE,
    mode = "max",
    verbose = 1
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_dice_coef",
    factor = 0.5,
    patience = 8,
    verbose = 1,
    mode = "max",
    min_lr = 1e-8
  ),
  callback_early_stopping(
    monitor = "val_dice_coef",
    patience = 20,
    verbose = 1,
    mode = "max",
    restore_best_weights = TRUE
  ),
  callback_tensorboard(
    log_dir = file.path(checkpoint_dir, "logs")
  )
)

# Обучение модели
cat("Starting DeepLabV3+ training for cell segmentation...\n")
cat("Model features:\n")
cat("- ResNet50 backbone with ImageNet weights\n")
cat("- ASPP module with multi-scale context\n") 
cat("- DeepLabV3+ decoder with skip connections\n")
cat("- Dice loss optimized for segmentation\n")
cat("- Input size:", img_height, "x", img_width, "\n")

history <- model %>% fit(
  train_dataset,
  epochs = epoch1,
  validation_data = val_dataset,
  callbacks = callbacks,
  verbose = 1
)

# Сохранение финальной модели
final_model_path <- file.path(checkpoint_dir, "final_deeplabv3_cell_segmentation.h5")
model %>% save_model_hdf5(final_model_path)
cat("Training completed! Final model saved to:", final_model_path, "\n")

# Функция для разморозки и дообучения (опционально)
unfreeze_and_finetune <- function(model, learning_rate = 1e-6) {
  # Размораживаем ResNet backbone для тонкой настройки
  for (layer in model$layers) {
    if (grepl("resnet50", layer$name)) {
      layer$trainable <- TRUE
    }
  }
  
  # Перекомпилируем с меньшим learning rate
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = dice_coef_loss,
    metrics = dice_coef)#list(dice_coef, iou_metric, "binary_accuracy")

  
  return(model)
}

# Опционально: дообучение с размороженными слоями
cat("Starting fine-tuning with unfrozen backbone...\n")
model <- unfreeze_and_finetune(model, learning_rate = 1e-6)

history_finetune <- model %>% fit(
  train_dataset,
  epochs = epoch2,
  validation_data = val_dataset,
  callbacks = callbacks,
  verbose = 1
)

# Сохранение дообученной модели
finetuned_model_path <- file.path(checkpoint_dir, "finetuned_deeplabv3_cell_segmentation.h5")
model %>% save_model_hdf5(finetuned_model_path)
cat("Fine-tuning completed! Model saved to:", finetuned_model_path, "\n")

a=get_weights(model)
saveRDS(a,"model_weights")



mdl=load_model_hdf5(/home/ivan/TRAIN/LRG/Crops
