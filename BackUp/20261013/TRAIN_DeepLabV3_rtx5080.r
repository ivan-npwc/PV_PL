# source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/DeepLabV3//TRAIN_DeepLabV3_rtx5080.r")
 
library(reticulate)
library(tensorflow)
library(keras)
library(tfdatasets)
library(tidyverse)

# ============================================================
# Параметры
# ============================================================
trainDir    <- "/home/ivan/TRAIN/LRG_Crops_LEVEL 2"
images_dir  <- file.path(trainDir, "Image")
masks_dir   <- file.path(trainDir, "Mask")

epoch1           <- 50L
dropout          <- 0.5
batch_size       <- 12L
img_height       <- 256L
img_width        <- 256L
validation_split <- 0.2
vision_dimensions=img_height


if (exists("model")==F){source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/DeepLabV3//CREATE_DeepLabV3_rtx5080.r") }
# ============================================================
# Проверка данных и создание датафрейма
# ============================================================
cat("Images found:", length(list.files(images_dir)), "\n")
cat("Masks found:",  length(list.files(masks_dir)), "\n")

image_files <- list.files(images_dir, full.names = TRUE,
                          pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
mask_files  <- list.files(masks_dir, full.names = TRUE,
                          pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)

get_base_name <- function(path) tools::file_path_sans_ext(basename(path))

image_names <- sapply(image_files, get_base_name, USE.NAMES = FALSE)
mask_names  <- sapply(mask_files,  get_base_name, USE.NAMES = FALSE)

common_names <- intersect(image_names, mask_names)
cat("Common image-mask pairs:", length(common_names), "\n")

if (length(common_names) == 0) {
  stop("No matching image-mask pairs found!")
}

# Фильтруем и сортируем
image_files <- sort(image_files[image_names %in% common_names])
mask_files  <- sort(mask_files[mask_names %in% common_names])

data_df <- data.frame(
  image = image_files,
  mask  = mask_files,
  stringsAsFactors = FALSE
)

# ============================================================
# Предобработка
# ============================================================
preprocess_image <- function(image_path) {
  image <- tf$io$read_file(image_path)
  image <- tf$image$decode_image(image, channels = 3L, expand_animations = FALSE)
  image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  image <- tf$image$resize(image, size = c(img_height, img_width))
  image
}

preprocess_mask <- function(mask_path) {
  mask <- tf$io$read_file(mask_path)
  mask <- tf$image$decode_image(mask, channels = 1L, expand_animations = FALSE)
  mask <- tf$image$convert_image_dtype(mask, dtype = tf$float32)
  mask <- tf$image$resize(mask, size = c(img_height, img_width))
  mask <- tf$round(mask)
  mask
}

# ============================================================
# Аугментация (все операции — чисто TF, без R-функций в графе)
# ============================================================
augment_fn <- function(image, mask) {
  
  # Случайное отражение по горизонтали
  seed <- tf$random$uniform(shape = shape(), minval = 0, maxval = 1)
  image <- tf$cond(seed > 0.5,
                   function() tf$image$flip_left_right(image),
                   function() image)
  mask  <- tf$cond(seed > 0.5,
                   function() tf$image$flip_left_right(mask),
                   function() mask)
  
  # Случайное отражение по вертикали
  seed2 <- tf$random$uniform(shape = shape(), minval = 0, maxval = 1)
  image <- tf$cond(seed2 > 0.5,
                   function() tf$image$flip_up_down(image),
                   function() image)
  mask  <- tf$cond(seed2 > 0.5,
                   function() tf$image$flip_up_down(mask),
                   function() mask)
  ##########################################
  # Случайный зум (scale_factor от 0.5 до 1.5, полностью через TF)
  # Случайный зум — одинаковый scale для image и mask
  seed3 <- tf$random$uniform(shape = shape(), minval = 0, maxval = 1)
  scale_factor <- tf$random$uniform(shape = shape(), minval = 0.5, maxval = 1.5)
  new_h <- tf$cast(tf$cast(img_height, tf$float32) * scale_factor, tf$int32)
  new_w <- tf$cast(tf$cast(img_width,  tf$float32) * scale_factor, tf$int32)
  new_size <- tf$stack(list(new_h, new_w))
  
  image <- tf$cond(seed3 > 0.5,
    function() tf$image$resize(tf$image$resize(image, new_size), c(img_height, img_width)),
    function() image)
  mask <- tf$cond(seed3 > 0.5,
    function() tf$image$resize(tf$image$resize(mask, new_size), c(img_height, img_width)),
    function() mask)
  ###########################################################
  # Яркость (только изображение)
  seed4 <- tf$random$uniform(shape = shape(), minval = 0, maxval = 1)
  image <- tf$cond(seed4 > 0.5,
                   function() tf$image$random_brightness(image, max_delta = 0.3),
                   function() image)
  
  # Контраст (только изображение)
  seed5 <- tf$random$uniform(shape = shape(), minval = 0, maxval = 1)
  image <- tf$cond(seed5 > 0.5,
                   function() tf$image$random_contrast(image, lower = 0.7, upper = 1.3),
                   function() image)
  # Клип значений после аугментации
  image <- tf$clip_by_value(image, 0.0, 1.0)
  mask  <- tf$round(mask)
  
  list(image, mask)
}

# ============================================================
# Создание tf.data.Dataset
# ============================================================
create_dataset <- function(df, batch_size, shuffle = FALSE, augment = FALSE) {
  
  dataset <- tensor_slices_dataset(list(df$image, df$mask)) %>%
    dataset_map(function(image_path, mask_path) {
      image <- preprocess_image(image_path)
      mask  <- preprocess_mask(mask_path)
      list(image, mask)
    }) %>%
    dataset_map(function(image, mask) {
      image <- tf$ensure_shape(image, list(img_height, img_width, 3L))
      mask  <- tf$ensure_shape(mask,  list(img_height, img_width, 1L))
      list(image, mask)
    })
  
  if (shuffle) {
    dataset <- dataset %>% dataset_shuffle(buffer_size = nrow(df))
  }
  
  if (augment) {
    dataset <- dataset %>%
      dataset_map(function(image, mask) augment_fn(image, mask))
  }
  
  dataset <- dataset %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)
  
  dataset
}

# ============================================================
# Разделение train / validation
# ============================================================
set.seed(123)
train_indices <- sample(seq_len(nrow(data_df)),
                        size = round((1 - validation_split) * nrow(data_df)))
train_df <- data_df[train_indices, ]
val_df   <- data_df[-train_indices, ]

cat("Training samples:  ", nrow(train_df), "\n")
cat("Validation samples:", nrow(val_df), "\n")

train_dataset <- create_dataset(train_df, batch_size, shuffle = TRUE, augment = TRUE)
val_dataset   <- create_dataset(val_df,   batch_size, shuffle = FALSE, augment = FALSE)

# ============================================================
# Проверка батча
# ============================================================
check_batch <- function(dataset) {
  iterator <- as_iterator(dataset)
  batch <- iter_next(iterator)
  img_batch  <- batch[[1]]
  mask_batch <- batch[[2]]
  cat("  Image batch shape:", as.character(img_batch$shape), "\n")
  cat("  Mask batch shape: ", as.character(mask_batch$shape), "\n")
  cat("  Image range:", as.numeric(tf$reduce_min(img_batch)),
      "to", as.numeric(tf$reduce_max(img_batch)), "\n")
  cat("  Mask range: ", as.numeric(tf$reduce_min(mask_batch)),
      "to", as.numeric(tf$reduce_max(mask_batch)), "\n")
}

cat("=== Training batch check ===\n")
check_batch(train_dataset)
cat("=== Validation batch check ===\n")
check_batch(val_dataset)

# ============================================================
# Колбэки
# ============================================================
checkpoint_dir <- file.path(trainDir, "checkpoints_deeplabv3")
unlink(checkpoint_dir,recursive=T)
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

# Путь для сохранения чекпоинтов (Linux-формат!)
filepath <- file.path(checkpoint_dir,
                      "CellSeg_DeepLabV3_val-dice-{val_dice_coef:.3f}_ep-{epoch:02d}.keras")

callbacks <- list(
  callback_model_checkpoint(
    filepath = filepath,
    monitor  = "val_dice_coef",
    save_best_only = TRUE,
    mode    = "max",
    verbose = 1L
  ),
  callback_reduce_lr_on_plateau(
    monitor  = "val_dice_coef",
    factor   = 0.2,
    patience = 4L,
    verbose  = 1L,
    mode     = "max",
    min_lr   = 1e-8
  ),
  callback_early_stopping(
    monitor  = "val_dice_coef",
    patience = 10L,
    verbose  = 1L,
    mode     = "max",
    restore_best_weights = TRUE
  ),
  callback_tensorboard(
    log_dir = file.path(checkpoint_dir, "logs")
  )
)

# ============================================================
# Обучение — фаза 1
# ============================================================
cat("\n=== Starting DeepLabV3+ training ===\n")
cat("  ResNet50 backbone + ImageNet weights\n")
cat("  ASPP + DeepLabV3+ decoder\n")
cat("  Dice loss\n")
cat("  Input size:", img_height, "x", img_width, "\n")
cat("  Batch size:", batch_size, "\n")
cat("  Epochs phase 1:", epoch1, "\n\n")

history <- model %>% fit(
  train_dataset,
  epochs          = epoch1,
  validation_data = val_dataset,
  callbacks       = callbacks,
  verbose         = 1L
)
########################################
saveRDS(history, file.path(checkpoint_dir, "training_history.rds"))
######################################################


# ============================================================
# Сохранение весов
# ============================================================
weights_path <- file.path(checkpoint_dir, "model_weights.rds")
w <- get_weights(model)
saveRDS(w, weights_path)
cat("Weights saved to:", weights_path, "\n")

# Сохранение модели целиком (Keras 3 формат)
model_path <- file.path(checkpoint_dir, "deeplabv3_final.keras")
save_model(model, model_path)
cat("Model saved to:", model_path, "\n")

# ============================================================
# Загрузка модели (пример)
# ============================================================
# loaded_model <- load_model(
#   model_path,
#   custom_objects = list(
#     dice_coef = dice_coef,
#     dice_loss = dice_loss
#   )
# )
