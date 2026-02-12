library(reticulate)
  library("tensorflow")
# Настройка Python
py_pth <- "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\tf_2_10_env/python.exe"
use_python(py_pth, required = TRUE)
use_condaenv("tf_2_10_env", required = TRUE)
tf$config$list_physical_devices('GPU')



  library("abind")
  library("parallel")
  library("doParallel")
  library("foreach")
  library("tensorflow")
  library("tfdatasets")
  library("purrr")
  library(EBImage)
  library(keras)
# Параметры
trainDir <- "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero"
images_dir <- file.path(trainDir, "Image")
masks_dir <- file.path(trainDir, "Mask")

epochs <- 100
batch_size <- 4L
img_height <- 512L  # DeepLabV3+ лучше работает с размерами кратными 32/16
img_width <- 512L
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
 # image <- tf$keras$applications$resnet$preprocess_input(image)
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
            scale_factor <- runif(1, 0.8, 1.2)  # Меньший диапазон для сохранения деталей
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
          true_fn = function() tf$image$random_brightness(image, max_delta = 0.2),
          false_fn = function() image
        )
        
        # Контраст (только для изображения)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_contrast(image, lower = 0.8, upper = 1.2),
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


source("C:\\Users\\usato\\SSL_DB\\PV_PL\\Modules\\UNET\\TRAIN TF2\\create_simple_deeplabv3.r")



# Колбэки
checkpoint_dir <- file.path(trainDir, "checkpoints_deeplabv3")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
BaseName <- basename(file.path(checkpoint_dir, "Val_{val_dice_coef:.3f}_epoch_{epoch:02d}.h5"))
filepath <- paste0(checkpoint_dir, "\\SealSegmentation_DeepLabV3_", BaseName)

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
    patience = 10,
    verbose = 1,
   mode = "max",
    min_lr = 1e-7
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



history <- model %>% fit(
  train_dataset,
  epochs = epochs,
  validation_data = val_dataset,
  callbacks = callbacks,
  verbose = 1
 )

