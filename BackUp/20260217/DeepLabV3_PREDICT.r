# Модуль предсказания для DeepLabV3+ сегментации клеток

library(reticulate)
library(tensorflow)
library(keras)
library(EBImage)
library(abind)

# Настройка Python (должна совпадать с настройками обучения)
py_pth <- "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\tf_2_10_env/python.exe"
use_python(py_pth, required = TRUE)
use_condaenv("tf_2_10_env", required = TRUE)

# Параметры предсказания (должны совпадать с обучением)
img_height <- 512L
img_width <- 512L

#' Загрузка обученной модели
#' 
#' @param model_path Путь к файлу модели .h5
#' @return Загруженная модель Keras
load_trained_model <- function(model_path) {
  if (!file.exists(model_path)) {
    stop("Файл модели не найден: ", model_path)
  }
  
  cat("Загрузка модели из:", model_path, "\n")
  
  # Загружаем модель с кастомными метриками
  model <- load_model_hdf5(
    model_path,
    custom_objects = list(
      dice_coef = dice_coef,
      dice_coef_loss = dice_coef_loss
    )
  )
  
  cat("Модель успешно загружена\n")
  return(model)
}

#' Предобработка изображения для предсказания
#' 
#' @param image_path Путь к изображению
#' @return Предобработанный тензор изображения
preprocess_image_for_prediction <- function(image_path) {
  # Загрузка и декодирование изображения
  image <- tf$io$read_file(image_path)
  image <- tf$image$decode_image(image, channels = 3, expand_animations = FALSE)
  image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  
  # Сохраняем оригинальные размеры
  original_shape <- dim(image)[1:2]
  
  # Ресайз до размера модели
  image_resized <- tf$image$resize(image, size = c(img_height, img_width))
  
  # Добавляем batch dimension
  image_batch <- tf$expand_dims(image_resized, axis = 0L)
  
  return(list(
    image = image_batch,
    original_shape = original_shape
  ))
}

#' Постобработка маски предсказания
#' 
#' @param prediction Тензор предсказания модели
#' @param original_shape Оригинальные размеры изображения
#' @param threshold Порог бинаризации (по умолчанию 0.5)
#' @return Постобработанная бинарная маска
postprocess_prediction <- function(prediction, original_shape, threshold = 0.5) {
  # Убираем batch dimension
  mask <- prediction[1,,,]
  
  # Бинаризация по порогу
  binary_mask <- as.array(mask > threshold)
  
  # Конвертируем в числовой формат
  binary_mask <- binary_mask * 1.0
  
  # Ресайз до оригинального размера
  if (!is.null(original_shape)) {
    binary_mask_resized <- resize(
      binary_mask, 
      w = original_shape[2], 
      h = original_shape[1]
    )
  } else {
    binary_mask_resized <- binary_mask
  }
  
  return(binary_mask_resized)
}

#' Предсказание маски для одного изображения
#' 
#' @param model Обученная модель Keras
#' @param image_path Путь к изображению
#' @param threshold Порог бинаризации
#' @return Список с оригинальным изображением и предсказанной маской
predict_single_image <- function(model, image_path, threshold = 0.5) {
  cat("Обработка изображения:", basename(image_path), "\n")
  
  # Предобработка
  preprocessed <- preprocess_image_for_prediction(image_path)
  
  # Предсказание
  cat("Выполнение предсказания...\n")
  prediction <- predict(model, preprocessed$image)
  
  # Постобработка
  cat("Постобработка маски...\n")
  mask <- postprocess_prediction(
    prediction, 
    preprocessed$original_shape, 
    threshold
  )
  
  # Загрузка оригинального изображения для визуализации
  original_image <- readImage(image_path)
  
  return(list(
    image = original_image,
    mask = mask,
    image_path = image_path
  ))
}

#' Пакетное предсказание для нескольких изображений
#' 
#' @param model Обученная модель Keras
#' @param image_dir Директория с изображениями
#' @param output_dir Директория для сохранения масок (опционально)
#' @param threshold Порог бинаризации
#' @param save_masks Сохранять ли маски в файлы
#' @return Список результатов предсказания
predict_batch_images <- function(model, image_dir, output_dir = NULL, 
                                threshold = 0.5, save_masks = FALSE) {
  
  # Получаем список изображений
  image_files <- list.files(
    image_dir, 
    pattern = "\\.(jpg|jpeg|png)$", 
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(image_files) == 0) {
    stop("В директории не найдено изображений: ", image_dir)
  }
  
  cat("Найдено изображений:", length(image_files), "\n")
  
  # Создаем директорию для сохранения если нужно
  if (save_masks && !is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    cat("Маски будут сохранены в:", output_dir, "\n")
  }
  
  results <- list()
  
  for (i in seq_along(image_files)) {
    cat(sprintf("[%d/%d] ", i, length(image_files)))
    
    tryCatch({
      # Предсказание для одного изображения
      result <- predict_single_image(model, image_files[i], threshold)
      
      # Сохранение маски если требуется
      if (save_masks && !is.null(output_dir)) {
        mask_filename <- paste0(
          tools::file_path_sans_ext(basename(image_files[i])), 
          "_mask.png"
        )
        mask_path <- file.path(output_dir, mask_filename)
        
        # Сохраняем маску как PNG
        writeImage(result$mask, mask_path, type = "png")
        cat("Маска сохранена:", mask_filename, "\n")
      }
      
      results[[i]] <- result
      
    }, error = function(e) {
      cat("Ошибка при обработке", basename(image_files[i]), ":", e$message, "\n")
      results[[i]] <- NULL
    })
  }
  
  # Убираем NULL результаты
  results <- results[!sapply(results, is.null)]
  
  cat("Успешно обработано изображений:", length(results), "\n")
  return(results)
}

#' Визуализация результатов предсказания
#' 
#' @param prediction_result Результат предсказания
#' @param show_original Показывать оригинальное изображение
#' @param show_mask Показывать маску
#' @param show_overlay Показывать наложение маски на изображение
visualize_prediction <- function(prediction_result, 
                                show_original = TRUE,
                                show_mask = TRUE, 
                                show_overlay = TRUE) {
  
  par(mfrow = c(1, show_original + show_mask + show_overlay))
  
  if (show_original) {
    plot(prediction_result$image, main = "Оригинальное изображение")
  }
  
  if (show_mask) {
    plot(as.raster(prediction_result$mask), main = "Предсказанная маска")
  }
  
  if (show_overlay) {
    # Создаем цветное наложение
    overlay <- rgbImage(
      red = prediction_result$image,
      green = pmax(prediction_result$image, prediction_result$mask),
      blue = prediction_result$image
    )
    plot(overlay, main = "Наложение маски")
  }
  
  par(mfrow = c(1, 1))
}

#' Оценка качества предсказания (если есть ground truth)
#' 
#' @param prediction_mask Предсказанная маска
#' @param ground_truth_mask Истинная маска
#' @return Метрики качества
evaluate_prediction <- function(prediction_mask, ground_truth_mask) {
  # Приводим к одинаковому размеру если нужно
  if (!all(dim(prediction_mask) == dim(ground_truth_mask))) {
    prediction_mask <- resize(prediction_mask, 
                            w = dim(ground_truth_mask)[2], 
                            h = dim(ground_truth_mask)[1])
  }
  
  # Вычисляем метрики
  intersection <- sum(prediction_mask * ground_truth_mask)
  union <- sum(prediction_mask) + sum(ground_truth_mask) - intersection
  
  dice <- (2 * intersection) / (sum(prediction_mask) + sum(ground_truth_mask))
  iou <- intersection / union
  accuracy <- sum(prediction_mask == ground_truth_mask) / length(prediction_mask)
  
  return(list(
    dice_coef = dice,
    iou = iou,
    accuracy = accuracy,
    precision = intersection / sum(prediction_mask),
    recall = intersection / sum(ground_truth_mask)
  ))
}

# Пример использования модуля предсказания:

#' Основная функция для запуска предсказаний
#' 
#' @param model_path Путь к файлу модели
#' @param input_path Путь к изображению или директории
#' @param output_dir Директория для сохранения результатов
#' @param threshold Порог бинаризации
run_prediction <- function(model_path, input_path, output_dir = NULL, threshold = 0.5) {
  
  # Загрузка модели
  model <- load_trained_model(model_path)
  
  # Определяем тип входных данных
  if (dir.exists(input_path)) {
    # Пакетная обработка директории
    cat("Режим пакетной обработки директории\n")
    results <- predict_batch_images(
      model = model,
      image_dir = input_path,
      output_dir = output_dir,
      threshold = threshold,
      save_masks = !is.null(output_dir)
    )
    
    # Визуализация первого результата
    if (length(results) > 0) {
      cat("Визуализация первого результата...\n")
      visualize_prediction(results[[1]])
    }
    
  } else if (file.exists(input_path)) {
    # Обработка одного изображения
    cat("Режим обработки одного изображения\n")
    result <- predict_single_image(model, input_path, threshold)
    
    # Визуализация
    visualize_prediction(result)
    
    # Сохранение если указана выходная директория
    if (!is.null(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      mask_path <- file.path(
        output_dir, 
        paste0(tools::file_path_sans_ext(basename(input_path)), "_mask.png")
      )
      writeImage(result$mask, mask_path, type = "png")
      cat("Маска сохранена:", mask_path, "\n")
    }
    
    results <- list(result)
    
  } else {
    stop("Указанный путь не существует: ", input_path)
  }
  
  return(results)
}

# Пример использования:
# 
# # 1. Предсказание для одного изображения
# results <- run_prediction(
#   model_path = "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero\\checkpoints_deeplabv3\\final_deeplabv3_cell_segmentation.h5",
#   input_path = "C:\\Users\\usato\\SSL_DB\\TEST\\test_image.png",
#   output_dir = "C:\\Users\\usato\\SSL_DB\\TEST\\results",
#   threshold = 0.5
# )
# 
# # 2. Пакетная обработка директории
# results <- run_prediction(
#   model_path = "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero\\checkpoints_deeplabv3\\final_deeplabv3_cell_segmentation.h5", 
#   input_path = "C:\\Users\\usato\\SSL_DB\\TEST\\test_images",
#   output_dir = "C:\\Users\\usato\\SSL_DB\\TEST\\results",
#   threshold = 0.5
# )

cat("Модуль предсказания загружен и готов к использованию!\n")