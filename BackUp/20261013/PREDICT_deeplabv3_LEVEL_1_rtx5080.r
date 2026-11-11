# source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/DeepLabV3/PREDICT_deeplabv3_LEVEL_1_rtx5080.r")

 #labelInput= "/mnt/c/Users/usato/SSL_UI/PV_UI/2023_H0068C_OPP/20230720_111049/20230720_111049_MINI2_20m" 
# ============================================================
# Библиотеки
# ============================================================
library(reticulate)
library(tensorflow)
library(keras3)
library(tfdatasets)
library(abind)
library(parallel)
library(tools)
library(EBImage)

# ============================================================
# Параметры
# ============================================================
PTHweight      <- "/home/ivan/LRG20251028_val93"
modelcreatepth <- "/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/DeepLabV3/CREATE_DeepLabV3_rtx5080.r"

Species           <- "LRG_level_1"
batch_size        <- 32L
batch_size_global <- batch_size * 10L
vision_dimensions <- 256L
resultBlob        <- NULL
IncludeBlobAnalis <- FALSE
Brush             <- 9
thresh_val        <- 0.009
MaskCreate        <- FALSE
ImageMaskConcetence <- FALSE

# ============================================================
# Загрузка модели и весов
# ============================================================
if (!file.exists(PTHweight)) stop("Weight path is invalid: ", PTHweight)
if (!exists("model")) source(modelcreatepth)

weight <- readRDS(PTHweight)
set_weights(model, weight)

# ============================================================
# Подготовка директорий
# ============================================================
if (MaskCreate) {
  IncludeBlobAnalis <- TRUE
  MskDir <- file.path(labelInput, "Predict", "MaskPredicted_level_1")
  unlink(MskDir, recursive = TRUE)
  dir.create(MskDir, showWarnings = FALSE, recursive = TRUE)
}

date1       <- substr(basename(labelInput), 1, 15)
predict_dir <- file.path(labelInput, "Predict", "TilesOverlap")
predsDir    <- file.path(labelInput, "Predict", "Preds")
lstpreds = list.files(predsDir, full.names=T, pattern = Species)
unlink(lstpreds, recursive = TRUE)
dir.create(predsDir, showWarnings = FALSE, recursive = TRUE)

kmlPathSave1   <- file.path(labelInput, "Predict",
                             paste0("#", basename(PTHweight), "#", date1, ".kml"))
listImage_glob <- list.files(predict_dir, full.names = TRUE, pattern = "\\.[Jj][Pp][Gg]$")
pth_resultBlob <- file.path(labelInput, "Predict",
                             paste0(Species, "_BlobTable_", date1, ".csv"))

if (length(listImage_glob) == 0) stop("No images found in: ", predict_dir)

# ============================================================
# Глобальные шаги
# ============================================================
global_steps <- max(1L, ceiling(length(listImage_glob) / batch_size_global))

# ============================================================
# Создание tf.data.Dataset для предсказания
# ============================================================
create_pred_dataset <- function(image_paths, batch_size, vision_dimensions) {
  dataset <- tensor_slices_dataset(list(img = image_paths)) %>%
    dataset_map(function(x) {
      img <- tf$io$read_file(x$img)
      img <- tf$image$decode_image(img, channels = 3L, expand_animations = FALSE)
      img <- tf$image$convert_image_dtype(img, dtype = tf$float32)
      img <- tf$image$resize(img, size = c(vision_dimensions, vision_dimensions))
      img
    }) %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)
  
  dataset
}

# ============================================================
# Основной цикл предсказания
# ============================================================
for (e in 1:global_steps) {
  
  # Выборка батча изображений
  n_take <- min(batch_size_global, length(listImage_glob))
  listImage <- listImage_glob[1:n_take]
  
  if (length(listImage_glob) > n_take) {
    listImage_glob <- listImage_glob[-(1:n_take)]
  }
  
  # Предсказание
  pred_dataset <- create_pred_dataset(listImage, batch_size, vision_dimensions)
  preds <- model %>% predict(pred_dataset)
  
  cat("Done prediction step", e, "of", global_steps, "\n")
  
  # ============================================================
  # Blob-анализ
  # ============================================================
  if (IncludeBlobAnalis) {
    
    result_batch <- mclapply(seq_along(listImage), function(i) {
      imgpth <- listImage[i]
      name   <- basename(imgpth)
      
      mask0 <- preds[i, , , ]
      # Убедиться что размерность правильная
      if (is.null(dim(mask0))) {
        dim(mask0) <- c(vision_dimensions, vision_dimensions)
      }
      
      img <- EBImage::Image(mask0, dim = c(vision_dimensions, vision_dimensions, 1))
	  display(img)
	  
      nmask  <- thresh(img, 18, 18, thresh_val)
      nmask1 <- fillHull(nmask)
      nmask2 <- opening(nmask1, makeBrush(Brush, shape = "disc"))
      nmask3 <- fillHull(nmask2)
      nmask3 <- dilate(nmask3, makeBrush(7, shape = "disc"))
      nmask4 <- bwlabel(nmask3)
      
      # Сохранение маски
      if (MaskCreate) {
        nmask5  <- transpose(nmask4)
        MaskPth <- file.path(MskDir, gsub("\\.[Jj][Pp][Gg]$", ".png", name))
        writeImage(nmask5, MaskPth)
      }
      
      # Вычисление blob-фич
      if (max(nmask4) != 0) {
        fts           <- computeFeatures.moment(nmask4)
        shapeFeatures <- computeFeatures.shape(nmask4)
        BlobTable     <- data.frame(fts, shapeFeatures, img = name, img_pth = imgpth)
        return(BlobTable)
      }
      
      return(NULL)
    }, mc.cores = max(1L, parallel::detectCores(logical = FALSE) - 1L))
    
    result_clean   <- result_batch[!sapply(result_batch, is.null)]
    resultBlob_tmp <- if (length(result_clean) > 0) do.call(rbind, result_clean) else NULL
    
    if (!is.null(resultBlob_tmp)) {
      resultBlob <- rbind(resultBlob, resultBlob_tmp)
    }
    
    cat("Done blob analysis step", e, "of", global_steps, "\n")
  }
  
  # ============================================================
  # Сохранение сырых предсказаний (если без blob-анализа)
  # ============================================================
  if (!IncludeBlobAnalis) {
    pthSavePreds <- file.path(predsDir, paste0("Preds_", Species, "_", e))
    Preds1 <- list(
      preds       = preds,
      DimPreds    = dim(preds),
      dimModel    = vision_dimensions,
      listImageBl = listImage
    )
    saveRDS(Preds1, pthSavePreds)
    cat("Saved predictions step", e, "of", global_steps, "\n")
  }
}

# ============================================================
# Сохранение blob-таблицы
# ============================================================
if (IncludeBlobAnalis && !is.null(resultBlob)) {
  write.csv(resultBlob, pth_resultBlob, row.names = FALSE)
  cat("Blob table saved to:", pth_resultBlob, "\n")
}

# ============================================================
# Совмещение масок с изображениями
# ============================================================
if (ImageMaskConcetence && MaskCreate) {
  
  ImagePath <- predict_dir
  MaskPath  <- MskDir
  PathCheck <- file.path(labelInput, "Predict", "Image_Mask_level_1")
  
  unlink(PathCheck, recursive = TRUE)
  dir.create(PathCheck, showWarnings = FALSE, recursive = TRUE)
  
  MskList <- list.files(MaskPath, pattern = "\\.png$")
  
  cat("Creating image+mask overlays...\n")
  
  rslt <- mclapply(MskList, function(pth) {
    mskP <- file.path(MaskPath, pth)
    ImgP <- file.path(ImagePath, gsub("\\.png$", ".JPG", pth))
    
    if (file.exists(ImgP)) {
      img <- readImage(ImgP)
      msk <- readImage(mskP)
      dm  <- dim(img)
      msk <- resize(msk, dm[1], dm[2])
      y1  <- channel(msk, "asred")
      a   <- img + y1
      PathCheckImg <- file.path(PathCheck, pth)
      writeImage(a, PathCheckImg)
    }
  }, mc.cores = max(1L, parallel::detectCores(logical = FALSE) - 1L))
  
  cat("Image+mask overlays saved to:", PathCheck, "\n")
}

cat("\n=== Prediction complete ===\n")
