library(reticulate)
library(tensorflow)
library(keras3)

# ============================================================
# Путь к модели (исправлен разделитель)
# ============================================================
ModelConvert <- "/home/ivan/TRAIN/LRG_Crops_LEVEL 2/checkpoints_deeplabv3/CellSeg_DeepLabV3_val-dice-0.880_ep-14.keras"
filepathRDS  <- tools::file_path_sans_ext(ModelConvert)

# ============================================================
# Метрики через Python (для правильных имён)
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
# Загрузка модели
# ============================================================
mdl <- load_model(
  ModelConvert,
  custom_objects = list(
    dice_coef = dice_coef,
    dice_loss = dice_loss,
    dice_coef_loss = dice_loss  # на случай если сохранено под старым именем
  )
)

# Перекомпиляция
mdl %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss      = dice_loss,
  metrics   = list(dice_coef)
)

# ============================================================
# Сохранение весов
# ============================================================
a1 <- get_weights(mdl)
saveRDS(a1, paste0(filepathRDS, "_weights.rds"))
cat("Weights saved to:", paste0(filepathRDS, "_weights.rds"), "\n")