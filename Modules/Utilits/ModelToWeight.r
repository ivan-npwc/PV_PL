
library(reticulate)
library(tensorflow)
library(keras)
library(tfdatasets)
library(tidyverse)

#use_condaenv("base",required = TRUE)
#py_config() 
ModelConvert =  "/home/ivan/TRAIN/LRG/Crops/checkpoints_deeplabv3\\CellSegmentation_DeepLabV3_Val_0.913_epoch_01.h5"

#tcltk::tk_choose.files()

filepathRDS=tools::file_path_sans_ext(ModelConvert)
K <- backend()

dice_coef <- function(y_true, y_pred, smooth = 1) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
}
attr(dice_coef, "py_function_name") <- "dice_coef"

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)
attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"

##########################################
#bce_dice_loss <- function(y_true, y_pred) {
#  result <- loss_binary_crossentropy(y_true, y_pred) +
#    (1 - dice_coef(y_true, y_pred))
#  return(result)
#}


mdl <- load_model_hdf5(ModelConvert, custom_objects = c(dice_coef = dice_coef,
                                                        dice_coef_loss=dice_coef_loss))


mdl <- mdl %>%
        compile(
        optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),     #optimizer_nadam              
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
    )	
a1=get_weights(mdl)
saveRDS(a1,"TRUE_SEAL_LEVEL_2")
