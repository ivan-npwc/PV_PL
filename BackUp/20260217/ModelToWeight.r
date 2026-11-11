

   library(keras)
   use_condaenv("base",required = TRUE)
   library("reticulate")
   conda_list <- conda_list()
   print(conda_list)
   Sys.sleep(5)
   py_config() 
  library("tensorflow")
  library("tfdatasets")
  tf$config$list_physical_devices('GPU')
  
  
  
  
  

pth=   "C:\\Users\\usato\\SSL_DB\\TRAIN\\TRAIN_pv\\No zero\\Checkpoints\\No zero_20250530_val_0.70_epoch_97.h5"


 dice_coef <<- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})
 dice_coef_loss <- function(y_true, y_pred) - dice_coef(y_true, y_pred)
 
unet1 <- load_model_hdf5(pth, custom_objects = c(dice_coef = dice_coef,
                                                        dice_coef_loss=dice_coef_loss))


a=get_weights(unet1)
saveRDS(a,"No zero_20250530_val_0.70_epoch_97")