
  library(reticulate)

  conda_remove("r-pytorch-gpu")  # Удалить старое


  conda_create("r-pytorch-gpu", packages = "python=3.9")
  use_condaenv("r-pytorch-gpu")
  
conda_install(
  envname = "r-pytorch-gpu",
  packages = c("pytorch==2.5.1", "torchvision==0.20.1", "torchaudio==2.5.1", "pytorch-cuda=11.8"),
  channel = c("pytorch", "nvidia"),
  pip = FALSE
)
  


    library(reticulate)
    use_condaenv("r-pytorch-gpu")
    # Проверка доступности GPU
    torch <- import("torch")
   torch$cuda$is_available()
   torch$version$cuda
   torch$cuda$get_device_name(0L)
   
   
   
  py_install("segmentation-models-pytorch", pip = TRUE)
  