https://heads0rtai1s.github.io/2021/02/25/gpu-setup-r-python-ubuntu/
https://tensorflow.rstudio.com/install/local_gpu
######################################################### NVIDIA driver instalation


  library(reticulate)

  #####################################################################
check_nvidia_driver <- function() {
  message("🔍 Проверка драйвера NVIDIA...")
  tryCatch({
    driver_version <- system("nvidia-smi --query-gpu=driver_version --format=csv,noheader", intern = TRUE)
    if (length(driver_version) > 0) {
      message("✅ Драйвер NVIDIA обнаружен: ", driver_version)
      return(TRUE)
    } else {
      message("❌ Драйвер NVIDIA не найден! Установите его вручную.")
      return(FALSE)
    }
  }, error = function(e) {
    message("❌ Ошибка: ", e$message)
    return(FALSE)
  })
}



###############################################################################
check_gpu <- function() {
  tryCatch({
    gpus <- tf$config$list_physical_devices("GPU")
    if (length(gpus) > 0) {
      message("✅ GPU обнаружена: ", gpus[[1]]$name)
      return(TRUE)
    }
    message("❌ GPU не обнаружена")
    return(FALSE)
  }, error = function(e) {
    message("❌ Ошибка проверки GPU: ", e$message)
    return(FALSE)
  })
}
#############################
   system("nvcc --version") 
  check_nvidia_driver()
  check_gpu()
  
  
  
  install_miniconda()

  
  
  # Accept ToS for Anaconda's main channels
conda_path <- file.path(Sys.getenv("LOCALAPPDATA"), "r-miniconda", "condabin", "conda.bat")
system2(conda_path, args = c("tos", "accept", "--override-channels", "--channel", "https://repo.anaconda.com/pkgs/main"))
system2(conda_path, args = c("tos", "accept", "--override-channels", "--channel", "https://repo.anaconda.com/pkgs/r"))
system2(conda_path, args = c("tos", "accept", "--override-channels", "--channel", "https://repo.anaconda.com/pkgs/msys2"))

# Создаем новое с numpy 1.x изначально
conda_create(
  envname = "tf_2_10_env",
  python_version = "3.9"
)
  
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    py_config() 
  
  
   agipth="C:\\Users\\usato\\Downloads\\Metashape-2.2.1-cp37.cp38.cp39.cp310.cp311-none-win_amd64.whl"
   pth= normalizePath(agipth,winslash = "/", mustWork=F)
   py_install(pth, pip = TRUE)
    py_run_string(paste0('import Metashape'))
   #restart
   
    library(reticulate)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    py_config() 
	


    py_install("numpy==1.24.4")
    py_install("tensorflow-gpu==2.10.0", pip = TRUE)
   # tensorflow_gpu_pth = "C:\\Users\\usato\\Downloads\\tensorflow_gpu-2.10.0-cp39-cp39-win_amd64.whl"
   # pth= normalizePath(tensorflow_gpu_pth,winslash = "/", mustWork=F)
    #py_install(pth, pip = TRUE)
    py_install("keras==2.10.0", pip = TRUE) 

	
	library(tensorflow)
	tensorflow::tf_version()
    
	
	
	   system("nvcc --version") 
       check_nvidia_driver()
       check_gpu()
  
########################################################################################




































nvidia-smi

cat /proc/driver/nvidia/version

#sudo sh ./NVIDIA-Linux-x86_64-550.144.03.run
#sudo ubuntu-drivers install --gpgpu
#sudo ubuntu-drivers install --gpgpu nvidia:560   #-server
#sudo ubuntu-drivers install nvidia:560



##################################  R INSTALATION
sudo apt update
sudo apt install software-properties-common dirmngr -y
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt update
sudo apt install r-base r-base-dev -y
######################################## DOWNLOAD TENSORFLOW AND PYTHON
R
install.packages("remotes")
install.packages("reticulate")
remotes::install_github("rstudio/tensorflow") # download tensorflow
reticulate::install_python()                  #python instalation IF dont work try to install other ways
#Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python") # set the path to python IW PUTHON WAS INSTALED MANUALY
q()
n
######################################### PYTHON INSTALATION (IN CASE)
#reticulate::install_miniconda()  
#Don't change the default python version. You might end up destroying Ubuntu. Instead, use Miniconda/Anaconda to create a virtual environment. 
#Do not install TensorFlow with conda. It may not have the latest stable version. pip is recommended since TensorFlow is only officially released to PyPI.
############################# CUDA INSTALATION
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.1-1_all.deb
sudo dpkg -i cuda-keyring_1.1-1_all.deb
sudo apt-get update
sudo apt-get -y install cuda-toolkit-11-8
nvidia-smi
############################################################## CudNN instalation (with tensorflow in some time)
R
install_tensorflow() #cuDNN 8.6 is automatically installed by install_tensorflow() via pip if a GPU is detected.

tf$constant("Hello TensorFlow!")
tf$device_lib$list_local_devices()
tf$config$list_physical_devices("GPU")


library(keras)
install_keras()
is_keras_available()
################################################################t    TEST

#######################################################################check cuda and video driver instalation
cat /proc/driver/nvidia/version
nvidia-smi
#################################################################### clean up all CUDA and NVIDIA drivers (by shure display can error)
# Remove CUDA Toolkit:
sudo apt-get --purge remove "*cublas*" "*cufft*" "*curand*" "*cusolver*" "*cusparse*" "*npp*" "*nvjpeg*" "cuda*" "nsight*" 
# Remove Nvidia Drivers:
sudo apt-get --purge remove "*nvidia*"
# Clean up the uninstall:
sudo apt-get autoremove
########################################################################################### else way to install cuda --change version 



sudo rstudio-server start
http://localhost:8787

library(tensorflow)
library(reticulate)

#reticulate::install_python(version="3.10") 
install_tensorflow(version=2.15) 
install_tensorflow(version="gpu") 
tf$config$list_physical_devices("GPU")

library(tensorflow)
tf$constant("Hello TensorFlow!")



 "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/windows-x86_64/cudnn-windows-x86_64-8.6.0.163_cuda11-archive.zip"




