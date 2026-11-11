

   #source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/Agisoft_create_model_with_masks.r")
 #  labelInput  = "/mnt/adata8tb/PV_DB/2024_H0470_OPP/20240919_110729/20240919_110729_M3T_20M"

    library(reticulate)
	bsname=basename(labelInput)
    drone <<- strsplit(bsname,"_")[[1]][3]
    print(drone)
        py_pth_win =  "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
	    py_pth_unx = "/home/ivan/anaconda3/bin/python"

	sys_info <- Sys.info()
	if (sys_info["sysname"] == "Windows") {py_pth=py_pth_win}
	if (sys_info["sysname"] == "Linux")     {py_pth=py_pth_unx}

    use_python(py_pth, required = TRUE)
	#####################################
    crs = "EPSG:4326"   #32610 4326
    imagefolder = file.path(labelInput,"Aerial_Images_For_Model")
  #  outputpath = file.path(labelInput,  paste0(bsname,"_Model.psx")); unlink(outputpath)

    outputpath = file.path(labelInput,  paste0(bsname,"_Model_no_water.psx")); unlink(outputpath)
    mskdir = file.path(labelInput,"Predict","WATER_MASK")
##############################################################
if (file.exists(list.files(imagefolder,full.names=T)[1])==F){print(paste0("SKIP    ", labelInput,"   NO AERILA IMAGES FOUND"));   next}
   image_folder = normalizePath(imagefolder,winslash = "/", mustWork=F)
   output_path = normalizePath(outputpath,winslash = "/", mustWork=F)
   mskdir = normalizePath(mskdir,winslash = "/", mustWork=F)
####################################################################
   # Функция для построения модели
    build_model <- function(image_folder, output_path,crs, downscale_photo) {
      py_run_string(paste0('
import Metashape
import os

doc = Metashape.Document()
################################################
#doc.open("', output_path, '")
#chunk = doc.chunk
############################################
chunk = doc.addChunk()

chunk.crs = Metashape.CoordinateSystem("', crs, '")


image_folder = r"', image_folder, '"
mask_dir = r"', mskdir, '"  # Директория с масками
image_files = [f for f in os.listdir(image_folder) if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))]


photo_paths = [os.path.join(image_folder, f) for f in image_files]
chunk.addPhotos(photo_paths)


for camera in chunk.cameras:

    filename = os.path.basename(camera.photo.path)
    filename_base = os.path.splitext(filename)[0]


    mask_filename = f"{filename_base}_mask.png"
    mask_path = os.path.join(mask_dir, mask_filename)


    if os.path.exists(mask_path):
        print(f"Using mask for {filename}: {mask_path}")
        try:

            mask = Metashape.Mask()
            mask.load(mask_path)
            camera.mask = mask
           # camera.mask_mode = Metashape.MaskModeIntersection
        except Exception as e:
            print(f"Ошибка при загрузке маски {mask_path}: {e}")
    else:
        print(f"Предупреждение: Маска не найдена для {filename} - {mask_path}")

chunk.matchPhotos(
    downscale=', downscale_photo, ',
    downscale_3d=1,
    generic_preselection=True,
    reference_preselection=True,
    keypoint_limit=200000,
    keypoint_limit_3d=900000,
    tiepoint_limit=50000,
    keypoint_limit_per_mpx=5000,
    guided_matching=True,
    mask_tiepoints=True  # Использовать маски при построении связующих точек
)

chunk.alignCameras(adaptive_fitting=True)

chunk.optimizeCameras(
    fit_f=True,              # Оптимизировать фокусное расстояние
    fit_cx=True,           # Оптимизировать центр проекции
    fit_cy=True,
    fit_b1=True,             # Коррекция радиальных искажений
    fit_b2=True,
    fit_k1=True,
    fit_k2=True,
    fit_k3=True,
    fit_p1=True,
    fit_p2=True,
    adaptive_fitting=True,   # Адаптивная оптимизация
    tiepoint_covariance=True # Учет ковариации точек
)

chunk.buildDepthMaps(
    downscale=2,
    filter_mode=Metashape.MildFiltering,
    max_neighbors= 16
)



chunk.buildModel(
    source_data=Metashape.DepthMapsData,
    surface_type=Metashape.Arbitrary,  #HeightField
    interpolation=Metashape.EnabledInterpolation,
    face_count=Metashape.CustomFaceCount,
    face_count_custom=50000000,
    vertex_colors=True,
    volumetric_masks=False
)

chunk.crs = Metashape.CoordinateSystem("', crs, '")
print(chunk.crs)

# Сохранение проекта
doc.save("', output_path, '")
del doc

'))
}

if (drone== "M3T" ){build_model(image_folder, output_path, crs,downscale_photo=2)}
if (drone != "M3T" ){build_model(image_folder, output_path, crs,downscale_photo=1)}
# Использование функции


###################################################################




