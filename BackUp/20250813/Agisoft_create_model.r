
    library(reticulate)
    labelInput
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
	#####################################
    crs = "EPSG:4326"   #32610 4326
    imagefolder = paste0(labelInput,"\\Aerial_Images_For_Model")
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
    outputply = paste0(labelInput,"\\",bsname,"_Model.ply")
##############################################################
if (file.exists(list.files(imagefolder,full.names=T)[1])==T){
   image_folder = normalizePath(imagefolder,winslash = "/", mustWork=F)
   output_path = normalizePath(outputpath,winslash = "/", mustWork=F)

####################################################################
   # Функция для построения модели
    build_model <- function(image_folder, output_path,crs) {
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
chunk.addPhotos([os.path.join(r"', image_folder, '", f) for f in os.listdir(r"', image_folder, '") if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))])

chunk.matchPhotos(
downscale=1,
downscale_3d=1, 
generic_preselection=True,
reference_preselection=True,
keypoint_limit=200000,
keypoint_limit_3d=900000,
tiepoint_limit=50000,
keypoint_limit_per_mpx=5000,
guided_matching=True
)
 
chunk.alignCameras(adaptive_fitting=True) #

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

chunk.buildDepthMaps(downscale=1, filter_mode=Metashape.MildFiltering) #, reuse_depth=False, max_neighbors=32
#chunk.buildPointCloud(source_data=Metashape.DepthMapsData)



chunk.buildModel( 
source_data=Metashape.DepthMapsData,  #  DepthMapsData PointCloudData
surface_type=Metashape.Arbitrary, #Surface type in [Arbitrary, HeightField]
interpolation=Metashape.EnabledInterpolation, 
face_count=Metashape.CustomFaceCount,
face_count_custom=50000000,
vertex_colors=True,
volumetric_masks=False
) 




chunk.crs = Metashape.CoordinateSystem("', crs, '")  
print(chunk.crs)
doc.save("', output_path, '")
del doc
'))
}
# Использование функции
build_model(image_folder, output_path, crs)
}
###################################################################




