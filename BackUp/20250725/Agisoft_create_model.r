# Установите пакет reticulate, если еще не установлен
install.packages("reticulate")
library(reticulate)

# Укажите путь к Python с установленным Metashape
use_python("C:/Path/To/Your/Python.exe")  # Обычно Python из установки Metashape

# Проверьте доступность модуля Metashape
py_run_string("import Metashape")

# Функция для построения модели
build_model <- function(image_folder, output_path) {
  py_run_string(paste0('
import Metashape
import os

doc = Metashape.Document()
doc.save(output_path := "', output_path, '")

chunk = doc.addChunk()
chunk.addPhotos([os.path.join(r"', image_folder, '", f) for f in os.listdir(r"', image_folder, '") if f.lower().endswith((".jpg", ".jpeg", ".tif", ".tiff", ".png"))])

chunk.matchPhotos(accuracy=Metashape.HighAccuracy, generic_preselection=True, reference_preselection=False)
chunk.alignCameras()
chunk.buildDepthMaps(quality=Metashape.MediumQuality, filter=Metashape.AggressiveFiltering)
chunk.buildModel(surface=Metashape.Arbitrary, interpolation=Metashape.EnabledInterpolation)
chunk.buildUV(mapping=Metashape.GenericMapping)
chunk.buildTexture(blending=Metashape.MosaicBlending, size=4096)

doc.save()
'))
}

# Использование функции
build_model("C:/path/to/images", "C:/output/project.psx")