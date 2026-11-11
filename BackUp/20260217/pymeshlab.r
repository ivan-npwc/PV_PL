

library(reticulate)
# Создайте новую среду с правильными параметрами
conda_create("mesh_env", 
             packages = c("python=3.9",  # Python 3.9 более стабилен
                         "gcc_linux-64",  # Компилятор
                         "gxx_linux-64",  # C++ компилятор
                         "libstdcxx-ng"), # C++ библиотеки
             channel = "conda-forge")

# Активируйте
use_condaenv("mesh_env")
conda_install("pymeshlab", channel = "conda-forge", envname = "mesh_env")


library(reticulate)
pymeshlab <- import("pymeshlab")



mesh = sl_m
threshold=100

  # Создаем временный файл
  temp_input <- tempfile(fileext = ".ply")
  temp_output <- tempfile(fileext = ".ply")
  
  # Сохраняем меш
  vcgPlyWrite(mesh, temp_input)
  
  # Создаем MeshSet
  ms <- pymeshlab$MeshSet()
  ms$load_new_mesh(temp_input)
  
  # Объединяем близкие вершины
  cat(sprintf("Объединение вершин с расстоянием < %f...\n", threshold))
  
  ms$apply_filter('merge_close_vertices',
                 threshold = as.numeric(threshold))
  
  # Сохраняем результат
  ms$save_current_mesh(temp_output)
  
  # Загружаем обратно в R
  result <- vcgImport(temp_output)
  
  # Удаляем временные файлы
  file.remove(temp_input, temp_output)
  
  vcgPlyWrite(result, "mesh.ply") 
