
    library(reticulate)
	library(sf) 
   # labelInput ="D:\\PV_DB/2023_H0054_OPP/20230503_082013/20230503_082013_MINI3PRO_20m_DUMMY/"
	bsname=basename(labelInput)
    py_pth = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
    use_python(py_pth, required = TRUE)
    crs = "EPSG:32610"   #32610 4326
  
    outputpath = paste0(labelInput,"\\",bsname,"_Model.psx")
	DirPolPred = paste0(labelInput,"Polygons\\Predict")
    outputply = paste0(labelInput,"\\",bsname,"_Model.ply")
	outputdir =  paste0(labelInput,"\\",bsname,"_Model"); unlink(outputdir,recursive=T); dir.create(outputdir)
	PthPolPred= list.files(DirPolPred, full.names=T)
	
	
	
if (file.exists(outputpath)==T){		
##############################################################
   psx_path = normalizePath(outputpath,winslash = "/")
  # output_ply = normalizePath(outputply,winslash = "/", mustWork=F)
   PolPred = st_read(PthPolPred)
####################################################################
   Pols1=NULL  
   PolPred <- PolPred %>% st_transform(crs)
   PolPred <- st_buffer(PolPred, dist = 4)
   Pols = unique(PolPred$Name)
     for (i in 1: length(Pols)) {
     SubPol = PolPred[PolPred$Name==Pols[i],]
     poly_coords <- data.frame(st_coordinates(SubPol$geometry[[1]])[,1:2])
	 poly_coords$PolName=Pols[i]
	 Pols1 = rbind(poly_coords,Pols1)
   }
   
 
#####################################################################
    export_model <- function(psx_path, output_ply, coords_str, crs) {
      py_run_string(paste0('
import Metashape
import os
import numpy as np
################################################
doc = Metashape.Document()
doc.open("', psx_path, '")
chunk = doc.chunk
target_crs = Metashape.CoordinateSystem("EPSG::32610")
doc.chunk.crs = target_crs
############################################
print(chunk.crs)
############################################
# Создаем полигон для обрезки
vertices = np.array([', coords_str, '])
polygon = Metashape.Geometry.Polygon(
    [Metashape.Vector(coord.tolist()) for coord in vertices]
)
chunk.shapes = Metashape.Shapes()
chunk.shapes.crs = chunk.crs
shape = chunk.shapes.addShape()
shape.geometry = polygon
shape.label = "Cropping Polygon"
shape.boundary_type = Metashape.Shape.BoundaryType.OuterBoundary
#print(shape.geometry)
############################################################
export_settings = {
    "format": Metashape.ModelFormatPLY,
    "binary": True,
    "precision": 9,
    "texture_format": Metashape.ImageFormatPNG,
	
    "save_texture": True,
    "save_uv": True,
    "strip_extensions": False,
	
	"clip_to_boundary": True,
 
	"crs": Metashape.CoordinateSystem("EPSG::32610"),
	"shift": Metashape.Vector( (435000, 5451000,-100) )
}



chunk.exportModel(
    r"', output_ply, '",
    **export_settings
)

del doc
'))
}



######################################################
for (i in 1: length(Pols)){
 
 pol=Pols[i]
  psx_path
  output_ply=paste0(outputdir,"\\",bsname,"_Model_", pol  ,".ply")
  coords_str <- paste(apply(Pols1[Pols1$PolName==pol,], 1, function(x) 
        paste0("[", x[1], ",", x[2], "]")), collapse=",")


export_model(psx_path, output_ply, coords_str,crs)


}
}
###################################################################




