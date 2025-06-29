
	System_data
    labelInput
    Species
   date1=substr(basename(labelInput),1,15)
  library(raster)
  

  PolygonDir=paste0(labelInput,"\\Polygons")
  ObserverCountDir=paste0(labelInput,"\\Observer_count")

  AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurements")
   HauloutDir=paste0(labelInput,"\\Polygons\\Haulout")
   RookeryDir=paste0(labelInput,"\\Polygons\\Rookery")
   ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude")
   ModelPolDir=paste0(labelInput,"\\Polygons\\Model")
  PredictPolDir=paste0(labelInput,"\\Polygons\\Predict")
   Animal_outline = paste0(labelInput,"\\Polygons\\Animal_outline")
   PredOptimisationDir=paste0(labelInput,"\\Polygons\\Prediction optimisation")
  

  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
   dir.create(AnimalMeasurDir,showWarnings = F)
 #  dir.create(HauloutDir,showWarnings = F)
  # dir.create(RookeryDir,showWarnings = F)
#  dir.create(ExcludeDir,showWarnings = F)
 #  dir.create(ModelPolDir,showWarnings = F)
  #  dir.create(PredOptimisationDir,showWarnings = F)
 dir.create(PredictPolDir,showWarnings = F)
 dir.create(Animal_outline,showWarnings = F)
 
 #unlink(RookeryDir, recursive=T)
 # unlink(HauloutDir, recursive=T)
 #  unlink(ExcludeDir, recursive=T)
 #   unlink(ModelPolDir, recursive=T)
#	 unlink(PredOptimisationDir, recursive=T)
###########################################################################						  		
###################################### DATA CORRECTION
ObserverCountDirOLD=paste0(labelInput,"\\Observer count")
 if (dir.exists(ObserverCountDirOLD)) {
listFiles=list.files(ObserverCountDirOLD, full.names=T)
file.copy(listFiles,ObserverCountDir)
}


MsrOLD=paste0(labelInput,"\\Polygons\\Animal_measurments")
 if (dir.exists(MsrOLD)) {
listMsr=list.files(MsrOLD, full.names=T)
file.copy(listMsr,AnimalMeasurDir)
}





unlink(ObserverCountDirOLD, recursive=T)
unlink(MsrOLD, recursive=T)













