  #function to filter original foto acording to predict polygon for future 3D model creature
   


    library(exifr)  # need exiftool adds  https://exiftool.org/
    library(sf)
	library(dplyr)
  
  # Define input directory path
   labelInput #<- "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
   
   PolDir=paste0(labelInput,"\\Polygons\\Predict")
   PolPth=list.files(PolDir, full.names=T)[1]
   if (file.exists(PolPth[1])==T){
   
   bsnm=basename(labelInput)
   tmpname=gsub(bsnm,"",labelInput)
   AerialDir= gsub("OPP","Aerial",tmpname)
   image_files=list.files(AerialDir, full.names=T)
     if (length(image_files)<10){
	 AerialDir= paste0(AerialDir,bsnm)
	 image_files=list.files(AerialDir, full.names=T)
	 }
   
   DirCopyTo=paste0(labelInput,"\\Aerial_Images_For_Model"); unlink(DirCopyTo,recursive=T);dir.create(DirCopyTo)
   
   
################################################################
  exif_data <- read_exif(image_files, tags = c("GPSLatitude", "GPSLongitude"))

  # Преобразование в sf объект
    photos_sf <- st_as_sf(exif_data, 
                     coords = c("GPSLongitude", "GPSLatitude"), 
                     crs = 4326) # WGS84

     polygon = st_read(PolPth)
	 buffered <- st_buffer(polygon, dist =30)
     photos_sf <- st_transform(photos_sf, st_crs(polygon))
	 # plot(buffered$geometry)
     # points(photos_sf) 
     photos_inside <- photos_sf[buffered, ]
    file.copy(photos_inside$SourceFile,DirCopyTo)
}



