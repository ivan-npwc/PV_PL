

                             library(sf)


							 labelInput
							 date1=substr(basename(labelInput),1,15)
					         Species
                             crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
							 pathTablePoints=paste0(labelInput, "\\Predict\\",Species, "_BlobTable_GEO_",date1, ".csv")
							Predict_polygonDir=paste0(labelInput, "\\Polygons\\Predict") 		

					         saveKMLpth <<- paste0(labelInput,"\\Predict\\",Species,"_", date1, ".kml")
				

   if(dir.exists(Predict_polygonDir)==T){Predict_polygon <-list.files(Predict_polygonDir,full.names=T)[1]}
	 

####################################################################################################

    dat<-read.csv(pathTablePoints)
    dat<-dat[is.na(dat$lat) ==F ,] 
    pnts <- data.frame(lat=dat$lon,   lon=dat$lat,  area=dat$s.area)
 
    points_crs <- st_as_sf(data.frame(lat = pnts$lat, 
                                      lon = pnts$lon, 
                                      area = pnts$area), 
                        coords = c("lat", "lon"), crs = crs)
 
 
  points_crs = st_transform(points_crs, 32610)
  polygon_to_filter = read_sf(Predict_polygon)
  polygon_crs = st_transform(polygon_to_filter, 32610)
  points_in_polygon <- st_intersection(points_crs, polygon_crs)
 

# 3. Создание KML с стилизацией
st_write(points_in_polygon, saveKMLpth, 
         driver = "KML",
         delete_dsn = TRUE,
         dataset_options = c(
        "NameField=area"
         ))


















