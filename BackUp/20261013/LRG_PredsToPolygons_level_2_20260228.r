    

	# source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/LRG_PredsToPolygons_level_2.r")

   #labelInput= "/mnt/c/Users/usato/SSL_UI/PV_UI/2024_H0590_OPP/20240916_133629/20240916_133629_MINI3PRO_20M" 

	 library(sp)
     library(EBImage)
    # library(raster)
     library(sf)
     library(dplyr)
	 library(smoothr)



  Species = "LRG_level_2"

  ImgOcFin=NULL
  if(exists("SppBLB_fin")){remove(SppBLB_fin)}
  
  PRJ=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  date1= substr(basename(labelInput),1,15)
#  RefTablePTH = file.path(labelInput, paste0(date1, ".csv"))
  predsDir= file.path(labelInput,"Predict","Preds")
  SpP_pth= file.path(labelInput,"Predict",paste0("SpP_",Species,"_",date1))
  kmlPathSave= file.path(labelInput, "Predict",  paste0("SpP_",Species,"_","level#2_",date1,".kml"));unlink(kmlPathSave)
      # dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------
  listPreds = list.files(predsDir,full.names=T,pattern= paste0("Preds_",Species))

#########################################################################
  for (f in 1:length(listPreds)) {
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
       dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------
#resultBlob_tmp=NULL
# i=4
#resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
   for (q in 1: length (listImageBl)) {
     name=basename(listImageBl[q])
     origImgPTH = listImageBl[q]
     origImg = readImage(origImgPTH)
     dim_orig =dim(origImg)

     bs = tools::file_path_sans_ext(name)
     splt = strsplit(bs,"_")

     west = as.numeric(splt[[1]][3])
     east = as.numeric(splt[[1]][4])
     south = as.numeric(splt[[1]][1])
     north = as.numeric(splt[[1]][2])

     RefSort = data.frame(west =west,east =east,south=south,north=north)


 msk = preds[q, , , ]
msk2 = as.Image(msk)
msk3 = resize(msk2, 256, 256)
msk4 = thresh(msk3, 18, 18, 0.009)
msk5 = fillHull(msk4)

# closing (dilate→erode) вместо opening — не разрушает тонкие объекты
msk6 = dilate(msk5, makeBrush(9,  shape = "disc"))
msk7 = erode(msk6,  makeBrush(7,  shape = "disc"))
msk8 = fillHull(msk7)

# resize на бинарной маске ДО bwlabel — без артефактов интерполяции
msk9  = resize(msk8, dim_orig[1], dim_orig[2])
msk10 = msk9 > 0.5
msk11 = as.Image(msk10)
msk12 = fillHull(msk11)
msk13 = bwlabel(msk12)

max(msk13)


	#   display(msk)
   
  if (max(msk13)==0) next
      oc=ocontour(msk13)
    #   dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------	   
ImgOc=NULL
for (o in 1: length(oc)){   #length oc is number blobs in the img
	            pre=data.frame(x=as.numeric(oc[o][[1]][,1]),y=as.numeric(oc[o][[1]][,2]),id=paste0(name,"#",o),img=name, blob=o)
	            ImgOc=rbind(pre,ImgOc)}
					
#-------------------------------------------------------------
    NSdifStep = (RefSort$north - RefSort$south) / dim_orig[2]
     WEdifStep = (RefSort$east - RefSort$west) / dim_orig[1]
#---------------------------------------------------------------
     ImgOc$lat = RefSort$west + (ImgOc$x * WEdifStep)
     ImgOc$lon = RefSort$south + ((dim_orig[2] - ImgOc$y) * NSdifStep)  # y координата инвертирована

	ImgOcFin=rbind(ImgOcFin,ImgOc)
	
print(paste0("IMGS ",q, "/", length(listImageBl), "  PREDS  ",f,"/",length(listPreds)  ))

}
}
#############################################################################################################################	
   coordinates(ImgOcFin) <- ~ lat+lon
    srPolygons=list()
	ListPol=unique(ImgOcFin$id)

for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(ImgOcFin[ImgOcFin$id==ListPol[s],])),paste0(ListPol[s]))
#print (paste0("Processing ", s,"/",length(ListPol), " blobs"))
}
SpP = SpatialPolygons(srPolygons)
proj4string(SpP) <- PRJ

info = data.frame(id = row.names(SpP))
info1 = strsplit(x = as.character(info$id), split = "#")
info2 = NULL
for (i in 1:length(info1)) {
  info2$img[i]  = paste0(info1[[i]][1])
  info2$blob[i] = info1[[i]][2]
}
info3 = data.frame(img = info2$img, blob = info2$blob)
row.names(info3) = paste0(info2$img, "#", info2$blob)
info3$blb = paste0(info2$img, "#", info2$blob)

SppBLB = SpatialPolygonsDataFrame(SpP, info3)
polygons = st_as_sf(SppBLB)

# --- Валидация и очистка ---
polygons = st_make_valid(polygons)
polygons = polygons[!st_is_empty(polygons), ]

# --- Сглаживание полигонов (smoothr) ---
# install.packages("smoothr")


# Небольшой буфер чтобы замкнуть соседние тайлы перед объединением
polygons_buf = st_buffer(polygons, dist = 0.00001)
polygons_buf = st_make_valid(polygons_buf)

merged = st_union(polygons_buf)
merged = st_make_valid(merged)

# Разбиение на отдельные полигоны
merged = st_collection_extract(merged, "POLYGON")
merged = st_cast(merged, "POLYGON")
merged = st_as_sf(merged)

# Фильтрация по площади (убираем мусорные осколки)
areas = as.numeric(st_area(merged))
merged = merged[areas > 0.05, ]

##########################################################################

##########################################################################

# --- Сглаживание контуров ---
# Метод "chaikin" — мягкое сглаживание углов (можно заменить на "ksmooth")
smoothed = smooth(merged, method = "ksmooth", smoothness = 13)

# Финальная валидация и упрощение
smoothed = st_make_valid(smoothed)
smoothed = smoothed[!st_is_empty(smoothed), ]

# Лёгкое упрощение после сглаживания чтобы убрать лишние точки
smoothed = st_simplify(smoothed, preserveTopology = TRUE, dTolerance = 0.0005)
smoothed = st_make_valid(smoothed)





write_sf(smoothed, kmlPathSave)