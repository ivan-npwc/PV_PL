     # source("/home/ivan/GIT_HUB/PV_PL/Modules/LRG/LRG_PredsToPolygons_level_2.r")
	   labelInput  = "/home/ivan/adata8tb/PV_DB/2023_H0052A_OPP/20230615_103330/20230615_103330_MAVIC2PRO_40m"


	 library(sp)
     library(EBImage)
     library(raster)
     library(sf)
     library(dplyr)



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
  
 # if (file.exists(listPreds[1])==T){
  
  
 # RefTable1  = read.csv(RefTablePTH)
 # RefTable=data.frame(imgName=RefTable1$imgName,west=RefTable1$west,east=RefTable1$east,south=RefTable1$south,north=RefTable1$north)
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


       msk=preds[q, , , ]
	  # msk1 =t(msk)
	   msk2 =as.Image(msk)
       msk3 =resize(msk2,256,256)
       msk4 = thresh(msk3, 18, 18, 0.009)
       msk5 <- fillHull(msk4)
       msk6 = opening(msk5, makeBrush(11,shape='disc') ) # shape='Gaussian', sigma=50
	   msk7 = erode(msk6, makeBrush(11, shape='diamond'))
	   msk8 = dilate(msk7, makeBrush(13, shape='diamond'))
	   msk9 = fillHull(msk8)
       msk10 = bwlabel(msk9)
	   msk11 =resize(msk10,dim_orig[1],dim_orig[2])
	   #msk12 = dilate(msk11, makeBrush(13, shape='diamond'))
	   msk13 = fillHull(msk11)
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
				
	#	 origImgPTH= paste0(labelInput,"\\Predict\\TilesOverlap\\",name) 
	#	  OrigImg=readImage(origImgPTH)
	#	  plot(OrigImg)		  
#		for (y in 1:length(oc)) {points(oc[[y]], col=2,cex=0.1)}		
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

SpP=SpatialPolygons(srPolygons);proj4string(SpP) <-PRJ
info=data.frame(id=row.names(SpP))
info1=strsplit(x=as.character(info$id),split="#")
info2=NULL

for (i in 1:length(info1)) {info2$img[i]= paste0(info1[[i]][1])
                            info2$blob[i]=info1[[i]][2]}

info3=data.frame(img= info2$img,  blob=info2$blob)
row.names(info3)=paste0(info2$img,"#",info2$blob)
info3$blb=paste0(info2$img,"#",info2$blob)

SppBLB=SpatialPolygonsDataFrame(SpP,info3)
polygons <- st_as_sf(SppBLB)

valid_polygons1 <- st_make_valid(polygons)
valid_polygons1 <- valid_polygons1[!st_is_empty(valid_polygons1), ]
clean_polygons1 <- st_simplify(valid_polygons1, preserveTopology = TRUE, dTolerance = 0.01)


buffered <- st_buffer(clean_polygons1, dist = 0.00001)
merged_polygons <- st_union(buffered)

final_result <- merged_polygons %>%
  st_collection_extract("POLYGON") %>%
  st_cast("POLYGON")

valid_polygons <- st_make_valid(final_result)
valid_polygons <- valid_polygons[!st_is_empty(valid_polygons), ]

 valid_polygons <- st_make_valid(valid_polygons)

clean_polygons <- st_simplify(valid_polygons, preserveTopology = TRUE, dTolerance = 0.01)
clean_polygons <- clean_polygons %>% st_cast("POLYGON")

areas <- st_area(clean_polygons)
ar=as.numeric(areas)
index=which(ar>0.1) #& ar < 0.5)

filter_pol = clean_polygons[index]

#buff <- st_buffer(filter_pol, dist = 0.00001)

write_sf(filter_pol,kmlPathSave)
#}
