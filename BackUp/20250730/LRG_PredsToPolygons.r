     
	 
	 library(sp)
     library(EBImage)
     library(raster)
     library(rgdal)
     library(sf)
     library(dplyr)

  labelInput  #= "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"       
  Species = "LRG"

  ImgOcFin=NULL
  if(exists("SppBLB_fin")){remove(SppBLB_fin)}
  
  PRJ=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  date1= substr(basename(labelInput),1,15)
  RefTablePTH=paste0(labelInput,"\\", date1, ".csv")
  predsDir=paste0(labelInput,"\\Predict\\Preds")
  SpP_pth=paste0(labelInput,"\\Predict\\SpP_",Species,"_",date1)
  kmlPathSave=paste0(labelInput,"\\Predict\\SpP_",Species,"_",date1,".kml")
      # dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------
  listPreds=list.files(predsDir,full.names=T,pattern=Species)  
  RefTable1=read.csv(RefTablePTH)
  RefTable=data.frame(imgName=RefTable1$imgName,west=RefTable1$west,east=RefTable1$east,south=RefTable1$south,north=RefTable1$north)
  for (f in 1:length(listPreds)) {
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
       dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------
#resultBlob_tmp=NULL
# i=4
#resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
   for (i in 1: length (listImageBl)) {
     name=basename(listImageBl[i])
	 RefSort=RefTable[RefTable$imgName==name,]
  
	   msk=preds[i, , , ]
	   msk=t(msk)
	   msk=as.Image(msk)
       msk = thresh(msk, 18, 18, 0.009)  
       msk <- fillHull(msk)
       msk = opening(msk, makeBrush(11,shape='disc') ) # shape='Gaussian', sigma=50
	   msk = erode(msk, makeBrush(11, shape='diamond'))
	   msk = dilate(msk, makeBrush(13, shape='diamond'))
	   msk = fillHull(msk)	   
       msk = bwlabel(msk)
	   msk=resize(msk,1024,1024) 
	   msk = dilate(msk, makeBrush(13, shape='diamond'))
	   nmask4 = fillHull(msk)
	   max(nmask4)
	#   display(msk)
   
  if (max(nmask4)!=0) {   
		   oc=ocontour(nmask4)
       dim(preds)=c(PredsRDS$DimPreds)
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
    NSdifStep=((RefSort$north-RefSort$south)*2)/1024 
    WEdifStep= ((RefSort$east-RefSort$west)*2)/1024
    NSdif50=(RefSort$north-RefSort$south)/2 
    RefSort$north50=RefSort$north+NSdif50  
    SNdif50=(RefSort$north-RefSort$south)/2
    RefSort$south50=RefSort$south-SNdif50 
    WEdiff50= (RefSort$east-RefSort$west)/2
    RefSort$west50=RefSort$west-WEdiff50 
    EWdiff50= (RefSort$east-RefSort$west)/2 
    RefSort$east50=RefSort$east+EWdiff50 
    Lonlim=c(RefSort$west50,RefSort$east50)
    Latlim=c(RefSort$south50,RefSort$north50)
#---------------------------------------------------------------
    ImgOc$lon=((1024-ImgOc$y)*NSdifStep)+RefSort$south50 
    ImgOc$lat=(ImgOc$x*WEdifStep)+RefSort$west50
	ImgOcFin=rbind(ImgOcFin,ImgOc)
	
print(paste0("IMGS ",i, "/", length(listImageBl), "  PREDS  ",f,"/",length(listPreds)  ))	
}
}
}
#############################################################################################################################	
  coordinates(ImgOcFin) <- ~ lat+lon
    srPolygons=list()
	ListPol=unique(ImgOcFin$id)
	
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(ImgOcFin[ImgOcFin$id==ListPol[s],])),paste0(ListPol[s]))
print (paste0("Processing ", s,"/",length(ListPol), " blobs"))}

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
clean_polygons1 <- st_simplify(valid_polygons1, preserveTopology = TRUE, dTolerance = 0.001)


buffered <- st_buffer(clean_polygons1, dist = 0.001)
merged_polygons <- st_union(buffered)

final_result <- merged_polygons %>% 
  st_collection_extract("POLYGON") %>% 
  st_cast("POLYGON")
  
valid_polygons <- st_make_valid(final_result)
valid_polygons <- valid_polygons[!st_is_empty(valid_polygons), ]
clean_polygons <- st_simplify(valid_polygons, preserveTopology = TRUE, dTolerance = 0.001)
clean_polygons <- clean_polygons %>% st_cast("POLYGON") 

areas <- st_area(clean_polygons)
ar=as.numeric(areas)
index=which(ar>0.15 & ar < 0.5)
filter_pol = clean_polygons[index]
buff <- st_buffer(filter_pol, dist = 0.1)

write_sf(buff,kmlPathSave)
