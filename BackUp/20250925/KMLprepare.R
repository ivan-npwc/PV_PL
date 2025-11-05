    require(sp)
	require(sf)
	require(EBImage)
	library(XML)
	
  
	#labelInput = "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m" #"D:\\PV_DB\\2023_H0054_OPP\\20230503_082013\\20230503_082013_MINI3PRO_20m_DUMMY"
	date1=substr(basename(labelInput),1,15)
	pth_table=paste0(labelInput,"\\",date1,".csv")
    KMLdir<<-paste0(labelInput,"\\",date1)
	doc.kml=list.files(KMLdir, pattern=".kml", full.names=T)
	 
	if(dir.exists (KMLdir)==F) {stop("No kml OPP found")}
###########################################################
GetTableKml= function(doc.kml) {
  doc <- htmlParse(doc.kml)
  west=   as.numeric(sapply(getNodeSet(doc, "//west"), xmlValue))
  east=as.numeric(sapply(getNodeSet(doc, "//east"), xmlValue))
  south=as.numeric(sapply(getNodeSet(doc, "//south"), xmlValue))
  north=as.numeric(sapply(getNodeSet(doc, "//north"), xmlValue))
  image=sapply(getNodeSet(doc, "//href"), xmlValue)
  west=west[seq_along(west) %% 2 > 0]
  east=east[seq_along(east) %% 2 > 0]
  south=south[seq_along(south) %% 2 > 0]
  north=north[seq_along(north) %% 2 > 0]
  pathDir=sub("doc.kml","",doc.kml)
  table=data.frame(west,east,south,north,image)
  table
}
#############################################################
GetTableKml_Dir=function(KMLdir) { 
  pathFolders=list.dirs(KMLdir,full.names = TRUE,recursive = F)
  KMLimg=NULL
  for (q1 in 1: length(pathFolders)) {
    subdir=pathFolders[q1]
    imgList= list.files(subdir) 
    if (length(imgList)>10) {
      doc.kml=paste0(subdir, "\\","doc.kml")
      geo.infoALL<<-GetTableKml(doc.kml)
      geo.info=geo.infoALL#[nchar(as.character(geo.infoALL$image))=="10",] # hier we sort image f livel only
      fLevelImg=geo.info$image
      fLevelImgPath=paste0(subdir,"\\",fLevelImg)
      KMLimg=rbind(KMLimg, data.frame(fLevelImgPath,geo.info,subdir))
    }}
  KMLimg
}
######################################################################
 check = length(list.dirs(KMLdir))
 if (check > 2) {TilesTable <- GetTableKml_Dir(KMLdir)}
 if (check < 2) {TilesTable <- GetTableKml(doc.kml)}

##############################################################
  	p1=data.frame(lon=TilesTable$west,lat=TilesTable$south)
	p2=data.frame(lon=TilesTable$east, lat=TilesTable$north)
    TilesTable$tileSize = pointDistance(p1,p2,lonlat=T)                                   # TILE SIZE
    minT= substr(TilesTable$image[TilesTable$tileSize==min(TilesTable$tileSize)][1],0,1)
    TilesTable$lvl= substr(TilesTable$image,0,1)
    out3=TilesTable[TilesTable$lvl== minT,]
	out3$bs_subdir=basename(out3$subdir)
	out4=data.frame(west=out3$west,  east =out3$east,   south=out3$south,    north =out3$north,     image= paste0(out3$bs_subdir,"_", out3$image))
	
   #########   PIXEL SIZE
 #  x = 3.221493e-08
 #  y = 1.751848e-08
  tlsize= min(TilesTable$tileSize)
   print(paste0("Tile size is  ", min(TilesTable$tileSize)))
  #if (tlsize > 1.85 |   tlsize < 1.37) {stop (paste0("export OPP with pixel size:  ", x,"_", y ))}

##############
	srPolygons=list()
	srPolygonsData=list()
	for(i in 1:length(out3[,1]))
	{
		srPolygons[[i]]=Polygons(list(Polygon(cbind(as.numeric(c(out4[i,1],out4[i,1],out4[i,2],out4[i,2])),as.numeric(c(out4[i,3],out4[i,4],out4[i,4],out4[i,3]))))),paste0(out4$image[i] ))
		srPolygonsData[[i]]=data.frame(dx=abs(as.numeric(out4[i,2])-as.numeric(out4[i,1])),dy=abs(as.numeric(out4[i,4])-as.numeric(out4[i,3])))
	}

	SpP=SpatialPolygons(srPolygons)
	srPolygonsData=do.call(abind, c(srPolygonsData, list(along = 1)))


	rows=rownames(sp::coordinates(SpP))
	rownames(srPolygonsData)=rows
	srPolygonsData=as.data.frame(srPolygonsData)

	data1=as.data.frame(rownames(srPolygonsData))
	rownames(data1)=rows
	SpP_data=SpatialPolygonsDataFrame(SpP,data1)

	SpP_data=subset(SpP_data,srPolygonsData[,1]<=quantile(srPolygonsData[,1],0.975))
	
	proj4string(SpP_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	SpP_data=spTransform(SpP_data,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	SpP_data
	################################################################################
	##################################################################################  matrix
	SpP_data_1 <- st_as_sf(SpP_data)

	CoordsTable=as.data.frame(sp::coordinates(SpP_data))
	CoordsTable$Lon=CoordsTable$V1;CoordsTable$V1=NULL
	CoordsTable$Lat=CoordsTable$V2;CoordsTable$V2=NULL
	
	CoordsTable$LonStep=0
	CoordsTable$LatStep=0
	
	  for (i in 2:length(CoordsTable[,1])) { 
	  CoordsTable$LonStep[i]= abs(CoordsTable$Lon[i]-CoordsTable$Lon[i-1])
	   CoordsTable$LatStep[i]= abs(CoordsTable$Lat[i]-CoordsTable$Lat[i-1]) 
	  }
	
LatCellSize=median(CoordsTable$LatStep, na.rm = T)
if (LatCellSize==0){LatCellSize=median(CoordsTable$LatStep[CoordsTable$LatStep !=0], na.rm = T)}
LonCellSize=median(CoordsTable$LonStep, na.rm = T)

grid_1 <- SpP_data_1 %>% 
	st_make_grid(cellsize = c(LonCellSize,LatCellSize) , what = "centers")


	coords <- do.call(rbind, st_geometry(grid_1))
	grid=SpatialPoints(coords, proj4string = CRS(proj4string(SpP_data)))

#get anchor labels assigned form dataframe
	tile_placement=grid%over%SpP_data	
#get number of columns
	coords1=coords
	coords1[,2]=c(coords[1,2],coords[1:(length(coords[,2])-1),2])

	dx=coords[,2]-coords1[,2]
	ncols=min((1:length(dx))[dx!=0])-1

	# generate anchor matrix
	tile_placement=as.character(tile_placement[,1])
	tile_placement1=matrix(tile_placement[(length(tile_placement):1)],ncol=ncols,byrow=TRUE)
#################################################################################################

dimOPP=dim(tile_placement1)
numImgs=dimOPP[1]*dimOPP[2]
OPPwidth=dimOPP[2]
OPPhight=dimOPP[1]
vec=rep(0,numImgs)
TableNear =data.frame(imgName=vec,leftName= vec,upName= vec, rightName=vec, downName=vec)
 
line1=1
 for (i in 1:OPPhight){       # обработка строчек
  for (y in 1:OPPwidth){     # обработка столбцов
TableNear$imgName[line1]=as.character(tile_placement1[i,y])
if (y-1>0 & y < OPPwidth) { TableNear$leftName[line1]=as.character(tile_placement1[i,y-1])} else { TableNear$leftName[line1]="edge"} #leftName  Y width
if (i-1>0 & i < OPPhight) {TableNear$upName[line1]=as.character(tile_placement1[i-1,y])} else {TableNear$upName[line1]="edge"}      #upName   I  hight
if (i-1>0 & i < OPPhight) {TableNear$downName[line1]=as.character(tile_placement1[i+1,y])} else {TableNear$downName[line1]="edge"}   #downName I   hight
if (y-1>0 & y < OPPwidth) { TableNear$rightName[line1]=as.character(tile_placement1[i,y+1])} else { TableNear$rightName[line1]="edge"}  #rightName  Y width
line1 =line1 +1
}}

TableNear=TableNear[is.na(TableNear$imgName)==F,]
out4$imgName=out4$image; out4$image=NULL

TableNear1=left_join(TableNear,out4, by="imgName")

 write.csv(TableNear1,pth_table, row.names = F)

