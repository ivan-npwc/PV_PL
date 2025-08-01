     
	
	 library(magick)
	 library(parallel)
	 library(doParallel)
	 library(foreach)
	 library(rgdal)
	 
	#labelInput = "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
     Batch=4000
	 crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
     date1=substr(basename(labelInput),1,15)
     Tpth = paste0(labelInput,"\\", date1, ".csv")
	 PredPolDir=paste0(labelInput, "\\Polygons\\Predict")
	 PredPolPth=list.files(PredPolDir, pattern="kml|shp", full.names=T)[1]
	 KMLdir <- paste0(labelInput,"\\",date1)
	 
if (dir.exists(KMLdir) == F) {stop("No kml OPP found")}  	 
	 
#	 if (file.exists(Tpth)==F) {
#	 source("Modules/01_Unzip.r")
#	 print("Done Unzip extra")
#	 source("Modules/02.0_KMLprepare.r")
#	 print("Done KMLprepare extra")
#	 }
	 
	 KMLDir = paste0(labelInput,"\\",date1)
     table=read.csv(Tpth)
     SaveDir=paste0(labelInput, "\\Predict\\TilesOverlap")
	 unlink(SaveDir, recursive=T)
	 dir.create(paste0(labelInput, "\\Predict"),showWarnings=F); dir.create(SaveDir,showWarnings=F)
##############################################################################################

   CombinationOne=data.frame(lat= table$west, lon=table$south,imgName= table$imgName)
   CombinationTo=data.frame(lat= table$west, lon=table$north,imgName= table$imgName)
   CombinationThry=data.frame(lat= table$east, lon=table$south,imgName= table$imgName)
   CombinationFor=data.frame(lat= table$east, lon=table$south,imgName= table$imgName)
   PointsPorderImage=rbind(CombinationOne,CombinationTo,CombinationThry,CombinationFor)
    coords <- data.frame(lat= PointsPorderImage$lat, lon=PointsPorderImage$lon)   
    data   <- data.frame(imgName= PointsPorderImage$imgName)   # data
    
    PointsImg <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
									 
         PredPol=readOGR(PredPolPth) # need kml
		 proj4string(PredPol) <-crs	
		 Index= PointsImg %over% PredPol 
		 PointsImg$Index=Index
		 ImgIn = PointsImg$imgName[is.na(PointsImg$Index)==F]
	     ImgIn =	unique(ImgIn[is.na(ImgIn)==F])
######################################
   exsts=list.files(SaveDir)
   NeedMore = ImgIn[!ImgIn %in% exsts]
    print(paste0("Need prepare ", length(NeedMore), " Images from  ",  length(ImgIn)))
	print(labelInput)
	
  #  if (length(NeedMore)>1) {

      Loops=round(length(NeedMore)/Batch)+1
      if (length(NeedMore) < Batch) {Loops=1}
	  
   for (r in 1: Loops) {
   
    cl <- makePSOCKcluster(detectCores (logical=FALSE)-2) 
    clusterEvalQ(cl, {library(magick)})
    registerDoParallel(cl)
   
    exsts=list.files(SaveDir)
    NeedMore1=ImgIn[!ImgIn %in% exsts]
	 if (length(NeedMore1)<Batch){Batch=length(NeedMore1)}
	
    foreach(i = 1:Batch) %dopar% {   #
  ################
 #for (i in 1:Batch){
  
	  selectRow <- table[table$imgName == NeedMore1[i],]
      path=    paste0(KMLdir,"\\",strsplit(selectRow$imgName,"_")[[1]][1],"\\",strsplit(selectRow$imgName,"_")[[1]][2])
	  CheckExists =	 paste0(SaveDir, "\\",basename(selectRow$imgName))

    #  if(file.exists(CheckExists)==F & file.exists(path)) {                                                                   
        img=image_read(path)  
        ################################################################################## CENTRAL WITH left right EARS
	   # lpi<- paste0(selectRow$rightName)
		lpi<- paste0(KMLdir,"\\",strsplit(selectRow$rightName,"_")[[1]][1],"\\",strsplit(selectRow$rightName,"_")[[1]][2])
        if (file.exists(lpi)==T) {left.img= image_read(lpi)} else {left.img= image_blank(512,512,color = "white")}

       #rimg<-paste0(selectRow$leftName)
	    rimg<- paste0(KMLdir,"\\",strsplit(selectRow$leftName,"_")[[1]][1],"\\",strsplit(selectRow$leftName,"_")[[1]][2])
        if (file.exists(rimg)==T){right.img=image_read(rimg)} else {right.img=  image_blank(512,512,color = "white")}
        ########
        #uimg<-paste0(selectRow$upName)                                                           # prepare up img for next up line level
        uimg<-paste0(KMLdir,"\\",strsplit(selectRow$upName,"_")[[1]][1],"\\",strsplit(selectRow$upName,"_")[[1]][2])
	    if (file.exists(uimg)==T) {up.img=image_read(uimg)} else{up.img= image_blank(512,512,color = "white")}  
        ##########
        #dimg = paste0(selectRow$downName)                                                          # prepare down img for next down line level
        dimg = paste0(KMLdir,"\\",strsplit(selectRow$downName,"_")[[1]][1],"\\",strsplit(selectRow$downName,"_")[[1]][2])
		if (file.exists(dimg)==T) {down.img=image_read(dimg)} else {down.img=  image_blank(512,512,color = "white")}
        
        leftCrop=image_crop(left.img, "256x512+256")
        leftJoin=image_append(c(leftCrop,img))
        ###
        RightCrop=image_crop(right.img, "256x512") 
        leftRightJoin=image_append(c(leftJoin,RightCrop))
        ############################################################## UP LEVEL
        
        Uplevel= table[table$imgName==paste0(selectRow$upName),] 
        if (length(Uplevel$imgName)==0) {
          UpLeft= image_blank(512,512,color = "white")
          UpRight=  image_blank(512,512,color = "white")
        } else {
	      # ulimg = paste0( Uplevel$rightName) 
		    ulimg =  paste0(KMLdir,"\\",strsplit(Uplevel$rightName,"_")[[1]][1],"\\",strsplit(Uplevel$rightName,"_")[[1]][2])
          if (file.exists(ulimg)==T) {UpLeft=image_read(ulimg)} else{UpLeft= image_blank(512,512,color = "white")}  
	      # urimg=paste0(Uplevel$leftName)
		   urimg = paste0(KMLdir,"\\",strsplit(Uplevel$leftName,"_")[[1]][1],"\\",strsplit(Uplevel$leftName,"_")[[1]][2])
          if (file.exists(urimg)==T) {UpRight=image_read(urimg)} else{UpRight=  image_blank(512,512,color = "white")}
		  }
        UplevelImg=image_append(c(UpLeft,up.img,UpRight))
        UpCrop=image_crop(UplevelImg, "1536x256+0+256")
        UpCrop1=image_crop(UpCrop, "1280x256+256")
        UpCrop2=image_crop(UpCrop1, "1024x256")
        ###
        leftRightUpJoin=image_append(c(UpCrop2,leftRightJoin), stack = T)
        #############################################################################   DOWN LEVEL 
        Downlevel= table[table$imgName==paste0(selectRow$downName),] 
        if (length(Downlevel$imgName)==0) {
          DownLeft=  image_blank(512,512,color = "white")
          DownRight=  image_blank(512,512,color = "white")
        } else { 
	      # dlimg = paste0( Downlevel$rightName) 
		  dlimg = paste0(KMLdir,"\\",strsplit(Downlevel$rightName,"_")[[1]][1],"\\",strsplit(Downlevel$rightName,"_")[[1]][2])
          if (file.exists(dlimg)==T) {DownLeft=image_read(dlimg)} else{DownLeft=  image_blank(512,512,color = "white")}
	       #drimg = paste0(Downlevel$leftName)
		    drimg = paste0(KMLdir,"\\",strsplit(Downlevel$leftName,"_")[[1]][1],"\\",strsplit(Downlevel$leftName,"_")[[1]][2])
          if (file.exists(drimg)==T) {DownRight=image_read(drimg)} else {DownRight=  image_blank(512,512,color = "white")}
        }
        DownlevelImg=image_append(c(DownLeft,down.img,DownRight))
        DownCrop=image_crop(DownlevelImg, "1536x256")
        DownCrop1=image_crop(DownCrop, "1280x256+256")
        DownCrop2=image_crop(DownCrop1, "1024x256")
        ############
        leftRightUpDownJoin=image_append(c(leftRightUpJoin,DownCrop2), stack = T)
        image_write(leftRightUpDownJoin,CheckExists,format="jpg")
      }
	
	 
    }
 stopCluster(cl)
 # } 
 # }   


    #  unlink(KMLdir, recursive=T)
	#  unlink(kmlOPP, recursive=T) 