
   library(reticulate)
  library("tensorflow")
  library("abind")
  library("doParallel")
  library("foreach")
  library("tfdatasets")
  library(tools)
  library(doMC) 
  library(EBImage)
  library(keras)
  library(parallel)


 #############################################
#num_physical_cores <- detectCores(logical = FALSE)
#num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
#batch_size <- num_cores_to_registr 
#batch_size_global = batch_size*10 
#registerDoMC(cores = num_cores_to_registr)
#computer_name <- Sys.info()["nodename"]		
####################################


        labelInput

		ImagePath= file.path(labelInput,"Predict","TilesOverlap")
		MaskPath=  file.path(labelInput,"Predict","MaskPredicted") 
		PathCheck= file.path(labelInput,"Predict","Image_Mask_pred")

		unlink(PathCheck, recursive=T);dir.create(PathCheck)
		MskList=list.files(MaskPath)
		ImgLst=list.files(ImagePath)
		
	#prefix=substr(ImgLst[1],1,15)
      #  prefix
		

 rslt <- mclapply(MskList, function(pth) {


			#pth=MskList[i]
			mskP=file.path(MaskPath,pth) 
			ImgP= file.path(ImagePath, gsub("png","jpg",pth))#prefix,"_",
			
		if (file.exists(ImgP)){	
			img=readImage(ImgP)
			msk=readImage(mskP)
			  y1 = channel(msk, 'asred')
			  a= img+y1
			  a1=resize(a,1024,1024)
		   PathCheckImg=file.path(PathCheck,pth)
		   writeImage(a1,PathCheckImg)
		   

		}
		})
