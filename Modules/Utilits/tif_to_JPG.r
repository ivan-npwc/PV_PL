
library(EBImage)
library(tiff)

#labelInput=  "C:\\Users\\usato\\SSL_UI\\PV_UI\\2023_H0068C_OPP\\20230720_111049\\20230720_111049_MINI2_20m"
outputDir = file.path(labelInput, "Predict", "SealCrop")
lstimgs=list.files(outputDir, full.names=T)

 for (d in 1:length(lstimgs)){
     pth =  lstimgs[d]

     img_tiff <- readTIFF(pth, all = TRUE, info = TRUE)
      if (length(img_tiff) > 0) {img_eb <- Image(img_tiff[[1]]) } else{img_eb =  img_tiff}


     if (dim(img_eb)[3] == 4) {  img_rgb <- img_eb[, , 1:3] }
     colorMode(img_rgb) <- Color



     newName = gsub("tif","JPG", pth)
     writeImage(img_rgb, newName, quality = 100, type = "jpeg")
     img=NULL
     }

unlink(lstimgs, recursive=T)