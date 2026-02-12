
library(EBImage)
library(tiff)

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
