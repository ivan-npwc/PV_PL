

# source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/CREATE_TR_DATA_SET_FROM_IMGS-MSKS.r")
#  source("/home/ivan/GIT_HUB/PV_PL/Modules/DeepLabV3/CREATE_TR_DATA_SET_FROM_IMGS-MSKS.r")


#file_path <- tcltk::tk_choose.files()


library(EBImage)

dirimgs = "/home/ivan/TRAIN/LRG/No zero/Image"
dirmsk =  "/home/ivan/TRAIN/LRG/No zero/Mask"

output_dir_imgs = "/home/ivan/TRAIN/LRG/Crops_Image"
output_dir_msks = "/home/ivan/TRAIN/LRG/Crops_Mask"
check_dir =  "/home/ivan/TRAIN/LRG/Check"


unlink(check_dir, recursive=T)
unlink(output_dir_imgs, recursive=T)
unlink(output_dir_msks, recursive=T)

dir.create(output_dir_imgs, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_msks, showWarnings = FALSE, recursive = TRUE)
dir.create(check_dir, showWarnings = FALSE, recursive = TRUE)



lstmask = list.files(dirmsk, full.names=T)

for (i in 1:length(lstmask)){

 pth = lstmask[i]
 bsnm = basename(pth)
 bsnm_no_ext= tools::file_path_sans_ext(bsnm)
 imh_pth = file.path(dirimgs,paste0(bsnm_no_ext,".jpg"))
 
 msk = readImage(pth)
 img = readImage(imh_pth)
 
  msk1 = bwlabel(msk)

       IndexBorderLeft=0.15* 1024
       IndexBorderRight=0.85* 1024
	   
      
vectorObjects=c(1:max(msk1))

 for (h in 1:length(vectorObjects)) {
     # h=1
      object <- vectorObjects[!vectorObjects %in% h]
      letter = rmObjects(msk1, object, reenumerate=FALSE)
	  letter1 = bwlabel(letter)
	
	  
	  fts = data.frame(computeFeatures.moment(letter1))  # coordinat
	  
	   fts=fts[fts$m.cx >= IndexBorderLeft & fts$m.cx <= IndexBorderRight,] 
       fts=fts[fts$m.cy >= IndexBorderLeft & fts$m.cy <= IndexBorderRight,]
	    if (length(fts[,1])==0)  {next}
		coords = ocontour(letter1)
		
	  # Получаем bounding box из контуров
        all_x = unlist(lapply(coords, function(x) x[, 1]))
        all_y = unlist(lapply(coords, function(x) x[, 2]))
        
        xmin = min(all_x)
        xmax = max(all_x)
        ymin = min(all_y)
        ymax = max(all_y)
        
        # Добавляем отступы
        padding = 150
        xmin = max(1, floor(xmin) - padding)
        xmax = min(dim(img)[2], ceiling(xmax) + padding)
        ymin = max(1, floor(ymin) - padding)
        ymax = min(dim(img)[1], ceiling(ymax) + padding)
	  
	    img_crop = img[xmin:xmax, ymin:ymax, ]
	    msk_crop = letter1[xmin:xmax, ymin:ymax]
	   # filter =  dilate(msk_crop, makeBrush(150, shape='disc'))
		
		#img_crop[filter == 0] = 0

		     y1 = channel(msk_crop, 'asred')
			 a= img_crop + y1
			 
	pth_save_img = file.path(output_dir_imgs, paste0(h,"#",	bsnm_no_ext,".JPG"))
    pth_save_msk = file.path(output_dir_msks, paste0(h,"#",	bsnm_no_ext,".png"))	
	pth_save_check =  file.path(check_dir,paste0(h,"#",	bsnm_no_ext,".JPG"))	
	
	writeImage(img_crop,pth_save_img)
	writeImage(msk_crop,pth_save_msk)
	writeImage(a,pth_save_check)
		   #  display(a)
	  }













}
