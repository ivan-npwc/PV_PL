		 # Load required libraries
		library(Rvcg)        # For 3D mesh operations
		library(rgl)         # For 3D visualization
		library(sf)          # For spatial data operations
		library(Morpho)      # For morphological operations on 3D data
		library(XML)         # For XML parsing
		library(dplyr)       # For data manipulation
		library(dbscan)      # For density-based clustering
		library(alphashape3d) # For alpha shape calculations
		library(plotly)
		library(RANN)
		#library(onion) # Для работы с кватернионами
		#library(EBImage)     # For image processing
		# Set coordinate reference system (CRS) and offset values
		crs <- 32610         # UTM zone 10N coordinate system
		E <- 435000          # Easting offset
		N <- 5451000         # Northing offset
		A <- -100            # Altitude offset (not currently used)

		# Define input directory path
		labelInput #<- "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
        
		# Set species and extract date from directory name
		Species <- "LRG"
		date1 <- substr(basename(labelInput), 1, 15)
		MeshDir = paste0(labelInput, "\\Predict\\MeshCrop")
		MeshSaveDir = paste0(labelInput, "\\Predict\\MeshCrop_proceed");unlink(MeshSaveDir,recursive=T);dir.create(MeshSaveDir)
	
		MeshLst=list.files(MeshDir, full.names=T)
		if (file.exists(MeshLst[1])==T){
		#source("C:\\Users\\usato\\SSL_DB\\PV_PL\\Modules\\LRG\\FUNCTIONS SEAL 3D PROCESING.r")# upload function to processing
		source("Modules\\LRG\\FUNCTIONS_SEAL_3D_MEASUREMENTS.r")# upload function to processing
		saveCSVPth= paste0(labelInput, "\\Predict\\",date1,"_sealVolume.csv")
		saveKMLpth= paste0(labelInput, "\\Predict\\",date1,"_sealVolume.kml")
		############################################################
		fin=NULL
		for (y in 1:length(MeshLst)){ #
		# y=3
		  slpth=  MeshLst[y]
		  nme=basename(slpth)
		  saveMeshPth=paste0(MeshSaveDir,"\\",nme)
		  flsize= file.size(MeshLst[y])
		  if (flsize < 90000) {next}
		  sl = vcgImport(slpth)
		          if (validate_mesh(sl)==F){next}
		  sl1= mesh_reconstract(sl)
		          if (validate_mesh(sl1)==F){next}
		  sl2 = close_small_holes(sl1)
		          if (validate_mesh(sl2)==F){next}
		  sl3= mesh_reconstract(sl2)
		          if (validate_mesh(sl3)==F){next}
		  sl4=frontalis_slice_filter(sl3)
		          if (validate_mesh(sl4)==F){next}
		  sl5=axial_slice_filter(sl4)
		          if (validate_mesh(sl5)==F){next}
		  sl6=saggital_slice_filter(sl5)
		          if (validate_mesh(sl6)==F){next}
		  pca_mesh=seal_pca(sl6)
		           if (validate_mesh(pca_mesh)==F){next}
		  check=vcgSmooth (pca_mesh,type = "laplace", iteration = 30)
		           if (validate_mesh(check)==F){next}
		  vec= t(check$vb)[,3]
		 altrng=diff(range(vec))
		  altrng
		  if (altrng < 0.21){message("Mesh is  flat   ", y); next}
		  ########################## check and fix reconstract 
		   determine_mesh_orientation(pca_mesh)
		   orientation=determine_mesh_orientation(pca_mesh)
		   iter=0;max_iter=10
		   
		   while(orientation == "midi up" && iter < max_iter) {
			iter <- iter + 1
			 message("Attemt ", iter, ": mesh overturn")
			pca_mesh=seal_pca(sl6)
		   orientation=determine_mesh_orientation(pca_mesh)
		   }  
		   determine_mesh_orientation(pca_mesh)
		##############################
		  
			sl8=frontalis_slice_filter(pca_mesh)
			    if (validate_mesh(sl8)==F){next}
			sl9 = axial_slice_filter(sl8)
			    if (validate_mesh(sl9)==F){next}
			sl10 = saggital_slice_filter(sl9)
			    if (validate_mesh(sl10)==F){next}
			sl11= mesh_reconstract(sl10)
			    if (validate_mesh(sl11)==F){next}
			sl12= mesh_reconstract(sl11)
			     if (validate_mesh(sl12)==F){next}
	################################################ cut 1
			bottom = Get_bottom(sl12, alt_correct=0.017) 
			trimmed_seal = trim_seal_by_ground (seal_mesh=sl12, ground_mesh=bottom, above = T)	
	################################################################################	cut 2
               
			trimmed_seal= mesh_reconstract(trimmed_seal)
			    if (validate_mesh(trimmed_seal)==F){next}
			trimmed_seal = close_small_holes(trimmed_seal)
			    if (validate_mesh(trimmed_seal)==F){next}
			sl13=frontalis_slice_filter(trimmed_seal)
			    if (validate_mesh(sl13)==F){next}
			sl14 = axial_slice_filter(sl13)
			   if (validate_mesh(sl14)==F){next}
			sl15 = saggital_slice_filter(sl14)
			   if (validate_mesh(sl15)==F){next}
			seal_mesh = mesh_reconstract(sl15)
			   if (validate_mesh(seal_mesh)==F){next}
			
			altmsh=median(t(seal_mesh$vb)[,3])
			if (altmsh<0){next}
			
			#seal_mesh = clip_mesh_bottom(sl16,percent=12)
			ground_mesh = Get_bottom(seal_mesh, alt_correct=0)
	            if (validate_mesh(ground_mesh)==F){next}       
            inside <- Rvcg::vcgClost(ground_mesh, seal_mesh, sign = TRUE)$quality > 0
            trimmed_ground_pts <- vert2points(ground_mesh)[inside, ]
            trimmed_ground_mesh <- vcgBallPivoting(trimmed_ground_pts,radius=0.02)
                 if (validate_mesh(trimmed_ground_mesh)==F){next}
			#sl_m=mergeMeshes(seal_mesh,trimmed_ground_mesh)
			sl_m=merg_seal_ground(seal_mesh,trimmed_ground_mesh)
                if (validate_mesh(sl_m)==F){next}
			sl_m1 = mesh_reconstract(sl_m)
			    if (validate_mesh(sl_m1)==F){next}
			sl_m2 = vcgSmooth(sl_m1,type = "laplace", iteration = 300)
			      if (validate_mesh(sl_m2)==F){next}
	        sl_m3=sl_m2
			
			for (i in 1:100){
			sl_m3=mesh_reconstract(sl_m3)
			if(sl_m3==F){break}
			sl_m3=close_small_holes(sl_m3)	  
			if(is_watertight(sl_m3)==T){break}
		     }
		if(is_watertight(sl_m3)==F){next}
		
		
		sl_m4=sl_m3
		scale_x <-  1.22
        scale_y <-  1.22
        scale_z <-  1.33

      # Масштабирование
       sl_m4$vb[1, ] <- sl_m3$vb[1, ] * scale_x  # X
       sl_m4$vb[2, ] <- sl_m3$vb[2, ] * scale_y  # Y
       sl_m4$vb[3, ] <- sl_m3$vb[3, ] * scale_z  # Z

		weightMesh = vcgVolume(sl_m3)
		spine= get_spine(sl_m3)
		
		
		chest = get_chest(sl_m3)
		
		
		
		spine_orig = measurement_to_orig(meshBase=sl3, measurement=spine)
		chest_orig =  measurement_to_orig(meshBase=sl3, measurement=chest)
		
		spine_length <- sum(sqrt(rowSums(diff(spine_orig)^2)))
		chest_length <- sum(sqrt(rowSums(diff(chest_orig)^2)))
		#pl(sl3);points3d(spine_orig,col=2,size=10);points3d(chest_orig,col=3,size=10)
#
	name = basename(slpth)
	name1=strsplit(name,"#")[[1]][2]
	name2=gsub(".ply","",name1)
	East= strsplit(name2,"N")[[1]][1]
	East1 = gsub("E","",East)
	Nord = strsplit(name2,"N")[[1]][2]
    info=data.frame(Nord=Nord,East=East1,weightMesh=weightMesh,spine_length=spine_length,chest_length=chest_length)	
    fin=rbind(info,fin)
   
   vcgPlyWrite(sl_m4,  saveMeshPth)

    message("Done ", y, " mesh")
#	print(fin)
	write.csv(fin,saveCSVPth,row.names=F)
  
	}

################################################################
library(sf)
library(dplyr)

fin =read.csv(saveCSVPth)
fin$weight=round(fin$weightMesh*3000)

fin$spine_length=round(fin$spine_length*100)
fin$chest_length=round(fin$chest_length*100)
fin$spine_chest_volume=paste0("Spine-", fin$spine_length," Chest-",fin$chest_length," Weight-",fin$weight)

# 2. Преобразование в sf-объект (предполагаем EPSG:4326 - WGS84)
seals_sf <- st_as_sf(fin, 
                    coords = c("East", "Nord"),
                    crs = crs)  




# 3. Создание KML с стилизацией
st_write(seals_sf, saveKMLpth, 
         driver = "KML",
         delete_dsn = TRUE,
         dataset_options = c(
        "NameField=spine_chest_volume"
         ))


}







  