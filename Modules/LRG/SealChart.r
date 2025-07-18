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
		E <- 430000          # Easting offset
		N <- 5451000         # Northing offset
		A <- -100            # Altitude offset (not currently used)

		# Define input directory path
		labelInput <- "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"
        
		# Set species and extract date from directory name
		Species <- "LRG"
		date1 <- substr(basename(labelInput), 1, 15)
		MeshDir = paste0(labelInput, "\\Predict\\MeshCrop")
		MeshSaveDir = paste0(labelInput, "\\Predict\\MeshCrop_procesid");unlink(MeshSaveDir,recursive=T);dir.create(MeshSaveDir)
		
		MeshSaveDirAgain = paste0(labelInput, "\\Predict\\MeshAgainCrop_procesid");unlink(MeshSaveDirAgain,recursive=T);dir.create(MeshSaveDirAgain)
		
		MeshLst=list.files(MeshDir, full.names=T)
		source("C:\\Users\\usato\\SSL_DB\\PV_PL\\Modules\\LRG\\FUNCTIONS SEAL 3D PROCESING.r")# upload function to processing
		saveCSVPth= paste0(labelInput, "\\Predict\\",date1,"_sealVolume.csv")
		saveKMLpth= paste0(labelInput, "\\Predict\\",date1,"_sealVolume.kml")
		############################################################
		fin=NULL
		 for (y in 1:length(MeshLst)){
		 #y=1
		  slpth=  MeshLst[y]
		  nme=basename(slpth)
		  saveMeshPth=paste0(MeshSaveDir,"\\",nme)
		  saveMeshAgainPth = paste0(MeshSaveDirAgain,"\\",nme)
		  flsize= file.size(MeshLst[y])
		  if (flsize < 90000) {unlink(slpth)} else {
		  sl = vcgImport(slpth)
		  
		  sl1= mesh_reconstract(sl)
		  sl2 = close_small_holes(sl1)
		  sl3= mesh_reconstract(sl2)
		  
		  sl4=frontalis_slice_filter(sl3)
		  sl5=axial_slice_filter(sl4)
		  sl6=saggital_slice_filter(sl5)
		 
		  pca_mesh=seal_pca(sl6)
		   check=vcgSmooth (pca_mesh,type = "laplace", iteration = 30)
		  vec= t(check$vb)[,3]
		  altrng=diff(range(vec))
		  altrng
		 # if (altrng>0.22){
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
			sl9 = axial_slice_filter(sl8)
			sl10 = saggital_slice_filter(sl9)
			
			 sl11= mesh_reconstract(sl10)
			 sl12= mesh_reconstract(sl11)
	################################################ cut 1
			bottom = Get_bottom(sl12, alt_correct=0.017) 
			trimmed_seal1 = trim_seal_by_surface (seal_mesh=sl12, surface_mesh=bottom, above = T)
			trimmed_seal2 = trim_seal_by_surface (seal_mesh=sl12, surface_mesh=bottom, above = F)
		     # check and fix  
			range1= diff(range(vert2points(trimmed_seal1)[,3]))
			range2= diff(range(vert2points(trimmed_seal2)[,3]))
			 if(range1>range2){trimmed_seal=trimmed_seal1} else{trimmed_seal=trimmed_seal2}
		################################################################################	cut 2

			trimmed_seal= mesh_reconstract(trimmed_seal)
			trimmed_seal = close_small_holes(trimmed_seal)
			
			sl13=frontalis_slice_filter(trimmed_seal)
			sl14 = axial_slice_filter(sl13)
			sl15 = saggital_slice_filter(sl14)
			sl16 = mesh_reconstract(sl15)
			sl17 = clip_mesh_bottom(sl16,percent=12)
 
			
			bottom1 = Get_bottom(sl16, alt_correct=0)
			sl_m=mergeMeshes(sl17,bottom1)
			
			#sl_m=merg_seal_ground(sl17,bottom1)
		

	    	pnts = PointySize(sl_m)
	        voxel = VoxeliSize(pnts)
	        meshagain = voxel_to_mesh(voxel)
	     ################################## 
		 # vpl(voxel)

    alpha_shape <- ashape3d(vert2points(sl_m), alpha =0.04)
	weightMesh=round(volume_ashape3d(alpha_shape)*10000*2,digits=0)
	alpha_shape <- ashape3d(vert2points(meshagain), alpha =0.04)
	weightVoxel=  round(volume_ashape3d(alpha_shape)*10000*2,digits=0)
##############################################get coords
	name = basename(slpth)
	name1=strsplit(name,"#")[[1]][2]
	name2=gsub(".ply","",name1)
	East= strsplit(name2,"N")[[1]][1]
	East1 = gsub("E","",East)
	Nord = strsplit(name2,"N")[[1]][2]
   info=data.frame(Nord=Nord,East=East1,weightMesh=weightMesh,weightVoxel=weightVoxel)	
   fin=rbind(info,fin)
   
   vcgPlyWrite(sl_m, saveMeshPth)
   vcgPlyWrite(meshagain, saveMeshAgainPth)
   
  #  message("Done ", y, " mesh")
	print(fin)
	write.csv(fin,saveCSVPth,row.names=F)
}
	}



################################################################
library(sf)
library(dplyr)

fin =read.csv(saveCSVPth)
fin$weight=paste0(fin$weightMesh,"_",fin$weightVoxel)


# 2. Преобразование в sf-объект (предполагаем EPSG:4326 - WGS84)
seals_sf <- st_as_sf(fin, 
                    coords = c("East", "Nord"),
                    crs = crs)  




# 3. Создание KML с стилизацией
st_write(seals_sf, saveKMLpth, 
         driver = "KML",
         delete_dsn = TRUE,
         dataset_options = c(
        "NameField=weight"
         ))














  