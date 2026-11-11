## Pan to OPP for 3D lrg measurment

setwd("C:\\Users\\usato\\SSL_DB\\PV_PL")
source("Modules\\LRG\\Agisoft_create_model.r")
source("Modules\\LRG\\Agisoft_create_OPP.r")




dirPan = "S:\\Marine Mammal Research Station\\Pan"
OPPdir = "S:\\Marine Mammal Research Station\\OPP"

listimgs=list.files(dirPan, full.names=T, recursive=T)
listPanImgs=data.frame(listimgs=listimgs)
listPanImgs$dirname=dirname(listPanImgs$listimgs)
listPan=data.frame(Pandir=unique(listPanImgs$dirname))

print(listPan)
length(listPan$Pandir)

for (i in 1:10) {
#today pattern nedded for future 
Pan = listPan$Pandir[i]
bsnmPan = basename(Pan)
OPPdiroutput = file.path(paste0(OPPdir,"\\",bsnmPan))
dir.create(OPPdiroutput)
OPPname = file.path(paste0(OPPdiroutput,"\\",bsnmPan,".psx"))

build_model(image_folder=Pan, output_path=OPPname, crs = "EPSG:4326")
create_opp(psx_path=OPPname, crs="EPSG:4326")




}
