 function(input, output, session) {
############################################################### 
  re = observeEvent(input$ResetSetting, {
   # nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-choose.dir(default = pthOPP)
	UseOnlyModelOPP<<-input$UseOnlyModelOPP
	NotUseThermalOPP  <<- input$NotUseThermalOPP
	if (UseOnlyModelOPP) {pattern="Model.psx"} else {pattern=".psx"}
    listOPP<-list.files(pthOPP,recursive=T, pattern = pattern, full.names=T)
	index = grepl("THERMAL", listOPP, ignore.case = TRUE)  
    
	thermalOPP= listOPP[index]
    listOPP = listOPP[!index]
	
    source("Modules/ListUniqueUpdate.r")
	NanmlsOnTiles<<-input$NanmlsOnTiles
    updateActionButton (session,'ResetSetting',   label=paste0("INPUT:___", pthOPP))
	message("The list of OPP has been updated", appendLF=TRUE)
	
   # pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
   # if (file.exists(pth_log)==F) {log1=NULL } else {log1<<-read.csv(pth_log)}
    })
########################################################################################### 
 observeEvent(input$Up, {
    Species <<-input$Species
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
  source("Modules/ListTMPUpdate.r")
  
 })
######################################################################################
  observeEvent(input$SPTD, {
    BatchIntens<<-input$BatchIntens
    Split<<-input$Split	       
    Smooth<<-input$Smooth       
    batch_size<<-input$batch_size
    epochs<<-input$epochs
    Deformation<<-input$Deformation
        source("Modules/ListUniqueUpdate.r")
    
  })
  ############################################################################### 
  observeEvent(input$Start_Batch_process, {
   # NanmlsOnTiles<<-  input$NanmlsOnTiles
   # SpeciesManager<<-input$SpeciesManager
   # ProsessManager<<-input$ProcessManager
   Shift_E <<- input$Shift_E
   Shift_N <<- input$Shift_N

    listOPP1<<-input$OPPListMngr
    Species <<-input$Species
    BatchProcessVector<<-input$Bath_Process
  	OPPListPred1<<-input$OPPListPred
  	NanmlsOnTiles<<-input$NanmlsOnTiles
	Terrain<<-input$Terrain
	Count_type<<-input$Count_type
	UseAllHauloutImages  <<- input$UseAllHauloutImages
	loopCheck<<-0
	HaulAsModel<<-input$HaulAsModel
	UseRookeryOnly<<-input$UseRookeryOnly
	#SubFold<<-input$SubFold
	UseEffort<<-input$UseEffort
	########################################################################
	 rep2 <<- NULL
   if (OPPListPred1=="All") {OPPListPred1 <<- listOPP}
   source("Modules/ListTMPUpdate.r")
   # pth <<- paste0(pthOPP,"\\log.csv")
  #  lg=data.frame(OPPListPred1=OPPListPred1,Process="Start",dtime=paste0(date())); write.csv(lg,pth,row.names = F)   
#############  	   
	while (OPPListPred1 != "") {
       k <<- length(OPPListPred1)
	   psx1 <<- OPPListPred1[1]
	   
	   labelInput <<- gsub(basename(psx1),"", psx1)
	   #  lg = read.csv(pth)
        withProgress(message = paste0("Doing  ",labelInput), value = k , {    # for all opp
   for (g in 1:length(BatchProcessVector)) {
               withProgress(message = paste0(BatchProcessVector[g]), value = g, {     # 
               action <<- paste0("Modules/", BatchProcessVector[g],".r")
			   source(action)       
      Sys.sleep(1) 
     	     print(paste0("Done  ",action,"   ",labelInput ))
			 
              # strt <<- data.frame(OPPListPred1=psx, Process=BatchProcessVector[g], dtime=paste0(date()))
              # lg = rbind(lg,strt)
             # write.csv(lg,pth,row.names = F)                
   }) 
   }
   OPPListPred1<<-OPPListPred1[!(OPPListPred1 %in% psx1)]
   message(paste0("Done  ",labelInput,"----",Sys.time() ),appendLF=TRUE)
   source("Modules/ListTMPUpdate.r")  
   })
   }

   beep()
   })
  #####################################################################################
  re21 = observeEvent( input$SQLite_path, {
    SQLite_path<<-file.choose()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'SQLite_path',   label= paste0("SQLite_path:  ",SQLite_path))
  }) 
  #######################################################################################  
       re25 = observeEvent( input$System_data, {
    System_data<<-choose.dir()
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'System_data',   label= paste0("System_data:  ",System_data))
  }) 
  ####################################################################################### 
       re26 = observeEvent( input$LRGH_MSRMNTS, {
    LRGH_MSRMNTS<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'LRGH_MSRMNTS',   label= paste0("LRGH_MSRMNTS:  ",LRGH_MSRMNTS))
  }) 
#################################
    re27 = observeEvent( input$LRG_pth, {
    LRG_pth<<-basename(file.choose())
    source("Modules/ListUniqueUpdate.r")
    updateActionButton (session,'LRG_pth',   label= paste0("LRG_pth:  ",LRG_pth))
  }) 
#################################
re28 = observeEvent(input$DarkTheme, {
DarkTheme<<-input$DarkTheme
    source("Modules/ListUniqueUpdate.r")
  }) 
#########################
re29 = observeEvent(input$type, {
type<<-input$type
    source("Modules/ListUniqueUpdate.r")
  }) 
############################################################################################  
  }
  
