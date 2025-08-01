options = list(display.mode='showcase')
navbarPage(actionLink('ResetSetting',   label=paste0("INPUT:___", labelInput), style = "font-size:12px;"),
			 
  id="nav", 
 ############################################################################################          
           tabPanel("CIC",
                     fluidPage (
					 theme = shinytheme(Theme),
                 fluidRow(column(width = 12, selectInput('Bath_Process', 'Bath Process', width="1000px",multiple = T,
                                                        c("00_DataStructure"="DataStructure",
														
														  "--------------------------------------------"="B",
														  "01_Unzip"="Unzip",
                                                          "02_KMLprepare"="KMLprepare",
                                                          "03_Image_prepare"="Image_prepare",								  
                                                       
                                                        #  "06_Geo_ref"="Geo_ref",
                                                        #  "07_KML"="KML",
                                                         # "--------------------------------------------"="B",
													
														   "--------------------------------------------"="B1",
														  "04_GET_IMGS_FOR_MODEL"="LRG/GET_IMGS_FOR_MODEL",
														  "05_Agisoft_create_model"="LRG/Agisoft_create_model",
														  "06_Agisoft_export_model"="LRG/Agisoft_export_model",
														  "--------------------------------------------"="B2",
														  "07_Predict_tf2"="UNET/LRG_PREDICT_tf2",
														  "08_PredsToPolygons"="LRG/LRG_PredsToPolygons",
														  "09_LRG_3d_CROP"="LRG/LRG_3d_CROP",
														  "10_LRG_3D_Measurenents"="LRG/LRG_3D_Measurenents",
														  # "--------------------------------------------"="B",

														#  "--------------------------------------------"="B2",
														# "01_Points_table_for_mask" = "Points_table_for_mask",
														#  "02_Animals_Count_On_Image"="Animals_Count_On_Image",
														#  "03_MaskImage create"="Points_create",
														#  "04_MaskCreateFromOutlines" = "MaskCreateFromOutlines",  
														  "--------------------------------------------"="B3"
                                                          ),
														    selected=listTMP$BatchProcessVector
                                                        )),
                          column(width = 12, selectInput('OPPListPred', 'OPP List', width="1000px",multiple = T,
                                                        c("All",listOPP),
							                            selected=listTMP$OPPListPred1)
														)																								
														), 
														#############
														 hr(),
				 fluidRow(column(width = 4,textInput("X", "X:", value="3.221493e-08")),
				          column(width = 4,textInput("Shift_E", "Shift_E:", value="435000"))
				          
				 
				 ),  #0.00000006442986
				  fluidRow(column(width = 4,textInput("Y", "Y:", value="1.751848e-08")),
				            column(width = 4,textInput("Shift_N", "Shift_N:", value="5451000"))
				  
				  ),  #0.00000003503696
														############										   
			     fluidRow(column(width = 11,selectInput('Species', 'Species', width="200px",multiple = F,
                                                         c(
														   "ANTUR"="ANT",
														   "LARGHA"="LRG"),
													          selected=listTMP$Species
                 )),
				 ################## 
				 fluidRow(column(width = 4, actionButton('Start_Batch_process', 'Start', width="200px")),
				          column(width = 4, actionButton('Up', 'Up', width="200px"))
                           ),
                
                ))),
#######################################################################################################
tabPanel("Settings",	 
		hr(), 
         fluidRow(column(width = 12, actionLink('System_data',label= paste0("System_data:  ", System_data), style = "font-size:12px;"))),
         hr(),
		  fluidRow(column(width = 12,actionLink('LRG_pth',label= paste0("'UNET SEGMENTATION LARGHA:  ", LRG_pth), style = "font-size:12px;"))),
		  hr(),
		 fluidRow(column(width = 12,actionLink('LRGH_MSRMNTS',label= paste0("VGG16 LRGH MSRMNTS:  ", LRGH_MSRMNTS), style = "font-size:12px;"))),
		 hr(),
		 checkboxInput("DarkTheme", "DarkTheme", value=DarkTheme),
		  hr(),
	  	   fluidRow(dataTableOutput("text")) 
),
#####################################################################################################

conditionalPanel("false", icon("crosshair"))
 ) 
