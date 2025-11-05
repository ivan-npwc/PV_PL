options = list(display.mode='showcase')
navbarPage(actionLink('ResetSetting',   label=paste0("INPUT:___", labelInput), style = "font-size:12px;"),
			 
  id="nav", 
 ############################################################################################          
           tabPanel("CIC",
                     fluidPage (
					 theme = shinytheme(Theme),
                 fluidRow(column(width = 12, selectInput('Bath_Process', 'Bath Process', width="1000px",multiple = T,
                                                        c("00_DataStructure"="DataStructure",														
                                                        #  "06_Geo_ref"="Geo_ref",
                                                        #  "07_KML"="KML",
														  "01_GET_IMGS_FOR_MODEL"="LRG/GET_IMGS_FOR_MODEL",
														  "02_Agisoft_create_model"="LRG/Agisoft_create_model",
														  "03_Agisoft_create_OPP"="LRG/Agisoft_create_OPP",
														  "04_Agisoft_export_model"="LRG/Agisoft_export_model",
														  "05_Agisoft_export_OPP"="LRG/Agisoft_export_OPP",
														  "06_Agisoft_export_report"="LRG/Agisoft_export_report",
														  "--------------------------------------------"="B2",
														  "06_Unzip"="Unzip",
                                                          "07_KMLprepare"="KMLprepare",
                                                          "08_Image_prepare"="Image_prepare",	
														   "--------------------------------------------"="B3",
														  "09_Predict_tf2"="UNET/LRG_PREDICT_tf2",
														  "10_PredsToPolygons"="LRG/LRG_PredsToPolygons",
														  "11_LRG_3d_CROP"="LRG/LRG_3d_CROP",
														  "12_LRG_3D_Measurenents"="LRG/LRG_3D_Measurenents",
														  
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
																								   
			     fluidRow(column(width = 4,selectInput('Species', 'Species',multiple = F,
                                                         c(
														   "ANTUR"="ANT",
														   "LARGHA"="LRG"),
													          selected=listTMP$Species
                 )),
				 ################## 
				 fluidRow(column(width = 4, actionButton('Start_Batch_process', 'Start', width="200px"), actionButton('Up', 'Up', width="200px"))
				         # column(width = 4, actionButton('Up', 'Up', width="200px"))
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
		 checkboxInput("UseOnlyModelOPP", "UseOnlyModelOPP", value=DarkTheme),
		 hr(),
		 
		 #############
														 hr(),
				 fluidRow(column(width = 4,textInput("X", "X:", value="3.221493e-08")),
				          column(width = 4,textInput("Shift_E", "Shift_E:", value="435000"))
				          
				 
				 ),  
				 
				  fluidRow(column(width = 4,textInput("Y", "Y:", value="1.751848e-08")),
				            column(width = 4,textInput("Shift_N", "Shift_N:", value="5451000"))
				  
				  ),  
	############
	     hr(),
	 checkboxInput("NotUseThermalOPP", "NotUseThermalOPP", value=DarkTheme),
		  hr(),
		 
		 
	  	   fluidRow(dataTableOutput("text")) 
),
#####################################################################################################

conditionalPanel("false", icon("crosshair"))
 ) 
