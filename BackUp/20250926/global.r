if (!require("Rcpp")) {install.packages("Rcpp"); library("Rcpp")}
if (!require("DT")) {install.packages("DT"); library("DT")}
if (!require("keras")) {install.packages("keras"); library("keras")}
if (!require("XML")) {install.packages("XML"); library("XML")}
if (!require("magick")) {install.packages("magick"); library("magick")}
if (!require("filesstrings")) {install.packages("filesstrings"); library("filesstrings")}
if (!require("abind")) {install.packages("abind"); library("abind")}
if (!require("reticulate")) {install.packages("reticulate"); library("reticulate")}
if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
if (!require("tensorflow")) {install.packages("tensorflow"); library("tensorflow")}
if (!require("tfdatasets")) {install.packages("tfdatasets"); library("tfdatasets")}
if (!require("purrr")) {install.packages("purrr"); library("purrr")}
if (!require("sp")) {install.packages("sp"); library("sp")}
if (!require("rgdal")) {install.packages("rgdal"); library("rgdal")}
if (!require("geosphere")) {install.packages("geosphere"); library("geosphere")}
if (!require("dismo")) {install.packages("dismo"); library("dismo")}
if (!require("rgeos")) {install.packages("rgeos"); library("rgeos")}
if (!require("kohonen")) {install.packages("kohonen"); library("kohonen")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("beepr")) {install.packages("beepr"); library("beepr")}
#if (!require("tcltk")) {install.packages("tcltk"); library("tcltk")}
#if (!require("sf")) {install.packages("sf"); library("sf")}
if (!require("spatialEco")) {install.packages("spatialEco");library("spatialEco")}
#if (!require("encryptr")) {install.packages("encryptr");library("encryptr")}
if (!require("RSQLite")) {install.packages("RSQLite")}
#if (!require("sparklyr")) {install.packages("sparklyr");library(sparklyr);spark_install(version = "2.1.0")}
if (!require("writexl")) {install.packages("writexl")}
if (!require("shinythemes")) {install.packages("shinythemes"); library("shinythemes")}
#install.packages(c("pkg1", "pkg2"))
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#  BiocManager::install("EBImage")
########################################
#library(reticulate)
#use_condaenv("base",required = TRUE)
#py_config() 
#library(tensorflow)
#tf$config$list_physical_devices('GPU')
#########################################################
listValue <<- readRDS("listUniq")
listTMP <<-readRDS("listTMP")
######################
labelInput<<-listValue$labelInput

    nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-substr(labelInput,0, nchar(labelInput)-nchaBName)
	site   <<-  strsplit(basename(pthOPP),"_")[[1]][2]
    listOPP  <<-listValue$listOPP
    LRG_pth<<-listValue$LRG_pth
    LRGH_MSRMNTS  <<-listValue$LRGH_MSRMNTS
    SQLite_path<<-  listValue$SQLite_path
    DarkTheme<<-listValue$DarkTheme; if (DarkTheme==T){Theme<<-"slate"} else {Theme<<-"lumen"}
    type<<-listValue$type
    System_data<<-listValue$System_data


listR_year=c("2016","2017","2018", "2019","2020","2021")
listR_site=                                                                             c("19",#severnoe
			                                                                              "20",#severo zapadnoe
																						  "30",#yugo vostochny
																						  "37",#kozlova
																						  "44",#kekurny
																						  "50",#sivuchy (SofOKH)
																						  "53",#vladimira
																						  "56",#anziferova
																						  "71",#lovushky
																						  "76",#raykoke
																						  "82",#srednego
																						  "95",#brat chirpoev
																						  "117",#moneron
																						  "118",#kuznezova
																						  "120",#opasnosty
																						  "138")# tuleny

#################################################################
pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
if (file.exists(pth_log)==F) {
  log1=NULL } else { log1<<-read.csv(pth_log) 
    }
########################################################################
lastBackUp=unique(listTMP$backUpDate)
if (lastBackUp != format(Sys.Date(),"%Y%m%d")) {
  from= "Modules"
  from1<<-list.files(from,full.names=T,recursive=T)
  to<<-paste0("BackUp\\",format(Sys.Date(),"%Y%m%d"))
 if (dir.exists(to)==F){dir.create(to)}
  file.copy(from1,to)
  file.copy("global.r",to)
  file.copy("server.r",to)
  file.copy("ui.r",to)
  
}
#####################################################
