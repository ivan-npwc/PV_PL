
 date_time=substr(basename(labelInput),1,15)
 
 from=  file.path(labelInput, paste0(date_time,".kmz"))
if (file.exists(from)==T) {  #stop("Please export OPP first")}
 
  to=  file.path(labelInput,date_time);if(dir.exists(to)==T) {unlink(to,recursive=T);dir.create(to)}
  unzip(from,exdir=to)
  
 # PredictDir=paste0(labelInput,"\\Predict")
 # if (Species !="NFSPup512"){unlink(PredictDir,recursive=T)}
	
 Tpth=file.path(labelInput, paste0(date_time, "_table.csv")); if (file.exists(Tpth)==T){unlink(Tpth)}

}


if (file.exists(from)==F) {  print("NO kmz opp found")}
