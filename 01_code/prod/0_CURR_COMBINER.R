

rm(list=ls())



library(data.table)
library(stringi)


data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"


curs <- c("EURUSD","USDJPY","GBPUSD","USDCHF","USDCAD","NZDUSD","AUDUSD","XAUUSD")

dataset_all <- data.table()

i<- 1

for (cur in curs)
{
  
  cat(paste0("Reading ",cur,"\n\n"))
  file_list <- list.files(paste0(data_input_dir,"duksacopy/",cur)) 
  file_list <- paste0(paste0(data_input_dir,"duksacopy/",cur,"/"),file_list)
  mylist <- lapply(file_list, fread)
  dataset <- rbindlist( mylist )
  names(dataset)[1] <- "Time"
  dataset <- dataset[,.(Time,Open, High, Low, Close)]
  names(dataset)[2:5] <- paste0(names(dataset)[2:5],"_",cur)

  if(i==1)
  {
    dataset_all <- dataset
    
  }else{
    
    dataset_all <- merge( dataset_all , dataset , all.x=T , all.y =T)
    
  }
  i<-i+1
  
  
}


dataset_all <- dataset_all[!is.na(Close_XAUUSD)&!is.na(Close_NZDUSD)&!is.na(Close_EURUSD)&!is.na(Close_AUDUSD)&!is.na(Close_GBPUSD)&!is.na(Close_USDJPY)&!is.na(Close_USDCHF)&!is.na(Close_USDCAD)]


dataset_all[,Time:=paste0(substr(Time, 7,10 ),"-",substr(Time, 4,5 ),"-",substr(Time, 1,2 )," ",substr(Time, 12,19 ))]

dataset_all<-dataset_all[order(Time)]


fwrite(dataset_all,paste0(data_input_dir,"dt_all_min.csv"))







