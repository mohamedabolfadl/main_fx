
#-- This script is to combine the indicators and the trade results into files ready for ML


rm(list=ls())
set.seed(123)

library(stringi)
library(data.table)
library(xts)
library(lubridate)

#--- Directory definitoins
data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"



target_to_the_end <- function(data_set,targ_Name)
{
  tmp <-data.frame(data_set[,targ_Name])
  colnames(tmp)<-targ_Name
  data_set[,targ_Name]<-NULL
  dataset_ret <- cbind(data_set, tmp)
  return(dataset_ret)
  
}





fls_to_read<-list.files(data_intermediate_dir)[grepl("_ALL.csv$",list.files(data_intermediate_dir))&grepl("^SL",list.files(data_intermediate_dir))]
#fls_to_read<-list.files(data_intermediate_dir)[grepl("^SL",list.files(data_intermediate_dir))]

#fls_to_read <- "SL_20_PF_1_SPREAD_2_ALL.csv"

print(fls_to_read)

#-- PARAMETERS
pipsize<-0.0001
#SPREAD<- 2*pipsize
#N <- 10e3
LIMIT_TIME_TRADE<- FALSE # Limit the hours into London/US times

#dt_ind <- fread(paste0(data_intermediate_dir,"dt_with_indicators.csv"))
dt_ind <- fread(paste0(data_intermediate_dir,"dt_with_indicators.csv"))

if("index" %in% names(dt_ind))
{
  setnames(dt_ind,"index","Time")
}

#-- Fix the formatting
dt_ind[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)]

for (fl in fls_to_read)
  {
 # fl <- "SL_15_PF_2_SPREAD_2_ALL.csv"
  dt_res <- fread(paste0(data_intermediate_dir,fl))

dt_joined <- merge(dt_res, dt_ind)
  


fwrite(dt_joined,paste0(data_intermediate_dir,"ML_",fl))




}




