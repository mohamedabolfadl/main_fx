

#-- Combine FEAT_SELECT results


set.seed(123)

library(data.table)
library(mlr)
library(ggplot2)


#--- Directoriewa
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"


conf <- fread("config_file.csv")


#-- General input
SL <- conf$SL[1]
PF <- conf$PF[1]
SPREAD <- 2
test_ratio <- conf$test_portion[1]

#instruments <- c("BUY_RES_EURUSD","SELL_RES_GBPUSD","BUY_RES_GBPUSD","SELL_RES_EURUSD",
#                 "SELL_RES_USDJPY","BUY_RES_USDJPY","SELL_RES_AUDUSD","BUY_RES_AUDUSD",
#                 "BUY_RES_USDCAD","SELL_RES_USDCAD","BUY_RES_NZDUSD","SELL_RES_NZDUSD",
#                 "SELL_RES_USDCHF","BUY_RES_USDCHF")

#instruments <- c(                 "SELL_RES_USDJPY","BUY_RES_NZDUSD","BUY_RES_USDCHF")
instruments <- fread("curr_list.csv")
instruments <- instruments$currs


i<-1
for (instrument in instruments)
{
 
  dt<-fread(paste0(data_output_dir,instrument,"/SELECTED_FEATURES_SL_",SL,"_PF_",PF,".csv")) 
  dt$instrument <- instrument
  dt$N_feats <- nrow(dt)
  
  #-- Rename SMA_50 and EMA_50 to SMA_2000 and EMA_2000 respectively
  #dt[grepl("_50",features),features:=gsub("_50","_2000",features)]
  
  
  if(i==1)
  {
    dt_res<-dt
    
  }else{
    
    dt_res <- rbind(dt_res,dt)
  }
  i<-i+1
  
}


fwrite(dt_res,paste0(data_output_dir,"SELECTED_FEATURES_SL_",SL,"_PF_",PF,"_ALL.csv"))



