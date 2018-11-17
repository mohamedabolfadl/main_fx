
#-- This script is to combine the indicators and the trade results into files ready for ML


rm(list=ls())



library(data.table)
library(xts)
library(lubridate)


#-- PARAMETERS
LEADING_INDS <- c("DPO","ZigZag")
MAX_PERIOD_vec<-c(24)
pipsize<-0.0001
SPREAD<- 2*pipsize
N <- 10e3
SL_vec<-c(30*pipsize)
TP_vec<-c(1)
LIMIT_TIME_TRADE<- TRUE # Limit the hours into London/US times
PAIR<-"EURUSD"



SL<-SL_vec
MAX_PERIOD<-MAX_PERIOD_vec
TP <- TP_vec
dt_res <- fread(paste0("output_v2/",PAIR,"_SL_",SL/pipsize,"_PF_",TP,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv"))
dt_ind <- fread(paste0("output/",PAIR,"_with_indicators.csv"))
if("index" %in% names(dt_ind))
{
  setnames(dt_ind,"index","Time")
}

#-- Fix the formatting
dt_ind[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)]
dt_res[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)]

#-- BUY file

dt_buy <- dt_res[,.(Time,BUY_RES)]
dt_buy<-merge(dt_ind,dt_buy)

dt_buy<-dt_buy[1e2:nrow(dt_buy),]
dt_buy[,BUY_RES:=as.factor(BUY_RES)]
dt_buy[,Time:=as.POSIXct(Time)][,Time_hour:=lubridate::hour(Time)][,Time_weekday:=wday(Time)][,Time:=NULL][,DPO:=NULL]


if(LIMIT_TIME_TRADE)
{
  dt_buy <- dt_buy[Time_hour<19 & Time_hour>5]
  
}


shuffled_names <- names(dt_buy)
shuffled_names[length(shuffled_names)]<-"BUY_RES"
shuffled_names[length(shuffled_names)-2]<-"Time_weekday"
dt_buy<-setcolorder(dt_buy,shuffled_names)




#dt_buy$Time <- as.character(dt_buy$Time)

fwrite(dt_buy,"output/ML_ready/dt_buy.csv")










