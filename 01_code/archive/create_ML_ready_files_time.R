
#-- This script is to combine the indicators and the trade results into files ready for ML


rm(list=ls())



library(data.table)
library(xts)
library(lubridate)



target_to_the_end <- function(data_set,targ_Name)
{
  tmp <-data.frame(data_set[,targ_Name])
  colnames(tmp)<-targ_Name
  data_set[,targ_Name]<-NULL
  dataset_ret <- cbind(data_set, tmp)
  return(dataset_ret)
  
}



#-- PARAMETERS
LEADING_INDS <- c("DPO","ZigZag")
MAX_PERIOD_vec<-c(24)
pipsize<-0.0001
SPREAD<- 2*pipsize
N <- 10e3
SL_vec<-c(15*pipsize)
TP_vec<-c(2)
LIMIT_TIME_TRADE<- FALSE # Limit the hours into London/US times
PAIR<-"EURUSD"



SL<-SL_vec
MAX_PERIOD<-MAX_PERIOD_vec
TP <- TP_vec
dt_res <- fread(paste0("output_v2/",PAIR,"_SL_",SL/pipsize,"_PF_",TP,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv"))
dt_ind <- fread(paste0("output/",PAIR,"_with_indicators_time.csv"))
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
dt_buy[,Time:=as.POSIXct(Time)][,Time_hour:=lubridate::hour(Time)][,Time_weekday:=wday(Time)][,Time:=NULL][,DPO:=NULL]


if(LIMIT_TIME_TRADE)
{
  dt_buy <- dt_buy[Time_hour<19 & Time_hour>5]
  
}


shuffled_names <- names(dt_buy)
shuffled_names[length(shuffled_names)]<-"BUY_RES"
shuffled_names[length(shuffled_names)-2]<-"Time_weekday"
dt_buy<-setcolorder(dt_buy,shuffled_names)


#-- LAG results
#i<-25
#N_wind<-48
#dt_buy[,BUY_RES_WINS:=0]

#while(i<(N_wind+1))
#{
#  dt_buy[,paste0("BUY_RES_",i):=as.numeric(shift(BUY_RES,i))]
#  coln <- paste0("BUY_RES_",i)
# dt_buy[,BUY_RES_WINS:=BUY_RES_WINS + .SD,.SDcols = coln]
# dt_buy[,BUY_RES_WINS:=lapply(.SD,sum,na.rm = T ),.SDcols = coln]
 
#  i<-i+1
#}




#dt_buy[,BUY_RES:=as.factor(BUY_RES)]
dt_buy<-as.data.table(target_to_the_end(as.data.frame(dt_buy),"BUY_RES"))





fwrite(dt_buy,"output/ML_ready/dt_buy_time.csv")










