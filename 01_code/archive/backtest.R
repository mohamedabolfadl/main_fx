

rm(list=ls())

tm<-Sys.time()

library(data.table)
library(xgboost)
library(rJava)
library(plotly)
library(lubridate)

#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"
models_dir <- "03_models/"
mql_path <- "C:/Users/Mohamed Ibrahim/AppData/Roaming/MetaQuotes/Terminal/A6DFBB1B8DE9672D2328FF3445436DEC/MQL4/Files/"
logs_dir <- "05_logs/"
prod_code_dir<- "01_code/prod/"

"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}




currs <- list.files(models_dir)


#-- Read the backtest file
backtest <- fread(data_input_dir+"backtest.csv")

backtest<-backtest[order(Time)]



cols_ord <- c("USDJPY_RSI"   ,    "USDJPY_TMS_green" ,"USDJPY_TMS_red" ,  "GBPUSD_RSI",      
              "GBPUSD_TMS_green" ,"GBPUSD_TMS_red" ,  "USDCHF_RSI"   ,    "USDCHF_TMS_green",
              "USDCHF_TMS_red" ,  "USDCAD_RSI"   ,    "USDCAD_TMS_green", "USDCAD_TMS_red"  ,
              "NZDUSD_RSI"   ,    "NZDUSD_TMS_green", "NZDUSD_TMS_red" ,  "AUDUSD_RSI"      ,
              "AUDUSD_TMS_green" ,"AUDUSD_TMS_red" ,  "XAUUSD_RSI"  ,     "XAUUSD_TMS_green",
              "XAUUSD_TMS_red" ,  "EURUSD_RSI"     ,  "EURUSD_TMS_green" ,"EURUSD_TMS_red"  )




mdls = list()
thresholds = list()


#-- Read the best parameters
bst_param<-fread(paste0(data_output_dir,"best_parameters.csv"))
bst_param[grepl("BUY",instrument),instrument:=paste0(substr(instrument,9,14),"_",substr(instrument,1,3))][grepl("SELL",instrument),instrument:=paste0(substr(instrument,10,15),"_",substr(instrument,1,4))]


i<-1
#-- Read the models
for (curr in currs)
{
  mdls[[i]] <- xgb.load(paste0(models_dir,curr,"/mdl_",curr))
  thresholds[[i]]<-bst_param[instrument==curr,threshold]
  i<-i+1
}


#    currs <- unlist(lapply(currs,gsub,pattern="XAUUSD",replacement="GOLD"))
    
    
    
    dt_feats <- backtest[,..cols_ord]
    mt_feats <-as.matrix(dt_feats)
    
    
    i<-1
    while (i< (length(currs)+1) )
    {
      
      dt_feats[,c(currs[i]):=predict(mdls[[i]],newdata = mt_feats)]
      dec <- predict(mdls[[i]],newdata = mt_feats)>thresholds[[i]]
      dt_feats[,c(paste0(currs[i],"_dec")):=dec]


      i<-i+1
    }

    
    
#-- Join back the time
    
    dec_cols<-names(dt_feats)[grepl("_dec",names(dt_feats))]
dt_all<-    cbind(backtest$Time,dt_feats[,..dec_cols])

setnames(dt_all,"V1","Time")
dt_all[,Time_form:=as.Date(Time,format="%Y.%m.%d %H:%M:%S")]
#-- Add week
dt_all[,Week:=year(Time_form)+"_"+week(Time_form)]

dt_all[,traded := AUDUSD_BUY_dec | AUDUSD_SELL_dec | EURUSD_BUY_dec | EURUSD_SELL_dec | GBPUSD_BUY_dec | GBPUSD_SELL_dec|
                  NZDUSD_BUY_dec | NZDUSD_SELL_dec | USDCAD_BUY_dec | USDCAD_SELL_dec | USDCHF_BUY_dec | USDCHF_SELL_dec|
                  USDJPY_BUY_dec | USDJPY_SELL_dec | XAUUSD_BUY_dec | XAUUSD_SELL_dec]


dt_all[,traded:=as.numeric(traded)]

table(dt_all$traded)


nrow(dt_all[traded>0])/length(unique(dt_all$Week))
#-- Select only lines which are traded    



sel <- dt_all[traded==1]








