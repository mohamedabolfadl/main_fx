
# TODO: 
# Slow and fast stochastic crossing

rm(list=ls())
set.seed(123)

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)
library(TTR)
library(stringi)

#--- Directory definitoins
data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"


COMPUTE_SRs <- TRUE


N <- 5e6 #-- Number of columns to read

#-- Read the minute data, skip the first few entries
#dt_min<-fread(file="df_xts_r.csv",nrows = (1e3)+N)
df_xts_r<-fread(file=paste0(data_input_dir,"dt_all_min.csv"),nrows = (1e3)+N)

#head(df_xts_r)


#df_xts_r<-fread(file=paste0(data_input_dir,"dt_dukascopy.csv"),nrows = (1e3)+N,colClasses = list(character = c("Time"), numeric = c("Open","High","Low","Close")))
#df_xts_r<-fread(file="df_xts_r.csv")

#-- For getting support and resistance
#MAX_PERIOD<-24 

#-- Removing "T" and "R" from the Time column and converting the time column to Date
#df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r[,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r):nrow(df_xts_r),]


curs <- c("USDJPY","GBPUSD","USDCHF","USDCAD","NZDUSD","AUDUSD","XAUUSD","EURUSD")
i<-1

for (cur in curs)
{
  #-- Select columns of the current curr
  cols <- c("Time",names(df_xts_r)[grepl(cur,names(df_xts_r))])
    dt_sell <- df_xts_r[,..cols]
  
  #-- Convert the dt to xts ovject
  dt_xts<-as.xts(dt_sell)
  dt<-to.hourly(dt_xts)   

  #-- Convert back to datatable
    dt_ind <- as.data.table(dt)
  
    
    close_col <- names(dt_ind)[grepl("Close", names(dt_ind))]
  high_col <- names(dt_ind)[grepl("High", names(dt_ind))]
  low_col <- names(dt_ind)[grepl("Low", names(dt_ind))]
  hlc_cols <- c(high_col,low_col,close_col)
  
  #-- Get the indicators
    #dt_ind[,paste0(cur,"_RSI"):=RSI(dt_ind[,..close_col])]
    #dt_ind[,paste0(cur,"_TMS_green"):=SMA(dt_ind$RSI,n=2)]
    #dt_ind[,paste0(cur,"_TMS_red"):=SMA(dt_ind$RSI,n=7)]
    #dt_ind[,paste0(cur,"_williamsAD"):=williamsAD(dt_ind[,..hlc_cols])]
    #dt_ind[,paste0(cur,"_EMA_50"):=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=50)]
    #dt_ind[,paste0(cur,"_EMA_200"):=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=200)]
    #dt_ind[,paste0(cur,"_EMA_1000"):=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=1000)]
    

    dt_ind[,RSI:=RSI(dt_ind[,..close_col])]
    dt_ind[,TMS_green:=SMA(dt_ind$RSI,n=2)]
    dt_ind[,TMS_red:=SMA(dt_ind$RSI,n=7)]
    dt_ind[,williamsAD:=williamsAD(dt_ind[,..hlc_cols])]
    dt_ind[,EMA_50:=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=50)]
    dt_ind[,EMA_100:=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=100)]
    dt_ind[,EMA_200:=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=200)]
    dt_ind[,EMA_1000:=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=1000)]
    dt_ind[,EMA_2000:=dt_ind[,..close_col]- EMA(dt_ind[,..close_col], n=2000)]

    dt_ind[,EMA_LOW_50:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=50)]
    #dt_ind[,EMA_LOW_100:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=100)]
    dt_ind[,EMA_LOW_200:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=200)]
    #dt_ind[,EMA_LOW_1000:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=1000)]
    dt_ind[,EMA_LOW_2000:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=2000)]
    
    dt_ind[,EMA_HIGH_50:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=50)]
    #dt_ind[,EMA_HIGH_100:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=100)]
    dt_ind[,EMA_HIGH_200:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=200)]
    #dt_ind[,EMA_HIGH_1000:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=1000)]
    dt_ind[,EMA_HIGH_2000:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=2000)]
    
        
    dt_ind[,SMA_50:=dt_ind[,..close_col]- SMA(dt_ind[,..close_col], n=50)]
    dt_ind[,SMA_100:=dt_ind[,..close_col]- SMA(dt_ind[,..close_col], n=100)]
    dt_ind[,SMA_200:=dt_ind[,..close_col]- SMA(dt_ind[,..close_col], n=200)]
    dt_ind[,SMA_1000:=dt_ind[,..close_col]- SMA(dt_ind[,..close_col], n=1000)]
    dt_ind[,SMA_2000:=dt_ind[,..close_col]- SMA(dt_ind[,..close_col], n=2000)]
    
    dt_ind[,adx:=ADX(dt_ind)$ADX]
    dt_ind[,atr:=ATR(dt_ind)$atr]
    #dt_ind[,chaikinVolatility:=chaikinVolatility(dt_ind[,c("High","Low")])]
    dt_ind[,VHF:=VHF(dt_ind[,..hlc_cols])]
    #dt_ind[,Open_dist:=  dt_ind[,"Close"]-dt_ind[,"Open"]]
    #dt_ind[,High_dist:=  dt_ind[,"Close"]-dt_ind[,"High"]]
    #dt_ind[,Low_dist:=  dt_ind[,"Close"]-dt_ind[,"Low"]]
    #dt_ind[,Candle_range:=  dt_ind[,"High"]-dt_ind[,"Low"]]
    
    
    names(dt_ind)<-paste0(cur,"_",gsub("dt_xts\\.","",names(dt_ind)))
    names(dt_ind)[1]<-"index"
        
  #-- Join to the master if(i==1file    
    if(i==1){
      dt_results_all <- data.table(index=dt_ind$index)
      
}
    dt_results_all<-merge(dt_results_all,dt_ind )

    i<-i+1
    }




#-- Bar size and distances
dt_results_all[,BAR_SIZE_USDJPY:=USDJPY_High-USDJPY_Low][,BAR_DIR_USDJPY:=USDJPY_Close-USDJPY_Open][,dist_High_USDJPY:=USDJPY_Close-USDJPY_High][,dist_Low_USDJPY:=USDJPY_Close-USDJPY_Low][,dist_Open_USDJPY:=USDJPY_Close-USDJPY_Open][,delta_Close_USDJPY:=USDJPY_Close-shift(USDJPY_Close)][,delta_Low_USDJPY:=USDJPY_Low-shift(USDJPY_Low)][,delta_High_USDJPY:=USDJPY_High-shift(USDJPY_High)]                      
dt_results_all[,BAR_SIZE_EURUSD:=EURUSD_High-EURUSD_Low][,BAR_DIR_EURUSD:=EURUSD_Close-EURUSD_Open][,dist_High_EURUSD:=EURUSD_Close-EURUSD_High][,dist_Low_EURUSD:=EURUSD_Close-EURUSD_Low][,dist_Open_EURUSD:=USDJPY_Close-EURUSD_Open][,delta_Close_EURUSD:=EURUSD_Close-shift(EURUSD_Close)][,delta_Low_EURUSD:=EURUSD_Low-shift(EURUSD_Low)][,delta_High_EURUSD:=EURUSD_High-shift(EURUSD_High)]                      
dt_results_all[,BAR_SIZE_GBPUSD:=GBPUSD_High-GBPUSD_Low][,BAR_DIR_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,dist_High_GBPUSD:=GBPUSD_Close-GBPUSD_High][,dist_Low_GBPUSD:=GBPUSD_Close-GBPUSD_Low][,dist_Open_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,delta_Close_GBPUSD:=GBPUSD_Close-shift(GBPUSD_Close)][,delta_Low_GBPUSD:=GBPUSD_Low-shift(GBPUSD_Low)][,delta_High_GBPUSD:=GBPUSD_High-shift(GBPUSD_High)]                      
dt_results_all[,BAR_SIZE_AUDUSD:=AUDUSD_High-AUDUSD_Low][,BAR_DIR_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,dist_High_AUDUSD:=AUDUSD_Close-AUDUSD_High][,dist_Low_AUDUSD:=AUDUSD_Close-AUDUSD_Low][,dist_Open_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,delta_Close_GBPUSD:=AUDUSD_Close-shift(AUDUSD_Close)][,delta_Low_AUDUSD:=AUDUSD_Low-shift(AUDUSD_Low)][,delta_High_AUDUSD:=AUDUSD_High-shift(AUDUSD_High)]                      
dt_results_all[,BAR_SIZE_NZDUSD:=NZDUSD_High-NZDUSD_Low][,BAR_DIR_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,dist_High_NZDUSD:=NZDUSD_Close-NZDUSD_High][,dist_Low_NZDUSD:=NZDUSD_Close-NZDUSD_Low][,dist_Open_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,delta_Close_NZDUSD:=NZDUSD_Close-shift(NZDUSD_Close)][,delta_Low_NZDUSD:=NZDUSD_Low-shift(NZDUSD_Low)][,delta_High_NZDUSD:=NZDUSD_High-shift(NZDUSD_High)]                      
dt_results_all[,BAR_SIZE_USDCHF:=USDCHF_High-USDCHF_Low][,BAR_DIR_USDCHF:=USDCHF_Close-USDCHF_Open][,dist_High_USDCHF:=USDCHF_Close-USDCHF_High][,dist_Low_USDCHF:=USDCHF_Close-USDCHF_Low][,dist_Open_USDCHF:=USDCHF_Close-USDCHF_Open][,delta_Close_USDCHF:=USDCHF_Close-shift(USDCHF_Close)][,delta_Low_USDCHF:=USDCHF_Low-shift(USDCHF_Low)][,delta_High_USDCHF:=USDCHF_High-shift(USDCHF_High)]                      
dt_results_all[,BAR_SIZE_USDCAD:=USDCAD_High-USDCAD_Low][,BAR_DIR_USDCAD:=USDCAD_Close-USDCAD_Open][,dist_High_USDCAD:=USDCAD_Close-USDCAD_High][,dist_Low_USDCAD:=USDCAD_Close-USDCAD_Low][,dist_Open_USDCAD:=USDCAD_Close-USDCAD_Open][,delta_Close_USDCAD:=USDCAD_Close-shift(USDCAD_Close)][,delta_Low_USDCAD:=USDCAD_Low-shift(USDCAD_Low)][,delta_High_USDCAD:=USDCAD_High-shift(USDCAD_High)]                      
#-- lags

lags_vec <- c(1,2,4)
inds_to_lag <- c("BAR_SIZE","BAR_DIR","dist_High","dist_Low","dist_Open","RSI")
for (ind in inds_to_lag)
{
  feats_to_lag <- names(dt_results_all)[grepl(ind,names(dt_results_all))]
  for (feat in feats_to_lag)
  {
    for (lag in lags_vec)
    {
      dt_results_all[,paste0(feat,"_",lag):=shift(get(feat),lag)]
      
    }
    
  }
  
  
}


#-- Cut first part
dt_results_all<-dt_results_all[1e3:nrow(dt_results_all),]





dt_results_all$index <- as.character(dt_results_all$index)

fwrite(dt_results_all,paste0(data_intermediate_dir,"dt_with_indicators.csv"))

#----- END ---------------


if(FALSE)
{
#------ Calculating TMS indicator -------

dt_ind[,RSI:=RSI(dt[,c("Close")])]
dt_ind[,TMS_green:=SMA(dt_ind$RSI,n=2)]
dt_ind[,TMS_red:=SMA(dt_ind$RSI,n=7)]

#---------------------------------------



# Append indicators
dt_ind[,adx:=ADX(dt)$ADX]
dt_ind[,atr:=ATR(dt)$atr]
dt_ind[,chaikinVolatility:=chaikinVolatility(dt[,c("High","Low")])]
dt_ind[,RSI:=RSI(dt[,c("Close")])]
dt_ind[,runVar:=runVar(dt[,c("Close")])]
dt_ind[,TDI_tdi:=TDI(dt[,c("Close")])$tdi]
dt_ind[,TDI_di:=TDI(dt[,c("Close")])$di]
dt_ind[,TRIX_trix:=TRIX(dt[,c("Close")])$TRIX]
dt_ind[,TRIX_signal:=TRIX(dt[,c("Close")])$signal]
dt_ind[,VHF:=VHF(dt[,c("High","Low","Close")])]
dt_ind[,williamsAD:=williamsAD(dt[,c("High","Low","Close")])]

#-- MOVING AVERAGES ---

#-- EMA
dt_ind[,EMA_50:=dt[,"Close"]- EMA(dt[,"Close"], n=50)]
dt_ind[,EMA_100:=dt[,"Close"]- EMA(dt[,"Close"], n=100)]
dt_ind[,EMA_200:=dt[,"Close"]-EMA(dt[,"Close"], n=200)]
dt_ind[,EMA_400:=dt[,"Close"]-EMA(dt[,"Close"], n=400)]
dt_ind[,EMA_500:=dt[,"Close"]-EMA(dt[,"Close"], n=500)]
dt_ind[,EMA_800:=dt[,"Close"]-EMA(dt[,"Close"], n=800)]
dt_ind[,EMA_1000:=dt[,"Close"]-EMA(dt[,"Close"], n=1000)]

#-- Highs
dt_ind[,EMA_100_H:=dt[,"High"]- EMA(dt[,"High"], n=100)]
dt_ind[,EMA_200_H:=dt[,"High"]-EMA(dt[,"High"], n=200)]
dt_ind[,EMA_400_H:=dt[,"High"]-EMA(dt[,"High"], n=400)]

#-- Lows
dt_ind[,EMA_100_L:=dt[,"Low"]- EMA(dt[,"Low"], n=100)]
dt_ind[,EMA_200_L:=dt[,"Low"]-EMA(dt[,"Low"], n=200)]
dt_ind[,EMA_400_L:=dt[,"Low"]-EMA(dt[,"Low"], n=400)]


#-- SMA

dt_ind[,SMA_50:=dt[,"Close"]- SMA(dt[,"Close"], n=50)]
dt_ind[,SMA_100:=dt[,"Close"]- SMA(dt[,"Close"], n=100)]
dt_ind[,SMA_200:=dt[,"Close"]-SMA(dt[,"Close"], n=200)]
dt_ind[,SMA_400:=dt[,"Close"]-SMA(dt[,"Close"], n=400)]
dt_ind[,SMA_500:=dt[,"Close"]-SMA(dt[,"Close"], n=500)]
dt_ind[,SMA_800:=dt[,"Close"]-SMA(dt[,"Close"], n=800)]
dt_ind[,SMA_1000:=dt[,"Close"]-SMA(dt[,"Close"], n=1000)]


#--- DISTANCE OF CLOSE TO HIGH LOW AND OPEN

dt_ind[,Open_dist:=  dt[,"Close"]-dt[,"Open"]]
dt_ind[,High_dist:=  dt[,"Close"]-dt[,"High"]]
dt_ind[,Low_dist:=  dt[,"Close"]-dt[,"Low"]]
dt_ind[,Candle_range:=  dt[,"High"]-dt[,"Low"]]



#--- Shifts of williams AD
dt_ind[,williamsAD_1:=shift(williamsAD,1)]
dt_ind[,williamsAD_2:=shift(williamsAD,2)]
dt_ind[,williamsAD_3:=shift(williamsAD,3)]
dt_ind[,williamsAD_4:=shift(williamsAD,4)]

#--- Shifts of adx
dt_ind[,adx_1:=shift(adx,1)]
dt_ind[,adx_2:=shift(adx,2)]
dt_ind[,adx_3:=shift(adx,3)]
dt_ind[,adx_4:=shift(adx,4)]

#--- Shifts of atr
dt_ind[,atr_1:=shift(atr,1)]
dt_ind[,atr_2:=shift(atr,2)]
dt_ind[,atr_3:=shift(atr,3)]
dt_ind[,atr_4:=shift(atr,4)]

#-- Shifts of EMA
dt_ind[,EMA_100_1:=shift(EMA_100,1)]
dt_ind[,EMA_100_2:=shift(EMA_100,2)]

dt_ind[,EMA_200_1:=shift(EMA_200,1)]
dt_ind[,EMA_200_2:=shift(EMA_200,2)]

dt_ind[,EMA_1000_1:=shift(EMA_1000,1)]
dt_ind[,EMA_1000_2:=shift(EMA_1000,2)]


#dt_ind$Open<-NULL
#dt_ind$High<-NULL
#dt_ind$Low<-NULL
#dt_ind$Close<-NULL



dt_ind$index <- as.character(dt_ind$index)

fwrite(dt_ind,paste0(data_intermediate_dir,"dt_with_indicators.csv"))





}