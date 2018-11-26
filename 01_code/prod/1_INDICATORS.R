# Attach indicators to each hour bin


# TODO: 
# 1. Correlations
# 2. Differences
# 3. Stochastics




rm(list=ls())
set.seed(123)

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)
library(TTR)
library(stringi)
library(roll)
library(lubridate)
library(timeDate)




#--- Directory definitoins
data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"



#--- INPUT PARAMETERS
N_short_term_corr<-24   # past day
N_long_term_corr<- 3240 # Past 6 months

MA_PER_1 <- 50
MA_PER_2 <- 100
MA_PER_3 <- 200
MA_PER_4 <- 1000
MA_PER_5 <- 2000

EMA_OF_HIGH <- F # Should we calculate EMA of the high
EMA_OF_LOW <- F  # Should we calcualte EMA of the LOW


#-- Read the minute data, skip the first few entries
df_xts_r<-fread(file=paste0(data_input_dir,"dt_all_min.csv"))



#-- DATA CHECK:
#   Close USDJPY 29.11.2013 19:59:00 is 102.465


#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r):nrow(df_xts_r),]


#-- REMOVE WEEKEND DATA
# PS: documentation says that 1 is monday, but after checking turned out to be 2
df_xts_r<-df_xts_r[lubridate::wday(Time) %in% c(2,3,4,5,6)]



#-- Crosses to attach indicators
curs <- c("USDJPY","GBPUSD","USDCHF","USDCAD","NZDUSD","AUDUSD","XAUUSD","EURUSD")
i<-1

for (cur in curs)
{
  
  cat(paste0("Reading ",cur,"\n\n"))
  
  
  #cur <- "EURUSD"
  
  #-- Select columns of the current curr
  cols <- c("Time",names(df_xts_r)[grepl(cur,names(df_xts_r))])
    dt_sell <- df_xts_r[,..cols]
  
  #-- Convert the dt to xts ovject
  dt_xts<-as.xts(dt_sell)
  dt<-to.hourly(dt_xts)   

  #-- Convert back to datatable
    dt_ind <- as.data.table(dt)
  
  #-- Get HLC columns    
  close_col <- names(dt_ind)[grepl("Close", names(dt_ind))]
  high_col <- names(dt_ind)[grepl("High", names(dt_ind))]
  low_col <- names(dt_ind)[grepl("Low", names(dt_ind))]
  hlc_cols <- c(high_col,low_col,close_col)
  
  

    #-- Stochastic
    dt_ind[,STO_fast:=TTR::stoch(dt_ind[,dt_xts.Close])[,1]]
    dt_ind[,STO_slow:=TTR::stoch(dt_ind[,dt_xts.Close])[,3]]
    
        #-- RSI
    dt_ind[,RSI:=RSI(dt_ind[,dt_xts.Close])]
    #-- ATR
    dt_ind[,atr:=ATR(dt_ind)$atr]
    #-- TMS
    dt_ind[,TMS_green:=SMA(dt_ind$RSI,n=2)]
    dt_ind[,TMS_red:=SMA(dt_ind$RSI,n=7)]
    #-- EMA
    dt_ind[,EMA_50:=dt_ind[,dt_xts.Close]- EMA(dt_ind[,dt_xts.Close], n=MA_PER_1)]
    dt_ind[,EMA_100:=dt_ind[,dt_xts.Close]- EMA(dt_ind[,dt_xts.Close], n=MA_PER_2)]
    dt_ind[,EMA_200:=dt_ind[,dt_xts.Close]- EMA(dt_ind[,dt_xts.Close], n=MA_PER_3)]
    dt_ind[,EMA_1000:=dt_ind[,dt_xts.Close]- EMA(dt_ind[,dt_xts.Close], n=MA_PER_4)]
    dt_ind[,EMA_2000:=dt_ind[,dt_xts.Close]- EMA(dt_ind[,dt_xts.Close], n=MA_PER_5)]
    #-- SMA   
    dt_ind[,SMA_50:=dt_ind[,dt_xts.Close]- SMA(dt_ind[,dt_xts.Close], n=MA_PER_1)]
    dt_ind[,SMA_100:=dt_ind[,dt_xts.Close]- SMA(dt_ind[,dt_xts.Close], n=MA_PER_2)]
    dt_ind[,SMA_200:=dt_ind[,dt_xts.Close]- SMA(dt_ind[,dt_xts.Close], n=MA_PER_3)]
    dt_ind[,SMA_1000:=dt_ind[,dt_xts.Close]- SMA(dt_ind[,dt_xts.Close], n=MA_PER_4)]
    dt_ind[,SMA_2000:=dt_ind[,dt_xts.Close]- SMA(dt_ind[,dt_xts.Close], n=MA_PER_5)]
    #-- TDI
    dt_ind[,TDI_tdi:=TDI(dt_ind[,dt_xts.Close])[,1]]
    dt_ind[,TDI_di:=TDI(dt_ind[,dt_xts.Close])[,2]]
    #-- Differences
    dt_ind[,DIFF_1_1:=dt_ind[,dt_xts.Close]-shift(dt_ind[,dt_xts.Close],1)]
    dt_ind[,DIFF_2_1:=dt_ind[,dt_xts.Close]-shift(dt_ind[,dt_xts.Close],2)]
    dt_ind[,DIFF_3_1:=dt_ind[,dt_xts.Close]-shift(dt_ind[,dt_xts.Close],3)]
    #-- Second order differences
    dt_ind[,DIFF_1_2:=DIFF_1_1-shift(DIFF_1_1,1)]
    dt_ind[,DIFF_1_3:=DIFF_1_1-shift(DIFF_1_1,2)]
    dt_ind[,DIFF_1_4:=DIFF_1_1-shift(DIFF_1_1,3)]
    
    #--Bar size
    dt_ind[,BAR_SIZE:=dt_xts.High-dt_xts.Low]    
    

    if(EMA_OF_LOW)
      {
    dt_ind[,EMA_LOW_50:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=MA_PER_1)]
    dt_ind[,EMA_LOW_200:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=MA_PER_3)]
    dt_ind[,EMA_LOW_2000:=dt_ind[,..low_col]- EMA(dt_ind[,..low_col], n=MA_PER_5)]
    }
    
    if(EMA_OF_HIGH)
      {
    dt_ind[,EMA_HIGH_50:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=MA_PER_1)]
    dt_ind[,EMA_HIGH_200:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=MA_PER_3)]
    dt_ind[,EMA_HIGH_2000:=dt_ind[,..high_col]- EMA(dt_ind[,..high_col], n=MA_PER_5)]
    }
     
    
    
    names(dt_ind)<-paste0(cur,"_",gsub("dt_xts\\.","",names(dt_ind)))
    names(dt_ind)[1]<-"index"
        
  #-- Join to the master    
    if(i==1){
      dt_results_all <- data.table(index=dt_ind$index)
      
    }
      
      dt_results_all<-merge(dt_results_all,dt_ind)
    

    i<-i+1
}




#--- Correlations
#################################################
#############    CORRELATIONS  ##################
#################################################



parseCormat <- function(mt,signal)
{
  N<-dim(mt)[1]
i<-1
res <- c()
  while(i<N)
    {
    res<-c(res,mt[i,(i+1):N])
    i<-i+1
  }  
return(res)
  }

split.along.dim <- function(a, n)
  setNames(lapply(split(a, arrayInd(seq_along(a), dim(a))[, n]),
                  array, dim = dim(a)[-n], dimnames(a)[-n]),
           dimnames(a)[[n]])


getCorNames <- function(mt,signal,prefix)
{
  
  N<-dim(mt)[1]
  i<-1
  nms <- colnames(mt)
  nms<-unlist(lapply(nms,gsub,pattern=signal,replacement=""))
  col_names <- c()
  while(i<N)
  {
      j<-i+1
      while(j<(N+1))
      {  
        col_names<-c(col_names,paste0(nms[i],"_",nms[j],"_corr"))
     j<-j+1
        }
    i<-i+1
  }  
  return(paste0(col_names,prefix))
}

close_cols_to_correlate <- c(paste0(curs,"_Close"))
diff_cols_to_correlate <- c(paste0(curs,"_DIFF_1_1"))





result_close_cor_short <- roll_cor(as.matrix(dt_results_all[,..close_cols_to_correlate]), width = N_short_term_corr)
dt_close_short_cor<-as.data.table(do.call(rbind, lapply(split.along.dim(result_close_cor_short,3),parseCormat,signal="_Close")))
names(dt_close_short_cor)<-getCorNames(result_close_cor_short[,,1],"_Close","_short")

result_close_cor_long <- roll_cor(as.matrix(dt_results_all[,..close_cols_to_correlate]), width = N_long_term_corr)
dt_close_long_cor<-as.data.table(do.call(rbind, lapply(split.along.dim(result_close_cor_long,3),parseCormat,signal="_Close")))
names(dt_close_long_cor)<-getCorNames(result_close_cor_long[,,1],"_Close","_long")


result_diff_cor_short <- roll_cor(as.matrix(dt_results_all[,..diff_cols_to_correlate]), width = N_short_term_corr)
dt_diff_long_cor<-as.data.table(do.call(rbind, lapply(split.along.dim(result_diff_cor_short,3),parseCormat,signal="_DIFF_1_1")))
names(dt_diff_long_cor)<-getCorNames(result_diff_cor_short[,,1],"_DIFF_1_1","_short")


result_diff_cor_long <- roll_cor(as.matrix(dt_results_all[,..diff_cols_to_correlate]), width = N_long_term_corr)
dt_diff_short_cor<-as.data.table(do.call(rbind, lapply(split.along.dim(result_diff_cor_long,3),parseCormat,signal="_DIFF_1_1")))
names(dt_diff_short_cor)<-getCorNames(result_diff_cor_long[,,1],"_DIFF_1_1","_long")



corr_tables <- cbind(dt_close_short_cor,dt_close_long_cor)
corr_tables <- cbind(corr_tables,dt_diff_long_cor)
corr_tables <- cbind(corr_tables,dt_diff_short_cor)




#-- Join the corr tables
dt_results_all <- cbind(dt_results_all,corr_tables)


################################################################
#############    FEATURE ENGINEERING ###########################
################################################################

#-- Averages
weights <- c(1 ,-1 ,1 ,1 ,-1 ,-1 ,-1 ,1 )/length(curs)


# RSI
rsi_cols <- names(dt_results_all)[grepl("RSI",names(dt_results_all))]
# "USDJPY_RSI" "GBPUSD_RSI" "USDCHF_RSI" "USDCAD_RSI" "NZDUSD_RSI" "AUDUSD_RSI" "XAUUSD_RSI" "EURUSD_RSI"
avg_rsi <-as.matrix(dt_results_all[,..rsi_cols])%*%weights
dt_results_all[,GEN_AVG_RSI:=avg_rsi]

# DIFF_1_1
diff_cols <- names(dt_results_all)[grepl("DIFF_1_1",names(dt_results_all))]
avg_diff <- as.matrix(dt_results_all[,..diff_cols])%*%weights
# "USDJPY_RSI" "GBPUSD_RSI" "USDCHF_RSI" "USDCAD_RSI" "NZDUSD_RSI" "AUDUSD_RSI" "XAUUSD_RSI" "EURUSD_RSI"
dt_results_all[,GEN_AVG_DIFF:=avg_diff]


# EMA
ema_cols <- names(dt_results_all)[grepl("EMA_50",names(dt_results_all))]
avg_ema_50 <- as.matrix(dt_results_all[,..ema_cols])%*%weights
# "USDJPY_RSI" "GBPUSD_RSI" "USDCHF_RSI" "USDCAD_RSI" "NZDUSD_RSI" "AUDUSD_RSI" "XAUUSD_RSI" "EURUSD_RSI"
dt_results_all[,GEN_AVG_ema_50:=avg_ema_50]





#-- Cut first part
dt_results_all<-dt_results_all[1e3:nrow(dt_results_all),]





dt_results_all$index <- as.character(dt_results_all$index)

#-- Must be 102.465
dt_results_all[index=="2013-11-29 19:59:00",USDJPY_Close]


fwrite(dt_results_all,paste0(data_intermediate_dir,"dt_with_indicators.csv"))

#----- END ---------------







