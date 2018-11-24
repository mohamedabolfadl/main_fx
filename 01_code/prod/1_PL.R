# Calculate P&L using classical for loop

# Input:
#   Minute data about OHLC
# 
# Output:
#   For each combination of SL and PL generates an output
# 




rm(list=ls())
set.seed(123)


library(data.table)
library(xts)
library(stringi)

#--- Directory definitoins
data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"


#-- Time the code
start_time<-Sys.time()

#-- Run inputs
curs <- c("USDJPY","GBPUSD","USDCHF","USDCAD","NZDUSD","AUDUSD","XAUUSD","EURUSD")
options(scipen=999)
pipsize<-0.0001
SL_vec<-c(10,20,25)
PF_vec<-c(1)
SPREAD_VAL <- c(3)
MAX_PERIOD<-50
N <- 5e6 #-- Number of columns to read

#-- Read the minute data, skip the first few entries
#dt_min<-fread(file="df_xts_r.csv",nrows = (1e3)+N)
#dt_min<-fread(file=paste0(data_input_dir,"dt_all_min.csv"),nrows = (1e3)+N)
#dt_min<-fread(file=paste0(data_input_dir,"dt_all_min.csv"),nrows = (100801))
dt_min<-fread(file=paste0(data_input_dir,"dt_all_min.csv"))

#,colClasses = list(character = c("Time"), numeric = c("Open","High","Low","Close")))

#-- Removing "T" and "R" from the Time column and converting the time column to Date
#dt_min[,Time:=substr]
dt_min[,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
#dt_min[,Time:=as.Date(Time, format = "%Y-%m-%d %H:%M:%S")]
#-- Cutting out the first 1e3 minutes
dt_min<-dt_min[(1+1e3):nrow(dt_min),]



#-- Convert data.table to xts
dt_xts<-as.xts(dt_min)

#- Check
#head(dt_min[,Time])
#head(dt_xts[,"Open_XAUUSD"])
#print("----------------")
#tail(dt_min[,Time])
#tail(dt_xts[,"Open_XAUUSD"])



#-- Hour index
inds<-endpoints(dt_xts,on="hours")
inds <- inds[2:length(inds)]





#-- Loop over the SL and PF
for(SL_VAL in SL_vec)
{
  #SL_VAL <- 20
  for(PF in PF_vec)
  {
    #PF <-1
    dt_results_all <- data.table(Time=dt_min[inds,Time])
    dt_results_all$Time <- as.character(dt_results_all$Time)
    for (curr in curs)
      {

      #curr <- "USDJPY"
      #curr="EURUSD"
            if(grepl("JPY",curr) | grepl("XAU",curr))
      {
        pipsize <-0.01
      }else{
        pipsize <- 0.0001
      }
      
      if(grepl("USD$",curr))
      {
        bs <- -1
      }else{
        bs <- 1
      }
      
      SL <- SL_VAL*pipsize
      #PF <- 1
      SPREAD<- SPREAD_VAL*pipsize 
      
      
    print(paste0("SL:",SL/pipsize))
    print(paste0("PF:",PF))
    cat(paste0("\n\n#################\n\n",curr,"\n\n"))
    #-- Results table
    dt_results <- data.table(Time=dt_min[inds,Time])
    dt_results[,paste0("buy_profit_",curr):=integer(length(inds))][,paste0("buy_loss_",curr):=integer(length(inds))][,paste0("sell_profit_",curr):=integer(length(inds))][,paste0("sell_loss_",curr):=integer(length(inds))]
    dt_results[,paste0("bs_",curr):=bs]
    i<-1L
    
    while(i<(length(inds)-MAX_PERIOD))
    {
      #--- BUY ---
#      set(dt_results,i,2L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.( get(paste0("High_",curr)) )][ get(paste0("High_",curr)) > (dt_min[inds[i], get(paste0("Close_",curr)) ]+SPREAD+PF*SL),which=T][1])
#      set(dt_results,i,3L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.( get(paste0("Low_",curr)))][get(paste0("Low_",curr))<(dt_min[inds[i],get(paste0("Close_",curr))]+SPREAD-SL),which=T][1])
      set(dt_results,i,2L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.( get(paste0("High_",curr)) )][ V1 > (dt_min[inds[i], get(paste0("Close_",curr)) ]+SPREAD+PF*SL),which=T][1])
      set(dt_results,i,3L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.( get(paste0("Low_",curr)))][V1<(dt_min[inds[i],get(paste0("Close_",curr))]+SPREAD-SL),which=T][1])
      #--- SELL ---
      set(dt_results,i,4L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(get(paste0("Low_",curr)))][V1<(dt_min[inds[i],get(paste0("Close_",curr))]+SPREAD-PF*SL),which=T][1])
      set(dt_results,i,5L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(get(paste0("High_",curr)))][V1>(dt_min[inds[i],get(paste0("Close_",curr))]+SPREAD+SL),which=T][1])
      if(i %% 1000 ==0)
      {  
        cat("\r",paste0(round(100*i/length(inds),1)," %"))
      }
      
      i<-i+1L
    }
    
    
    #-- Fill buy results
    dt_results[,paste0("BUY_RES_",curr):=0]
    dt_results[is.na( get(paste0("buy_loss_",curr)) ) & !is.na(get(paste0("buy_profit_",curr))),paste0("BUY_RES_",curr):=1]
    dt_results[!is.na(get(paste0("buy_loss_",curr))) & !is.na(get(paste0("buy_profit_",curr))) & get(paste0("buy_profit_",curr))<get(paste0("buy_loss_",curr)),paste0("BUY_RES_",curr):=1]
    
    #-- Fill sell results
    dt_results[,paste0("SELL_RES_",curr):=0]
    dt_results[is.na(get(paste0("sell_loss_",curr))) & !is.na(get(paste0("sell_profit_",curr))),paste0("SELL_RES_",curr):=1]
    dt_results[!is.na(get(paste0("sell_loss_",curr))) & !is.na(get(paste0("sell_profit_",curr))) & get(paste0("sell_profit_",curr))<get(paste0("sell_loss_",curr)),paste0("SELL_RES_",curr):=1]
    
    #-- Convert time to string so that the fwrite change of time is overrided
    dt_results$Time <- as.character(dt_results$Time)
    fwrite(dt_results,paste0(data_intermediate_dir,"SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,"_",curr,".csv" ))
    dt_results_all<-merge(dt_results_all,dt_results)
    print("----------------------------------------")
    
    }
    
    fwrite(dt_results_all,paste0(data_intermediate_dir,"SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,"_ALL.csv" ))
    
      }
  
}






