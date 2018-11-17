# To check
# 1. PL functionality
# 2. Zigzag future


rm(list = ls())

library(xts)
library(TTR)


get_time_range<-function(i,df)
{
  return(c(t(as.matrix(df[i:min(i+1,nrow(df)),c("Open","High","Low","Close")]))))
  
  
  
}


# Convert the original format to "
# 2005-10-21 18:47:22"
fix_time <- function(Day,Hour)
{
  if(nchar(Hour)<6)
  {
    N_zeros<-6-nchar(Hour)
    
    Hour= paste0(paste(replicate(N_zeros, "0"), collapse = ""),Hour)
    
  }
  
  ret<-paste0(substring(Day,1,4),"-",substring(Day,5,6),"-",substring(Day,7,8)," ",substring(Hour,1,2),":",substring(Hour,3,4),":",substring(Hour,5,6))
  
  return(ret)
}

##----- MACRO parameters

pipsize = 0.0001 # Changes for JPY pairs
SL = 15 # Stop loss in pips
PF = 2 # Profit factor
spread = 1 # Spread in pips


library(readr)
if(FALSE)
{
  print("Reading the file")
  # Read file
  #df<-read.csv(file="FX/data_FX.txt",nrows = 500000)
  df<-read_csv(file="FX/data_FX.txt")
  
  print("Renaming the column names")
  # Rename the ugly column names
  colnames(df)<-c("TICK","Day","Hour","Open","High","Low","Close","Volume")
  
  print("Drop useless columns")
  # Drop useless columns
  df[,"TICK"]<-NULL
  df[,"Volume"]<-NULL
  
  print("Create column with the POSIXct format")
  # Create column with the POSIXct format
  df$Time<-mapply(fix_time,df$Day,df$Hour )
  
  # Original times
  df[,"Day"]<-NULL
  df[,"Hour"]<-NULL
  
  # Reorder
  df<-df[,c("Time","Open","High","Low","Close")]
  
  # Give rownames the time stamps for easier xts conversion
  rownames(df)<-df$Time
  time_column<-df$Time
  df$Time<-NULL
  
  print("Get the POSixct of the time")
  # Get the POSixct of the time
  time_vec<-as.POSIXct(rownames(df),tz="EST")
  
  print("Converting to xts object for easier time handling")
  # Converting to xts object for easier time handling
  df_xts<-as.xts(df)
  
  
  # M15 data
  df_M15 <- to.minutes15(df_xts)
  colnames(df_M15)<-c("Open","High","Low","Close")
  
  # M30 data
  df_M30 <- to.minutes30(df_xts)
  colnames(df_M30)<-c("Open","High","Low","Close")
  
  # H1 data
  df_H1 <- to.hourly(df_xts)
  colnames(df_H1)<-c("Open","High","Low","Close")
  
  # Daily data
  df_D1 <- to.daily(df_xts)
  colnames(df_D1)<-c("Open","High","Low","Close")
  
  # Choose which data you need
  dt<- df_H1
  timeTrade<-dt[,0]
  # Object which carries the indicators
  dt_ind<-as.data.frame(dt)
  
  
  #dt_ind$<-(dt[,c("Open","High","Low","Close")])
  
  print("Appending indicators")
  # Append indicators
  dt_ind$adx<-ADX(dt)$ADX
  dt_ind$aroon<-aroon(dt[,"Close"])$oscillator
  dt_ind$atr<-ATR(dt)$atr
  dt_ind$bbands_up<-dt[,"Close"]-BBands(dt[,"Close"])$up # Relate to Close
  dt_ind$bbands_dn<-dt[,"Close"]-BBands(dt[,"Close"])$dn # Relate to Close
  dt_ind$CCI<-CCI(dt[,c("High","Low","Close")])
  dt_ind$chaikinVolatility<-chaikinVolatility(dt[,c("High","Low")])
  dt_ind$CLV<-CLV(dt[,c("High","Low","Close")])
  dt_ind$DonchianChannel_high<-dt[,"Close"]-DonchianChannel(dt[,c("Close")])$high
  #dt_ind$DonchianChannel_up<-dt[,"Close"]-DonchianChannel(dt[,c("Close")])$up
  dt_ind$DPO<-DPO(dt[,c("Close")])
  dt_ind$DVI_dvi<-DVI(dt[,c("Close")],n=20)$dvi
  dt_ind$DVI_str<-DVI(dt[,c("Close")],n=20)$dvi.str
  dt_ind$DVI_mag<-DVI(dt[,c("Close")],n=20)$dvi.mag
  dt_ind$GMMA<-dt[,"Close"]-GMMA(dt[,c("Close")])[,"short lag 10"]
  dt_ind$KST_kst<-KST(dt[,c("Close")])$kst
  dt_ind$KST_signal<-KST(dt[,c("Close")])$signal
  print("Somewhere in the middle")
  dt_ind$MACD_macd<-MACD(dt[,c("Close")])$macd
  dt_ind$MACD_signal<-MACD(dt[,c("Close")])$signal
  #dt_ind$MFI<-MFI(dt[,c("High","Low","Close")])
  dt_ind$Pbands_up<-dt[,"Close"]-PBands(dt[,"Close"])$up # Relate to Close
  dt_ind$Pbands_dn<-dt[,"Close"]-PBands(dt[,"Close"])$dn # Relate to Close
  dt_ind$ROC<-TTR::ROC(dt[,c("Close")])
  dt_ind$RSI<-RSI(dt[,c("Close")])
  dt_ind$runPercentRank<-runPercentRank(dt)
  dt_ind$runVar<-runVar(dt[,c("Close")])
  dt_ind$SAR<-dt[,"Close"]-SAR(dt[,c("High","Low")])
  dt_ind$SNR<-SNR(dt[,c("High","Low","Close")],n=10)
  dt_ind$stoch_fastK<-stoch(dt[,c("High","Low","Close")])$fastK
  dt_ind$stoch_fastD<-stoch(dt[,c("High","Low","Close")])$fastD
  dt_ind$stoch_slowD<-stoch(dt[,c("High","Low","Close")])$slowD
  dt_ind$stoch_SMI<-stoch(dt[,c("High","Low","Close")])$SMI
  dt_ind$stoch_signal<-stoch(dt[,c("High","Low","Close")])$signal
  dt_ind$TDI_tdi<-TDI(dt[,c("Close")])$tdi
  dt_ind$TDI_di<-TDI(dt[,c("Close")])$di
  dt_ind$TRIX_trix<-TRIX(dt[,c("Close")])$TRIX
  dt_ind$TRIX_signal<-TRIX(dt[,c("Close")])$signal
  dt_ind$ultimateOscillator<-ultimateOscillator(dt[,c("High","Low","Close")])
  dt_ind$VHF<-VHF(dt[,c("High","Low","Close")])
  dt_ind$volatility<-volatility(dt[,c("Open","High","Low","Close")])
  dt_ind$williamsAD<-williamsAD(dt[,c("High","Low","Close")])
  dt_ind$WPR<-WPR(dt[,c("High","Low","Close")])
  dt_ind$ZigZag<-ZigZag(dt[,c("High","Low")])
  dt_ind$EMA_50<-dt[,"Close"]- EMA(dt[,"Close"], n=50)
  dt_ind$EMA_100<-dt[,"Close"]- EMA(dt[,"Close"], n=100)
  dt_ind$EMA_200<-dt[,"Close"]-EMA(dt[,"Close"], n=200)
  dt_ind$Open_dist<-  dt[,"Close"]-dt[,"Open"]
  dt_ind$High_dist<-  dt[,"Close"]-dt[,"High"]
  dt_ind$Low_dist<-  dt[,"Close"]-dt[,"Low"]
  dt_ind$Open<-NULL
  dt_ind$High<-NULL
  dt_ind$Low<-NULL
  dt_ind$Close<-NULL
  
  
  names(dt_ind)
  
  # EMAs 10-100-200 and the same for other crosses
  
  
  # PL labels
  dt$buy_target <- 0
  dt$buy_time <- 0
  dt$sell_target <- 0
  dt$sell_time <- 0
  
  # Getting the minute data in xts back to r for easier access
  df_xts_r <- as.data.frame(df_xts)
  
  
  
  # Save
  # dt df_xts df_xts_r
  dt<-as.data.frame(dt)
  dt$Time<-rownames(dt)
  df_xts_r$Time<-rownames(df_xts_r)
  dt_ind$Time<-rownames(dt_ind)
  
  write.csv(dt,file="dt.csv",row.names = FALSE)
  write.csv(df_xts_r,file="df_xts_r.csv",row.names = FALSE)
  write.csv(dt_ind,file="dt_ind.csv",row.names = FALSE)
  
}
#break

#df_xts<-as.xts(read_csv(file="df_xts.csv"))
dt<-read_csv(file="dt.csv")
df_xts_r<-read_csv(file="df_xts_r.csv")

rownames(dt)<-dt$Time
rownames(df_xts_r)<-df_xts_r$Time

dt$Time<-NULL
df_xts_r$Time<-NULL

#break
i<-1
TP_time<-numeric(nrow(dt))
SL_time<-numeric(nrow(dt))

start_time <- Sys.time()


print("Starting the loop")
#while(i<nrow(dt)+1)
while(i<2000)
{
  # print(i)
  
  position<-as.numeric(dt[i,"Close"])
  # Entry time in the hour chart
  entryTime <- rownames(dt)[i]
  # Index of the entry in the minute chart
  minuteEntryIndex<-match(entryTime,rownames(df_xts_r))
  
  ##-------- Buy label
  
  #TP_time[i] <- which(df_xts_r[minuteEntryIndex:nrow(df_xts_r),"High"]>(position+(SL*PF+spread)*pipsize))[1]
  # Time when SL is hit
  #SL_time[i] <- which(df_xts_r[minuteEntryIndex:nrow(df_xts_r),"Low"]<(position-SL*pipsize))[1]
  # Time when TP is hit
  TP_time[i] <- which(df_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"High"]>(position+(SL*PF+spread)*pipsize))[1]
  # Time when SL is hit
  SL_time[i] <- which(df_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"Low"]<(position-SL*pipsize))[1]
  
  # Check for NA (when the TP or SL is never met) and checking if the TP or SL occured in the immediate following minute
  if(!is.na(TP_time[i]))
  {
    if(TP_time[i]==1)
    {
      #res<-get_time_range(minuteEntryIndex,df_xts_r)
      #plot(res)
      print("Buying: TP has suspisciously occured in the following minute")
      print(paste("Position:",position," TP:",position+(SL*PF+spread)*pipsize," i:",i,"entryTime:",entryTime))
      #i<-1e6
      #break
    }
  }else{
    
    TP_time[i]<-1e9
    
  }
  
  if(!is.na(SL_time[i]))
  {
    if(SL_time[i]==1)
    {
      #res<-get_time_range(i,df_xts_r)
      #plot(res)
      print("Buying: SL has suspisciously occured in the following minute")
      print(paste("Position:",position," SL:",position-SL*pipsize," i:",i,"entryTime:",entryTime))
      #i<-1e6
      #break
    }     
  }  else{
    
    SL_time[i]<-1e9
    
  }
  
  # Fill the Profit or Loss
  if(TP_time[i]<SL_time[i])
  {
    #print("Won smth")
    dt[i,"buy_target"]<- 1
    dt[i,"buy_time"]<- TP_time[i]
    
  }else
  {
    dt[i,"buy_target"]<- -1
    dt[i,"buy_time"]<- -SL_time[i]
    
  }
  
  
  
  
  
  # Sell label
  #TP_time[i] <- which(df_xts_r[minuteEntryIndex:nrow(df_xts_r),"Low"]<(position-(SL*PF+spread)*pipsize))[1]
  # Time when SL is hit
  #SL_time[i] <- which(df_xts_r[minuteEntryIndex:nrow(df_xts_r),"High"]>(position+SL*pipsize))[1]
  
  # Time when TP is hit
  TP_time[i] <- which(df_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"Low"]<(position-(SL*PF+spread)*pipsize))[1]
  # Time when SL is hit
  SL_time[i] <- which(df_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"High"]>(position+SL*pipsize))[1]
  
  
  
  
  # Check for NA (when the TP or SL is never met) and checking if the TP or SL occured in the immediate following minute
  if(!is.na(TP_time[i]))
  {
    if(TP_time[i]==1)
    {
      #res<-get_time_range(minuteEntryIndex,df_xts_r)
      #plot(res)
      print("Selling: TP has suspisciously occured in the following minute")
      print(paste("Position:",position," TP:",position+(SL*PF+spread)*pipsize," i:",i,"entryTime:",entryTime))
      #i<-1e6
      #break
    }
  }else{
    
    TP_time[i]<-1e9
    
  }
  
  if(!is.na(SL_time[i]))
  {
    if(SL_time[i]==1)
    {
      #res<-get_time_range(minuteEntryIndex,df_xts_r)
      #plot(res)
      print("Selling: SL has suspisciously occured in the following minute")
      print(paste("Position:",position," SL:",position-SL*pipsize," i:",i,"entryTime:",entryTime))
      #i<-1e6
      #break
    }     
  }  else{
    
    SL_time[i]<-1e9
    
  }
  
  # Fill the Profit or Loss
  if(TP_time[i]<SL_time[i])
  {
    #print("Won smth")
    dt[i,"sell_target"]<- 1
    dt[i,"sell_time"]<- TP_time[i]
    
  }else
  {
    dt[i,"sell_target"]<- -1
    dt[i,"sell_time"]<- -SL_time[i]
    
  }
  #if(i==10)
  #{
  # break
  #}
  
  
  
  i<-i+1
}

end_time <- Sys.time()

print(end_time - start_time)

if(FALSE)
{
  dt_ind<-read_csv(file="dt_ind.csv")
  rownames(dt_ind)<-dt_ind$Time
  dt_ind$Time<-NULL
  
  dt_int_buy<-cbind(dt_ind,as.vector(dt$buy_target))
  dt_int_sell<-cbind(dt_ind,as.vector(dt$sell_target))
  names(dt_int_buy)[ncol(dt_int_buy)]<-"btarget"
  names(dt_int_sell)[ncol(dt_int_sell)]<-"starget"
  
  
  dt_int_buy$Time<-rownames(dt_int_buy)
  dt_int_sell$Time<-rownames(dt_int_sell)
  
  dt_df<-as.data.frame(dt)
  dt_df$Time<-rownames(dt_df)
  
  
  dt_int_buy$DPO<-NULL
  dt_int_sell$DPO<-NULL
  
  dt_int_buy$ZigZag<-NULL
  dt_int_sell$ZigZag<-NULL
  
  
  #names(dt_int_buy)<-c(names(dt_ind),"btarget")
  #names(dt_int_sell)<-c(names(dt_ind),"starget")
  
  write.csv(dt_int_buy,file="H1_buy.csv",row.names = FALSE)
  write.csv(dt_int_sell,file="H1_sell.csv",row.names = FALSE)
  write.csv(dt_df,file="H1_all_targets.csv",row.names = FALSE)
  
  
  
}









