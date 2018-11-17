library(data.table)
library(xts)
library(TTR)

# Convert the original format to "
# 2005-10-21 18:47:22"
fix_time <- function(Day,Hour)
{
  if(nchar(Hour)<6)
  {
    N_zeros<-6-nchar(Hour)
    
    Hour= paste0(paste(replicate(N_zeros, "0"), collapse = ""),Hour)
    
  }
  
  ret<-paste0(substring(Day,1,4),"-",substring(Day,6,7),"-",substring(Day,9,10)," ",substring(Hour,1,2),":",substring(Hour,3,4),":",substring(Hour,5,6))
  
  return(ret)
}


#-- Combining all the years ----
i<-2000
dt<-fread(paste0("data/HISTDATA_COM_MT_EURUSD_M1",i,"/DAT_MT_EURUSD_M1_",i,".csv"))
colnames(dt) <- c("Day","Hour","Open","High","Low","Close","Volume")
dt[,Volume:=NULL]

i<-i+1
while(i<2002)
{
  dt_c<-fread(paste0("data/HISTDATA_COM_MT_EURUSD_M1",i,"/DAT_MT_EURUSD_M1_",i,".csv"))
  colnames(dt_c) <- c("Day","Hour","Open","High","Low","Close","Volume")
  dt_c[,Volume:=NULL]
  dt<-rbind(dt,dt_c)
  i<-i+1
}
fwrite(dt,"data/FULL_EURUSD.csv")

#-- Formatting time ----

#dt$Time<-mapply(fix_time,dt$Day,dt$Hour )
dt[,Time:=paste0(gsub("\\.","-",Day)," ",Hour)]

# Remove old times
dt[,Day:=NULL]
dt[,Hour:=NULL]
# Reorder
setcolorder(dt, c("Time","Open","High","Low","Close"))

# Concert time to POSIXct

dt[,Time:=as.POSIXct(Time)]


print("Converting to xts object for easier time handling")
# Converting to xts object for easier time handling
dt_xts<-as.xts.data.table(dt)
# Strore original datatable because dt will then have indicators attached
dt_orig<-dt
fwrite(dt_orig,"variables/dt_orig.csv")
#-- Choosing the correct time periodicity-----------

# M15 data
dt_M15 <- to.minutes15(dt_xts)
colnames(dt_M15)<-c("Open","High","Low","Close")

# M30 data
dt_M30 <- to.minutes30(dt_xts)
colnames(dt_M30)<-c("Open","High","Low","Close")

# H1 data
dt_H1 <- to.hourly(dt_xts)
colnames(dt_H1)<-c("Open","High","Low","Close")

# Daily data
dt_D1 <- to.daily(dt_xts)
colnames(dt_D1)<-c("Open","High","Low","Close")

# Choose which data you need
dt<- dt_H1
timeTrade<-dt[,0]
# Object which carries the indicators
dt_ind<-as.data.table(dt)




#------------- Attaching Indicators -----------------
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


#----- dt INITIALIZATION ------------
# PL labels
dt$buy_target <- 0
dt$buy_time <- 0
dt$sell_target <- 0
dt$sell_time <- 0


# Getting the minute data in xts back to r for easier access
dt_xts_r <- as.data.table(dt_xts)



# Save
# dt df_xts df_xts_r
dt<-as.data.table(dt)
setnames(dt,"index","Time")
setnames(dt_xts_r,"index","Time")
setnames(dt_ind,"index","Time")


fwrite(dt,file="variables/dt.csv",row.names = FALSE)
fwrite(dt_xts_r,file="variables/df_xts_r.csv",row.names = FALSE)
fwrite(dt_ind,file="variables/dt_ind.csv",row.names = FALSE)



