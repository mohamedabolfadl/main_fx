
# TODO: 
# Slow and fast stochastic crossing

rm(list=ls())

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)
library(TTR)
library(stringr)

pipsize<-0.0001



#df_xts_r<-fread(file="df_xts_r.csv",nrows = (1e3)+(1e5))
df_xts_r<-fread(file="df_xts_r.csv")

#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r),]

dt<-to.hourly(as.xts(df_xts_r))   
names(dt)<-c("Open","High","Low","Close")

dt_ind <- as.data.table(dt)

#------ Calculating TMS indicator -------

dt_ind[,RSI:=RSI(dt[,c("Close")])]
dt_ind[,TMS_green:=SMA(dt_ind$RSI,n=2)]
dt_ind[,TMS_red:=SMA(dt_ind$RSI,n=7)]

#-- Get crossings
#-- Difference of green and red
dt_ind[,TMS_diff:=as.numeric(TMS_green-TMS_red)]
#-- Shift it
dt_ind[,TMS_diff_shift:=as.numeric(shift(TMS_diff))]
#-- get the sign difference
dt_ind[TMS_diff_shift<0 & TMS_diff>0,TMS_signal:=1]
dt_ind[TMS_diff_shift>0 & TMS_diff<0,TMS_signal:=-1]
dt_ind[is.na(TMS_signal),TMS_signal:=0]

#-- Clean up
dt_ind[,TMS_diff:=NULL]
dt_ind[,TMS_diff_shift:=NULL]

#---------------------------------------

dt_ind[,SMA_3:=dt[,"Close"]- SMA(dt[,"Close"], n=3)]


# Append indicators
dt_ind[,adx:=ADX(dt)$ADX]
dt_ind[,aroon:=aroon(dt[,"Close"])$oscillator]
dt_ind[,atr:=ATR(dt)$atr]
dt_ind[,bbands_up:=dt[,"Close"]-BBands(dt[,"Close"])$up]
dt_ind[,bbands_dn:=dt[,"Close"]-BBands(dt[,"Close"])$dn ]# Relate to Close
dt_ind[,CCI:=CCI(dt[,c("High","Low","Close")])]
dt_ind[,chaikinVolatility:=chaikinVolatility(dt[,c("High","Low")])]
dt_ind[,CLV:=CLV(dt[,c("High","Low","Close")])]
dt_ind[,DonchianChannel_high:=dt[,"Close"]-DonchianChannel(dt[,c("Close")])$high]
#dt_ind[,DonchianChannel_up:=dt[,"Close"]-DonchianChannel(dt[,c("Close")])$up
dt_ind[,DPO:=DPO(dt[,c("Close")])]
dt_ind[,DVI_dvi:=DVI(dt[,c("Close")],n=20)$dvi]
dt_ind[,DVI_str:=DVI(dt[,c("Close")],n=20)$dvi.str]
dt_ind[,DVI_mag:=DVI(dt[,c("Close")],n=20)$dvi.mag]
dt_ind[,GMMA:=dt[,"Close"]-GMMA(dt[,c("Close")])[,"short lag 10"]]
dt_ind[,KST_kst:=KST(dt[,c("Close")])$kst]
dt_ind[,KST_signal:=KST(dt[,c("Close")])$signal]

dt_ind[,MACD_macd:=MACD(dt[,c("Close")])$macd]
dt_ind[,MACD_signal:=MACD(dt[,c("Close")])$signal]
#dt_ind[,MFI:=MFI(dt[,c("High","Low","Close")])
dt_ind[,Pbands_up:=dt[,"Close"]-PBands(dt[,"Close"])$up] # Relate to Close
dt_ind[,Pbands_dn:=dt[,"Close"]-PBands(dt[,"Close"])$dn ]# Relate to Close
dt_ind[,ROC:=TTR::ROC(dt[,c("Close")])]
dt_ind[,RSI:=RSI(dt[,c("Close")])]
dt_ind[,runPercentRank:=runPercentRank(dt)]
dt_ind[,runVar:=runVar(dt[,c("Close")])]
dt_ind[,SAR:=dt[,"Close"]-SAR(dt[,c("High","Low")])]
dt_ind[,SNR:=SNR(dt[,c("High","Low","Close")],n=10)]
dt_ind[,stoch_fastK:=stoch(dt[,c("High","Low","Close")])$fastK]
dt_ind[,stoch_fastD:=stoch(dt[,c("High","Low","Close")])$fastD]
dt_ind[,stoch_slowD:=stoch(dt[,c("High","Low","Close")])$slowD]
dt_ind[,stoch_SMI:=stoch(dt[,c("High","Low","Close")])$SMI]
dt_ind[,stoch_signal:=stoch(dt[,c("High","Low","Close")])$signal]
dt_ind[,TDI_tdi:=TDI(dt[,c("Close")])$tdi]
dt_ind[,TDI_di:=TDI(dt[,c("Close")])$di]
dt_ind[,TRIX_trix:=TRIX(dt[,c("Close")])$TRIX]
dt_ind[,TRIX_signal:=TRIX(dt[,c("Close")])$signal]
dt_ind[,ultimateOscillator:=ultimateOscillator(dt[,c("High","Low","Close")])]
dt_ind[,VHF:=VHF(dt[,c("High","Low","Close")])]
dt_ind[,volatility:=volatility(dt[,c("Open","High","Low","Close")])]
dt_ind[,williamsAD:=williamsAD(dt[,c("High","Low","Close")])]
dt_ind[,WPR:=WPR(dt[,c("High","Low","Close")])]
#dt_ind[,ZigZag:=ZigZag(dt[,c("High","Low")])]

#-- MOVING AVERAGES ---
dt_ind[,EMA_50:=dt[,"Close"]- EMA(dt[,"Close"], n=50)]
dt_ind[,EMA_100:=dt[,"Close"]- EMA(dt[,"Close"], n=100)]
dt_ind[,EMA_200:=dt[,"Close"]-EMA(dt[,"Close"], n=200)]
dt_ind[,EMA_400:=dt[,"Close"]-EMA(dt[,"Close"], n=400)]
dt_ind[,EMA_500:=dt[,"Close"]-EMA(dt[,"Close"], n=500)]
dt_ind[,EMA_800:=dt[,"Close"]-EMA(dt[,"Close"], n=800)]
dt_ind[,EMA_1000:=dt[,"Close"]-EMA(dt[,"Close"], n=1000)]


dt_ind[,SMA_50:=dt[,"Close"]- SMA(dt[,"Close"], n=50)]
dt_ind[,SMA_100:=dt[,"Close"]- SMA(dt[,"Close"], n=100)]
dt_ind[,SMA_200:=dt[,"Close"]-SMA(dt[,"Close"], n=200)]
dt_ind[,SMA_400:=dt[,"Close"]-SMA(dt[,"Close"], n=400)]
dt_ind[,SMA_500:=dt[,"Close"]-SMA(dt[,"Close"], n=500)]
dt_ind[,SMA_800:=dt[,"Close"]-SMA(dt[,"Close"], n=800)]
dt_ind[,SMA_1000:=dt[,"Close"]-SMA(dt[,"Close"], n=1000)]


#-- MOVING AVERAGES ---
MAs<-c(50,100,200,400,800)
EMA_names<-paste0("EMA_",MAs)
SMA_names<-paste0("SMA_",MAs)
i<-1
while(i<(length(EMA_names)+1))
{
  sma_name<-SMA_names[i]
  dt_ind[,eval(quote(sma_name)):=(dt[,"Close"]- SMA(dt[,"Close"], n=MAs[i]))/pipsize]
  ema_name<-EMA_names[i]
  dt_ind[,eval(quote(ema_name)):=(dt[,"Close"]- EMA(dt[,"Close"], n=MAs[i]))/pipsize]
i<-i+1  
}


x<-c(EMA_names,SMA_names)
#-- Crossing of the MA with price
i<-1
while(i<(length(x)+1))
{
  ma_name <- x[i]
  ma_shift_name <- paste0(ma_name,"_shift")
  ma_cross_name <- paste0(ma_name,"_cross")
 dt_ind[,eval(quote(ma_shift_name)):=lapply(.SD,stats::lag,k=1),.SDcols=ma_name]
 dt_ind[,eval(quote(ma_name)):=lapply(.SD,as.numeric),.SDcols=ma_name]
 dt_ind[,eval(quote(ma_shift_name)):=lapply(.SD,as.numeric),.SDcols=ma_shift_name][,TMP:=dt_ind[[ma_name]]*dt_ind[[ma_shift_name]]]
 dt_ind[TMP<0,(ma_cross_name):=1][,TMP:=NULL][,(ma_shift_name):=NULL]
# dt_ind[is.na(dt_ind[[ma_cross_name]]) , .SD:=0,.SDcols=ma_cross_name ]
 #[,(ma_cross_name):=ifelse(is.na( .SD ),0,1),.SDcols=ma_cross_name]
 
i<-i+1
}

cross_cols <- names(dt_ind)[grepl("_cross$",names(dt_ind))]

#-- Crossing of MA with different periods
combs<-as.data.table(t(combn(x,m=2)))

i<-1
while(i<(nrow(combs)+1))
{
  nm1 <- combs[i,V1]
  nm2 <- combs[i,V2]
  
  digit1<-as.numeric(str_extract_all(nm1, "[0-9]+")[[1]])
  digit2<-as.numeric(str_extract_all(nm2, "[0-9]+")[[1]])
  
  if(abs(digit1-digit2)>0)
    {
  cross_name<-paste0(nm1,"_cross_",nm2)
    dt_ind[,TMP_diff:=dt_ind[[nm2]]-dt_ind[[nm1]]][,TMP_DIFF_SHIFTED:=shift(TMP_diff)][,TMP:=TMP_diff*TMP_DIFF_SHIFTED]
    #head(dt_ind[,.(TMP_diff,TMP_DIFF_SHIFTED,TMP)],100)
    if(digit1<digit2)
{
      dt_ind[TMP<0 & TMP_diff>0,eval(quote(cross_name)):= 1]
      dt_ind[TMP<0 & TMP_diff<0,eval(quote(cross_name)):=-1]
    }else{
      dt_ind[TMP<0& TMP_diff>0,eval(quote(cross_name)):=-1]
      dt_ind[TMP<0& TMP_diff<0,eval(quote(cross_name)):= 1]
      
}  
    }
  i<-i+1
}

#-- Clean tmps
dt_ind[,TMP:=NULL][,TMP_DIFF_SHIFTED:=NULL][,TMP_diff:=NULL]

#table(dt_ind$EMA_50_cross_EMA_100)
#dt_ind[,EMA_50_shift:=lag(EMA_50,1)][,EMA_50:=as.numeric(EMA_50)][,EMA_50_shift:=as.numeric(EMA_50_shift)]
#dt_ind[ !is.na(EMA_50) & !is.na(EMA_50_shift)&((EMA_50_shift)<0 & (EMA_50)>0 ) |((EMA_50_shift)>0 & (EMA_50)<0 ) ,EMA_50_cross:=1][is.na(EMA_50_cross),EMA_50_cross:=0][,EMA_50_shift:=NULL]
#dt_ind[,SMA_200_shift:=lag(SMA_200,1)][,SMA_200:=as.numeric(SMA_200)][,SMA_200_shift:=as.numeric(SMA_200_shift)]
#dt_ind[ !is.na(SMA_200) & !is.na(SMA_200_shift)&((SMA_200_shift)<0 & (SMA_200)>0 ) |((SMA_200_shift)>0 & (SMA_200)<0 ) ,SMA_200_cross:=1][is.na(SMA_200_cross),SMA_200_cross:=0][,SMA_200_shift:=NULL]




#--------- OSCILLATOR LOCATIONS -------

#-- ADX
minv<- -9999999999
maxv=40
dt_ind[as.numeric(adx)  < minv,adx_reg:="L"][as.numeric(adx)>maxv,adx_reg:="H"][is.na(adx_reg),adx_reg:="M"]

#(dt_ind$adx)
# >40


#-- Aroon
minv<- -90
maxv<-90
dt_ind[as.numeric(aroon)<minv,aroon_reg:="L"][as.numeric(aroon)>maxv,aroon_reg:="H"][is.na(aroon_reg),aroon_reg:="M"]

#table(dt_ind$aroon_reg)
# >80 <-80

#-- CCI
minv=-200
maxv=200
dt_ind[as.numeric(CCI)<minv,CCI_reg:="L"][as.numeric(CCI)>maxv,CCI_reg:="H"][is.na(CCI_reg),CCI_reg:="M"]

#(dt_ind$CCI_reg)
# >200 <-200

#-- CLV
minv=-0.76
maxv=0.76
dt_ind[as.numeric(CLV)<minv,CLV_reg:="L"][as.numeric(CLV)>maxv,CLV_reg:="H"][is.na(CLV_reg),CLV_reg:="M"]

#(dt_ind$CLV_reg)
# >80 <-80


#-- MACD_macd
minv=-0.27
maxv=0.27
dt_ind[as.numeric(MACD_macd)<minv,MACD_macd_reg:="L"][as.numeric(MACD_macd)>maxv,MACD_macd_reg:="H"][is.na(MACD_macd_reg),MACD_macd_reg:="M"]

#
#(dt_ind$MACD_macd_reg)
#dt_ind[,MACD_macd_reg:=NULL]
# >0.17 <-0.17


#-- MACD_signal
minv=-0.16
maxv=0.16
dt_ind[as.numeric(MACD_signal)<minv,MACD_signal_reg:="L"][as.numeric(MACD_signal)>maxv,MACD_signal_reg:="H"][is.na(MACD_signal_reg),MACD_signal_reg:="M"]


#
#(dt_ind$MACD_signal_reg)
# >0.16 <-0.16

#-- ROC
minv=-0.00145
maxv=0.00145
dt_ind[as.numeric(ROC)<minv,ROC_reg:="L"][as.numeric(ROC)>maxv,ROC_reg:="H"][is.na(ROC_reg),ROC_reg:="M"]

#
#(dt_ind$ROC_reg)
#dt_ind[,ROC_reg:=NULL]

# >0.00125 <-0.00125

#-- RSI
minv=30
maxv=70
dt_ind[as.numeric(RSI)<minv,RSI_reg:="L"][as.numeric(RSI)>maxv,RSI_reg:="H"][is.na(RSI_reg),RSI_reg:="M"]

#
#(dt_ind$RSI_reg)
#dt_ind[,RSI_reg:=NULL]

# >67 <33

#-- SAR
minv=-0.006
maxv=0.006
dt_ind[as.numeric(SAR)<minv,SAR_reg:="L"][as.numeric(SAR)>maxv,SAR_reg:="H"][is.na(SAR_reg),SAR_reg:="M"]

#
#(dt_ind$SAR_reg)
# >0.006 <-0.006

#-- SNR
minv=-9999999999
maxv=3.4
dt_ind[as.numeric(SNR)<minv,SNR_reg:="L"][as.numeric(SNR)>maxv,SNR_reg:="H"][is.na(SNR_reg),SNR_reg:="M"]

#
#(dt_ind$SNR_reg)
# >3.4

#-- stoch_fastK
minv=0.11
maxv=0.89
dt_ind[as.numeric(stoch_fastK)<minv,stoch_fastK_reg:="L"][as.numeric(stoch_fastK)>maxv,stoch_fastK_reg:="H"][is.na(stoch_fastK_reg),stoch_fastK_reg:="M"]

#
#(dt_ind$stoch_fastK_reg)
# >0.89 <0.11

#-- stoch_fastD
minv=0.13
maxv=0.86
dt_ind[as.numeric(stoch_fastD)<minv,stoch_fastD_reg:="L"][as.numeric(stoch_fastD)>maxv,stoch_fastD_reg:="H"][is.na(stoch_fastD_reg),stoch_fastD_reg:="M"]

#
#table(dt_ind$stoch_fastD_reg)
# >0.86 <0.13

#-- stoch_slowD
minv=0.14
maxv=0.85
dt_ind[as.numeric(stoch_slowD)<minv,stoch_slowD_reg:="L"][as.numeric(stoch_slowD)>maxv,stoch_slowD_reg:="H"][is.na(stoch_slowD_reg),stoch_slowD_reg:="M"]

#
#(dt_ind$stoch_slowD_reg)
# >0.85 <0.14

#-- TDI_tdi
minv=-0.15
maxv=0.1
dt_ind[as.numeric(TDI_tdi)<minv,TDI_tdi_reg:="L"][as.numeric(TDI_tdi)>maxv,TDI_tdi_reg:="H"][is.na(TDI_tdi_reg),TDI_tdi_reg:="M"]

#
#(dt_ind$TDI_tdi_reg)
#dt_ind[,TDI_tdi_reg:=NULL]
# >0.4 <-0.4

#-- TDI_td
minv=-0.16
maxv=0.16
dt_ind[as.numeric(TDI_di)<minv,TDI_di_reg:="L"][as.numeric(TDI_di)>maxv,TDI_di_reg:="H"][is.na(TDI_di_reg),TDI_di_reg:="M"]

#
#(dt_ind$TDI_di_reg)
#dt_ind[,TDI_di_reg:=NULL]
# >0.4 <-0.4

#-- TRIX_trix
minv=-0.021
maxv=0.021
dt_ind[as.numeric(TRIX_trix)<minv,TRIX_trix_reg:="L"][as.numeric(TRIX_trix)>maxv,TRIX_trix_reg:="H"][is.na(TRIX_trix_reg),TRIX_trix_reg:="M"]

#
#(dt_ind$TRIX_trix_reg)
# >0.021 <-0.021

#-- TRIX_signal
minv=-0.019
maxv=0.019
dt_ind[as.numeric(TRIX_signal)<minv,TRIX_signal_reg:="L"][as.numeric(TRIX_signal)>maxv,TRIX_signal_reg:="H"][is.na(TRIX_signal_reg),TRIX_signal_reg:="M"]

#
#(dt_ind$TRIX_signal_reg)
# >0.019 <-0.019

#-- ultimateOscillator
minv=37
maxv=61
dt_ind[as.numeric(ultimateOscillator)<minv,ultimateOscillator_reg:="L"][as.numeric(ultimateOscillator)>maxv,ultimateOscillator_reg:="H"][is.na(ultimateOscillator_reg),ultimateOscillator_reg:="M"]

#
#(dt_ind$ultimateOscillator_reg)
# >61 <37


#-- VHF
minv=0.3
maxv=0.6
dt_ind[as.numeric(VHF)<minv,VHF_reg:="L"][as.numeric(VHF)>maxv,VHF_reg:="H"][is.na(VHF_reg),VHF_reg:="M"]

#
#(dt_ind$VHF_reg)
# >0.6 <0.3

#-- VHF
minv=-0.26
maxv=0.41
dt_ind[as.numeric(williamsAD)<minv,williamsAD_reg:="L"][as.numeric(williamsAD)>maxv,williamsAD_reg:="H"][is.na(williamsAD_reg),williamsAD_reg:="M"]


#
#(dt_ind$williamsAD_reg)
# >0.41 <-0,26

#-- WPR
minv=0.1
maxv=0.9
dt_ind[as.numeric(WPR)<minv,WPR_reg:="L"][as.numeric(WPR)>maxv,WPR_reg:="H"][is.na(WPR_reg),WPR_reg:="M"]

#quantile(dt_ind$WPR,c(0.1,.9),na.rm=T)
#table(dt_ind$WPR_reg)
# >0.9 < 0.1




#--- DISTANCE OF CLOSE TO HIGH LOW AND OPEN

dt_ind[,Open_dist:=  dt[,"Close"]-dt[,"Open"]]
dt_ind[,High_dist:=  dt[,"Close"]-dt[,"High"]]
dt_ind[,Low_dist:=  dt[,"Close"]-dt[,"Low"]]
       
       



dt_ind$Open<-NULL
dt_ind$High<-NULL
dt_ind$Low<-NULL
dt_ind$Close<-NULL





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




end_time<-Sys.time()

end_time-start_time




dt_ind$index <- as.character(dt_ind$index)

fwrite(dt_ind,"output/EURUSD_with_indicators.csv")

















