
# TODO: 
# Slow and fast stochastic crossing

rm(list=ls())

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)
library(TTR)
library(stringr)

COMPUTE_SRs <- FALSE


pipsize<-0.0001



#df_xts_r<-fread(file="df_xts_r.csv",nrows = (1e3)+(1e5))
df_xts_r<-fread(file="df_xts_r.csv")
MAX_PERIOD<-24 ## For getting support and resistance

#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r),]

dt_xts<-as.xts(df_xts_r)
dt<-to.hourly(dt_xts)   

names(dt)<-c("Open","High","Low","Close")

dt_ind <- as.data.table(dt)

if(COMPUTE_SRs)
  {
#---- WINDOW OF SAMPLES WHICH A PEAK/VALLEY IS SEARCHED FOR -5 TO 5
N_rang <- 10

i<-1
dt_ind[,HIGH_PREV:=0]
dt_ind[,HIGH_NEXT:=0]
dt_ind[,LOW_PREV:=999]
dt_ind[,LOW_NEXT:=999]



while(i<(N_rang+1))
{
  #-- FILL the peaks
  #  dt_ind[,paste0("Lag_",i):=shift(High,i)]
  dt_ind[,TMP:=shift(High,i)]
  dt_ind[,HIGH_PREV:=pmax(HIGH_PREV,TMP)]
  dt_ind[,TMP:=NULL]

  dt_ind[,TMP:=shift(High,i,type="lead")]
  dt_ind[,HIGH_NEXT:=pmax(HIGH_NEXT,TMP)]
  dt_ind[,TMP:=NULL]
#-------------------------------
  
  #-- FILL the lows
  #  dt_ind[,paste0("Lag_",i):=shift(High,i)]
  dt_ind[,TMP:=shift(Low,i)]
  dt_ind[,LOW_PREV:=pmin(LOW_PREV,TMP)]
  dt_ind[,TMP:=NULL]
  
  dt_ind[,TMP:=shift(Low,i,type="lead")]
  dt_ind[,LOW_NEXT:=pmin(LOW_NEXT,TMP)]
  dt_ind[,TMP:=NULL]
  
#----------------------------------
    i<-i+1
}

#-- Mark peaks
dt_ind[High>HIGH_PREV & High>HIGH_NEXT,PEAK:=TRUE][is.na(PEAK),PEAK:=FALSE]
#-- Mark lows
dt_ind[Low<LOW_PREV & Low<LOW_NEXT,VALLEY:=TRUE][is.na(VALLEY),VALLEY:=FALSE]


#-- Score the peaks/Valleys according to how long they are the highes
peaks<-dt_ind[PEAK==T,.(index,High)]
valleys<-dt_ind[VALLEY==T,.(index,Low)]


#-- CREATE PEAK SRs
peaks[,PEAK_STRENGTH:=0]
peaks[,PEAK_STRENGTH_DATE_DIFF:=0]

i<-2L
while(i<nrow(peaks))
{
  val<-min(i-which(peaks[1:(i-1L) ,High]>peaks[i ,High]))
  if(length(val)>0)
  {
    set(peaks,i,"PEAK_STRENGTH",val)
    set(peaks,i,"PEAK_STRENGTH_DATE_DIFF",peaks[i,index]-peaks[max(which(peaks[1:(i-1L) ,High]>peaks[i ,High])),index]   )
    }
    i<-i+1L
}

#--------------

#--- VALLEY SRs

valleys[,VALLEY_STRENGTH:=0]
valleys[,VALLEY_STRENGTH_DATE_DIFF:=0]

i<-2L
while(i<nrow(valleys))
{
  val<-min(i-which(valleys[1:(i-1L) ,Low]<valleys[i ,Low]))
  if(length(val)>0)
  {
    set(valleys,i,"VALLEY_STRENGTH",val)
    set(valleys,i,"VALLEY_STRENGTH_DATE_DIFF",valleys[i,index]-valleys[max(which(valleys[1:(i-1L) ,Low]<valleys[i ,Low])),index]   )
  }
  
  i<-i+1L
}

#------------

#-- Spread the PEAK/VALLEY VALUE OVER ALL THE ROWS; TURNED OUT OT BE USELESS
#spreadVal<-function(x)
#{
#  return(rep(x[length(x)],length(x)))
#}
#dt_ind_SR<-cbind(dt_ind,as.data.table(unlist(period.apply(dt_ind[,High],dt_ind[PEAK==TRUE,which=T],spreadVal))))
#setnames(dt_ind_SR,"V1","PEAK_VAL")
#dt_ind_SR<-cbind(dt_ind_SR,as.data.table(unlist(period.apply(dt_ind_SR[,Low],dt_ind[VALLEY==TRUE,which=T],spreadVal))))
#setnames(dt_ind_SR,"V1","VALLEY_VAL")

dt_ind_SR<-dt_ind
# JOIN THE SR COLUMNS
dt_ind_SR<-merge(dt_ind_SR,peaks[,.(index,PEAK_STRENGTH,PEAK_STRENGTH_DATE_DIFF)],all.x = T,all.y = T)
dt_ind_SR<-merge(dt_ind_SR,valleys[,.(index,VALLEY_STRENGTH,VALLEY_STRENGTH_DATE_DIFF)],all.x = T,all.y = T)

#-- ADD THE SR COLUMNS

i<-as.integer((N_rang/2)+10L)
#-- Look back in hours to find SR
N_LOOK_BACK <- 1e3L
#-- Number of highs and lows
N_SR_count <- 3


#-- Initialize SR columns
j<-1
while(j<(1+N_SR_count))
{
  dt_ind_SR[,paste0("SR_High_",j):=0]
  dt_ind_SR[,paste0("SR_High_",j,"_strength_dt"):=0]

  dt_ind_SR[,paste0("SR_Low_",j):=0]
  dt_ind_SR[,paste0("SR_Low_",j,"_strength_dt"):=0]
  
    
  j<-j+1
}


st_time<-Sys.time()

while(i<(nrow(dt_ind_SR)+1))
{
  
  prevHighs <- dt_ind_SR[max(1L,i-N_LOOK_BACK):(i-(N_rang/2)-1L),][PEAK==TRUE,.(index,PEAK_STRENGTH,PEAK_STRENGTH_DATE_DIFF,High)]

  prevLows <- dt_ind_SR[max(1L,i-N_LOOK_BACK):(i-(N_rang/2)-1L),][VALLEY==TRUE,.(index,VALLEY_STRENGTH,VALLEY_STRENGTH_DATE_DIFF,Low)]

  
#-- Highs
  set(dt_ind_SR,i,"SR_High_1", prevHighs[1,High])
  set(dt_ind_SR,i,"SR_High_1_time", prevHighs[1,index])
  set(dt_ind_SR,i,"SR_High_1_strength", prevHighs[1,PEAK_STRENGTH])
  set(dt_ind_SR,i,"SR_High_1_strength_dt", prevHighs[1,PEAK_STRENGTH_DATE_DIFF])
  
  set(dt_ind_SR,i,"SR_High_2", prevHighs[2,High])
  set(dt_ind_SR,i,"SR_High_2_time", prevHighs[2,index])
  set(dt_ind_SR,i,"SR_High_2_strength", prevHighs[2,PEAK_STRENGTH])
  set(dt_ind_SR,i,"SR_High_2_strength_dt", prevHighs[2,PEAK_STRENGTH_DATE_DIFF])

  set(dt_ind_SR,i,"SR_High_3", prevHighs[3,High])
  set(dt_ind_SR,i,"SR_High_3_time", prevHighs[3,index])
  set(dt_ind_SR,i,"SR_High_3_strength", prevHighs[3,PEAK_STRENGTH])
  set(dt_ind_SR,i,"SR_High_3_strength_dt", prevHighs[3,PEAK_STRENGTH_DATE_DIFF])
  

  #-- Low
  set(dt_ind_SR,i,"SR_Low_1", prevLows[1,Low])
  set(dt_ind_SR,i,"SR_Low_1_time", prevLows[1,index])
  set(dt_ind_SR,i,"SR_Low_1_strength", prevLows[1,VALLEY_STRENGTH])
  set(dt_ind_SR,i,"SR_Low_1_strength_dt", prevLows[1,VALLEY_STRENGTH_DATE_DIFF])
  
  set(dt_ind_SR,i,"SR_Low_2", prevLows[2,Low])
  set(dt_ind_SR,i,"SR_Low_2_time", prevLows[2,index])
  set(dt_ind_SR,i,"SR_Low_2_strength", prevLows[2,VALLEY_STRENGTH])
  set(dt_ind_SR,i,"SR_Low_2_strength_dt", prevLows[2,VALLEY_STRENGTH_DATE_DIFF])
  
  set(dt_ind_SR,i,"SR_Low_3", prevLows[3,Low])
  set(dt_ind_SR,i,"SR_Low_3_time", prevLows[3,index])
  set(dt_ind_SR,i,"SR_Low_3_strength", prevLows[3,VALLEY_STRENGTH])
  set(dt_ind_SR,i,"SR_Low_3_strength_dt", prevLows[3,VALLEY_STRENGTH_DATE_DIFF])
  

  if(i%%5000 == 0)
  {
    print(Sys.time()-st_time)
    st_time<-Sys.time()
    
    print(100*i/nrow(dt_ind_SR))
    }
  i<-i+1L
}

dt_ind_SR[,':='(SR_High_1_dist=Close-SR_High_1,SR_High_2_dist=Close-SR_High_2,SR_High_3_dist=Close-SR_High_3,
                SR_Low_1_dist=Close-SR_Low_1,SR_Low_2_dist=Close-SR_Low_2,SR_Low_3_dist=Close-SR_Low_3)]

dt_ind<-dt_ind_SR[,.(index,Open,High,Low,Close,
                  SR_High_1_dist,SR_High_1_strength,SR_High_1_strength_dt,
                  SR_High_2_dist,SR_High_2_strength,SR_High_2_strength_dt,
                  SR_High_3_dist,SR_High_3_strength,SR_High_3_strength_dt,
                  SR_Low_1_dist,SR_Low_1_strength,SR_Low_1_strength_dt,
                  SR_Low_2_dist,SR_Low_2_strength,SR_Low_2_strength_dt,
                  SR_Low_3_dist,SR_Low_3_strength,SR_Low_3_strength_dt)]

#dt_ind<-dt_ind_SR[,.(index,Open,High,Low,Close,SR_High_1,SR_High_1_strength,SR_High_1_strength,SR_High_1_strength_dt)]

dt_ind$index <- as.character(dt_ind$index)



fwrite(dt_ind,"output/dt_ind_SR.csv")

} else{
  
  dt_ind<-fread("output/dt_ind_SR.csv")
  
}

inds <- endpoints(dt_xts,on="hours")
inds <- inds[2:length(inds)]



#------ Calculating TMS indicator -------

dt_ind[,RSI:=RSI(dt[,c("Close")])]
dt_ind[,TMS_green:=SMA(dt_ind$RSI,n=2)]
dt_ind[,TMS_red:=SMA(dt_ind$RSI,n=7)]

#-- Get crossings
#-- Difference of green and red
#dt_ind[,TMS_diff:=as.numeric(TMS_green-TMS_red)]
#-- Shift it
#dt_ind[,TMS_diff_shift:=as.numeric(shift(TMS_diff))]
#-- get the sign difference
#dt_ind[TMS_diff_shift<0 & TMS_diff>0,TMS_signal:=1]
#dt_ind[TMS_diff_shift>0 & TMS_diff<0,TMS_signal:=-1]
#dt_ind[is.na(TMS_signal),TMS_signal:=0]

#-- Clean up
#dt_ind[,TMS_diff:=NULL]
#dt_ind[,TMS_diff_shift:=NULL]

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


dt_ind$Open<-NULL
dt_ind$High<-NULL
dt_ind$Low<-NULL
dt_ind$Close<-NULL



dt_ind$index <- as.character(dt_ind$index)

fwrite(dt_ind,"output/EURUSD_with_indicators_time.csv")







#---- Junk
#dt_tst <- dt_tst[,.(index,High,PEAK,V1)]




#------------------------------------------------


#-- Plot checks

dt_ind$inds<-seq(1,nrow(dt_ind))
dt_sel<-dt_ind[500:1500,]
library(ggplot2)
ggplot(dt_sel)+geom_point(aes(x=inds,y=Low,color=VALLEY))



inds <- endpoints(dt_xts,on="hours")
inds <- inds[2:length(inds)]


#T_search<-  12*60 # window for finding an SR
#N_high <- 5 # Number of highs to find
#N_low <- 5  # Number of lows to find

i<-1L

df_xts_r[,SR_High_1:=0][,SR_High_2:=0][,SR_High_3:=0][,SR_High_4:=0][,SR_High_5:=0]
df_xts_r[,SR_Low_1:=0][,SR_Low_2:=0][,SR_Low_3:=0][,SR_Low_4:=0][,SR_Low_5:=0]

while(i<(length(inds)-MAX_PERIOD))
{
  
  j<- inds[i]-(T_search/2)-5
  
  highFound<-FALSE
  lowFound<-FALSE
  highCount<-0L
  lowCount<-0L
  
  while(!(highFound & lowFound) & (j-T_search/2)>5)
  {
    #print("EnteredLoop")
    if(  (!highFound) & df_xts_r[j,High]>(-0.00001+max(df_xts_r[(j-T_search/2):(j+T_search/2),High])) )
    {
      #print("EnteredH")
      highCount<-highCount+1L
      set(df_xts_r,inds[i],highCount+5L, df_xts_r[j,High])
      
    }
    if( (!lowFound) & df_xts_r[j,Low]<(0.00001+min(df_xts_r[(j-T_search/2):(j+T_search/2),Low]) ))
    {
      #print("EnteredL")
      lowCount<-lowCount+1L
      set(df_xts_r,inds[i],lowCount+10L, df_xts_r[j,Low])
      
    }
    
    highFound <- highCount==5
    lowFound <- lowCount==5
    
    
    
    j<- j-1
  }
  
  #print((j-T_search/2))
  #print("-----------DONE------")
  
  
  if(i %% 10 ==0)
  {  
    print(paste0(round(100*i/length(inds),1)," %"))
  }
  
  i<-i+1L
  }
  









