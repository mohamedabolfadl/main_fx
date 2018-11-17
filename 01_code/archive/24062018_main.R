# Calculate P&L using classical for loop

# TODO: Should SPREAD be added twice???

rm(list=ls())

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)
pipsize<-0.0001


SL_vec<-c(7*pipsize)
PF_vec<-c(2)
MAX_PERIOD<-24
SPREAD<- 2*pipsize

N <- 5e6

#-- Read the minute data
dt_min<-fread(file="df_xts_r.csv",nrows = (1e3)+N)

#-- Removing "T" and "R" from the Time column and converting the time column to Date
dt_min[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]

#-- Cutting out the first 1e3 minutes
dt_min<-dt_min[(1+1e3):nrow(dt_min),]

#-- Convert data.table to xts
dt_xts<-as.xts(dt_min)

#-- Hour index
inds<-endpoints(dt_xts,on="hours")
inds <- inds[2:length(inds)]


for(SL in SL_vec)
{
  
  for(PF in PF_vec)
  {

    
        
    print(paste0("SL:",SL/pipsize))
    
    #PF<-TP_fac
    print(paste0("PF:",PF))
    


#-- Results table
dt_results <- data.table(Time=dt_min[inds,Time],buy_profit=integer(length(inds)),buy_loss=integer(length(inds)),sell_profit=integer(length(inds)),sell_loss=integer(length(inds)))



i<-1L
#PF<-2
#SL<-15*pipsize

while(i<(length(inds)-MAX_PERIOD))
{
  
  #--- BUY ---
  #start <- inds[i]
  #end <-inds[i+MAX_PERIOD]
  
#   if(FALSE)
# {  #-- DEBUG
#     i<-2622L
#   start<-103727
#   end<-start+24*60
#   dt_min[start,Time]
# 
#   position <- dt_min[start,Close]
#   profLoc <- dt_min[(start+1):end,.(High)][High>(dt_min[start,Close]+SPREAD+PF*SL),which=T][1]
#   lossLoc <- dt_min[(start+1):end,.(Low)][Low<(dt_min[start,Close]+SPREAD-SL),which=T][1]
# 
#   #-- Prof
#   dt_min[(start+1):end,.(High)][High>(dt_min[start,Close]+SPREAD+PF*SL),which=T][1]
#   
# #-- Loss
#   dt_min[(start+1):end,.(Low)][Low<(dt_min[start,Close]+SPREAD-SL),which=T][1]
#   
#   
#   plot(dt_min[(start+1):end,(High)])
#   lines(dt_min[(start+1):end,(Low)])
#   plot(rep(position+SPREAD-SL,end-start))
#   
#   #-----
#     
# }

  
  #trade<-dt_min[(inds[i]+1):inds[i+MAX_PERIOD],]
    
  set(dt_results,i,2L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(High)][High>(dt_min[inds[i],Close]+SPREAD+PF*SL),which=T][1])
  set(dt_results,i,3L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(Low)][Low<(dt_min[inds[i],Close]+SPREAD-SL),which=T][1])

  
  #dt_results[Time ==dt_min[inds[i],Time] ,buy_profit:=dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(High)][High>(dt_min[inds[i],Close]+SPREAD+PF*SL),which=T][1] ]
  #dt_results[Time ==dt_min[inds[i],Time] ,buy_loss:=dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(Low)][Low<(dt_min[inds[i],Close]+SPREAD-SL),which=T][1] ]
  
  
  
  #--- SELL ---
  
  set(dt_results,i,4L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(Low)][Low<(dt_min[inds[i],Close]-SPREAD-PF*SL),which=T][1])
  set(dt_results,i,5L,dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(High)][High>(dt_min[inds[i],Close]-SPREAD+SL),which=T][1])
  
  #dt_results[Time ==dt_min[inds[i],Time] ,sell_profit:=dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(Low)][Low<(dt_min[inds[i],Close]-SPREAD-PF*SL),which=T][1] ]
  #dt_results[Time ==dt_min[inds[i],Time] ,sell_loss:=dt_min[(inds[i]+1):inds[i+MAX_PERIOD],.(High)][High>(dt_min[inds[i],Close]-SPREAD+SL),which=T][1]]
  
  
  
  if(i %% 1000 ==0)
  {  
  print(paste0(round(100*i/length(inds),1)," %"))
  }
  
  i<-i+1L
}


#-- Fill buy results
dt_results[,BUY_RES:=0]
dt_results[is.na(buy_loss) & !is.na(buy_profit),BUY_RES:=1]
dt_results[!is.na(buy_loss) & !is.na(buy_profit) & (buy_profit<buy_loss),BUY_RES:=1]

#-- Fill sell results
dt_results[,SELL_RES:=0]
dt_results[is.na(sell_loss) & !is.na(sell_profit),SELL_RES:=1]
dt_results[!is.na(sell_loss) & !is.na(sell_profit) & (sell_profit<sell_loss),SELL_RES:=1]
#table(dt_results$BUY_RES)
#table(dt_results$SELL_RES)

#tmp <- dt_results
#tmp$Time <- as.character(dt_results$Time)
#fwrite(tmp,paste0("output_v2/EURUSD_SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv" ))

dt_results$Time <- as.character(dt_results$Time)
fwrite(dt_results,paste0("output_v2/EURUSD_SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv" ))

print("----------------------------------------")

  }
  
  }






