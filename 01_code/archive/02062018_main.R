
# TODO: Check the timing using dummy data

rm(list=ls())

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)

#MAX_PERIOD_vec<-c(12,24,48)
#pipsize<-0.0001
#SPREAD<- 2*pipsize
#N <- 5e6
#SL_vec<-c(10*pipsize,15*pipsize,20*pipsize)
#TP_vec<-c(1,2,3)

MAX_PERIOD_vec<-c(48)
pipsize<-0.0001
SPREAD<- 2*pipsize
N <- 5e6
SL_vec<-c(20*pipsize)
TP_vec<-c(3)


df_xts_r<-fread(file="formatted/df_xts_r.csv",nrows = (1e3)+N)
#df_xts_r<-fread(file="df_xts_r.csv")

#-- Removing "T" and "R" from the Time column and converting the time column to Date
#df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
#df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)]
df_xts_r[,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]


#fwrite(df_xts_r,"formatted/df_xts_r.csv")
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r),]

df_xts<-as.xts(df_xts_r)

dt_xts_Low<-as.xts(df_xts_r[,.(Time,Low)])
dt_xts_High<-as.xts(df_xts_r[,.(Time,High)])
dt_xts_Close<-as.xts(df_xts_r[,.(Time,Close)])

df_xts_r[,LowTemp:=Low]
df_xts_r[,HighTemp:=High]
df_xts_r[,CloseTemp:=Close]


x_low<-endpoints(dt_xts_Low,on="hours")
#-- Remove first element
x_low<-x_low[(2:length(x_low))]

SL<-15*pipsize
TP<-2*SL




repeat_first<-function(x)
{
  return(as.vector(rep(first(x),length(x))))
  
}

dt_joint_orig <- as.data.table(dt_xts_Close)
dt_joint_orig <-cbind(dt_joint_orig,as.data.table(dt_xts_Low))
dt_joint_orig <-cbind(dt_joint_orig,as.data.table(dt_xts_High))

dt_joint_orig<-dt_joint_orig[,.(index,High,Low,Close)]



#SL<-15*pipsize
#TP_fac<-2
#MAX_PERIOD<-24
#i<-24


for(MAX_PERIOD in MAX_PERIOD_vec)
{
for(SL in SL_vec)
{
  
  for(TP_fac in TP_vec)
  {
    print(paste0("MAX_PERIOD:",MAX_PERIOD))
    print(paste0("SL:",SL/pipsize))
    TP<-TP_fac*SL
    print(paste0("TP:",TP/pipsize))
    
dt_final <- data.table()



for (i in 1:MAX_PERIOD) {
  ind_low <- x_low[seq(i,length(x_low),MAX_PERIOD)]
  
  
  dt_time <- df_xts_r[ind_low,.(Time)]
  
  #-- Replicating the entries on the time periods
  entries<-as.data.table(unlist(period.apply(as.vector(dt_xts_Close$Close),ind_low-1,repeat_first)))
  names(entries)<-"entries"
  
  dt_joint <-cbind(dt_joint_orig,entries)
  

  #-- P&L
  dt_joint[,':='(buy_Loss=(entries-Low+SPREAD)/pipsize, buy_Profit=(High-entries-SPREAD)/pipsize,sell_Profit=(entries-Low-SPREAD)/pipsize, sell_Loss=(High-entries+SPREAD)/pipsize )]
  
  #-- Fixing entries at the last minute
  dt_joint[ind_low,':='(buy_Profit=-SPREAD/pipsize,buy_Loss=SPREAD/pipsize,sell_Profit=-SPREAD/pipsize,sell_Loss=SPREAD/pipsize)]
  

  
  buy_Loss<-period.apply(as.vector( dt_joint$buy_Loss ) , ind_low , function(x) which(x>=(SL/pipsize) )[ 1 ] )
  #  names(buy_Loss)<-c("buy_loss")
  buy_Profit<-period.apply(as.vector( dt_joint$buy_Profit ) , ind_low , function(x) which(x>(TP/pipsize) )[ 1 ] )
  #names(buy_Profit)<-c("buy_profit")
  sell_Loss<-period.apply(as.vector( dt_joint$sell_Loss ) , ind_low , function(x) which(x>=(SL/pipsize) )[ 1 ] )
  #names(sell_Loss)<-c("sell_loss")
  sell_Profit<-period.apply(as.vector( dt_joint$sell_Profit ) , ind_low , function(x) which(x>(TP/pipsize) )[ 1 ])
  
  
  dt_res<-data.table(buy_loss=buy_Loss ,
               buy_profit=buy_Profit,
               sell_loss=sell_Loss,
               sell_profit=sell_Profit)
  names(dt_res)<-c("buy_loss"  ,  "buy_profit" , "sell_loss",   "sell_profit")

  dt_res[,BUY_RES:=0][(buy_profit<buy_loss) | (!is.na(buy_profit) & is.na(buy_loss)),BUY_RES:=1]
  dt_res[,SELL_RES:=0][(sell_profit<sell_loss) | (!is.na(sell_profit) & is.na(sell_loss)),SELL_RES:=1]
  
  
  nrow(dt_res[is.na(sell_loss)])

  dt_res<-dt_res[2:nrow(dt_res),]
  
    
  if((dt_time[nrow(dt_time),Time] ==dt_joint[nrow(dt_joint),index]) )
  {
    dt_time <- dt_time[1:(nrow(dt_time)-1),]
  }  
  
  
   if(nrow(dt_time)!=nrow(dt_res))
  {
    print("out")
    print(i)
    break
  }
  dt_res<-cbind(dt_time,dt_res)
  dt_final<-rbind(dt_final,dt_res)
  print(paste0(i,"/",MAX_PERIOD))
}

print("----------------------------")



#-- Sort according to the time
dt_final <- dt_final[order(Time)]




fwrite(dt_final,paste0("output/EURUSD_SL_",SL/pipsize,"_PF_",TP/SL,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv" ))

  }
}


}

end_time<-Sys.time()

end_time-start_time



