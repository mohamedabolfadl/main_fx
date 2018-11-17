
# TODO: Check the timing using dummy data

rm(list=ls())

start_time<-Sys.time()


options(scipen=999)

library(data.table)
library(xts)

MAX_PERIOD<-24
pipsize<-0.0001
SPREAD<- 2*pipsize
N <- 5e6
SL_vec<-c(10*pipsize,15*pipsize,20*pipsize)
TP_vec<-c(1,2,3)

df_xts_r<-fread(file="df_xts_r.csv",nrows = (1e3)+N)
#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r),]

df_xts<-as.xts(df_xts_r)

dt_xts_Low<-as.xts(df_xts_r[,.(Time,Low)])
dt_xts_High<-as.xts(df_xts_r[,.(Time,High)])
dt_xts_Close<-as.xts(df_xts_r[,.(Time,Close)])

df_xts_r[,LowTemp:=Low]
df_xts_r[,HighTemp:=High]
df_xts_r[,CloseTemp:=Close]


x_low<-endpoints(dt_xts_Low,on="hours")
x_high<-endpoints(dt_xts_High,on="hours")
x_close<-endpoints(dt_xts_Close,on="hours")



repeat_first<-function(x)
{
  return(as.vector(rep(first(x),length(x))))
  
}

#names(dt_final)<-c("Time"     ,   "buy_loss"  ,  "buy_profit" , "sell_loss" ,  "sell_profit" ,"BUY_RES" ,    "SELL_RES")




#SL<-15*pipsize
#TP<-2*SL


for(SL in SL_vec)
{
  
  for(TP_fac in TP_vec)
  {
    
    print(paste0("SL:",SL/pipsize))
    
    TP<-TP_fac*SL
    print(paste0("TP:",TP/pipsize))
    
    dt_final <- data.table()
    
    
    for (i in 1:MAX_PERIOD) {
      ind_low <- x_low[seq(i+1,length(x_low),MAX_PERIOD)]
      ind_close <- x_close[seq(i+1,length(x_low),MAX_PERIOD)]
      ind_high <- x_high[seq(i+1,length(x_low),MAX_PERIOD)]  
      
      
      #-- Checks
      #    head(ind_low)
      #  head(df_xts_r[x_low])
      #  if(i==1)
      #{  dt_time <- df_xts_r[c(x_low[i+1],ind_low),.(Time)]
      #}else{
      #  dt_time <- df_xts_r[ind_low,.(Time)]
      #}  
      #    head(dt_time)
      
      dt_time <- df_xts_r[ind_low,.(Time)]
      
      #-- Skip first entry
      #ind_low <-ind_low[2:length(ind_low)]
      #ind_close <-ind_close[2:length(ind_close)]
      #ind_high <-ind_high[2:length(ind_high)]
      
      
      entries<-as.data.table(unlist(period.apply(as.vector(dt_xts_Close$Close),ind_low,repeat_first)))
      names(entries)<-"entries"
      
      dt_joint <- as.data.table(dt_xts_Close)
      dt_joint <-cbind(dt_joint,as.data.table(dt_xts_Low))
      dt_joint <-cbind(dt_joint,as.data.table(dt_xts_High))
      dt_joint <-cbind(dt_joint,entries)
      
      #  dt_joint <- dt_joint[,.(index,High,Low,Close,entries)][,Difference:=diff(entries)]
      
      dt_joint <- dt_joint[,.(index,High,Low,Close,entries)]
      
      #-- P&L
      dt_joint[,':='(buy_Loss=(entries-Low+SPREAD)/pipsize, buy_Profit=(High-entries-SPREAD)/pipsize,sell_Profit=(entries-Low-SPREAD)/pipsize, sell_Loss=(High-entries+SPREAD)/pipsize )]
      
      
      #-- Get index of hitting targets
      buy_loss<-as.data.table(period.apply(as.vector( dt_joint$buy_Loss ) , ind_low , function(x) which(x>=(SL/pipsize) )[ 1 ] ))
      names(buy_loss)<-c("buy_loss")
      buy_profit<-as.data.table(period.apply(as.vector( dt_joint$buy_Profit ) , ind_low , function(x) which(x>(TP/pipsize) )[ 1 ] ))
      names(buy_profit)<-c("buy_profit")
      sell_loss<-as.data.table(period.apply(as.vector( dt_joint$sell_Loss ) , ind_low , function(x) which(x>=(SL/pipsize) )[ 1 ] ))
      names(sell_loss)<-c("sell_loss")
      sell_profit<-as.data.table(period.apply(as.vector( dt_joint$sell_Profit ) , ind_low , function(x) which(x>(TP/pipsize) )[ 1 ] ))
      names(sell_profit)<-c("sell_profit")
      
      dt_res <- cbind(buy_loss,buy_profit)
      dt_res <- cbind(dt_res,sell_loss)
      dt_res <- cbind(dt_res,sell_profit)
      
      dt_res[,BUY_RES:=0][(buy_profit<buy_loss) | (!is.na(buy_profit) & is.na(buy_loss)),BUY_RES:=1]
      dt_res[,SELL_RES:=0][(sell_profit<sell_loss) | (!is.na(sell_profit) & is.na(sell_loss)),SELL_RES:=1]
      
      
      if(i<MAX_PERIOD)
      {  #-- Trim off first result as it corresponds to the first non hour entry
        dt_res<-dt_res[2:nrow(dt_res),]
      }  
      
      # if(nrow(dt_time)!=nrow(dt_res))
      #{
      #  print("out")
      #  print(i)
      #  break
      #}
      dt_res<-cbind(dt_time,dt_res)
      dt_final<-rbind(dt_final,dt_res)
      print(i)
    }
    
    print("----------------------------")
    
    
    
    #-- Sort according to the time
    dt_final <- dt_final[order(Time)]
    
    
    
    
    fwrite(dt_final,paste0("output/EURUSD_SL_",SL/pipsize,"_PF_",TP/SL,"_SPREAD_",SPREAD/pipsize,"_LIFETIME_",MAX_PERIOD,".csv" ))
    
  }
}





end_time<-Sys.time()

end_time-start_time



