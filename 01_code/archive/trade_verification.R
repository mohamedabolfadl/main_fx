#-- Verify that the trades are correctly computed




rm(list=ls())

library(xts)
library(data.table)
library(ggplot2)

MAX_PERIOD<-24
pipsize<-0.0001
SPREAD<- 2*pipsize
N <- 5e5
#SL_vec<-c(10*pipsize,15*pipsize,20*pipsize)
#TP_vec<-c(1,2,3)


#-- Read minute data
df_xts_r<-fread(file="df_xts_r.csv",nrows = (1e3)+N)
#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]
df_xts_r<-df_xts_r[(1+1e3):nrow(df_xts_r),]

df_xts<-as.xts(df_xts_r)

dt_xts_Low<-as.xts(df_xts_r[,.(Time,Low)])
dt_xts_High<-as.xts(df_xts_r[,.(Time,High)])
dt_xts_Close<-as.xts(df_xts_r[,.(Time,Close)])

#df_xts_r[,LowTemp:=Low]
#df_xts_r[,HighTemp:=High]
#df_xts_r[,CloseTemp:=Close]
#x_low<-endpoints(dt_xts_Low,on="hours")
#x_high<-endpoints(dt_xts_High,on="hours")
#x_close<-endpoints(dt_xts_Close,on="hours")

#--- Read trade results

#-- PARAMETERS
LEADING_INDS <- c("DPO","ZigZag")
MAX_PERIOD_vec<-c(24)
#pipsize<-0.0001
#SPREAD<- 2*pipsize
N <- 5e5
SL_vec<-c(15*pipsize)
TP_vec<-c(2)
PAIR<-"EURUSD"



SL<-SL_vec
MAX_PERIOD<-MAX_PERIOD_vec
TP <- TP_vec
dt_res <- fread(paste0("output_v2/",PAIR,"_SL_",SL/pipsize,"_PF_",TP,"_SPREAD_",SPREAD/pipsize,"_MAXPERIOD_",MAX_PERIOD,".csv"),nrows = (1e3)+N)



#dt_res[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)]



#-- Pick up random 100 trades

inds<-sample(nrow(dt_res),1000)

for(ind in inds)
{
  #ind<-4844
  print(ind)
  chosenTime <- dt_res[ind,Time]
  buyres <- dt_res[ind,BUY_RES]
  #-- Minute entry
if(nrow(df_xts_r[Time==chosenTime])>0   )
  {
  print("entered!")
  entryPoint<-  df_xts_r[Time==chosenTime,which=T]
  #entryPoint <- 103727
  lows <- df_xts_r[(entryPoint+1):(entryPoint+MAX_PERIOD_vec*60),Low]
highs <- df_xts_r[(entryPoint+1):(entryPoint+MAX_PERIOD_vec*60),High]
times <- df_xts_r[(entryPoint+1):(entryPoint+MAX_PERIOD_vec*60),Time]
entryPosition <-   rep(df_xts_r[entryPoint,Close],length(times))
SLPosition <-   rep(df_xts_r[entryPoint,Close]-SL+SPREAD,length(times))
TPPosition <-   rep(df_xts_r[entryPoint,Close]+TP*SL+SPREAD,length(times))

dt_plot<-data.table(low=lows,high=highs,Time=times,entry=entryPosition,SL=SLPosition,TP=TPPosition)
dt_plot$x<-seq(1,nrow(dt_plot))
if(buyres<0.5)
  {
ggplot(dt_plot)+geom_line(aes(x=x,y=low))+
  geom_line(aes(x=x,y=high))+
  geom_line(aes(x=x,y=entry),color="blue")+
  geom_line(aes(x=x,y=SL),color="red")+
  geom_line(aes(x=x,y=TP),color="green")+
  geom_vline(xintercept=dt_res[ind,buy_profit],color="green")+
  geom_vline(xintercept=dt_res[ind,buy_loss],color="red")+
  ggtitle("LOSS")
  
}else{
  ggplot(dt_plot)+geom_line(aes(x=x,y=low))+
    geom_line(aes(x=x,y=high))+
    geom_line(aes(x=x,y=entry),color="blue")+
    geom_line(aes(x=x,y=SL),color="red")+
    geom_line(aes(x=x,y=TP),color="green")+
    geom_vline(xintercept=dt_res[ind,buy_profit],color="green")+
    geom_vline(xintercept=dt_res[ind,buy_loss],color="red")+
    ggtitle("PROFIT")
}
ggsave(paste0("tradechecks/",ind,".png"))
}


}




#---------------- END ---------------------------------------
ggplot(dt_plot)+geom_line(aes(x=x,y=low))+
  geom_line(aes(x=x,y=high))+
  geom_line(aes(x=x,y=entry),color="blue")+
  geom_line(aes(x=x,y=SL),color="red")+
  geom_line(aes(x=x,y=TP),color="green")+
  geom_vline(xintercept=dt_res[ind,buy_profit],color="green")+
  geom_vline(xintercept=dt_res[ind,buy_loss],color="red")+
  ggtitle("PROFIT")


check_inds <- list.files("tradechecks/check")

check_inds<-as.integer(gsub("\\.png","",check_inds))
i<-1
while(i<=length(check_inds))
{

      entryPoint <- check_inds[i]
  tim <- df_xts_r[entryPoint,Time]
      sel_col <- dt_res[Time==tim]  
            buy_res <- dt_res[Time==tim,BUY_RES]
            print(entryPoint)
            print(buy_res)
            print(sel_col[,.(buy_profit,buy_loss)])
            print("-------------")
  if(FALSE)
    {
    lows <- df_xts_r[entryPoint:(entryPoint+MAX_PERIOD_vec*60),Low]
    highs <- df_xts_r[entryPoint:(entryPoint+MAX_PERIOD_vec*60),High]
    times <- df_xts_r[entryPoint:(entryPoint+MAX_PERIOD_vec*60),Time]
    entryPosition <-   rep(df_xts_r[entryPoint,Close],length(times))
    SLPosition <-   rep(df_xts_r[entryPoint,Close]-SL+SPREAD,length(times))
    TPPosition <-   rep(df_xts_r[entryPoint,Close]+TP*SL,length(times))
    
    dt_plot<-data.table(low=lows,high=highs,Time=times,entry=entryPosition,SL=SLPosition,TP=TPPosition)
    
    if(buyres==0)
    {
      ggplot(dt_plot)+geom_line(aes(x=Time,y=low))+
        geom_line(aes(x=Time,y=high))+
        geom_line(aes(x=Time,y=entry),color="blue")+
        geom_line(aes(x=Time,y=SL),color="red")+
        geom_line(aes(x=Time,y=TP),color="green")+ggtitle("LOSS")
    }else{
      ggplot(dt_plot)+geom_line(aes(x=Time,y=low))+
        geom_line(aes(x=Time,y=high))+
        geom_line(aes(x=Time,y=entry),color="blue")+
        geom_line(aes(x=Time,y=SL),color="red")+
        geom_line(aes(x=Time,y=TP),color="green")+ggtitle("PROFIT")
    }
  }
  i<-i+1
}



