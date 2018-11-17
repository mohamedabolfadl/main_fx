# To check
# 1. PL functionality
# 2. Zigzag future


rm(list = ls())

library(xts)
library(TTR)
library(data.table)

pipsize = 0.0001 # Changes for JPY pairs
SL = 15 # Stop loss in pips
PF = 2 # Main Profit factor
PF_1 = 1 # Profit factor
PF_2 = 1.5 # Profit factor
spread = 1 # Spread in pips



  # EMAs 10-100-200 and the same for other crosses

#break

get_time_range<-function(i_start,i_end,df)
{
  return(c(t(as.matrix(df[i_start:min(i_end,nrow(df)),c("Open","High","Low","Close")]))))
  
  
}
#df_xts<-as.xts(read_csv(file="df_xts.csv"))
dt<-fread("variables/dt.csv")

#-- Fixing the wrong entries
dt[,Time:=gsub("T"," ",Time)]
dt[,Time:=gsub("Z","",Time)]

dt$prev_b_1<-0
dt$prev_b_2<-0
dt$prev_s_1<-0
dt$prev_s_2<-0

dt$buy_target_1 <- 0
dt$buy_time_1 <- 0
dt$sell_target_1 <- 0
dt$sell_time_1 <- 0

dt$buy_target_2 <- 0
dt$buy_time_2 <- 0
dt$sell_target_2 <- 0
dt$sell_time_2 <- 0

dt_xts_r<-fread("variables/df_xts_r.csv")
dt_xts_r[,Time:=gsub("T"," ",Time)]
dt_xts_r[,Time:=gsub("Z","",Time)]

rownames(dt)<-dt$Time
rownames(dt_xts_r)<-dt_xts_r$Time
dt$Time<-NULL
dt_xts_r$Time<-NULL

#break
i<-1
TP_time<-numeric(nrow(dt))
SL_time<-numeric(nrow(dt))
TP_time_1<-numeric(nrow(dt))
SL_time_1<-numeric(nrow(dt))
TP_time_2<-numeric(nrow(dt))
SL_time_2<-numeric(nrow(dt))


minuteEntryIndex_b_1<-1
minuteEntryIndex_b_2<-1
minuteEntryIndex_s_1<-1
minuteEntryIndex_s_2<-1

start_time <- Sys.time()


print("Starting the loop")
while(i<nrow(dt)+1)
  #while(i<2000)
{
  # print(i)
  
  position<-as.numeric(dt[i,"Close"])
  # Entry time in the hour chart
  entryTime <- rownames(dt)[i]
  # Index of the entry in the minute chart
  minuteEntryIndex<-match(entryTime,rownames(dt_xts_r))
  
  #-------- BUY LABEL -------------------------------
  
  #------ Base TP ----
  # Time when TP is hit
 
  TP_time[i] <-which(get_time_range((minuteEntryIndex+1),(minuteEntryIndex+1000),dt_xts_r)>(position+(SL*PF+spread)*pipsize))[1]
  SL_time[i] <-which(get_time_range((minuteEntryIndex+1),(minuteEntryIndex+1000),dt_xts_r)<(position-SL*pipsize))[1]
  
  
  # Check for NA (when the TP or SL is never met) and checking if the TP or SL occured in the immediate following minute
  if(!is.na(TP_time[i]))
  {
    if(TP_time[i]<5)
    {
      #res<-get_time_range(minuteEntryIndex,dt_xts_r)
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
    if(SL_time[i]<5)
    {
      #res<-get_time_range(i,dt_xts_r)
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
    dt[i,"buy_time"]<- floor(TP_time[i]/4)+1
    
  }else
  {
    dt[i,"buy_target"]<- -1
    dt[i,"buy_time"]<- floor(SL_time[i]/4)+1
    
  }
  
 
  #-------------------------- SELL LABEL ---------------------------------
  #----- Base TP -------
  # Time when TP is hit
  #TP_time[i] <- which(dt_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"Low"]<(position-(SL*PF+spread)*pipsize))[1]
  # Time when SL is hit
  #SL_time[i] <- which(dt_xts_r[(minuteEntryIndex+1):(minuteEntryIndex+1000),"High"]>(position+SL*pipsize))[1]
  
  TP_time[i] <-which(get_time_range((minuteEntryIndex+1),(minuteEntryIndex+1000),dt_xts_r)<(position-(SL*PF+spread)*pipsize))[1]
  SL_time[i] <-which(get_time_range((minuteEntryIndex+1),(minuteEntryIndex+1000),dt_xts_r)>(position+SL*pipsize))[1]
  
  
  
  # Check for NA (when the TP or SL is never met) and checking if the TP or SL occured in the immediate following minute
  if(!is.na(TP_time[i]))
  {
    if(TP_time[i]<5)
    {
      #res<-get_time_range(minuteEntryIndex,dt_xts_r)
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
    if(SL_time[i]<5)
    {
      print("Selling: SL has suspisciously occured in the following minute")
      print(paste("Position:",position," SL:",position-SL*pipsize," i:",i,"entryTime:",entryTime))
      
    }     
  }  else{
    
    SL_time[i]<-1e9
    
  }
  
  # Fill the Profit or Loss
  if(TP_time[i]<SL_time[i])
  {
    #print("Won smth")
    dt[i,"sell_target"]<- 1
    dt[i,"sell_time"]<- floor(TP_time[i]/4)+1
    
  }else
  {
    dt[i,"sell_target"]<- -1
    dt[i,"sell_time"]<- floor(SL_time[i]/4)+1
    
  }
  
  if(i%%5000==0)
  {
    print("--------------")
    print(paste("We are in date",entryTime,"with",i,"/97531"))
    print( Sys.time()-start_time)
    print("---------------")
  }  
  
  
  i<-i+1
}

end_time <- Sys.time()

print(end_time - start_time)

  dt_ind<-fread("variables/dt_ind.csv")
  dt_ind[,Time:=gsub("T"," ",Time)]
  dt_ind[,Time:=gsub("Z","",Time)]
  #rownames(dt_ind)<-dt_ind$Time
  #dt_ind$Time<-NULL
  
 
  
  dt_int_buy<-cbind(dt_ind,as.vector(dt$prev_b_1),as.vector(dt$prev_b_2),as.vector(dt$buy_target))
  dt_int_sell<-cbind(dt_ind,as.vector(dt$prev_s_1),as.vector(dt$prev_s_2),as.vector(dt$sell_target))
  names(dt_int_buy)[(ncol(dt_int_buy)-2):ncol(dt_int_buy)]<-c("prev_b_1","prev_b_2","btarget")
  names(dt_int_sell)[(ncol(dt_int_sell)-2):ncol(dt_int_sell)]<-c("prev_s_1","prev_s_2","starget")
  
  
  dt_int_buy[,Time:=dt_ind$Time]
  dt_int_sell[,Time:=dt_ind$Time]
  
  dt_df<-as.data.table(dt)
  dt_df$Time<-rownames(dt_df)
  
  
  dt_int_buy$DPO<-NULL
  dt_int_sell$DPO<-NULL
  
  dt_int_buy$ZigZag<-NULL
  dt_int_sell$ZigZag<-NULL
  
  
  
  dt_df<-dt_df[buy_target!=0,]
  dt_int_buy<-dt_int_buy[btarget!=0,]
  dt_int_sell<-dt_int_sell[starget!=0,]
  
  unique(dt_df$buy_target)
  
  
  fwrite(dt_int_buy,file="variables/H1_buy.csv",row.names = FALSE)
  fwrite(dt_int_sell,file="variables/H1_sell.csv",row.names = FALSE)
  fwrite(dt_df,file="variables/H1_all_targets.csv",row.names = FALSE)








