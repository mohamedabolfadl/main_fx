
#-- TO DO:
# CLEAN UP THE NAs with average of the neighbouring columns
# GEnerate dcast for high/Low


#To get the column name from a variable:
#  a<-"05"
#  - nrow(dt_casted[is.na(get(a))])
#To directly access a numerical column
#  - nrow(dt_casted[is.na(`05`)])
#To access column name from a variable
#    a<-"05"  
#    dt_casted[[a]]

rm(list=ls())



library(data.table)
library(xts)

#-- Input paramters
pipsize<-0.001
SPREAD<- 1*pipsize
SL<-15*pipsize
TP<-2*SL
#-- Load the data


#-- Hour data
#-- Time-OHLC-buy buy target biy time sell target sell time
dt<-fread(file="dt.csv")


#-- minute data: Time ohlc
df_xts_r<-fread(file="df_xts_r.csv",nrows = 1e5)

#-- Removing "T" and "R" from the Time column and converting the time column to Date
df_xts_r[,Time:=gsub("T"," ",Time)][,Time:=gsub("Z","",Time)][,Time:=as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S")]

#-- Get 5 minute signal to reduce number of columns
dt_5m<-as.data.table(align.time(to.minutes5(as.xts(df_xts_r)),5*60))
names(dt_5m)<-c("Time","Open","High","Low","Close")



#-- Extracting the minute part
df_xts_r[,minute_part:=substr(Time,15,16)][,hour_part:=substr(Time,12,13)][,dt_part:=substr(Time,1,10)]
dt_5m[,minute_part:=substr(Time,15,16)][,hour_part:=substr(Time,12,13)][,dt_part:=substr(Time,1,10)]


modes <- c("Close","High","Low")

for(MODE in modes)
{
#-- Change to low, high, close

#dt_sel <- df_xts_r[(.N-1e4):.N,.(Close,minute_part,hour_part,dt_part)]
colList <- c(MODE,"minute_part","hour_part","dt_part")
  dt_sel <- dt_5m[(.N-27e3):.N,..colList]
#-- Creating the minute columns
dt_casted <- dcast(dt_sel,dt_part+hour_part ~ minute_part, value.var = MODE)



#-- Shifted time key to prepare for joins
N_future<-4
i<-1
while (i<N_future) {
  dt_casted[,paste0("lagged_hour","_",i):=shift(hour_part,i,type="lead")][,paste0("lagged_dt","_",i):=shift(dt_part,i,type="lead")]

  
  i<-i+1
}



lag_names<- names(dt_casted)[(length(names(dt_casted))-(N_future*2)+3)  :length(names(dt_casted))]

#-- The columns that should be selected in order to be joined on each step
nms <- setdiff(names(dt_casted),lag_names )

i<-1
while (i<N_future) {
 
  #-- join the rows
  dt_casted<-merge(dt_casted,dt_casted[,..nms]  ,by.x=c(paste0("lagged_dt","_",i),paste0("lagged_hour","_",i)),by.y=c("dt_part","hour_part"))
  
  
  
  #-- Rename the columns to replace x and y
  names(dt_casted)<-sapply(names(dt_casted),gsub,pattern="\\.x",replacement="")
  names(dt_casted)<-sapply(names(dt_casted),gsub,pattern="\\.y",replacement=paste0("_",i))
  
  
  
    i<-i+1
}

exclude<-c("lagged_dt_3","lagged_hour_3","lagged_dt_2","lagged_hour_2","lagged_dt_1","lagged_hour_1","dt_part","hour_part" )
nms <- setdiff(names(dt_casted),exclude)
nms <- paste0(MODE,"_",nms)
names(dt_casted)<-c(exclude, nms)
dt_casted[,date_hour:=paste0(dt_part,"_",hour_part)]

dt_casted[,':='(lagged_dt_3=NULL,lagged_hour_3=NULL,lagged_dt_2=NULL,lagged_dt_1=NULL,lagged_hour_1=NULL,lagged_hour_2=NULL,dt_part=NULL,hour_part=NULL)]
#fwrite(dt_casted, "data/Lows.csv")
#fwrite(dt_casted, "data/Highs.csv")
fwrite(dt_casted, paste0("data/",MODE,".csv"))




}




#-- Get P&L

N_cols <-    ncol(dt_casted)
dt_casted[,MAX_TOP:=max(dt_casted[,10:N_cols],na.rm = T)]

dt_casted[,buy_win_id:=which(dt_casted[,10:N_cols])>(EntryPosition-SPREAD+TP)]






#--- JUNK
#-- Self join test
dt<-data.table(keya=c("a","b","c","d"),keyb=c("w","x","y","z"),value1=c(1,2,3,4),value2=c(5,6,7,8),keyc=c("b","c","d","e"),keyd=c("x","y","z","p"))
res<-merge(dt,dt,by.x=c("keyc","keyd"),by.y=c("keya","keyb"), all.x = T)


dt_casted[,lagged_hour:=NULL]
dt_casted[,lagged_dt:=NULL]

tst<-dt_casted[1]
dt_sel <- df_xts_r[(.N-1e4):.N,.(High,Low,minute_part,hour_part,dt_part)]
dt_sel <- df_xts_r[(.N-1e4):.N,.(Low,minute_part,hour_part,dt_part)]

dt_casted<-melt(dt_sel,measure="High",id=c("minute_part"))

dt_casted<-dcast(dt_sel,High+Low~minute_part+hour_part)




lag(c(1,2,3,4,5,6,7),1)



dt_casted[,lagged_hour:=NULL]

shift

lag



















