




logs_dir <- "05_logs/"
data_output_dir<-"02_data/output/"


Sys.setenv("plotly_username" = "mabolfadl")
Sys.setenv("plotly_api_key" = "V4x9Bjg3EzM0svwyffRS")


bst_param<-fread(paste0(data_output_dir,"best_parameters.csv"))
bst_param[grepl("BUY",instrument),instrument:=paste0(substr(instrument,9,14),"_",substr(instrument,1,3))][grepl("SELL",instrument),instrument:=paste0(substr(instrument,10,15),"_",substr(instrument,1,4))]



#-- Read models file
currs <- list.files(models_dir)



#-- Read all the logs



lgs <- list.files((logs_dir))

i<-1
while(i<(length(lgs)+1))
{
  
  dt<-  fread(paste0(logs_dir,lgs[i]))
  
  if(i==1)
  {
    dt_all <- dt  
  }else{
    
    dt_all <- rbind(dt_all,dt)
  }
  
  
  i<-i+1
}


#dt_all[,Time_sub:=as.POSIXct(gsub("T"," ",substr(Time,1,19)))]
dt_all[,Time_sub:=gsub("T"," ",substr(Time,9,16))]








i<-1
#-- Read the models
for (curr in currs)
{
  mdls[[i]] <- xgb.load(paste0(models_dir,curr,"/mdl_",curr))
  thresholds[[i]]<-bst_param[instrument==curr,threshold]
  i<-i+1
}







cols_ord <- c("USDJPY_RSI"   ,    "USDJPY_TMS_green" ,"USDJPY_TMS_red" ,  "GBPUSD_RSI",      
              "GBPUSD_TMS_green" ,"GBPUSD_TMS_red" ,  "USDCHF_RSI"   ,    "USDCHF_TMS_green",
              "USDCHF_TMS_red" ,  "USDCAD_RSI"   ,    "USDCAD_TMS_green", "USDCAD_TMS_red"  ,
              "NZDUSD_RSI"   ,    "NZDUSD_TMS_green", "NZDUSD_TMS_red" ,  "AUDUSD_RSI"      ,
              "AUDUSD_TMS_green" ,"AUDUSD_TMS_red" ,  "XAUUSD_RSI"  ,     "XAUUSD_TMS_green",
              "XAUUSD_TMS_red" ,  "EURUSD_RSI"     ,  "EURUSD_TMS_green" ,"EURUSD_TMS_red"  )



x <- as.matrix(dt_all[,..cols_ord])


i<-1

while(i<(length(mdls)+1))
{
  
  y<-predict(mdls[[i]],newdata=x)
if(i==1)
{
  all<-list(y)
  
}else{
  
  all<-list(all,y)
}
  
    i<-i+1
}


all<-unlist(all)

x=data.table(
Time=c("2018-01-01",
       "2018-01-02",
       "2018-01-03",
       "2018-01-04",
       "2018-01-05",
       "2018-01-06",
       "2018-01-07",
       "2018-01-08",
       "2018-01-09",
       "2018-01-10",
       "2018-01-11",
       "2018-01-12",
       "2018-01-13",
       "2018-01-14",
       "2018-01-15",
       "2018-01-16",
       "2018-01-17",
       "2018-01-18",
       "2018-01-19",
       "2018-01-20",
       "2018-01-21",
       "2018-01-22",
       "2018-01-23"
),
price =(c(1.13271,
          1.13297
         , 1.13375
         , 1.13324
         , 1.13425
         , 1.13357
         , 1.13206
         , 1.12854
         , 1.12994
         , 1.13125
         , 1.13075
         , 1.12899
         , 1.12997
         , 1.13178
         , 1.13222
         , 1.1315
         , 1.13173
         , 1.13509
         , 1.13316
         , 1.13241
         , 1.13229
         ,1.15
         ,1.16))
)






x$Time<-as.Date(x$Time)
x=as.xts(x)

RSI(x)

, TMS red = 54.24278923
, TMS green = 53.78064716
, RSI = 52.60773155




RSI(price)











up <- momentum(price, n=1, na.pad=TRUE)
which.dn <- which(up < 0)
dn <- up*0
dn[which.dn] <- -up[which.dn]
up[which.dn] <- 0


#mavgDn<-TTR::EMA(dn,n=27)
#mavgUp<-TTR::EMA(up,n=27)

#dn[1]<-NA

mavgDn<-TTR::EMA(dn,n=14,wilder = T)
mavgUp<-TTR::EMA(up,n=14,wilder=T)


100 * mavgUp / ( mavgUp + mavgDn )


n=14


up <- momentum(price, n=1, na.pad=TRUE)
which.dn <- which(up < 0)
dn <- up*0
dn[which.dn] <- -up[which.dn]
up[which.dn] <- 0

maArgs <- list(n=n)
# Default Welles Wilder EMA
  maType <- 'EMA'
  maArgs$wilder <- TRUE


  mavgUp <- do.call( maType, c( list(up), maArgs ) )
  mavgDn <- do.call( maType, c( list(dn), maArgs ) )

rsi <- 100 * mavgUp / ( mavgUp + mavgDn )


























