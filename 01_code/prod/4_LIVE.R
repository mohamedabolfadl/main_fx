

rm(list=ls())

tm<-Sys.time()

library(data.table)
library(xgboost)
library(rJava)
library(plotly)

#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"
models_dir <- "03_models/"
mql_path <- "C:/Users/Mohamed Ibrahim/AppData/Roaming/MetaQuotes/Terminal/A6DFBB1B8DE9672D2328FF3445436DEC/MQL4/Files/"
logs_dir <- "05_logs/"
prod_code_dir<- "01_code/prod/"


cols_ord <- c("USDJPY_RSI"   ,    "USDJPY_TMS_green" ,"USDJPY_TMS_red" ,  "GBPUSD_RSI",      
              "GBPUSD_TMS_green" ,"GBPUSD_TMS_red" ,  "USDCHF_RSI"   ,    "USDCHF_TMS_green",
              "USDCHF_TMS_red" ,  "USDCAD_RSI"   ,    "USDCAD_TMS_green", "USDCAD_TMS_red"  ,
              "NZDUSD_RSI"   ,    "NZDUSD_TMS_green", "NZDUSD_TMS_red" ,  "AUDUSD_RSI"      ,
              "AUDUSD_TMS_green" ,"AUDUSD_TMS_red" ,  "XAUUSD_RSI"  ,     "XAUUSD_TMS_green",
              "XAUUSD_TMS_red" ,  "EURUSD_RSI"     ,  "EURUSD_TMS_green" ,"EURUSD_TMS_red"  )



#-- Read models file
currs <- list.files(models_dir)


mdls = list()
thresholds = list()


#-- Read the best parameters
bst_param<-fread(paste0(data_output_dir,"best_parameters.csv"))
bst_param[grepl("BUY",instrument),instrument:=paste0(substr(instrument,9,14),"_",substr(instrument,1,3))][grepl("SELL",instrument),instrument:=paste0(substr(instrument,10,15),"_",substr(instrument,1,4))]


i<-1
#-- Read the models
for (curr in currs)
{
mdls[[i]] <- xgb.load(paste0(models_dir,curr,"/mdl_",curr))
thresholds[[i]]<-bst_param[instrument==curr,threshold]
  i<-i+1
}


Sys.time()-tm




#------ Get the features
while(T)
  {
fls <- list.files(mql_path)
  
if(length(fls)==8)
  {
#-- Remove all the files since we already read them
file.remove(paste0(mql_path,fls))

fls<-unlist(lapply(fls,gsub,pattern="GOLD",replacement="XAUUSD"))

#-- Parse them  
  
lst<-lapply(fls,strsplit,split="_")
  

dt_feats=data.table(USDJPY_RSI=numeric(1),USDJPY_TMS_green=numeric(1),USDJPY_TMS_red=numeric(1),
                    GBPUSD_RSI=numeric(1),GBPUSD_TMS_green=numeric(1),GBPUSD_TMS_red=numeric(1),
                    USDCHF_RSI=numeric(1),USDCHF_TMS_green=numeric(1),USDCHF_TMS_red=numeric(1),
                    USDCAD_RSI=numeric(1),USDCAD_TMS_green=numeric(1),USDCAD_TMS_red=numeric(1),
                    NZDUSD_RSI=numeric(1),NZDUSD_TMS_green=numeric(1),NZDUSD_TMS_red=numeric(1),
                    AUDUSD_RSI=numeric(1),AUDUSD_TMS_green=numeric(1),AUDUSD_TMS_red=numeric(1),
                    XAUUSD_RSI=numeric(1),XAUUSD_TMS_green=numeric(1),XAUUSD_TMS_red=numeric(1),
                    EURUSD_RSI=numeric(1),EURUSD_TMS_green=numeric(1),EURUSD_TMS_red=numeric(1)
                    )

names(dt_feats)
i<-1
while(i<(length(lst)+1))
{
   dt_feats[,c(paste0(lst[[i]][[1]][1],"_RSI")):=as.numeric( gsub(",",".",gsub("csv","",lst[[i]][[1]][2]) )  ) ]
  dt_feats[,c(paste0(lst[[i]][[1]][1],"_TMS_green")):=as.numeric( gsub(",",".",gsub("csv","",lst[[i]][[1]][3]) )  ) ]
  dt_feats[,c(paste0(lst[[i]][[1]][1],"_TMS_red")):=as.numeric( gsub(",",".",gsub("\\.csv","",lst[[i]][[1]][4]) )  ) ]
  
  i<-i+1
}
currs <- unlist(lapply(currs,gsub,pattern="XAUUSD",replacement="GOLD"))




mt_feats <-as.matrix(dt_feats)


i<-1
while (i< (length(currs)+1) )
{
  
  dt_feats[,c(currs[i]):=predict(mdls[[i]],newdata = mt_feats)]
  dec <- predict(mdls[[i]],newdata = mt_feats)>thresholds[[i]]
  dt_feats[,c(paste0(currs[i],"_dec")):=dec]
  
  if(dec==T)
  {
  fwrite(list(1),paste0(mql_path,currs[i],".csv"))  
    
  }
  
    i<-i+1
}


Sys.sleep(10)
fls <- list.files(mql_path)
file.remove(paste0(mql_path,fls))
dt_feats$Time <- Sys.time()
fwrite(dt_feats,paste0(logs_dir,"logs_",gsub(":",",",Sys.time()),".csv"))

Sys.sleep(3)

source(file=paste0(prod_code_dir,"4_PLOT_EXPORT.R"))

}


if(minute(Sys.time())==21 | minute(Sys.time())==41 )
{
  .jinit()                           # this starts the JVM
  jRobot <- .jnew("java/awt/Robot")  # Create object of the Robot class
  
  # Let java sleep 500 millis between the simulated mouse events
  .jcall(jRobot,, "setAutoDelay",as.integer(500))
  
  # move mouse to 100,200 and select the text up to (100,300)         
  .jcall(jRobot,, "mouseMove",as.integer(100),as.integer(200))
  .jcall(jRobot,, "mousePress",as.integer(16))
  .jcall(jRobot,, "mouseMove",as.integer(100),as.integer(300))
  .jcall(jRobot,, "mouseRelease",as.integer(16))
  
}
}






Sys.time()-tm



View(dt_feats)















