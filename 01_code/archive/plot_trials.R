rm(list=ls())


library(quantmod)
library(data.table)
library(lubridate)
library(mlr)
library(ggplot2)
library(xgboost)
library(crayon)
library(plotly)
library(caret)
library(mlrHyperopt)
library(lubridate)
library(parallelMap)

#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"






"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}




dt_names <- names(fread(data_intermediate_dir+"ML_SL_15_PF_1_SPREAD_3_ALL.csv"))
dt <- fread(data_intermediate_dir+"ML_SL_15_PF_1_SPREAD_3_ALL.csv",select=c("Time","EURUSD_Open","EURUSD_Close","EURUSD_High","EURUSD_Low","BUY_RES_EURUSD","SELL_RES_EURUSD","buy_profit_EURUSD","buy_loss_EURUSD","sell_profit_EURUSD","sell_loss_EURUSD"))
dt_sel<-copy(dt)

#dt <- fread(data_input_dir+"dt_all_min.csv")
#pair <- "EURUSD"
#cols = c("Time",names(dt)[grepl(pair,names(dt))])
#dt_sel <- dt[1000:5200,..cols]



dt_sel[,Time:=as.POSIXct(Time)]
#dt_sel<-as.xts(dt_sel)


ind <- 4e3
SL <- 15
pip <- 0.0001
window_before <- 50
window_after <-  50
SPREAD <- 3  

ind<-1e2

while(ind< nrow(dt_sel))
{
dt_sel[ind,]

mid_loc <-  window_before+1
#mid_loc <-  20
relevant_cols <- c("Time" ,names(dt_sel)[grepl("(Open|Close|High|Low)",names(dt_sel))])
#-- BUY
chartSeries(dt_sel[,..relevant_cols]  ,type = "candlesticks"
            ,  subset=dt_sel[max(1,ind-window_before),Time]+"::"+dt_sel[min(nrow(dt_sel),ind+window_after),Time]
            ,  TA="addVo();addLines(v="+mid_loc+",on=-1)"
            , theme = ifelse(dt_sel[ind,BUY_RES_EURUSD]>0,"white","black"))

addLines(h=dt_sel[ind,EURUSD_Close]+SPREAD*pip , col=6)          # Entry
addLines(h=dt_sel[ind,EURUSD_Close]+SPREAD*pip+SL*pip, col=6)   # TP
addLines(h=dt_sel[ind,EURUSD_Close]+SPREAD*pip -SL*pip , col=6)  # SL
ans <- readline(prompt="save?")

if(ans=="c")
{
  
  ## Save plot
  
}
#-- SELL
chartSeries(dt_sel[,..relevant_cols],type = "candlesticks"
            ,  subset=dt_sel[max(1,ind-window_before),Time]+"::"+dt_sel[min(nrow(dt_sel),ind+window_after),Time]
            ,  TA="addVo();addLines(v="+(window_before+1)+",on=-1)"
            , theme = ifelse(dt_sel[ind,SELL_RES_EURUSD]>0,"white","black"))
addLines(h=dt_sel[ind,EURUSD_Close], col=6)   # Entry
addLines(h=dt_sel[ind,EURUSD_Close]-SPREAD*pip +SL*pip, col=6) # SL
addLines(h=dt_sel[ind,EURUSD_Close] -SPREAD*pip-SL*pip , col=6)# TP
ans <- readline(prompt="save?")

if(ans=="y")
{
  
  ## Save plot
  
}


ind<-ind+1
}





#p <-   dt_sel %>% plot_ly(x = ~Time, type="ohlc",
#          open = ~Open_EURUSD, close = ~Close_EURUSD,
#          high = ~High_EURUSD, low = ~Low_EURUSD) %>%
#  layout(title = "Basic OHLC Chart")
#p
















