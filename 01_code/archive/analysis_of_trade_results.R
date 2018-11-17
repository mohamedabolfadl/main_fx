


rm(list=ls())

library(data.table)
library(plotly)


data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"
models_dir <- "03_models/"
mql_path <- "C:/Users/Mohamed Ibrahim/AppData/Roaming/MetaQuotes/Terminal/A6DFBB1B8DE9672D2328FF3445436DEC/MQL4/Files/"
logs_dir <- "05_logs/"
prod_code_dir<- "01_code/prod/"





"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


res <- fread( data_output_dir+ "dt_total_result_test_3.csv")

res[,trades:=abs(BUY_RES_USDJPY)+
                  abs(SELL_RES_USDJPY)+
                  abs(BUY_RES_GBPUSD)+
                  abs(SELL_RES_GBPUSD)+
                  abs(BUY_RES_USDCHF)+
                  abs(SELL_RES_USDCHF)+
                  abs(BUY_RES_USDCAD)+
                  abs(SELL_RES_USDCAD)+
                  abs(BUY_RES_NZDUSD)+
                  abs(SELL_RES_NZDUSD)+
                  abs(BUY_RES_AUDUSD)+
                  abs(SELL_RES_AUDUSD)+
                  abs(BUY_RES_XAUUSD)+
                  abs(SELL_RES_XAUUSD)+
                  abs(BUY_RES_EURUSD)+
                  abs(SELL_RES_EURUSD)
                  ]


mean_trades <- res[,(avg_trades_per_per=sum(trades)),by=ret_per]

hist(mean_trades$V1)




#plot_ly(data=mean_trades,x=~ret_per,y=~V1,type="scatter",mode="lines")
plot_ly(data=mean_trades,x=~ret_per,y=~V1)








