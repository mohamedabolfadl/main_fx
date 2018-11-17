




rm(list=ls())

library(data.table)
library(xts)
library(TTR)
library(lubridate)
library(stringi)




#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"
models_dir <- "03_models/"
mql_path <- "C:/Users/Mohamed Ibrahim/AppData/Roaming/MetaQuotes/Terminal/A6DFBB1B8DE9672D2328FF3445436DEC/MQL4/Files/"
logs_dir <- "05_logs/"
prod_code_dir<- "01_code/prod/"

pr = "GOLD"

"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


#-- Read the csv
d<-fread(data_input_dir+pr+"_rsi_confirmation.csv")

#-- Convert to date
#d[,Time:=as.Date(Time,format="%Y.%m.%d %H:%M:%S")]
#d[,Time:=as.Date(Time)]

#-- Flip the order
d<-d[order(Time)]



#-- Get ground truth RSO

d[,RSI_ref:=RSI(d$Close)]
d[,TMS_green_ref:=SMA(d$RSI_ref,n=2)]
d[,TMS_red_ref:=SMA(d$RSI_ref,n=7)]



d<-d[,c(1,2,3,6,4,7,5,8)]


#-- MSE RSI
d[,err:=abs(RSI_mql-RSI_ref)]

mean(d[!is.na(err),err])

