



library(data.table)
library(lubridate)



TARGET <- "BUY_RES_AUDUSD"

dt <- fread("02_data/intermediate/ML_SL_15_PF_1_SPREAD_3_ALL.csv")
dt[,index:= seq(1,nrow(dt))]

#-- Create the index, time lookup table
dt_time_lut <<- dt[,.(index,Time)]
dt_time_lut[,ret_per:=paste0(year(Time),"_",week(Time))]

dt[,hour:=as.numeric(lubridate::hour(Time))]

setnames(dt,TARGET,"TARGET")

time <- names(dt)[grepl("ime",names(dt))]
closes <- names(dt)[grepl("Close$",names(dt))]
opens <- names(dt)[grepl("Open$",names(dt))]
low <- names(dt)[grepl("Low$",names(dt))]
high <- names(dt)[grepl("High$",names(dt))]
bss <- names(dt)[grepl("bs",names(dt))]
ress <- names(dt)[grepl("RES",names(dt))]
pl <- names(dt)[grepl("(profit|loss)",names(dt))]

nonFeats <- c(time,bss,ress,pl)

relcols <- setdiff(names(dt),nonFeats)


dt_sel <- dt[,..relcols]


#-- parsing target as character so that H2O makes a classification task


#dt_sel[,TARGET:=as.character(TARGET)]
fwrite(dt_sel,paste0("02_data/intermediate/",TARGET,"_h2o_ML_SL_15_PF_1_SPREAD_3_ALL.csv"))



## Init h2o OPT
library(h2o)
h2o.init()

