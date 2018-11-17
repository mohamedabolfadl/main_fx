


library(data.table)
library(stringi)
library(lubridate)


#-- read dukascopy files

dt_res <- data.table()

fls <- list.files("02_data/input/duksacopy/")


for (fl in fls)
{
  rd<-fread(paste0("02_data/input/duksacopy/",fl))
  dt_res<-rbind(dt_res,rd)
  
}



dt_tmp <- dt_res
dt_tmp[,Time:=paste0(substr(`Local time`,7,10),"-",substr(`Local time`,4,5),"-",substr(`Local time`,1,2)," ",substr(`Local time`,12,19))]
#dt_tmp[,Time_chng:=as.POSIXct(Time)]
#lubridate::wday(dt_tmp[2e3,Time])
#-- Remove weekends
dt_write <- dt_tmp[wday(Time) %in% c(2,3,4,5,6)  ,.(Time,Open,High,Low,Close)]

#-- Checks
head(dt_write)
tail(dt_write)
#dt_write[1e3:1100,]

fwrite(dt_write,"02_data/input/dt_dukascopy.csv")

