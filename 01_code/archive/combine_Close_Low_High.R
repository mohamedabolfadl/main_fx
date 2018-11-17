rm(list=ls())

library(data.table)



highs<- fread("data/High.csv")
lows<- fread("data/Low.csv")
closes<- fread("data/Close.csv")

closes<-closes[,.(date_hour,Close_00)]
names(closes)<-c("date_hour","entry")

dt_lows<-merge(closes,lows,by="date_hour")
dt_highs<-merge(closes,highs,by="date_hour")


#-- Choose the hours where there is no NA in the close row




#-- Get the first hit of SL or TP in both tables



#-- Merge the tables 



