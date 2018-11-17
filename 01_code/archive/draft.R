


library(data.table)


dt <- fread("02_data/input/df_xts_r.csv")



randomSamples <- sample(nrow(dt),20)

dt_sel <- dt[randomSamples,]



dt_o<-read.csv("02_data/input/df_xts_r.csv")

dt_sel_o <- dt_o[randomSamples,]





