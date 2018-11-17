rm(list=ls())

library(h2o)
library(data.table)
library(ggplot2)





dt<-fread("output/ML_ready/dt_buy_time.csv")

names(dt)

cols<-setdiff(names(dt),"BUY_RES")
dt_inds <- dt[,..cols]


h2o.init()

dt_hex<-as.h2o(dt)

NN_model<-h2o.deeplearning(x=c("TMS_green","TMS_red","williamsAD","adx","atr","SR_High_1_dist","SR_High_1_strength","SR_High_1_strength_dt"),
                 training_frame=dt_hex,
                 autoencoder = T,
                 hidden = c (8,6,2,6,8),
                 activation = "Tanh",
                 input_dropout_ratio = 0.1,
                 l1=0.001,
                 epochs=500)

train_supervised_features2 = h2o.deepfeatures(NN_model, dt_hex, layer=5)
df = as.data.frame(train_supervised_features2)
df$label = as.character(as.vector(dt_hex[,76]))
names(df)
ggplot(df, aes(x= DF.L5.C1, y = DF.L5.C8)) + geom_jitter(aes(col=label))


dataset <- dt[,c("TMS_green","TMS_red","williamsAD","adx","atr","SR_High_1_dist","SR_High_1_strength_dt","BUY_RES")]
cols<-setdiff(names(dataset),"BUY_RES")

dataset[, (cols) := lapply(.SD, scale), .SDcols=cols]

dataset <- as.data.frame(dataset)
for(j in 1:ncol(dataset)){
#print(j)
    dataset[is.na(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
    dataset[is.infinite(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
    
      }


dataset<-as.data.table(dataset)
library(FactoMineR)
PCA(na.omit(dataset[1:1e3,]))


library(ggfortify)

dt_i <- na.omit(dataset[,..cols])
dt_l <- na.omit(dataset)
#x<-prcomp(dt_c)

autoplot(prcomp(dt_i), data =as.data.frame(dt_l), colour = 'BUY_RES')



library(Rtsne)
cols <- setdiff(names(dataset),"BUY_RES")
dt_i <- na.omit(dataset[,..cols])
dt_l <- na.omit(dataset)

#dt_i <- unique(dt_i)
dt_i_matrix<-as.matrix(dt_i)
tsn<-Rtsne(dt_i_matrix, verbose=T,pca=T,max_iter = 500, preplexity=50)
plot(tsn$Y, col=dt_l$BUY_RES+1,pch=20)

