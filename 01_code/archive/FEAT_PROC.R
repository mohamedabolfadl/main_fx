rm(list=ls())

set.seed(123)

library(h2o)
library(data.table)
library(ggplot2)

date_code <- as.character(Sys.time())
date_code <- paste0(substr(date_code,9,10),substr(date_code,6,7),substr(date_code,3,4))
date_code <- "010718"
data_output_dir<-paste0("02_data/output/",date_code,"/")
data_input_dir<-paste0("02_data/input/",date_code,"/")
data_intermediate_dir<-paste0("02_data/intermediate/",date_code,"/")
models <- paste0("03_models/prod/",date_code,"/")
models_XGB<-paste0(models,"XGB/")
models_NN<-paste0(models,"NN/")
models_RF<-paste0(models,"RF/")
models_GLM<-paste0(models,"GLM/")



#-- Input parameters


pipsize<-0.0001
PF<-2
SL<-15*pipsize
SPREAD<-1*pipsize

N_output <- 3
N_layers <- 4



#dt<-fread(paste0(data_intermediate_dir,"BUY_SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,".csv"))
dt<-fread(paste0(data_intermediate_dir,"dt_with_indicators.csv"))


dt_tm <- dt$index
names(dt)

cols<-setdiff(names(dt),c("BUY_RES","index","Time"))
dt_inds <- dt[,..cols]





h2o.init()

dt_hex<-as.h2o(dt)
layers <- c(rev(floor(seq(N_output,ncol(dt_inds),length.out = 2+N_layers))),floor(seq(N_output,ncol(dt_inds),length.out = 2+N_layers))[2:(2+N_layers)] )

NN_model<-h2o.deeplearning(x=cols,
                 training_frame=dt_hex,
                 autoencoder = T,
                 hidden = layers,
                 activation = "Tanh",
                 epochs=500)

h2o.performance(NN_model)

train_supervised_features2 = h2o.deepfeatures(NN_model, dt_hex, layer=(length(layers)+1)/2)

dt_save <- cbind(dt_tm,as.data.table(train_supervised_features2))
names(dt_save)[1] <- "Time"

fwrite(dt_save,paste0(data_intermediate_dir,"dt_with_indicators_AE_",N_output,".csv"))



#-- Plot
# df = as.data.frame(train_supervised_features2)
# df$label = as.character(as.vector(dt_hex[,length(names(dt_hex))]))
# names(df)
# ggplot(df, aes(x= DF.L1.C1, y = DF.L1.C2)) + geom_jitter(aes(col=label))





#dataset <- dt[,c("TMS_green","TMS_red","williamsAD","adx","atr","SR_High_1_dist","SR_High_1_strength_dt","BUY_RES")]
dataset <- dt
cols<-setdiff(names(dataset),"BUY_RES")

dataset[, (cols) := lapply(.SD, scale), .SDcols=cols]

dataset <- as.data.frame(dataset)
for(j in 1:ncol(dataset)){
#print(j)
    dataset[is.na(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
    dataset[is.infinite(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
    
      }


dataset<-as.data.table(dataset)


library(ggfortify)

dt_i <- na.omit(dataset[,..cols])
dt_l <- na.omit(dataset)
#x<-prcomp(dt_c)

autoplot(prcomp(dt_i), data =as.data.frame(dt_l), colour = 'BUY_RES')



library(Rtsne)
cols <- setdiff(names(dataset),"BUY_RES")
dt_i <- dataset[,..cols]
dt_l <- dataset

randSamps <- sample(nrow(dt_l),1e4)

dt_i_matrix<-as.matrix(dt_i[randSamps,])
tsn<-Rtsne(dt_i_matrix, verbose=T,pca=T,max_iter = 500, preplexity=30)
plot(tsn$Y, col=dt_l[randSamps,BUY_RES]+1,pch=20)

