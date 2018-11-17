# TODO: ADD GLM column
# Script to:
# 1. write csv files containing the AUC for RF and GBM for different trees 
# 2. write a csv containing the feature importance based on Recursive Feature Elimination

## GOOD READS
# Reference of caret on all methods:
# https://topepo.github.io/caret/feature-selection-overview.html
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html
# https://www.analyticsvidhya.com/blog/2016/12/introduction-to-feature-selection-methods-with-an-example-or-how-to-select-the-right-variables/
# Libraries: FSelector , varSelRF



##-------- CARET
# source : http://topepo.github.io/caret/recursive-feature-elimination.html#rfe
# source : https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/


#Read the data
rm(list = ls())
library(readr)
library(caret)
library(data.table)
set.seed(7)

##------------- READING ALL RELEVANT CSV DATA
dataset <- fread("output/ML_ready/dt_buy.csv")
#dataset <- read_csv("data/data_feature_eng.csv", n_max =500)


dataset[,ZigZag:=NULL]

targetName<-"BUY_RES"
x<-dataset[,1:ncol(dataset)-1]
dataset[[targetName]]<-as.factor(dataset[[targetName]])
y<-dataset[[targetName]]


#-------- H2O varImp
library(h2o)     #has rf
myh20 <- h2o.init(nthreads = -1)
#move from R to h2o
datasetH <- as.h2o(x=dataset,
                   destination_frame= "datasetH")
#RF columns (input vs. output)
idxy <- ncol(dataset)
idxx <- 1:ncol(dataset)-1
#split data
#splits <- h2o.splitFrame(datasetH,c(0.8,0.2))     
#train <- h2o.assign(splits[[1]], key="train")   
#valid <- h2o.assign(splits[[2]], key="valid") 


train <- datasetH[1:floor(0.8*nrow(datasetH)),]
valid <- datasetH[(1+floor(0.8*nrow(datasetH))):nrow(datasetH),]


# Trees to check
trees_vec = c(20,50,100,250,500,1000)

# Initializing feature importance dataframe
featimp <-data.frame(matrix(0, nrow = length(names(dataset))-1, ncol = 4))
rownames(featimp) <- setdiff(names(dataset),targetName)
colnames(featimp) <- c("RF","GB","RFE","Overall")

# Initializing performance frame
perfM <-data.frame(matrix(0, nrow = length(trees_vec), ncol = 2))
rownames(perfM) <- as.character(trees_vec)
colnames(perfM) <- c("RF","GB")


for(ntr in trees_vec)
{
  print(ntr)
  #ntr = trees_vec[i]
  
  # make random forest
  my_imp.rf<- h2o.randomForest(y=idxy,x=idxx,
                               training_frame = train,
                               validation_frame = valid,
                               model_id = "my_imp.rf",
                               ntrees=ntr)
  
  # find importance
  imp_rf <- h2o.varimp(my_imp.rf)
  imp_rf
  perf <- h2o.performance(my_imp.rf, valid = TRUE)
  curr_auc<-h2o.auc(perf)
  perfM
  for(fe in imp_rf[,"variable"])
  {
    featimp[fe,1]<-featimp[fe,1]+curr_auc*imp_rf[imp_rf$variable==fe,][3]
  }
  perfM[as.character(ntr),"RF"]<-curr_auc
  ch <-imp_rf$scaled_importance*0.85
  
  # make gradient boost
  my_imp.gb<- h2o.gbm(y=idxy,x=idxx,
                      training_frame = train,
                      validation_frame = valid,
                      model_id = "my_imp.gb",
                      ntrees=ntr)
  
  # find importance
  imp_gb <- h2o.varimp(my_imp.gb)
  imp_gb
  perf <- h2o.performance(my_imp.gb, valid = TRUE)
  curr_auc<-h2o.auc(perf)
  for(fe in imp_rf[,"variable"])
  {
    featimp[fe,2]<-featimp[fe,2]+curr_auc*imp_rf[imp_rf$variable==fe,][3]
  }
  
  perfM[as.character(ntr),"GB"]<-curr_auc
}


png(paste("figs/ntrees_vs_AUC_RF.png"))
plot(rownames(perfM),perfM$RF)
dev.off()

png(paste("figs/ntrees_vs_AUC_GBM.png"))
plot(rownames(perfM),perfM$GB)
dev.off()


write.csv(perfM, file = "res/Tree_Performance.csv")
write.csv(featimp[order(-featimp$GB),], file = "res/feature_importance.csv")








if(FALSE)
{
  # RFE  [Slow and takes only numerical values]
  
  library(caret)
  # define the control using a random forest selection function
  # Options: lmFuncs rfFuncs nbFuncs treebagFuncs caretFuncs
  control <- rfeControl(functions=caretFuncs, method="cv", number=5)
  # run the RFE algorithm
  results <- rfe(x, y, sizes=c(1:5, 10, 15, 20, 25), rfeControl=control)
  # summarize the results
  print(results)
  # list the chosen features
  rnk<-predictors(results)
  sc<-1
  for(fe in rnk)
  {
    featimp[fe,3]<-sc
    sc<-sc-1/length(rnk)
  }
  # plot the results
  plot(results, type=c("g", "o"))
  
  featimp[order(-featimp$GB),]
  write.csv(featimp[order(-featimp$GB),], file = "res/feature_importance.csv")
}




