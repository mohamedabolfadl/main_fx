

# Script to plot the accuracy/AUC/F-1 score vs number of features added

library(h2o)
h2o.init()
h2o.removeAll()
#------------ Importing the dataset ------------  
#train <- h2o.importFile("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_CORR_CORR_CORR.csv")
train <- h2o.importFile("output/ML_ready/dt_buy.csv")
N_data<-20e3

train<-train[(nrow(train)-N_data):nrow(train),]
# Identify predictors and response
y <- "BUY_RES"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
#test[,y] <- as.factor(test[,y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5



##----------- GBM

my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  max_depth = 5,
                  ntrees = 100,
                  learn_rate = 0.1,
                  nfolds = nfolds,
                  seed = 123)



varimp<-h2o.varimp(my_gbm)
varimp$relative_importance<-NULL
varimp$percentage<-NULL


print(varimp)
#Nvars<-nrow(varimp)
Nvars<-nrow(varimp)
i<-1
accuracy_vec <- numeric(Nvars)
auc_vec <- numeric(Nvars)
f1_vec <- numeric(Nvars)
while(i<(Nvars+1))
{
  print(paste(i,"/",Nvars))
  my_gbm <- h2o.gbm(x = varimp[1:i,1],
                    y = y,
                    training_frame = train,
                    distribution = "bernoulli",
                    max_depth = 5,
                    ntrees = 100,
                    learn_rate = 0.1,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,
                    seed = 123)
  
  #perf <- h2o.performance(my_gbm, newdata = test)
  
  accuracy_vec[i] <-my_gbm@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores[4,3]
  auc_vec[i] <- my_gbm@model$cross_validation_metrics_summary[2,1]
  f1_vec[i] <- my_gbm@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores[1,3]
  #auc_vec[i] <- h2o.auc(perf)
  #accuracy_vec[i] <- max(h2o.accuracy(perf)[,2])
  #f1_vec[i] <- max(h2o.F1(perf)[,2])
  print(paste("Accuracy",accuracy_vec[i]))
  print(paste("AUC",auc_vec[i]))
  print(paste("F1",f1_vec[i]))
  
  
  i<-i+1
}


varimp$accuracy<-0
varimp$f1<-0
varimp$auc<-0
varimp[1:Nvars,"accuracy"]<-accuracy_vec
varimp[1:Nvars,"auc"]<-auc_vec
varimp[1:Nvars,"f1"]<-f1_vec


max(accuracy_vec)
max(auc_vec)
max(f1_vec)

png(paste("figs/accuracy_vs_features.png"))
plot(accuracy_vec)
dev.off()

png(paste("figs/auc_vs_features.png"))
plot(auc_vec)
dev.off()

png(paste("figs/f1_vs_features.png"))
plot(f1_vec)
dev.off()

write.csv(varimp, file = "performance_vs_number_of_features_GBM.csv",row.names = FALSE)

##-------------------  GLM

my_glm <- h2o.glm(
  training_frame=train, 
  x=x, 
  y=y, 
  family='binomial',
  nfolds = nfolds,
  seed=123
) 

varimp_GLM<-h2o.varimp(my_glm)
varimp_GLM$relative_importance<-NULL
varimp_GLM$percentage<-NULL



print(varimp_GLM)

#Nvars<-nrow(varimp)
Nvars<-nrow(varimp_GLM)
i<-1
accuracy_vec <- numeric(Nvars)
auc_vec <- numeric(Nvars)
f1_vec <- numeric(Nvars)
while(i<(Nvars))
{
  print(i)
  if(varimp_GLM[i,1] %in% names(train))
  {
    my_glm <- h2o.glm(
      training_frame=train, 
      x=varimp_GLM[1:i,1], 
      y=y, 
      family='binomial',
      nfolds = nfolds,
      seed=123
    ) 
    
    
    accuracy_vec[i] <-my_glm@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores[4,3]
    auc_vec[i] <- my_glm@model$cross_validation_metrics_summary[2,1]
    f1_vec[i] <- my_glm@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores[1,3]
    #auc_vec[i] <- h2o.auc(perf)
    #accuracy_vec[i] <- max(h2o.accuracy(perf)[,2])
    #f1_vec[i] <- max(h2o.F1(perf)[,2])
  }
  i<-i+1
}


varimp_GLM$accuracy<-0
varimp_GLM$f1<-0
varimp_GLM$auc<-0

varimp_GLM[1:Nvars,"accuracy"]<-accuracy_vec
varimp_GLM[1:Nvars,"auc"]<-auc_vec
varimp_GLM[1:Nvars,"f1"]<-f1_vec


max(accuracy_vec)
max(auc_vec)
max(f1_vec)

png(paste("figs/accuracy_vs_features_GLM.png"))
plot(accuracy_vec)
dev.off()

png(paste("figs/auc_vs_features_GLM.png"))
plot(auc_vec)
dev.off()

png(paste("figs/f1_vs_features_GLM.png"))
plot(f1_vec)
dev.off()

write.csv(varimp_GLM, file = "performance_vs_number_of_features_GLM.csv",row.names = FALSE)


