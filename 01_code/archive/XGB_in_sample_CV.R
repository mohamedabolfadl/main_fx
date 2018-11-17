#-- Script for XGB
# NOTES:
# 1. In time series, never do in sample validation
# For categorial variables, first binarize the variable, since xgboost does not accept factors or strings



rm(list = ls())
library(xgboost)
library(ModelMetrics)
library(readr)
set.seed(123)


#get_r2 <- function (x, y) cor(x, y) ^ 2
get_r2 <- function(x, y) summary(lm(y~x))$r.squared

# Function to binarize categorial variables
Binarize_Features <- function(data_set, features_to_ignore=c(), leave_out_one_level=FALSE) {
  text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
  print(text_features)
  for (feature_name in setdiff(text_features, features_to_ignore)) {
    feature_vector <- as.character(data_set[[feature_name]])
    # check that data has more than one level
    if (length(unique(feature_vector)) == 1)
      #print(feature_name)
      next
    # We set any non-data to text
    feature_vector[is.na(feature_vector)] <- 'NA'
    feature_vector[is.infinite(feature_vector)] <- 'INF'
    feature_vector[is.nan(feature_vector)] <- 'NAN'
    # loop through each level of a feature and create a new column
    first_level=TRUE
    for (newcol in unique(feature_vector)) {
      if (first_level && leave_out_one_level) {
        # avoid dummy trap and skip first level
        first_level=FALSE
      } else {
        data_set[,paste0(feature_name,"_",newcol)] <- ifelse(feature_vector==newcol,1,0)
      }
    }
    # remove original feature
    data_set <- data_set[,setdiff(names(data_set),feature_name)]
  }
  return (data_set)
}


target_to_the_end <- function(data_set,targ_Name)
{
  tmp <-data.frame(data_set[,targ_Name])
  colnames(tmp)<-targ_Name
  data_set[,targ_Name]<-NULL
  dataset_ret <- cbind(data_set, tmp)
  return(dataset_ret)
  
}


#-- INPUT HERE: 
dataset <- read_csv(file="regression.csv")
targetName<-"ElantrSales"

#-- If you want to threshold a regression result
#dataset$Binary<-0
#dataset[dataset$ElantraSales>20000,"Binary"]<-1
#dataset[dataset$ElantraSales<10000,"Binary"]<- 2
#dataset$ElantraSales<-NULL
#targetName<-"Binary"

#-- Binarize variables
#dataset$Month <- as.factor(dataset$Month)
#dataset<-Binarize_Features(dataset)

#-- Classification or regression
problem_type<-"reg:linear"

# Other options: 
# Binary classification:        binary:logistic
# Multiclass classification:    multi:softmax   must also define num_class
# Logistic regression:          reg:logistic
# Linear regression:            reg:linear

n_class <- 3


#-- Checking if the target name is correct
if( !(targetName%in% names(dataset)))
{
  print("Wrong target name!")
  print(paste0("Your input:",targetName))
  print(paste0("Closest name is ",names(dataset)[which.min(adist(targetName,names(dataset)))]))
  targetName<-names(dataset)[which.min(adist(targetName,names(dataset)))]
}




#-- Sending the target to the end
dataset<-target_to_the_end(dataset,targetName)

#-- Renaming the target name
names(dataset)[ncol(dataset)]<-"Target"

#-- Index of the target
N_target<-ncol(dataset)

#-- Cross Validation folds 
#-- CAREFUL WITH TIME SERIES NO CV ARE ALLOWED WITHIN TRAINING!!! JUST TRAIN AND TEST
k<-5

#-- Search grid for the XGB parameters
params <- expand.grid(
  eta=c(0.1,0.3,0.01),                #-- Learning rate
  nrounds=c(50,100,150,200),          #-- Number of trees        
  max.depth=c(2,5,10,15),             #-- Maximum depth of the tree     
  colsample_bytree = c(0.5,0.8,1)     #-- Sample per tree
)

#-- Initialize the metric of each parameter set
params$metric<- 99999

#-- Only in case of binary classification, we need the AUC to be high so we initialize the metric to 0
if(problem_type == "binary:logistic")
{
  params$metric<- 0
 
}

#-- Add the IDs of the cross validation folds as a column
dataset$cv_i<-seq(1:nrow(dataset))%%k


#-- Two column dataset with target and the CV ID
dataset_with_cv <- dataset
dataset_with_cv[, setdiff(names(dataset_with_cv),c("cv_i",targetName)) ]<-NULL



j<-0

#-- best metric to find the absolute best
best_metric<-99999

#-- Handle AUC as an opposite case
if(problem_type == "binary:logistic" | problem_type == "reg:linear")
{
  best_metric<- 0
  
}

while(j<nrow(params))
{
  
  
  i<-0
  
  #-- Cumulative metric performance
  metric_cum<-0
  
  #-- Initialize array with the results of the CV folds
  res<-numeric(k)
  
  #-- Loop throught the k folds
  while (i<k)
  {
    
    #-- Extract train and test and remove the cv_i column
    test<-dataset[dataset$cv_i==i , setdiff(names(dataset),"cv_i")   ]
    train <-dataset[dataset$cv_i!=i , setdiff(names(dataset),"cv_i") ]
    
    #-- Finally build the classifier
   if(problem_type=="multi:softmax")
   {
     classifier = xgboost(data = as.matrix(train[-N_target]), label = train$Target, colsample_bytree = params[1+j,"colsample_bytree"], nrounds = params[1+j,"nrounds"],  max.depth = params[1+j,"max.depth"], eta = params[1+j,"eta"],  nthread = 8,  objective = problem_type, verbose = FALSE, num_class = n_class)
   }
     else
    {
      classifier = xgboost(data = as.matrix(train[-N_target]), label = train$Target, colsample_bytree = params[1+j,"colsample_bytree"], nrounds = params[1+j,"nrounds"],  max.depth = params[1+j,"max.depth"], eta = params[1+j,"eta"],  nthread = 8,  objective = problem_type, verbose = FALSE)
      
    }
     
     
    
    y_pred = predict(classifier, newdata = as.matrix(test[-N_target]))
    
    dataset_with_cv[dataset_with_cv$cv_i==i,paste0("CV_XGB_",j)]<-y_pred
    
    if(problem_type == "binary:logistic"){
    metric_cum<-metric_cum+auc(test$Target, y_pred)
    }else{
      
      if(problem_type=="num_class")
      {
        metric_cum<-metric_cum+ModelMetrics::mlogLoss(test$Target, y_pred)
      }
      else{
        #metric_cum<-metric_cum+ModelMetrics::rmse(test$Target, y_pred)
        metric_cum<-metric_cum+get_r2(test$Target, y_pred)
        
      }
      
    }
    
    i<-i+1
  }
  params[j+1,"metric"]<-metric_cum/k 
  if(( problem_type == "reg:linear" | problem_type == "binary:logistic") & (metric_cum/k) > best_metric)
  {
    print(params[j+1,])
    print(metric_cum/k)
    print("-----------------------------------")
    best_metric<-metric_cum/k
    bst_classifier<-classifier
    best_pred <- y_pred
      }else{
    
    if( problem_type != "binary:logistic"  & problem_type != "reg:linear" & (metric_cum/k) < best_metric )
    {
      print(params[j+1,])
      print(metric_cum/k)
      print("-----------------------------------")
      best_metric<-metric_cum/k
      bst_classifier<-classifier
      best_pred <- y_pred
    }
    
  }
  
  j<-j+1
  
}


print(xgb.importance(feature_names = names(train), model = bst_classifier))

#-- Setup dataframe with the results
df_res<-data.frame(pred=best_pred,actual=test$Target,refrence=test$Target)


#-- Plot actual vs predicted for regression
if( problem_type == "reg:linear" ){
  library(ggplot2)
  ggplot(df_res)+geom_point(aes(x=actual,y=pred,color='Predictions'))+geom_line(aes(x=actual,y=actual,color='Reference'))
}

