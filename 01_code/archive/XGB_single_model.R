#-- Script for XGB
# NOTES:
# 1. In time series, never do in sample validation
# For categorial variables, first binarize the variable, since xgboost does not accept factors or strings



rm(list = ls())
library(xgboost)
library(ModelMetrics)
library(data.table)
library(readr)

set.seed(123)
#get_r2 <- function (x, y) cor(x, y) ^ 2
get_r2 <- function(x, y) summary(lm(y~x))$r.squared

PF<-2

profitPips<-function(y_pred,y_actual)
{
  
  th_vec <- seq(0,1,0.01)
  maxpips<- -99
  
  for(th in th_vec)
  { 
    dt <- data.table(actual=y_actual,pred_cont=y_pred)
    dt[,pred_decision:=pred_cont>th][,pred_decision:=as.integer(pred_decision)][pred_decision==1,CORRECT:=pred_decision==actual]
    dt_sel<-dt[!is.na(CORRECT)]
    pips<-PF*nrow(dt_sel[CORRECT==TRUE])-nrow(dt_sel)
    if(pips>maxpips)
    {
      maxpips<-pips
      th_max <-th
    }
  }
  
  return(c(maxpips,th_max))
}


# Function to binarize categorial variables
Binarize_Features <- function(data_set, text_features,features_to_ignore=c(),  leave_out_one_level=FALSE) {
  #text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
  #text_features <- c(names(data_set[sapply(data_set, is.character)]))
  
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


#-- CONTROL PANEL: 

targetName<-"BUY_RES"
SHUFFLE_FLAG<-TRUE
BINARIZE_FEATURES_FLAG<-TRUE
SKIP_EARLY_FLAG <- TRUE
SKIP_SAMPLE_SIZE <- 40e3
ratio_train<-0.8
ratio_test<-0.1

#-- Specific columns
#select_cols<-c("williamsAD","EMA_200","adx","VHF","chaikinVolatility","atr","BUY_RES")
#select_cols<-c("williamsAD","EMA_200","adx","runVar","atr","SAR","VHF","Time_weekday","Time_hour","BUY_RES")
#select_cols<-c("williamsAD","VHF","EMA_200","EMA_100","BUY_RES")
#select_cols<-c("EMA,"BUY_RES")

#dataset <- fread("output/ML_ready/dt_buy.csv",select = select_cols)
dataset <- fread("output/ML_ready/dt_buy.csv")




#-- skip the first N samples as they are too long ago
if(SKIP_EARLY_FLAG)
{
  dataset<-dataset[SKIP_SAMPLE_SIZE:nrow(dataset),]
}

#-- remove lagging
names_sel <- setdiff(names(dataset), c("ZigZag","DPO"))
dataset<-dataset[,..names_sel]

#-- Classification or regression
problem_type<-"binary:logistic"
n_class <- 2
# Other options: 
# Binary classification:        binary:logistic
# Multiclass classification:    multi:softmax   must also define num_class
# Logistic regression:          reg:logistic
# Linear regression:            reg:linear



#-- Binarize variables
range_cols <- names(dataset)[grep("_reg",names(dataset))]
binary_features <- c("Time_weekday","Time_hour",range_cols)

if(BINARIZE_FEATURES_FLAG)
{
  if(binary_features %in% names(dataset))
  {
    dataset[,':='(Time_weekday=as.character(Time_weekday),Time_hour=as.character(Time_hour))]
    
  }
  dataset<-as.data.frame(dataset)
  dataset<-Binarize_Features(dataset, text_features=binary_features)
  
}else{
  dataset<-as.data.frame(dataset)
  
}



#-- Split data into train, CV and test
dataset_CV <- dataset[    floor(ratio_train*nrow(dataset)):floor((ratio_train+ratio_test)*nrow(dataset)),  ]
dataset_test <- dataset[    (1+floor((ratio_train+ratio_test)*nrow(dataset))):nrow(dataset),  ]
dataset <- dataset[1:(-1+floor(ratio_train*nrow(dataset))),]

#-- Shuffle training set
if(SHUFFLE_FLAG)
{
  dataset<-dataset[sample(nrow(dataset),nrow(dataset)),]
}

#-- Sending the target to the end
dataset<-target_to_the_end(dataset,targetName)
dataset_CV<-target_to_the_end(dataset_CV,targetName)
dataset_test<-target_to_the_end(dataset_test,targetName)

#-- Renaming the target name
names(dataset)[ncol(dataset)]<-"Target"
names(dataset_CV)[ncol(dataset_CV)]<-"Target"
names(dataset_test)[ncol(dataset_test)]<-"Target"

#-- Index of the target
N_target<-ncol(dataset)


  params <- expand.grid(
    eta=c(0.01),                #-- Learning rate
    nrounds=c(100),          #-- Number of trees        
    gamma=c(0),
    max.depth=c(6),             #-- Maximum depth of the tree     
    colsample_bytree = c(1) ,    #-- Sample per tree
    subsample = c(1),
    lambda = c(0),
    min_child_weight=c(1),
    lambda_bias = c(0),
    alpha=c(0)
  )
  

  #-- Extract train and test and remove the cv_i column
  CV<-dataset_CV
  train <-dataset
  
  #-- Finally build the classifier
  classifier = xgboost(data = as.matrix(train[-N_target]), label = train$Target,
                       colsample_bytree = params[1,"colsample_bytree"],
                       nrounds = params[1,"nrounds"],
                       #early_stopping_rounds=100,
                       max.depth = params[1,"max.depth"],
                       eta = params[1,"eta"],
                       gamma = params[1,"gamma"],
                       subsample = params[1,"subsample"],
                       lambda = params[1,"lambda"],
                       min_child_weight=params[1,"min_child_weight"],
                       lambda_bias = params[1,"lambda_bias"],
                       alpha=params[1,"alpha"],
                       nthread = 8,  objective = problem_type, verbose = FALSE)
  
  y_pred_train = predict(classifier, newdata = as.matrix(train[-N_target]))
  
  print(paste0("AUC_train : ",auc(train$Target, y_pred_train)))
  
  
  y_pred = predict(classifier, newdata = as.matrix(CV[-N_target]))
  
  
  #-- Get the AUC
  metric_cum<-auc(CV$Target, y_pred)
  
  print(paste0("AUC (test) : ",auc(CV$Target, y_pred)))
  print(paste0("ProfitPips : ",profitPips(y_actual=CV$Target, y_pred=y_pred)[1]))
  
  #metric_cum<-profitPips(y_actual=CV$Target, y_pred=y_pred)[1]
  

    test<-dataset_test
    y_pred = predict(classifier, newdata = as.matrix(test[-N_target]))
    print(paste0("Test metric : ",profitPips(y_actual=test$Target,y_pred=y_pred)[1]))
    
    
    

    y_actual=test$Target
    th_vec <- seq(0,1,0.01)
    th_vec <- 0.47
    
    maxpips<- -99
    
    for(th in th_vec)
    { 
      dt <- data.table(actual=y_actual,pred_cont=y_pred)
      dt[,pred_decision:=pred_cont>th][,pred_decision:=as.integer(pred_decision)][pred_decision==1,CORRECT:=pred_decision==actual]
      dt_sel<-dt[!is.na(CORRECT)]
      pips<-PF*nrow(dt_sel[CORRECT==TRUE])-nrow(dt_sel)
      if(pips>maxpips)
      {
        maxpips<-pips
        th_max <-th
      }
    }
    
