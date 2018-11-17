#-- Script for XGB
# NOTES:
# 1. In time series, never do in sample validation
# For categorial variables, first binarize the variable, since xgboost does not accept factors or strings



rm(list = ls())

set.seed(123)


#-- Input parameters
pipsize<-0.0001
PF<-1
SL<-15*pipsize
SPREAD<-2*pipsize
#-- CONTROL PANEL: 

targetName<-"BUY_RES"
SHUFFLE_FLAG<-TRUE
BINARIZE_FEATURES_FLAG<-TRUE
SKIP_EARLY_FLAG <- TRUE
SINGLE_PARAM<-TRUE
SCALE_VARS<-FALSE
LAST_PART_PERCENTAGE <- 0.9 # Last percentage to be selected form the data...start 2000 till end 2015 -> 16 years
ratio_train<-0.7
ratio_test<-0.15

library(xgboost)
library(ModelMetrics)
library(data.table)
library(readr)
library(stringi)

#--- Directory definitoins
date_code <- as.character(Sys.time())
date_code <- paste0(substr(date_code,9,10),substr(date_code,6,7),substr(date_code,3,4))
data_output_dir<-paste0("02_data/output/",date_code,"/")
data_input_dir<-paste0("02_data/input/",date_code,"/")
data_intermediate_dir<-paste0("02_data/intermediate/",date_code,"/")
models <- paste0("03_models/prod/",date_code,"/")
models_XGB<-paste0(models,"XGB/")
models_NN<-paste0(models,"NN/")
models_RF<-paste0(models,"RF/")
models_GLM<-paste0(models,"GLM/")

#-- Create directories
dir.create(file.path(data_output_dir))
dir.create(file.path(data_input_dir))
dir.create(file.path(data_intermediate_dir))
dir.create(file.path(models))
dir.create(file.path(models_XGB))
dir.create(file.path(models_GLM))
dir.create(file.path(models_NN))
dir.create(file.path(models_RF))





#-- Func to calculate profit pips
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
    #print(pips)
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



#-- Specific columns
#select_cols<-c("williamsAD","EMA_200","EMA_50","EMA_800","EMA_1000","SMA_200","SMA_1000","adx","VHF","chaikinVolatility","atr","BUY_RES")
#select_cols<-c("EMA_200","EMA_50","EMA_800","EMA_1000","BUY_RES")
#select_cols<-c("williamsAD","EMA_200","adx","runVar","atr","SAR","VHF","Time_weekday","Time_hour","BUY_RES")
#select_cols<-c("williamsAD","VHF","EMA_200","EMA_100","BUY_RES")
#select_cols<-c("EMA,"BUY_RES")

#dataset <- fread("output/ML_ready/dt_buy.csv",select = select_cols)
dataset <- fread(paste0(data_intermediate_dir,"BUY_SL_",SL/pipsize,"_PF_",PF,"_SPREAD_",SPREAD/pipsize,".csv"))

if(SCALE_VARS)
{
  
  cols <- setdiff(names(dataset),targetName)
  dataset[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
  #dataset$atr<-scale(dataset$atr)
  #dataset[,.SD = lapply(scale,.SD),.SDcols=names(dataset)]
}



#-- skip the first N samples as they are too long ago
if(SKIP_EARLY_FLAG)
{
  dataset<-dataset[floor((1-LAST_PART_PERCENTAGE )*nrow(dataset)):nrow(dataset),]
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



#-- Checking if the target name is correct
if( !(targetName%in% names(dataset)))
{
  print("Wrong target name!")
  print(paste0("Your input:",targetName))
  print(paste0("Closest name is ",names(dataset)[which.min(adist(targetName,names(dataset)))]))
  targetName<-names(dataset)[which.min(adist(targetName,names(dataset)))]
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

#-- Save the files for h2o usage
#fwrite(dataset,"output/buy_train_data.csv")
#fwrite(dataset_CV,"output/buy_validation_data.csv")
#fwrite(dataset_test,"output/buy_test_data.csv")


#-- Index of the target
N_target<-ncol(dataset)

#-- Cross Validation folds 
params <- expand.grid(
  eta=c(00.007),                #-- Learning rate
  nrounds=c(75),          #-- Number of trees        
  
  gamma=c(15),
  max.depth=c(4),             #-- Maximum depth of the tree     
  min_child_weight = c(1),
  
  colsample_bytree = c(0.6) ,    #-- Sample per tree
  subsample = c(0.5),
  
  lambda = c(2),
  lambda_bias = c(0.0001),
  alpha=c(0.0001)
)

#-- Initialize the metric of each parameter set
params$metric<- 99999

params<-params[sample(nrow(params),nrow(params)),]



#-- Only in case of binary classification, we need the AUC to be high so we initialize the metric to 0
if(problem_type == "binary:logistic")
{
  params$metric<- 0
  
}



j<-1

#-- best metric to find the absolute best
best_metric<- -999




while(j<(1+nrow(params)))
{
  
  
  i<-0
  
  #-- Cumulative metric performance
  metric_cum<-0
  
  

    #-- Extract train and test and remove the cv_i column
    CV<-dataset_CV
    train <-dataset
    
    #-- Finally build the classifier
    classifier = xgboost(data = as.matrix(train[-N_target]), label = train$Target,
                         colsample_bytree = params[j,"colsample_bytree"],
                         nrounds = params[j,"nrounds"],
                         max.depth = params[j,"max.depth"],
                         eta = params[j,"eta"],
                         gamma = params[j,"gamma"],
                         min_child_weight = params[j,"min_child_weight"],
                         subsample = params[j,"subsample"],
                         lambda = params[j,"lambda"],
                         lambda_bias = params[j,"lambda_bias"],
                         alpha=params[j,"alpha"],
                         nthread = 8,  objective = problem_type, verbose = FALSE)

    
    
    y_pred = predict(classifier, newdata = as.matrix(CV[-N_target]))
    
    
     #-- Get the AUC
     metric_cum<-auc(CV$Target, y_pred)
     #metric_cum<-profitPips(y_actual=CV$Target, y_pred=y_pred)[1]
      

#-- Store the AUC
      params[j,"metric"]<-metric_cum

        if( metric_cum > best_metric)
  {
          thresh <- profitPips(y_actual=CV$Target, y_pred=y_pred)[2]
          
              print(params[j,])
          y_pred_train = predict(classifier, newdata = as.matrix(train[-N_target]))
          print(paste0("AUC (train):",auc(train$Target, y_pred_train)))   
          
                 print(paste0("AUC (test):",auc(CV$Target, y_pred)))  
                 
                 test_1_preds <- as.integer(y_pred >thresh )

                 
                 ModelMetrics::confusionMatrix(CV$Target,y_pred,thresh)
                 
           best_metric<-metric_cum
    bst_classifier<-classifier
    best_pred <- y_pred
    print(metric_cum)
    feat_imp <- xgb.importance(feature_names = names(train), model = bst_classifier)
    print(xgb.importance(feature_names = names(train), model = bst_classifier))
    #xgb.save(bst_classifier, 'models/xgb.model')

    test<-dataset_test
    y_pred = predict(classifier, newdata = as.matrix(test[-N_target]))
    #print(paste0("Test metric : ",profitPips(y_actual=test$Target,y_pred=y_pred)[1]))
    print(paste0("AUC (test 2): ",auc(test$Target,y_pred)))
    
    
        print("-----------------------------------")
    
      }
 
      print(paste0(100*j/nrow(params),"%")) 
  j<-j+1
  
}



thrs<-seq(0.01,1,0.01)
max_acc <-0
dt_res<-data.table(Target=CV$Target,y_pred=y_pred)

for (th in thrs)
{
  #th<-0.15
  dt_res[,pred:=0]
  dt_res[y_pred>th,pred:=1]
  
  if(nrow(dt_res[pred==1 & Target==1])>0)
  {  
    acc<-100*nrow(dt_res[pred==1 & Target==1])/nrow(dt_res[pred==1])
    if(acc>max_acc)
    {
      max_acc<-acc
      max_th<-th
      print(max_th)
      print(max_acc)
    }
    
  }
}

th<-max_th
dt_res[,pred:=0]
dt_res[y_pred>th,pred:=1]

print("Correct preds:")
nrow(dt_res[pred==1 & Target==1])
print("Wrong preds:")
nrow(dt_res[pred==1 & Target!=1])



