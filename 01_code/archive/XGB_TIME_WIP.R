#-- Script for XGB
# NOTES:
# 1. In time series, never do in sample validation
# For categorial variables, first binarize the variable, since xgboost does not accept factors or strings



rm(list = ls())
library(xgboost)
library(ModelMetrics)
library(data.table)
library(readr)

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
PF<-2
set.seed(123)
#-- CONTROL PANEL: 
#-- Classification or regression
problem_type<-"binary:logistic"
n_class <- 2
targetName<-"BUY_RES"
SHUFFLE_FLAG<-TRUE
BINARIZE_FEATURES_FLAG<-TRUE
SKIP_EARLY_FLAG <- TRUE
SINGLE_PARAM<-TRUE
SCALE_VARS<-FALSE
#LAST_PART_PERCENTAGE <- 0.9 # Last percentage to be selected form the data...start 2000 till end 2015 -> 16 years
#ratio_train<-0.8
#ratio_test<-0.1
CV_k <- 5
test_size <- 1/(CV_k+2) # k test blocks in CV region + 1 time test + 1 block train


#-- Specific columns
#select_cols<-c("williamsAD","EMA_200","EMA_50","EMA_800","EMA_1000","SMA_200","SMA_1000","adx","VHF","chaikinVolatility","atr","BUY_RES")
#select_cols<-c("EMA_200","EMA_50","EMA_800","EMA_1000","BUY_RES")
#select_cols<-c("williamsAD","EMA_200","adx","runVar","atr","SAR","VHF","Time_weekday","Time_hour","BUY_RES")
#select_cols<-c("williamsAD","VHF","EMA_200","EMA_100","BUY_RES")
#select_cols<-c("EMA,"BUY_RES")
#select_cols<-c("TMS_green","TMS_red","BUY_RES_WINS","BUY_RES")

#dataset <- fread("output/ML_ready/dt_buy_time.csv",select = select_cols)
dataset <- fread(paste0(data_intermediate_dir,"BUY_SL_20_PF_1_SPREAD_1.csv"))

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




if(SCALE_VARS)
{
  
  cols <- setdiff(names(dataset),targetName)
  dataset[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
}



#-- skip the first N samples as they are too long ago
if(SKIP_EARLY_FLAG)
{
  dataset<-dataset[floor((1-LAST_PART_PERCENTAGE )*nrow(dataset)):nrow(dataset),]
}





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

#-- Adding the prediction column
#-- Attaching the CV id column



dataset<-target_to_the_end(dataset,targetName)
names(dataset)[ncol(dataset)]<-"Target"


library(corrplot)
corrplot(as.matrix(cor(dataset)))

#-- Index of the target
#N_target<-ncol(dataset)

#-- Cross Validation folds 

params <- expand.grid(
  eta=c(0.1),                #-- Learning rate
  nrounds=c(200),          #-- Number of trees        
  
  gamma=c(0),
  max.depth=c(6),             #-- Maximum depth of the tree     
  min_child_weight = c(1),
  
  colsample_bytree = c(1) ,    #-- Sample per tree
  subsample = c(1),
  
  lambda = c(0),
  lambda_bias = c(0),
  alpha=c(0)
)

params <- as.data.table(params)

#-- shuffle the parameters
params<-params[sample(nrow(params),nrow(params)),]


dataset <- as.data.table(dataset)
#-- Include the CV indicies
dataset$CV_i <-  1+floor( seq(1,nrow(dataset))/round(nrow(dataset)/(CV_k+2)))

#-- Extract hold out test
dataset_test <- dataset[CV_i == max(dataset$CV_i)]

#-- Extract training set
dataset <- dataset[CV_i < max(dataset$CV_i)]

#-- Initialize best metric
best_metric<- -9999.0

#-- Features
feats<-setdiff(names(dataset),c("Target","CV_i"))

j<-1L

while(j<(1+nrow(params)))
{
  
  
  i<-1
  
  #-- Cumulative metric performance
  metric_sum<-0

  while( i <  (CV_k+1) )
  {
    
    train <- dataset[CV_i==i]
    test <- dataset[CV_i==(i+1)]
    
    classifier = xgboost(data = as.matrix(train[,..feats]), label = train$Target,
                         colsample_bytree = params[j,colsample_bytree],
                         nrounds = params[j,nrounds],
                         max.depth = params[j,max.depth],
                         eta = params[j,eta],
                         gamma = params[j,gamma],
                         min_child_weight = params[j,min_child_weight],
                         subsample = params[j,subsample],
                         lambda = params[j,lambda],
                         lambda_bias = params[j,lambda_bias],
                         alpha=params[j,alpha],
                         nthread = 8,
                         objective = problem_type, verbose = FALSE)
    
    
    curr_auc <- auc(test$Target,predict(classifier, newdata = as.matrix(test[,..feats])))
    set(params, j, paste0("CV_",i), curr_auc)
    metric_sum<-i*curr_auc + metric_sum
    
    i<- i + 1L
    
  }
  

  classifier_test = xgboost(data = as.matrix(dataset[ ,..feats]), label = dataset$Target,
                       colsample_bytree = params[j,colsample_bytree],
                       nrounds = params[j,nrounds],
                       max.depth = params[j,max.depth],
                       eta = params[j,eta],
                       gamma = params[j,gamma],
                       min_child_weight = params[j,min_child_weight],
                       subsample = params[j,subsample],
                       lambda = params[j,lambda],
                       lambda_bias = params[j,lambda_bias],
                       alpha=params[j,alpha],
                       nthread = 8,
                       objective = problem_type, verbose = FALSE)
  
  
  
  #
  params[j,"CV_avg"]<-metric_sum/sum(seq(1,CV_k))
  params[j,"metric_test"]<-auc(dataset_test$Target,predict(classifier_test, newdata = as.matrix(dataset_test[,..feats])))
  
  
  
  
  
  
  
  if( metric_sum/sum(seq(1,CV_k)) > best_metric)
  {
    print(params[j,])
    
    # y_pred_train = predict(classifier, newdata = as.matrix(train[-N_target]))
    # print(paste0("AUC (train):",auc(train$Target, y_pred_train)))   
    #  print(paste0("AUC (test):",auc(CV$Target, y_pred)))  
    bst_classifier <- classifier
    
    best_metric<-metric_sum/sum(seq(1,CV_k))
    #feat_imp <- xgb.importance(feature_names = feats, model = bst_classifier)
    print(xgb.importance(feature_names = feats, model = bst_classifier))
    #xgb.save(bst_classifier, 'models/xgb.model')
    
    
    print("-----------------------------------")
    
  }
  
  print(paste0(100*j/nrow(params),"%")) 
  j<-j+1L
  
}





#---- RUN TO HERE







library(ROCR)
#data(ROCR.simple)
pred <- prediction( dataset_with_preds$y_pred ,dataset_with_preds$Target)

#-- ROC curve
perf <- performance(pred,"tpr","fpr")
plot(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])


perf <- performance(pred,"acc")
plot(perf)
cutoffs_accuracy <- data.frame(cut=perf@y.values[[1]], acc=perf@x.values[[1]])
str(perf)


thrs<-seq(0.01,1,0.01)
max_acc <-0
dt_res<-dataset_with_preds[,.(Target,y_pred)]

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

th<-max_acc
dt_res[,pred:=0]
dt_res[y_pred>th,pred:=1]

print("Correct preds:")
nrow(dt_res[pred==1 & Target==1])
print("Wrong preds:")
nrow(dt_res[pred==1 & Target!=1])



#
dataset <- as.data.frame(dataset)
for(j in 1:ncol(dataset)){
  #print(j)
  dataset[is.na(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
  dataset[is.infinite(dataset[,j]), j] <- mean(dataset[,j], na.rm = TRUE)
  
}

#-- Correlations
library(qtlcharts)
iplotCorr(mat=dataset[1e4:2e4,], group = dataset[1e4:2e4,"Target"], reorder=T)


library(corrr)
library(ggplot2)
rplot(correlate(dataset[1e4:2e4,]))


