#-- Script for XGB
# NOTES:
# 1. In time series, never do in sample validation
# For categorial variables, first binarize the variable, since xgboost does not accept factors or strings



rm(list = ls())
library(xgboost)
library(ModelMetrics)
library(data.table)
library(readr)


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
LAST_PART_PERCENTAGE <- 0.3 # Last percentage to be selected form the data...start 2000 till end 2015 -> 16 years
ratio_train<-0.8
ratio_test<-0.1
SPLIT_TRAIN_TEST<-FALSE
#-- Specific columns
#select_cols<-c("williamsAD","EMA_200","EMA_50","EMA_800","EMA_1000","SMA_200","SMA_1000","adx","VHF","chaikinVolatility","atr","BUY_RES")
#select_cols<-c("EMA_200","EMA_50","EMA_800","EMA_1000","BUY_RES")
#select_cols<-c("williamsAD","EMA_200","adx","runVar","atr","SAR","VHF","Time_weekday","Time_hour","BUY_RES")
#select_cols<-c("williamsAD","VHF","EMA_200","EMA_100","BUY_RES")
#select_cols<-c("EMA,"BUY_RES")
#select_cols<-c("TMS_green","TMS_red","BUY_RES_WINS","BUY_RES")

#dataset <- fread("output/ML_ready/dt_buy_time.csv",select = select_cols)
dataset <- fread("output/ML_ready/dt_buy_time.csv")

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
#dataset$y_pred<- -9999.0



  dataset<-target_to_the_end(dataset,targetName)
  names(dataset)[ncol(dataset)]<-"Target"


#-- Index of the target
N_target<-ncol(dataset)

#-- Cross Validation folds 

  params <- expand.grid(
    N_train=c(4000),            #--- Number of samples for training
    N_test=c(168),             #-- Number of samples for testing
    
    eta=c(0.1),                #-- Learning rate
    nrounds=c(200),          #-- Number of trees        
    
    gamma=c(0),
    max.depth=c(4),             #-- Maximum depth of the tree     
    min_child_weight = c(1),
    
    colsample_bytree = c(1) ,    #-- Sample per tree
    subsample = c(1),
    
    lambda = c(0),
    lambda_bias = c(0),
    alpha=c(0)
  )


  #-- shuffle the parameters
params<-params[sample(nrow(params),nrow(params)),]
best_metric<- -9999.0




j<-1


#-- Plot 2 D
#ggplot(dataset, aes(x=williamsAD, y=chaikinVolatility, color=BUY_RES)) +geom_point()

#y_pred_ind <- as.integer(grep("y_pred",names(dataset)))


#-- Initialize results
dataset_pred<-data.table(y_pred=numeric(nrow(dataset)))
dataset_pred[,y_pred:=-999]

dataset_train<-data.table(y_train=numeric(nrow(dataset)))
dataset_train[,y_train:=-999]


dataset<-as.data.table(dataset)

feats<-names(dataset)[1:(-1+length(names(dataset)))]

while(j<(1+nrow(params)))
{
  
  
  i<-1
  
  #-- Cumulative metric performance
  metric_cum<-0
  N_train <- params[i,"N_train"]
  N_test <- params[i,"N_test"]
  
  while( i < nrow(dataset)-(N_train+N_test+1))
  {
    
    train <- dataset[i:(i+N_train-1),]
    test <- dataset[(i+N_train):(i+N_train+N_test-1),]
    
    classifier = xgboost(data = as.matrix(train[,..feats]), label = train$Target,
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
                         nthread = 8,
                         objective = problem_type, verbose = FALSE)
    
    set( dataset_pred ,seq((i+N_train),(i+N_train+N_test-1)) ,1L , predict(classifier, newdata = as.matrix(test[,..feats]))  )
    set( dataset_train ,seq(i,(i+N_train-1)) ,1L , predict(classifier, newdata = as.matrix(train[,..feats]))  )
    
    
    #dataset[seq((i+N_train),(i+N_train+N_test-1)),]
    
    #i<- i + (N_train+N_test)
    i<- i + (N_test)
    
      }
  
  
  dataset<-cbind(cbind(dataset,dataset_pred),dataset_train)
  dataset_with_preds<-dataset[y_pred>-10]
  dataset_with_train<-dataset[y_train>-10]
  
  #-- Get the AUC
  metric_cum<-auc(dataset_with_preds$Target,dataset_with_preds$y_pred)
  metric_train<-auc(dataset_with_train$Target,dataset_with_train$y_train)
  
  #metric_cum<-profitPips(y_actual=CV$Target, y_pred=y_pred)[1]
  
    
  params[j,"metric"]<-metric_cum
  params[j,"metric_train"]<-metric_train
  
  

  
  

  
  if( metric_cum > best_metric)
  {
    print(params[j,])
   
   # y_pred_train = predict(classifier, newdata = as.matrix(train[-N_target]))
  # print(paste0("AUC (train):",auc(train$Target, y_pred_train)))   
  #  print(paste0("AUC (test):",auc(CV$Target, y_pred)))  
    
    best_metric<-metric_cum
    bst_classifier<-classifier
    feat_imp <- xgb.importance(feature_names = names(train), model = bst_classifier)
    print(xgb.importance(feature_names = names(train), model = bst_classifier))
    #xgb.save(bst_classifier, 'models/xgb.model')
    
    
    print("-----------------------------------")
    
  }
  
  print(paste0(100*j/nrow(params),"%")) 
  j<-j+1
  
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








