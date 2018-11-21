#-- Train a single model on all the features (No feature selection)




rm(list=ls())

set.seed(123)



library(data.table)
library(lubridate)
library(mlr)
library(ggplot2)
library(xgboost)
library(crayon)
library(plotly)
library(caret)
library(parallelMap)
#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"

#parallelStartSocket(2)

#-- Linux
#parallelStartMultiCore()

#------------------------------------------------------------#
##############################################################
#------------------------------------------------------------#


#------------------------------------------------------------#
################## DEFINE THE CONFIGURATIONS #################
#------------------------------------------------------------#

config_file <- data.table(
  instruments = c("BUY_RES_USDCAD","SELL_RES_USDCAD"), # Which models are to be trained in this script
  SL = 15, # Stop loss
  PF = 1,  # Profit factor
  SPREAD = 3, # Spread, make sure there is a file with the specified spread
  #indicator_filter = c("EMA","TMS","SMA","atr","dist","RSI","williams"),
  indicator_filter = c("EMA","TMS","SMA","atr","RSI","williams"),
  indicator_pair_filter = c("AND"),
  pair_filter = c("CAD","XAU"),
  preprocess_steps = c("center","scale"),
  test_portion = 0.3, # Out of sample test part for final evaluation
  window_type =  "FixedWindowCV", #"GrowingWindowCV",
  initial.window = 1e4,    # Window size for training
  horizon = 1e4, # Future window for testing
  initial.window_stack = 5e3,    # Window size for training
  horizon_stack = 1e4, # Future window for testing
  REMOVE_FAST_WINS = T, # Flag to remove the positive trades which are finished in less than MIN_TRADE_TIME
  MIN_TRADE_TIME = 15,
  CORRELATION_FEATURE_SELECTION = T, # Flag whether to filter out the highly correlated features
  CORRELATION_THRESHOLD = 0.9, # Filter to indicate the maximum correlation between indicators to be included in the training
  READ_SELECTED_FEATURES = F,
  returns_period = "week", #"month","day" defines the period of aggregating the returns
  WRITE_FLAG = F
)


all_pairs <- c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCHF","NZDUSD","XAUUSD","USDCAD")
instruments <- data.table(currs = unique(config_file$instruments))


indicator_filter = unique(config_file[!is.na(indicator_filter),indicator_filter]) 
indicator_pair_filter = unique(config_file[,indicator_pair_filter])
pair_filter = unique(config_file[,pair_filter])
MIN_TRADE_TIME = config_file$MIN_TRADE_TIME[1]
preprocess_steps <- unique(config_file[!is.na(preprocess_steps),preprocess_steps])
CORRELATION_FEATURE_SELECTION <- config_file$CORRELATION_FEATURE_SELECTION[1] # use correlations to select the features
#-- Read the configurations
returns_period = config_file$returns_period[1] #"month","day" defines the period of aggregating the returns
READ_SELECTED_FEATURES <- config_file$READ_SELECTED_FEATURES[1]
WRITE_FLAG <- config_file$WRITE_FLAG[1]
SPREAD <- config_file$SPREAD[1] # Spread, make sure there is a file with the specified spread
SL <- config_file$SL[1]
PF <- config_file$PF[1]
test_ratio <- config_file$test_portion[1]
initial.window<-config_file$initial.window[1]
horizon <- config_file$horizon[1]
initial.window_stack<-config_file$initial.window_stack[1]
horizon_stack <- config_file$horizon_stack[1]
wind <- config_file$window_type[1]
REMOVE_FAST_WINS<-config_file$REMOVE_FAST_WINS[1]
CORRELATION_THRESHOLD <- config_file$CORRELATION_THRESHOLD[1]

#------------------------------------------------------------#
############### DEFINE THE FUNCTIONS #########################
#------------------------------------------------------------#
# sharpe_ratio = function(task, model, pred, feats, extra.args) {
#   
#   predTable <- as.data.table(pred)
#   predTable <- predTable[response==T]
#   predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
#   if(nrow(predTable)>5)
#   {  
#     predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
#     (predTable[nrow(predTable), equity])/((1+max(predTable$drawdown)))
#   }else{
#     
#     (0)
#   }
# }
# sharpe_ratio = makeMeasure(
#   id = "prob_pos_week", name = "prob_pos_week",
#   properties = c("classif", "classif.multi", "req.pred",
#                  "req.truth"),
#   minimize = FALSE,  fun = sharpe_ratio
# )

get_sharpe=function(dt_curr,dt_time_lut_prediction_period,PF)
{
  dt_portfolio <-  merge(dt_time_lut_prediction_period,dt_curr,all.x = T,by="index")
  
  #-- Add equity, returns and drawdown
  dt_portfolio[TARGET==1 & decision==1,returns:=PF][TARGET==0 & decision==1,returns:=-1][is.na(returns),returns:=0][,equity:=cumsum(returns)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  
  mean_returns <- dt_portfolio[,.(mean_returns=sum(returns)),by="ret_per"]
  
  return(list(mean_returns,mean(mean_returns$mean_returns),var(mean_returns$mean_returns),max(dt_portfolio$drawdown)))
  
}

get_mean_returns_and_variance=function(dt_res,dt_time_lut_prediction_period,PF)
{
  
  step <-0.01
  th<-step
  bst_sharpe <- -999
  bst_thr <- 0.01
  
  #--DEBUG
  #th<-0.54
  
  
  
  while(th<0.95)
  {
    dt_curr<-copy(dt_res)
    dt_curr[,decision:=as.numeric(prediction>th)]
    dt_curr <- dt_curr[decision>0.5]
    ret_varg<-get_sharpe(dt_curr,dt_time_lut_prediction_period,PF)
    
    if((ret_varg[[2]]==0 & ret_varg[[3]]==0))
    {
      curr_sharpe<-0
    }else{
      #-- SHARPE RATIO CALCULATION
      #curr_sharpe <- ret_varg[[2]]/sqrt(1+ret_varg[[3]]+ret_varg[[4]])
      #curr_sharpe <- ret_varg[[2]]/(1+sqrt(ret_varg[[3]]))
      #curr_sharpe <- ret_varg[[2]]/(0.01+sqrt(ret_varg[[3]]))
      curr_sharpe <-1-pnorm(0,ret_varg[[2]],sqrt(ret_varg[[3]]))
      
    }
    
    
    if(curr_sharpe>bst_sharpe)
    {
      bst_sharpe<-curr_sharpe
      bst_thr <- th
      bst_mean_ret <- ret_varg[[2]]
      #bst_var_ret <- sqrt(1+ret_varg[[3]]+ret_varg[[4]])
      #bst_var_ret <- (1+sqrt(ret_varg[[3]]))
      #bst_var_ret <- (0.01+sqrt(ret_varg[[3]]))
      bst_var_ret <- (sqrt(ret_varg[[3]]))
      
    }
    
    th<-th+step
  }
  
  
  return(list(bst_mean_ret,bst_var_ret,bst_thr))
  
}



train_and_predict = function(dt,nrounds,eta,max_depth,initial.window,horizon,target_name="TARGET",index_name="index")
{
  
  #-- Get feature columns and target columns
  feat_cols <-setdiff(names(dt),target_name)
  target_col <-target_name
  #-- CHeck if index column is there
  index_col_available <- index_name %in% names(dt)
  
  #-- Exclude index from the feature columns
  if(index_col_available)
  {
    feat_cols <- setdiff(feat_cols,index_name)
  }
  
  
  #-- Initialize the resultant table
  dt_res <- data.table(prediction=numeric(0),index=numeric(0),TARGET=numeric(0))
  
  
  i<-1+initial.window
  while(i< (nrow(dt)-horizon-1) )
  {
    
    #-- subset train and prediction and index
    dt_train <- copy(dt[(i-initial.window):i-1,])  
    dt_predict <- copy(dt[i:(i+horizon-1),] )
    
    if(index_col_available)
    {
      dt_index <- copy(dt_predict[,..index_name])
    }
    
    dt_vars_cols_train <- dt_train[,..feat_cols]
    dt_target_train    <- dt_train[,..target_col]
    
    #print(dt_vars_cols_train)
    xgb <- xgboost(data = as.matrix(dt_vars_cols_train), 
                   label = as.matrix(dt_target_train), 
                   eta = eta,
                   max_depth = max_depth, 
                   nround=nrounds,
                   objective = "binary:logistic",
                   #     early_stopping_rounds = 3,
                   colsample_bytree = 0.5,
                   subsample = 0.8,
                   #eval_metric = "map",
                   verbose = F
    )
    #print(xgb.importance(model=xgb,feature_names = feat_cols))
    #-- Predict
    y_pred <- predict(xgb,newdata=as.matrix(dt_predict[,..feat_cols]))
    #-- Include predictions
    dt_index<-cbind(dt_index,data.table(prediction=y_pred))  
    #-- Include the ground truth
    dt_index<-cbind(dt_index,dt_predict[,..target_col])  
    dt_res <- rbind(dt_res,dt_index)  
    rm(dt_index)
    cat("\r",round(100.0*i/(nrow(dt)-horizon-1))+"%")
    i<-i+horizon
  }
  
  cat("\n\n")
  return(dt_res) 
}



"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


#-- Sharpe ratio function
# Only the pred is used
# TODO: Use Sortino ratio instead and modify the way the sharpe_ratio is calculated
sharpe_ratio = function(task, model, pred, feats, extra.args) {
  
  predTable <- as.data.table(pred)
  #-- Select only the trades we label as true because they build up the portfolio
  predTable <- predTable[response==T]
  if(nrow(predTable)>5)
  {  
    #-- Get the equity and drawdown
    predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
    #-- Calculate the modified sharpe ratio by including the drawdown
    (predTable[nrow(predTable), equity])/((1+max(predTable$drawdown)))
  }else{
    
    (0)
  }
}

#-- Set the sharpe ratio as a custom function for optimizing the models
sharpe_ratio = makeMeasure(
  id = "sharpe_ratio", name = "sharpe_ratio",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE,  fun = sharpe_ratio
)

#-- Get the optimal threshold to maximize the portfolio
getBestThresh <- function(dt)
{
  res_orig <- as.data.table(dt)
  thresh_vec <- seq(0.01,0.99,0.01)
  bst_thresh <-0
  max_sharpe_ratio <- -991
  bst_drawdown <- -9999
  max_avg_ret <- -999
  iters <- max(dt$iter)
  
  for (th in thresh_vec)
  {
    res_sel <- copy(res_orig)
    res_sel[,response:=prob.TRUE>th]
    res_sel <- res_sel[response==T]
    if(nrow(res_sel)>10)
    {  
      
      #-- Compute the sharpe ratio as average ret per tra over the variance
      #-- Net equity
      res_sel[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
      total_ret <-  res_sel[nrow(res_sel), equity]
      std_ret <- sqrt(var(res_sel$equity))
      min_drawdown <- max(res_sel$drawdown)
      sharpe_ratio <- total_ret/((1+min_drawdown)*iters)
      
      if(sharpe_ratio>max_sharpe_ratio)
      {
        max_sharpe_ratio <- sharpe_ratio
        max_avg_ret <- total_ret
        bst_thresh <- th
        bst_drawdown <- min_drawdown
        bst_dt <- res_sel
      }
      
    }
  }
  
  return( list(max(bst_dt$drawdown),max_avg_ret, nrow(bst_dt[equity<0]) ,  max_sharpe_ratio, bst_thresh ,bst_dt))
  
}

#------------------------------------------------------------#
###############  READ THE DATA AND FORMAT THE COLUMNS ########
#------------------------------------------------------------#

#-- Read the main ML file
dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))

#-- Attach the index column
dt[,index:= seq(1,nrow(dt))]

#-- Create the index, time lookup table
dt_time_lut <- dt[,.(index,Time)]



#- Setting the trades with quick wins to zeros, because they are probably resulting from news

if(REMOVE_FAST_WINS)
{
  
  for(pr in all_pairs)
  {
    dt[ get("buy_profit_"+pr)<MIN_TRADE_TIME  ,("BUY_RES_"+pr):=0]
    dt[ get("sell_profit_"+pr)<MIN_TRADE_TIME  ,("SELL_RES_"+pr):=0]
  }
  
}

#-- COmputing the aggregation column
if(returns_period=="week")
{
  dt_time_lut[,ret_per:=year(Time)+"_"+week(Time)]
  
}else
{
  if(returns_period=="day")
  {
    dt_time_lut[,ret_per:=year(Time)+"_"+month(Time)+"_"+day(Time)]
    
    
  }else{
    
    if(returns_period=="month")
    {
      
      dt_time_lut[,ret_per:=year(Time)+"_"+month(Time)]
      
    }else{
      
      stop("Incorrect aggregation period")
    }
  }
}






#-- Group the column types
results_cols <- names(dt)[grepl("BUY_RES",names(dt)) | grepl("SELL_RES",names(dt))]

#-- Remove near zero variance features
#inds_nearZero_var <- caret::nearZeroVar(x=dt[sample(1e4)])
#nonZeroVarCols <- setdiff(names(dt),names(dt)[inds_nearZero_var])
profit_loss_cols <-  names(dt)[grepl("profit",names(dt)) | grepl("loss",names(dt))]
bs_cols <- names(dt)[grepl("^bs",names(dt))]
ohlc_cols <- names(dt)[grepl("Low$",names(dt)) | grepl("Close$",names(dt)) | grepl("Open$",names(dt)) | grepl("High$",names(dt))]
full_features <- setdiff(names(dt),c("index","Time",results_cols,profit_loss_cols,ohlc_cols,bs_cols))




##########################################################################################
##########    PREPROCESSING: HARD FILTER OF INDICATOR TYPES    ###########################
##########################################################################################


#-- Hard filter of indicator types
if(length(indicator_filter)>0)
{
  regex_cmd <- "("+paste0(indicator_filter,collapse = "|")+")"
  indicators_filtered<-full_features[grepl(regex_cmd,full_features)]
}else{
  
  indicators_filtered<-full_features
}



#-- Hard filter of pair types
if(length(pair_filter)>0)
{
  regex_cmd <- "("+paste0(pair_filter,collapse = "|")+")"
  pairs_filtered<-full_features[grepl(regex_cmd,full_features)]
}else{
  
  pairs_filtered<-full_features
}



#-- Combine indicator and pair filter
if(indicator_pair_filter=="AND")
{
  
  full_features <- intersect(indicators_filtered,pairs_filtered)
}else{
  full_features <- unique(c(indicators_filtered,pairs_filtered))
}









#-- Feature selection

if(CORRELATION_FEATURE_SELECTION)
{
  dt_features <- copy(dt[,..full_features])
  
  #-- Remove near zero variance features
  inds_nearZero_var <- caret::nearZeroVar(x=dt_features[sample(seq(1e4,nrow(dt_features)),5e3)])
  if(length(inds_nearZero_var)>0)
  {
    nonZeroVarCols <- setdiff(names(dt_features),names(dt_features)[inds_nearZero_var])
  }else{
    
    nonZeroVarCols<-names(dt_features)
  }
  
  dt_features<-dt_features[,,..nonZeroVarCols]
  
  
  
  
  cols <- names(dt_features)
  dt_features[,(cols):=lapply(.SD,as.numeric),.SDcols = cols]
  #dt_features <- sapply( dt_features, as.numeric )
  cr <- cor(dt_features[sample(seq(2e4,nrow(dt_features)),5e3),], use="complete.obs")
  
  highly_correlated_features<-caret::findCorrelation(x=cr,cutoff=CORRELATION_THRESHOLD)
  
  correlated_features_to_be_excluded<-names(dt_features)[highly_correlated_features]
  
  
  
  feat_cols<-setdiff(full_features,correlated_features_to_be_excluded)
  unique_relevant_cols <- c("index",feat_cols,results_cols)
  dt_sel<-dt[,..unique_relevant_cols]
  
}else{
  
  #-- Previous implementation
  #feat_cols <- names(dt)[grepl("TMS",names(dt)) | grepl("RSI$",names(dt))]
  feat_cols <- full_features
  index_cols <- "index"
  relevant_cols <- c(index_cols,feat_cols,results_cols)
  dt_sel <- dt[,..relevant_cols]
  
}




if(length(preprocess_steps)>0)
{
  dt_feature_part <- dt_sel[,..feat_cols]
  
  preProcValues <- preProcess(dt_feature_part, method = c("center", "scale"))
  tmp <- predict(preProcValues,dt_feature_part)
  
  dt_sel[,(feat_cols):=tmp]
  
}


#------------------------------------------------------------#
############## SPLIT TRAIN AND TEST ##########################
#------------------------------------------------------------#

dt_test <- dt_sel[floor(nrow(dt_sel)*(1-test_ratio)):nrow(dt),]
dt_sel <- dt_sel[1:(floor(nrow(dt_sel)*(1-test_ratio))-1),]


#-- Remove tests since we are not validating at this point
rm(dt_test)
rm(tmp)
rm(dt)
rm(dt_feature_part)
rm(dt_features)

#------------------------------------------------------------#
################## CREATE MLR TASK ##########################
#------------------------------------------------------------#


models_with_performance_issues <- c("classif.neuralnet","classif.ksvm","classif.extraTrees","classif.fdausc.glm","classif.fdausc.kernel","classif.fdausc.knn","classif.fdausc.np","classif.randomForestSRC","classif.featureless","classif.bartMachine","classif.blackboost","classif.cforest","classif.evtree","classif.gausspr","classif.rda")
#already_tested <- c("classif.ctree","classif.h2o.randomForest","classif.naiveBayes","classif.C50","classif.IBk","classif.nnTrain","classif.ada","classif.J48","classif.JRip","classif.ksvm","classif.lqa","classif.binomial","classif.earth","classif.LiblineaRL2LogReg","classif.pamr","classif.extraTrees","classif.mda","classif.plsdaCaret","classif.h2o.glm","classif.mlp","classif.probit","classif.h2o.gbm","classif.nnet","classif.h2o.deeplearning","classif.neuralnet","classif.randomForest","classif.glmnet","classif.cvglmnet","classif.OneR","classif.ranger","classif.gamboost","classif.plr","classif.rotationForest","classif.gbm","classif.logreg","classif.multinom","classif.LiblineaRL1LogReg","classif.adaboostm1","classif.nodeHarvest","classif.PART","classif.saeDNN","classif.rpart","classif.dbnDNN","classif.svm","classif.qda","classif.xgboost","classif.glmboost")
#classif_learners = all_learners[grepl("^classif",class) & installed==T & prob==T & !(class %in% already_tested)  &!(class %in% models_with_performance_issues) &!(class %in% c("classif.rFerns","classif.rknn","classif.RRF","classif.rrlda","classif.sda",
#                                                                                                                                                                               "classif.clusterSVM","classif.dcSVM","classif.fnn","classif.gaterSVM","classif.geoDA",
#                                                                                                                                                                               "classif.knn","classif.LiblineaRL1L2SVC")) ,class]


#-- Choose the classifiers
all_learners<-as.data.table(listLearners())

classif_learners = all_learners[grepl("^classif",class) & installed==T & prob==T   &
                                  !(class %in% models_with_performance_issues) &
                                  !(class %in% c("classif.rFerns","classif.rknn","classif.RRF","classif.rrlda","classif.sda","classif.knn","classif.LiblineaRL1L2SVC")) ,class]





#-- classif_learners<-c("classif.neuralnet")

#fwrite(data.table(classifiers=classif_learners),data_output_dir+"valid_classifiers.csv")




for (curr_model in unique(config_file$instruments))
{

  
cat("\n########  "+curr_model+"   ############\n")
  
dt_curr<-  copy(dt_sel)
  lrnrs = lapply(classif_learners,makeLearner,predict.type="prob")
  
  print(length(classif_learners))
  
#  curr_model = "SELL_RES_USDJPY"
setnames(dt_curr,curr_model,"TARGET")

feats_and_target <- c(feat_cols,"TARGET")
dt_train <- dt_curr[,..feats_and_target]

rm(dt_curr)
#-- Get only non NA rows
dt_train <- na.omit(dt_train)

tsk <- makeClassifTask(id=curr_model,data=as.data.frame(dt_train), target="TARGET")

#-- TO check what are the available measures
#listMeasures(tsk)

#-- Make the resampling strategy
rsmpl_desc = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)

#-- Benchmark
bmr<-benchmark(lrnrs,tsk,rsmpl_desc,measures = auc)

print(bmr)



#-- Get the iteration results and store them
res <- as.data.table(bmr)
fwrite(res,data_output_dir+curr_model+"/performance_iterations_"+Sys.Date()+".csv")


#-- Get the mean and variance of the auc
eta_val = 0.0001
#res[,.(sharpe=mean(auc),eta_val=std(auc)),by="learner.id"]
res_sharpe<-merge(res[,.(stdev=sqrt(var(auc))),by="learner.id"],res[,.(mean_v=mean(auc)),by="learner.id"])
res_sharpe[,sharpe:=mean_v/stdev][order(-sharpe)]
res_sharpe<-res_sharpe[order(-sharpe)]
fwrite(res_sharpe,data_output_dir+curr_model+"/res_sharpe_"+Sys.Date()+".csv")



predictions_str <- as.data.table(getBMRPredictions(bmr,as.df = T))
data_baselearners<-merge(dcast(data=predictions_str, id  ~  learner.id, value.var = "prob.1"),unique(predictions_str[,.(id,truth)],by=c("id","truth")))
rm(predictions_str)

data_baselearners<- data_baselearners[order(id)]
data_baselearners[,id:=NULL]



dt_train_cor <- data_baselearners[,truth:=NULL]
cr<-as.data.table(cor(dt_train_cor))
cr$learner.id <- names(cr)
fwrite(cr,data_output_dir+curr_model+"/correlation_matrix_"+Sys.Date()+".csv")
fwrite(config_file,data_output_dir+curr_model+"/config_file_"+Sys.Date()+".csv")

cat("\n######################################\n")


#res_sharpe[,.(learner.id,sharpe)]

#-- Get performance matrix for easy matrix multiplication with the correlation matrix
##perf_mat <- res_sharpe$sharpe %*% t(res_sharpe$sharpe)
#perf_mat <-as.data.table(perf_mat)
#names(perf_mat)<-as.character(res_sharpe$learner.id)


}


parallelStop()



if("STACK"!="STACK")
  {
bst_learners_stack <- unique(c("truth",as.vector(res_sharpe[order(-sharpe)][,learner.id])[seq(1,2)],as.vector(res_sharpe[order(-mean_v)][,learner.id])[seq(1,2)]))
dt_train <- data_baselearners[,..bst_learners_stack]
#-- Classifier task
tsk_stack <- makeClassifTask(id=curr_model+"_stack",data=as.data.frame(dt_train), target="truth")

classif_learners<-c("classif.glmnet")
#classif_learners<-unique(c("classif.glmnet",as.vector(res_sharpe[order(-sharpe)][,learner.id])[seq(1,5)],as.vector(res_sharpe[order(-mean_v)][,learner.id])[seq(1,5)]))

lrnrs_stack = lapply(classif_learners,makeLearner,predict.type="prob")
rsmpl_desc_stack = makeResampleDesc(method=wind,initial.window=initial.window_stack,horizon=horizon_stack, skip =horizon)
bmr_stack<-benchmark(lrnrs_stack,tsk_stack,rsmpl_desc,measures = auc)


}



