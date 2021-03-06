
#-- Optimizes the parameters of each model for each pair

# Conclusions:
# XGBOOST:
# Low eta, low nrounds small max depth, high subsample, low colsample_bytree


# TODO:
# 2. Remove night trades

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
library(mlrHyperopt)
library(PerformanceAnalytics)
library(parallelMap)
#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"



JUST_CHECKING <- T

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
  instruments = c("BUY_RES_AUDUSD","SELL_RES_AUDUSD"), # Which models are to be trained in this script
  SL = 15, # Stop loss
  PF = 1,  # Profit factor
  SPREAD = 3, # Spread, make sure there is a file with the specified spread
  indicator_filter = c("EMA","TMS","atr","dist","corr","DIFF"),
  # indicator_filter = c("EMA"),
  indicator_pair_filter = c("AND"),
  pair_filter = c("AUD"),
  N_MAX_CONSECUTIVE_TRADES = c(2),
#  pair_filter = c("AUD","NZD","JPY"),
  SCALE_AND_MEAN_FLAG = F,
  #preprocess_steps = c("center","scale"),
  preprocess_steps = c(""),
  test_portion = 0.3, # Out of sample test part for final evaluation
  window_type =  "FixedWindowCV", #"GrowingWindowCV", "FixedWindowCV"
  initial.window = 1e3,    # Window size for training
  horizon = 24, # Future window for testing
  initial.window_stack = 1e4,    # Window size for training
  horizon_stack = 1e4, # Future window for testing
  REMOVE_FAST_WINS = T, # Flag to remove the positive trades which are finished in less than MIN_TRADE_TIME
  MIN_TRADE_TIME = 15,
  CORRELATION_FEATURE_SELECTION = T, # Flag whether to filter out the highly correlated features
  CORRELATION_THRESHOLD = 0.8, # Filter to indicate the maximum correlation between indicators to be included in the training
  READ_SELECTED_FEATURES = F,
  returns_period = "week", #"month","day" defines the period of aggregating the returns
  WRITE_FLAG = F
)


all_pairs <- c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCHF","NZDUSD","XAUUSD","USDCAD")
instruments <- data.table(currs = unique(config_file$instruments))


SCALE_AND_MEAN_FLAG <- config_file$SCALE_AND_MEAN_FLAG[1]
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
PF <<- config_file$PF[1]
test_ratio <- config_file$test_portion[1]
initial.window<-config_file$initial.window[1]
horizon <- config_file$horizon[1]
initial.window_stack<-config_file$initial.window_stack[1]
horizon_stack <- config_file$horizon_stack[1]
wind <- config_file$window_type[1]
REMOVE_FAST_WINS<-config_file$REMOVE_FAST_WINS[1]
CORRELATION_THRESHOLD <- config_file$CORRELATION_THRESHOLD[1]
N_MAX_CONSECUTIVE_TRADES<<-config_file$N_MAX_CONSECUTIVE_TRADES[1]



#------------------------------------------------------------#
############### DEFINE THE FUNCTIONS #########################
#------------------------------------------------------------#
prob_pos_ret = function(task, model, pred, feats, extra.args) {

  pred <- as.data.table(pred)
setnames(pred,c("truth","id"),c("TARGET","index"))
  #  print(head(pred))
  
    step <-0.01
  th<-step
  bst_sharpe <- -999
  bst_thr <- 0.01
  while(th<0.95)
  {
    dt_curr<-copy(pred)
    dt_curr[,decision:=as.numeric(prob.1>th)]
    
    
    
    rolls <- roll_sum(as.matrix(dt_curr[,decision]),width=N_MAX_CONSECUTIVE_TRADES)
    rolls<-as.data.table(rolls)
    names(rolls)<-"triggered_trades"
    
    
    dt_curr<-cbind(dt_curr,rolls)
    dt_curr[triggered_trades>1 & !is.na(triggered_trades),decision:=0]
    
    dt_curr <- dt_curr[decision>0.5]
    
#-- LIMIT THE CONSECUTIVE TRADES HERE
    

    ret_varg<-get_sharpe(dt_curr,dt_time_lut,PF)
    if((ret_varg[[2]]==0 & ret_varg[[3]]==0))
    {
      curr_sharpe<-0
    }else{
      curr_sharpe <-1-pnorm(0,ret_varg[[2]],sqrt(ret_varg[[3]]))
    }
    if(curr_sharpe>bst_sharpe)
    {
      bst_sharpe<-curr_sharpe
      bst_thr <- th
      bst_mean_ret <- ret_varg[[2]]
      bst_var_ret <- (sqrt(ret_varg[[3]]))
    }
    th<-th+step
  }
  
  
  return(bst_sharpe)
  
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  if(nrow(predTable)>5)
  {
    predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
    (predTable[nrow(predTable), equity])/((1+max(predTable$drawdown)))
  }else{

    (0)
  }
}
prob_pos_ret = makeMeasure(
  id = "prob_pos_week", name = "prob_pos_week",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE,  fun = prob_pos_ret
)



#######  GET PARAMETER SEARCH SPACE ################

getParamSearchSpace <- function(model)
{
  
  
  if(model=="classif.plsdaCaret")
    
  {
    ps = makeParamSet(
      makeIntegerParam("ncomp", lower = 1L, upper = 10L)#,
      # makeDiscreteParam("probMethod", values=c("softmax","Bayes")),                            # Very slow
      #  makeDiscreteParam("method", values=c("kernelpls","widekernelpls","simpls","oscorespls")) # Very slow
    )
    return(ps)
  }
  
  if(model=="classif.pamr")
  {
    ps = makeParamSet(
      makeIntegerParam("n.threshold", lower = 20L, upper = 40L),
      makeNumericParam("offset.percent",lower=30,upper=70),
      makeLogicalParam("scale.sd"),
      makeLogicalParam("remove.zeros"),
      makeDiscreteParam("sign.contrast", values=c("both","negative","positive"))#,                         
      #    makeDiscreteParam("probMethod", values=c("softmax","Bayes"))#,                          
      #    makeDiscreteParam("method", values=c("kernelpls","widekernelpls","simpls","oscorespls")) # very slow
    )
    return(ps)
  }
  
  
  
  if(model=="classif.qda")
  {
    ps = makeParamSet(
      makeNumericParam("nu",lower=2,upper=20),
      makeDiscreteParam("method", values=c("moment","mle","mve","t")),   
      makeDiscreteParam("predict.method", values=c("plug-in","predictive","debiased"))#,   
    )
    
    return(ps)
  }
  
  
  if(model=="classif.glmnet")
  {
    
    ps = makeParamSet(
      makeNumericParam("alpha",lower=0,upper=1),
      makeNumericParam("thresh",lower=-7,upper=2, trafo =    function(x) 10^x),
      makeIntegerParam("nlambda", lower = 80L, upper = 120L)#,
      #  makeNumericParam("lambda.min.ratio",lower=0,upper=1)
    )
    return(ps)
    
  }
  
  
  if(model=="classif.svm")
  {
    
    ps=makeParamSet(
      makeNumericParam("nu", lower = -0.5, upper = 1.5),
      makeNumericParam("cost", lower = 0, upper = 10)
    )
    
    return(ps)
    
  }
  
  if(model=="classif.nnTrain")
  {
    
  ps = makeParamSet(
    makeIntegerVectorParam("hidden",len = 2,lower = 10,upper = 1000),
    makeDiscreteParam("activationfun", values=c("sigm","tanh")),
    makeNumericParam("learningrate",lower=0,upper=5),
    makeNumericParam("hidden_dropout",lower=0,upper=1),
    makeIntegerParam("numepochs", lower = 20L, upper = 100L),
    makeNumericParam("visible_dropout",lower=0,upper=1)
  )
  #[Tune] Result: hidden=55,56; activationfun=sigm; learningrate=1.91; hidden_dropout=0.404; numepochs=5; visible_dropout=0.775 : auc.test.mean=0.5410692
  return(ps)
  
  }
  
  
  
  
  if(model=="classif.h2o.deeplearning")
  {
    ps = makeParamSet(
    makeIntegerVectorParam("hidden",len = 2,lower = 10,upper = 100),
    #  makeDiscreteParam("activation", values=c("Rectifier")),
    # makeNumericParam("l1",lower=0.001,upper=1),
    #  makeNumericParam("l2",lower=0.001,upper=1),
    # makeNumericParam("input_dropout_ratio",lower=0,upper=1),
    #  makeNumericParam("rho",lower=0,upper=10),
    makeNumericParam("epochs", lower = 8, upper = 20),
    makeNumericParam("rate",lower=0,upper=1)#,
    # makeLogicalParam("quiet_mode")
  )
  return(ps)
  
}

  
  if(model=="classif.xgboost")
  {
    ps = makeParamSet(
      makeIntegerParam("nrounds", lower = 2L, upper = 10),
      #  makeDiscreteParam("activation", values=c("Rectifier")),
       makeNumericParam("eta",lower=0.001,upper=0.01),
      #  makeNumericParam("l2",lower=0.001,upper=1),
      # makeNumericParam("input_dropout_ratio",lower=0,upper=1),
      #  makeNumericParam("rho",lower=0,upper=10),
      makeNumericParam("colsample_bytree", lower = 0.31, upper = 0.5),
      makeIntegerParam("max_depth", lower = 2L, upper = 4L),
      makeNumericParam("subsample",lower=0.81,upper=0.99)#,
      # makeLogicalParam("quiet_mode")
    )
    return(ps)
    
  }
  
  
  
  
}



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
dt_time_lut <<- dt[,.(index,Time)]



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
  cr <- cor(na.omit(dt_features[sample(seq(1e4,nrow(dt_features)),5e3),]), use="complete.obs")
  
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

print(feat_cols)


if(length(preprocess_steps)>0 & SCALE_AND_MEAN_FLAG ==T)
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


#--------------------------------------------------------#
#######  Save the models used for all the pairs ##########
#--------------------------------------------------------#


all_pairs <- c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCHF","NZDUSD","USDCAD")

pairs <- c("BUY_RES_"+all_pairs,"SELL_RES_"+all_pairs)
#-- Load all the models

i<-1
for(pair in pairs)
{
  curr_model<-fread(data_output_dir+pair+"/top_models.csv")
  curr_model$pair <- pair
#  print(curr_model)
  if(i==1)
  {
    all_mdls <- curr_model 
  }else{
    
    all_mdls<-rbind(all_mdls,curr_model)
  }
  i<-i+1
}

fwrite(all_mdls,data_output_dir+"models_per_pair.csv")


#--------------------------------------------------------#
###############  READ BEST MODELS FOR THIS PAIR ##########
#--------------------------------------------------------#


pairs <- unique(config_file$instruments)


curr_model <- pairs[1]


dt_models<-fread(data_output_dir+curr_model+"/top_models.csv")
classif_learners<-unique(dt_models$models)


dt_sel <- merge(dt_sel,dt_time_lut[,.(index,Time)],all.x=T,by="index")
dt_sel[,hour:=as.numeric(lubridate::hour(Time))]
dt_sel[,Time:=NULL]

dt_curr<-  copy(dt_sel)




if(!JUST_CHECKING)
  {
lrnrs = lapply(classif_learners,makeLearner,predict.type="prob")
}

#  curr_model = "SELL_RES_USDJPY"
setnames(dt_curr,curr_model,"TARGET")
feat_cols<-c(feat_cols,"hour")
feats_and_target <- c(feat_cols,"TARGET")
dt_train <- dt_curr[,..feats_and_target]

feats_and_target <- c(feat_cols,"TARGET")
dt_train <- dt_curr[,..feats_and_target]



rm(dt_curr)


#-- Get only non NA rows
dt_train <- na.omit(dt_train)
dt_train = createDummyFeatures(as.data.frame(dt_train),target="TARGET")
tsk <- makeClassifTask(id=curr_model,data=as.data.frame(dt_train), target="TARGET")
#-- Make the resampling strategy
rsmpl_desc = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)





#-- Hyperparameter optimization
# based on https://www.kaggle.com/getting-started/42758


#learner_name<- "classif.nnTrain"
learner_name<- "classif.xgboost"

lrnr <- makeLearner(learner_name,predict.type="prob")
ps = getParamSearchSpace(learner_name)
#-- Set the maximum number of iterations
ctrl = makeTuneControlRandom(maxit = 10L)

#-- Tune the hyperparameters
res = tuneParams(lrnr, task = tsk, resampling =
                   rsmpl_desc,
                 par.set = ps, control = ctrl,measures = prob_pos_ret)

fwrite(as.data.table(as.matrix(t(c(unlist(res$x),res$y)))),data_output_dir+"xgboost_best.csv")
res_prsed <- as.data.table(res$opt.path$env$path)
plotOptPath(op=res$opt.path)

fwrite(res_prsed,data_output_dir+"xgboost_all_tune_results.csv")

setnames(res_prsed,"prob_pos_week.test.mean","prob_pos_week")

# "nrounds"          "eta"              "colsample_bytree" "max_depth"        "subsample"       
# "prob_pos_week"   

ggplot(res_prsed,aes(x=nrounds,y=prob_pos_week))+geom_point()+geom_smooth()   # There is an optima ar aaround 270
ggplot(res_prsed,aes(x=eta,y=prob_pos_week))+geom_point()+geom_smooth()       # There is an optima at around 0.03
ggplot(res_prsed,aes(x=subsample,y=prob_pos_week))+geom_point()+geom_smooth() # No clear correlation
ggplot(res_prsed,aes(x=max_depth,y=prob_pos_week))+geom_point()+geom_smooth() # 4 seems to be the best


# Inaccurate
#data = generateHyperParsEffectData(res,partial.dep = T)
#plt = plotHyperParsEffect(data, x = "nrounds", y = "eta", z =
#                            "prob_pos_week.test.mean",
#                          plot.type = "heatmap",  partial.dep.learn=makeLearner("regr.glm"))
#plt



learner_name<- "classif.nnTrain"
lrnr <- makeLearner(learner_name,predict.type="prob")
ps = getParamSearchSpace(learner_name)
#-- Set the maximum number of iterations
ctrl = makeTuneControlRandom(maxit = 50L)
#-- Tune the hyperparameters
res_nn = tuneParams(lrnr, task = tsk, resampling =
                   rsmpl_desc,
                 par.set = ps, control = ctrl,measures = prob_pos_ret)


fwrite(as.data.table(as.matrix(t(c(unlist(res_nn$x),res$y)))),data_output_dir+"nnTrain_best.csv")

res_prsed_nn <- as.data.table(res_nn$opt.path$env$path)
fwrite(res_prsed_nn,data_output_dir+"nnTrain_all_tune_results.csv")




if(F)
{
#-- Set the best parameters to the learner
lrnr_best = setHyperPars(lrnr,par.vals=res$x)

#-- Redo the predictions to get the probabilies
res_pred <- resample(learner = lrnr_best, tsk,resampling = rsmpl_desc , measures = auc)

#-- Parse the results
preds <- as.data.table(res_pred$pred$data)
preds <- preds[,.(id,prob.1,truth)]
setnames(preds,c("id","prob.1","truth"),c("index","prediction","TARGET"))



ret_var<-get_mean_returns_and_variance(preds,dt_time_lut,PF)

cat(red("Probability of positive weekly returns = "+ round(100*(1-pnorm(0,ret_var[[1]]/ret_var[[2]])),2)+" mean returns = "+ret_var[[1]]+" std(returns) = "+ret_var[[2]]+"\n"))



getParamSet(lrnr) # check the setting

hyper.control <- makeHyperControl(
  mlr.control = makeTuneControlRandom(maxit = 20L), #search strategy
  resampling = rsmpl_desc, #resampling strategy
  measures = auc) # measure to use (if you want to use more, than provide the measures with a list

res <- hyperopt(tsk, learner = lrnr,
                hyper.control = hyper.control)
res$x
newlearnernnet <- setHyperPars(classif.lrn, par.vals = res$x)


}

