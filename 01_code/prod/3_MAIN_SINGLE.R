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
#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"


#------------------------------------------------------------#
##############################################################
#------------------------------------------------------------#


#------------------------------------------------------------#
################## DEFINE THE CONFIGURATIONS #################
#------------------------------------------------------------#

config_file <- data.table(
  SL = 15, # Stop loss
  PF = 1,  # Profit factor
  test_portion = 0.3, # Out of sample test part for final evaluation
  FEAT_SEL_get_feature_importance = F, # Flag for getting the feature importance
  featSelProb=0.8,
  FEAT_SEL_nrounds = 10, # Number of trees used in feature selection model
  FEAT_SEL_eta = 0.1,    # Learning rate of the xgboost in feature selection
  FEAT_SEL_initial.window = 1e4, # Portion of the dataset to be used for training
  FEAT_SEL_horizon = 1e3,        # Future samples to be used for testing
  FEAT_SEL_window_type = "FixedWindowCV",# "GrowingWindowCV", # "FixedWindowCV",
  FEAT_SEL_max_iterations = 2e2,
  TRAIN_nrounds = c(50), # Search space for number of trees
  TRAIN_eta = c(0.1),       # Search space for learning ratre
  TRAIN_max_depth = c(4,6,8),    # Search space for tree depth
  TRAIN_window_type =  "FixedWindowCV", #"GrowingWindowCV",
  TRAIN_initial.window = 1e4,    # Window size for training
  TRAIN_horizon = 1e3,           # Future window for testing
  notes="Using AUC as criteria"
)
all_pairs <- c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCHF","NZDUSD","XAUUSD","USDCAD")

#-- List of pairs to trade
REMOVE_FAST_WINS <- T # Flag to remove the positive trades which are finished in less than MIN_TRADE_TIME
MIN_TRADE_TIME <- 15
instruments <- data.table(currs = c("BUY_RES_EURUSD"))
FEATURE_SELECTION <- T # use correlations to select the features
#-- Read the configurations
returns_period = "week" #"month","day" defines the period of aggregating the returns
READ_SELECTED_FEATURES <- F
WRITE_FLAG <- F
SPREAD <- 3 # Spread, make sure there is a file with the specified spread
SL <- config_file$SL[1]
PF <- config_file$PF[1]
test_ratio <- config_file$test_portion[1]
initial.window<-config_file$TRAIN_initial.window[1]
horizon <- config_file$TRAIN_horizon[1]
wind <- config_file$TRAIN_window_type[1]


#------------------------------------------------------------#
############### DEFINE THE FUNCTIONS #########################
#------------------------------------------------------------#

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


#-- Read the basic features table
basic_cols <-fread(data_intermediate_dir+"basic_features.csv")


dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))

dt[,index:= seq(1,nrow(dt))]

if(REMOVE_FAST_WINS)
{
  
  for(pr in all_pairs)
  {
    dt[ get("buy_profit_"+pr)<MIN_TRADE_TIME  ,("BUY_RES_"+pr):=0]
    dt[ get("sell_profit_"+pr)<MIN_TRADE_TIME  ,("SELL_RES_"+pr):=0]
  }
  
}

dt_time_lut <- dt[,.(index,Time)]

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



#-- Extract variables that should not be included in the training 
#logical_feats <- names(dt)[grepl("chaikin",names(dt))]
#overfit_feats <- names(dt)[grepl("_Open$",names(dt)) | grepl("_High$",names(dt)) | grepl("_Close$",names(dt)) | grepl("_Low$",names(dt))]
#non_feat_cols <- c(logical_feats,overfit_feats,"Time",names(dt)[grepl("buy_profit",names(dt))| grepl("^bs_",names(dt)) | grepl("buy_loss",names(dt)) | grepl("sell_loss",names(dt)) | grepl("sell_profit",names(dt))  ] )
#exclude_feats <- names(dt)[grepl("EMA",names(dt)) | grepl("williams",names(dt))]
#-- Extract only the feature variables
#feat_cols <- setdiff(names(dt),non_feat_cols)
#feat_cols <- setdiff(feat_cols,exclude_feats)

results_cols <- names(dt)[grepl("BUY_RES",names(dt)) | grepl("SELL_RES",names(dt))]

if(FEATURE_SELECTION)
{
#-- Feature selection

  profit_loss_cols <-  names(dt)[grepl("profit",names(dt)) | grepl("loss",names(dt))]
  bs_cols <- names(dt)[grepl("^bs",names(dt))]
  ohlc_cols <- names(dt)[grepl("Low$",names(dt)) | grepl("Close$",names(dt)) | grepl("Open$",names(dt)) | grepl("High$",names(dt))]
  full_features <- setdiff(names(dt),c("index","Time",results_cols,profit_loss_cols,ohlc_cols,bs_cols))
  dt_features <- dt[,..full_features]
  inds_nearZero_var <- caret::nearZeroVar(x=dt_features[sample(5e3)])
  nonZeroVarCols <- setdiff(names(dt_features),names(dt_features)[inds_nearZero_var])
  dt_features <- dt_features[,..nonZeroVarCols]



#dt_all[,(name_fields):=lapply(.SD,gsub,pattern="\\d",replacement=""),.SDcols = name_fields]

cols <- names(dt_features)
dt_features[,(cols):=lapply(.SD,as.numeric),.SDcols = cols]
#dt_features <- sapply( dt_features, as.numeric )
cr <- cor(dt_features[sample(seq(1e4,nrow(dt_features)),5e3),], use="complete.obs")

highly_correlated_features<-findCorrelation(x=cr,cutoff=0.8)

correlated_features_to_be_excluded<-names(dt_features)[highly_correlated_features]



unique_features<-setdiff(full_features,correlated_features_to_be_excluded)
unique_relevant_cols <- c("index",unique_features,results_cols)
dt_sel<-dt[,..unique_relevant_cols]

}else{
  
  feat_cols <- names(dt)[grepl("TMS",names(dt)) | grepl("RSI$",names(dt))]
  index_cols <- "index"
  relevant_cols <- c(index_cols,feat_cols,results_cols)
  dt_sel <- dt[,..relevant_cols]
  
}


#------------------------------------------------------------#
############## SPLIT TRAIN AND TEST ##########################
#------------------------------------------------------------#

dt_test <- dt_sel[floor(nrow(dt_sel)*(1-test_ratio)):nrow(dt),]
dt_sel <- dt_sel[1:(floor(nrow(dt_sel)*(1-test_ratio))-1),]


#------------------------------------------------------------#
############  READ THE SELECTED FEATURES #####################
#------------------------------------------------------------#

if(READ_SELECTED_FEATURES)
{
dt_bst_features <- fread(paste0(data_output_dir,"SELECTED_FEATURES_SL_",SL,"_PF_",PF,"_ALL.csv"))
dt_bst_features<- dt_bst_features[order(avg_sharpe)]
instruments <- unique(dt_bst_features$instrument)
}
#------------------------------------------------------------#
############  LOOP THROUGH THE INSTRUMENTS AND SAVE BEST #####
#------------------------------------------------------------#



initial.window=1e4
horizon=1e3

#-- Select the features and the target column
results_cols <- c(names(dt_sel)[grepl("BUY_RES",names(dt_sel)) | grepl("SELL_RES",names(dt_sel)) ] )

icurr <- 1

for (curr_instrument in unique(results_cols))
{
  
  if(curr_instrument!="SELL_RES_USDCAD")
    {
  cat(yellow("\n############################################################################################\n"))
  cat(yellow("##################################   "+curr_instrument+" "+icurr+"/"+length(unique(results_cols))+"  ################################\n"))
  cat(yellow("############################################################################################\n"))

  icurr <- icurr+1
  

#-- Format the datatable for the target instrument
#curr_instrument <- "BUY_RES_EURUSD"
dt_curr<-copy(dt_sel)
dt_curr[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
dt_curr$TARGET <- as.numeric(dt_curr$TARGET)

#-- Define the search grid for hyperparameter search
#params<- as.data.table(expand.grid(
#  nrounds=c(50,100),
#  eta = c(0.05,0.1,0.3),
#  max_depth=c(3,4,5)
#))

params<- as.data.table(expand.grid(
  nrounds=c(1000),
  eta = c(0.01),
  max_depth=c(4)
))


feat_cols <- setdiff(names(dt_curr),results_cols)
dt_curr <- dt_curr[,..feat_cols]
#-- Replacing infinite with NA
for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.infinite(dt_curr[[j]])), j, NA)


#-- Initialize results
params[,mean_ret:=numeric(nrow(params))][,var_ret:=numeric(nrow(params))][,threshold:=numeric(nrow(params))]

ii<-1

while(ii<(nrow(params)+1))
{
nrounds = params[ii,nrounds]
eta=params[ii,eta]
max_depth=params[ii,max_depth]
#horizon=params[ii,horizon]

cat(green("nrounds = "+ nrounds + " eta = "+eta+" max_depth = "+max_depth+" horizon ="+horizon+"\n\n"))
dt_res <- train_and_predict(dt_curr,nrounds,eta,max_depth,initial.window,horizon,target_name="TARGET",index_name="index")
dt_time_lut_prediction_period <- dt_time_lut[index%in%dt_res$index]
ret_var<-get_mean_returns_and_variance(dt_res,dt_time_lut_prediction_period,PF)

params[ii,mean_ret:=ret_var[[1]]]
params[ii,var_ret:=ret_var[[2]]]
params[ii,threshold:=ret_var[[3]]]
#cat(red("Best ratio = "+ round(100*ret_var[[1]]/ret_var[[2]])+" mean returns = "+ret_var[[1]]+" std returns + 1 = "+ret_var[[2]]+"\n"))
cat(red("Probability of positive weekly returns = "+ round(100*(1-pnorm(0,ret_var[[1]]/ret_var[[2]])),2)+" mean returns = "+ret_var[[1]]+" std(returns) = "+ret_var[[2]]+"\n"))
cat(blue(round(100*ii/nrow(params)))+"% finished")

cat(yellow("\n###########################################\n\n"))


ii<-ii+1

}

}


fwrite(params,data_output_dir+"/"+curr_instrument+"/params_"+curr_instrument+".csv")

}



#------------------------------------------------------------#
###########  JOIN ALL RESULTS INTO ONE TABLE #################
#------------------------------------------------------------#


#-- Join all results tables
i<-1
for (curr_instrument in unique(results_cols))
{
  
  if(file.exists(data_output_dir+"/"+curr_instrument+"/params_"+curr_instrument+".csv"))
{  params<-fread(data_output_dir+"/"+curr_instrument+"/params_"+curr_instrument+".csv")
  params$instrument <- curr_instrument
  
  if(i==1)
  {
    params_all<-params
  }else{
    params_all<-rbind(params_all,params)
    
  }
  
  i<-i+1
  }
  }

params_all[,sharpe_ratio:=100*(1-pnorm(0,mean_ret,var_ret))]
params_all<-params_all[!is.na(sharpe_ratio) & !is.nan(sharpe_ratio)]


best_parameters <- params_all[,.SD[which.max(sharpe_ratio)],by="instrument"]

if(WRITE_FLAG)
{  
  fwrite(params_all,data_output_dir+"params_all_horizon_"+horizon+" window "+initial.window+".csv")
  fwrite(best_parameters,data_output_dir+"best_parameters_"+horizon+" window "+initial.window+".csv")
}


#------------------------------------------------------------#
###########  APPEND CURRENT PARAMETERS TO EXISTING ###########
#------------------------------------------------------------#


if(F)
  {
#-- Read current total parameters
params_total <- fread(data_output_dir+"params_total.csv")
#-- Add rows of the new run
params_total<-rbind(params_total,params_all)
#-- Save the current total
fwrite(params_total,data_output_dir+"params_total.csv")
params_total<-params_total[!is.na(sharpe_ratio) & !is.nan(sharpe_ratio)]
best_parameters <- params_total[,.SD[which.max(sharpe_ratio)],by="instrument"]
}

#------------------------------------------------------------#
########## PREDICTING ON THE TEST SET ########################
#------------------------------------------------------------#

#-- Reduce the horizon to be more careful
#horizon = 1e3

dt_verification <- rbind(dt_sel[(nrow(dt_sel)-initial.window+1):nrow(dt_sel),],dt_test)

i<-1

while(i<(nrow(best_parameters)+1))
{
  curr_instrument<-best_parameters[i,instrument]
  
  
  cat(yellow("\n##############################################################################################\n"))
  cat(yellow("##################################   "+curr_instrument+"   #####################################\n"))
  cat(yellow("##############################################################################################\n"))
  
  
  dt_curr<-copy(dt_verification)
  dt_curr[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
  dt_curr$TARGET <- as.numeric(dt_curr$TARGET)
  
  nrounds=best_parameters[i,nrounds]
  eta = best_parameters[i,eta]
  max_depth=best_parameters[i,max_depth]
  
  
  feat_cols <- setdiff(names(dt_curr),results_cols)
  dt_curr <- dt_curr[,..feat_cols]
  #-- Replacing infinite with NA
  for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.infinite(dt_curr[[j]])), j, NA)
  
  dt_res <- train_and_predict(dt_curr,nrounds,eta,max_depth,initial.window,horizon,target_name="TARGET",index_name="index")
  #-- Adding the decision
  dt_res[,decision:= as.numeric(prediction>best_parameters[i,threshold])]
  
  #-- Getting the result per trade
  dt_res[decision>0 & TARGET>0,RESULT:=1][decision>0 & TARGET<1,RESULT:=-1][is.na(RESULT),RESULT:=0]
  
  
  #-- Renaming the columns and removing useless ones
  dt_res<-dt_res[,.(index,RESULT)]
  setnames(dt_res,"RESULT",curr_instrument)
  if(i==1)
  {
    dt_total_result=dt_res
    
  }else{
    
    dt_total_result<-merge(dt_total_result,dt_res,by="index")
    
  }
  
  i<-i+1
}



instruments <- unique(best_parameters$instrument) 
#instruments<-instruments[!grepl("XAU",instruments)]
dt_total_result$equity<-NULL
dt_total_result[,equity:=0]
dt_total_result$result<-NULL
dt_total_result[,result:=0]

for (inst in instruments)
{
  print(inst)
  vec <- as.vector(dt_total_result[,..inst])
  dt_total_result$result<-dt_total_result$result + vec
  
}


dt_total_result[,equity:=cumsum(result)]
dt_total_result[,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
dt_total_result<-merge(dt_total_result,dt_time_lut,all.x=T,by="index")

returns_per_instrument_per_period<-dt_total_result[,lapply(.SD,sum),.SDcols=results_cols,by=ret_per]
returns_per_instrument_per_period[,ret_per:=NULL]
cov_mat <- cov(returns_per_instrument_per_period) 


max(dt_total_result$drawdown)

test_mean_returns <- dt_total_result[,.(mean_ret=sum(result)),by="ret_per"]

cat("Mean returns = "+mean(test_mean_returns$mean_ret))
cat("Std returns = "+sqrt(var(test_mean_returns$mean_ret)))
cat("Probability of positive week = "+(1-pnorm(0,mean(test_mean_returns$mean_ret),sqrt(var(test_mean_returns$mean_ret)))))


plot_ly(dt_total_result,x=~Time,  y = ~equity,mode="lines",type="scatter")




if(WRITE_FLAG)
  {
fwrite(dt_total_result,data_output_dir+"dt_total_result_test_3.csv")

}
