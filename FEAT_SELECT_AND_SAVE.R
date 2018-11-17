

rm(list = ls())


set.seed(123)

library(data.table)
library(mlr)
library(ggplot2)


#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"



conf <- fread("config_file.csv")

#-- General input
SL <- conf$SL[1]
PF <- conf$PF[1]
SPREAD <- 2
test_ratio <- conf$test_portion[1]
featSelProb <- conf$featSelProb[1]
windowType <- conf$FEAT_SEL_window_type[1]
initial.window <- conf$FEAT_SEL_initial.window[1]
horizon <- conf$FEAT_SEL_horizon[1]
eta     <-conf$FEAT_SEL_eta[1]
nrounds  <-conf$FEAT_SEL_nrounds[1]
randiters <- conf$FEAT_SEL_max_iterations[1]
GET_FEATURE_IMPORTANCE <- conf$FEAT_SEL_get_feature_importance[1]



sharpe_ratio = function(task, model, pred, feats, extra.args) {
  
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  if(nrow(predTable)>10)
  {  
  predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
    (predTable[nrow(predTable), equity])/((1+max(predTable$drawdown)))
  }else{
  
    (0)
}
    }
sharpe_ratio = makeMeasure(
  id = "sharpe_ratio", name = "sharpe_ratio",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE,  fun = sharpe_ratio
)




#instruments <- c("BUY_RES_EURUSD","SELL_RES_GBPUSD","BUY_RES_GBPUSD","SELL_RES_EURUSD",
#                 "SELL_RES_USDJPY","BUY_RES_USDJPY","SELL_RES_AUDUSD","BUY_RES_AUDUSD",
#                 "BUY_RES_USDCAD","SELL_RES_USDCAD","BUY_RES_NZDUSD","SELL_RES_NZDUSD",
#                 "SELL_RES_USDCHF","BUY_RES_USDCHF")

instruments <- fread("curr_list.csv")
instruments <- instruments$currs


for (instrument in instruments)
{
  
  
  #-- Create directory for the outputs
  dir.create(file.path(paste0(data_output_dir,instrument)))
  
  #-- Read all the results
  dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
  
  # #-- Bar size and distances
  # dt[,BAR_SIZE_USDJPY:=USDJPY_High-USDJPY_Low][,BAR_DIR_USDJPY:=USDJPY_Close-USDJPY_Open][,dist_High_USDJPY:=USDJPY_Close-USDJPY_High][,dist_Low_USDJPY:=USDJPY_Close-USDJPY_Low][,dist_Open_USDJPY:=USDJPY_Close-USDJPY_Open][,delta_Close_USDJPY:=USDJPY_Close-shift(USDJPY_Close)][,delta_Low_USDJPY:=USDJPY_Low-shift(USDJPY_Low)][,delta_High_USDJPY:=USDJPY_High-shift(USDJPY_High)]                      
  # dt[,BAR_SIZE_EURUSD:=EURUSD_High-EURUSD_Low][,BAR_DIR_EURUSD:=EURUSD_Close-EURUSD_Open][,dist_High_EURUSD:=EURUSD_Close-EURUSD_High][,dist_Low_EURUSD:=EURUSD_Close-EURUSD_Low][,dist_Open_EURUSD:=USDJPY_Close-EURUSD_Open][,delta_Close_EURUSD:=EURUSD_Close-shift(EURUSD_Close)][,delta_Low_EURUSD:=EURUSD_Low-shift(EURUSD_Low)][,delta_High_EURUSD:=EURUSD_High-shift(EURUSD_High)]                      
  # dt[,BAR_SIZE_GBPUSD:=GBPUSD_High-GBPUSD_Low][,BAR_DIR_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,dist_High_GBPUSD:=GBPUSD_Close-GBPUSD_High][,dist_Low_GBPUSD:=GBPUSD_Close-GBPUSD_Low][,dist_Open_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,delta_Close_GBPUSD:=GBPUSD_Close-shift(GBPUSD_Close)][,delta_Low_GBPUSD:=GBPUSD_Low-shift(GBPUSD_Low)][,delta_High_GBPUSD:=GBPUSD_High-shift(GBPUSD_High)]                      
  # dt[,BAR_SIZE_AUDUSD:=AUDUSD_High-AUDUSD_Low][,BAR_DIR_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,dist_High_AUDUSD:=AUDUSD_Close-AUDUSD_High][,dist_Low_AUDUSD:=AUDUSD_Close-AUDUSD_Low][,dist_Open_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,delta_Close_GBPUSD:=AUDUSD_Close-shift(AUDUSD_Close)][,delta_Low_AUDUSD:=AUDUSD_Low-shift(AUDUSD_Low)][,delta_High_AUDUSD:=AUDUSD_High-shift(AUDUSD_High)]                      
  # dt[,BAR_SIZE_NZDUSD:=NZDUSD_High-NZDUSD_Low][,BAR_DIR_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,dist_High_NZDUSD:=NZDUSD_Close-NZDUSD_High][,dist_Low_NZDUSD:=NZDUSD_Close-NZDUSD_Low][,dist_Open_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,delta_Close_NZDUSD:=NZDUSD_Close-shift(NZDUSD_Close)][,delta_Low_NZDUSD:=NZDUSD_Low-shift(NZDUSD_Low)][,delta_High_NZDUSD:=NZDUSD_High-shift(NZDUSD_High)]                      
  # dt[,BAR_SIZE_USDCHF:=USDCHF_High-USDCHF_Low][,BAR_DIR_USDCHF:=USDCHF_Close-USDCHF_Open][,dist_High_USDCHF:=USDCHF_Close-USDCHF_High][,dist_Low_USDCHF:=USDCHF_Close-USDCHF_Low][,dist_Open_USDCHF:=USDCHF_Close-USDCHF_Open][,delta_Close_USDCHF:=USDCHF_Close-shift(USDCHF_Close)][,delta_Low_USDCHF:=USDCHF_Low-shift(USDCHF_Low)][,delta_High_USDCHF:=USDCHF_High-shift(USDCHF_High)]                      
  # dt[,BAR_SIZE_USDCAD:=USDCAD_High-USDCAD_Low][,BAR_DIR_USDCAD:=USDCAD_Close-USDCAD_Open][,dist_High_USDCAD:=USDCAD_Close-USDCAD_High][,dist_Low_USDCAD:=USDCAD_Close-USDCAD_Low][,dist_Open_USDCAD:=USDCAD_Close-USDCAD_Open][,delta_Close_USDCAD:=USDCAD_Close-shift(USDCAD_Close)][,delta_Low_USDCAD:=USDCAD_Low-shift(USDCAD_Low)][,delta_High_USDCAD:=USDCAD_High-shift(USDCAD_High)]                      
  # #-- lags
  # 
  # lags_vec <- c(1,2,4)
  # inds_to_lag <- c("BAR_SIZE","BAR_DIR","dist_High","dist_Low","dist_Open","RSI")
  # for (ind in inds_to_lag)
  # {
  #   feats_to_lag <- names(dt)[grepl(ind,names(dt))]
  #   for (feat in feats_to_lag)
  #   {
  #     for (lag in lags_vec)
  #     {
  #       dt[,paste0(feat,"_",lag):=shift(get(feat),lag)]
  #       
  #     }
  #     
  #   }
  #   
  #   
  # }
  # 
  # 
  # #-- Cut first part
  # dt<-dt[1e3:nrow(dt),]
  # 
  logical_feats <- names(dt)[grepl("chaikin",names(dt))]
  
  #-- Exclude prices
  overfit_feats <- names(dt)[grepl("_Open$",names(dt)) | grepl("_High$",names(dt)) | grepl("_Close$",names(dt)) | grepl("_Low$",names(dt))]
  non_feat_cols <- c(logical_feats,overfit_feats,"Time",names(dt)[grepl("buy_profit",names(dt))| grepl("^bs_",names(dt)) | grepl("buy_loss",names(dt)) | grepl("sell_loss",names(dt)) | grepl("sell_profit",names(dt))  ] )
  
  feat_cols <- setdiff(names(dt),non_feat_cols)
  dt_sel <- dt[,..feat_cols]
  #-- Split train and test
  dt_test <- dt_sel[floor(nrow(dt_sel)*(1-test_ratio)):nrow(dt),]
  dt_sel <- dt_sel[1:(floor(nrow(dt_sel)*(1-test_ratio))-1),]
  
  # instruments <- c("BUY_RES_EURUSD","SELL_RES_GBPUSD","BUY_RES_GBPUSD","SELL_RES_EURUSD",
  #                   "SELL_RES_USDJPY","BUY_RES_USDJPY","SELL_RES_AUDUSD","BUY_RES_AUDUSD",
  #                   "BUY_RES_USDCAD","SELL_RES_USDCAD","BUY_RES_NZDUSD","SELL_RES_NZDUSD",
  #                   "SELL_RES_USDCHF","BUY_RES_USDCHF")
  
  
  
  #-- Set the target column
  dt_curr<-copy(dt_sel)
  dt_curr[,TARGET:=.SD >  0.5,.SDcols = instrument]
  dt_curr$TARGET <- as.factor(dt_curr$TARGET)
  results_cols <- c(names(dt_curr)[grepl("BUY_RES",names(dt_curr)) | grepl("SELL_RES",names(dt_curr)) ] )
  feat_cols <- setdiff(names(dt_curr),results_cols)
  dt_curr <- dt_curr[,..feat_cols]
  for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.infinite(dt_curr[[j]])), j, NA)
  
  #dt_curr[is.infinite(dt_curr)]<-0
  #-- Create weights
  
  #-- Make task
  tsk <- makeClassifTask(id="FX",data=as.data.frame(dt_curr), target="TARGET")
  
  
  #-- Make the resampling strategy
  #rsmpl_desc = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)
  rsmpl_desc = makeResampleDesc(method=windowType,initial.window=initial.window,horizon=horizon, skip =horizon)
  
  
  
  ctrl = makeFeatSelControlRandom(maxit = randiters, prob = featSelProb )
#  ctrl = makeFeatSelControlSequential(method="sfs",maxit = randiters)
  
  lrn_fast = makeLearner("classif.xgboost", eta=eta,nrounds=nrounds, predict.type = "prob" )
  
  
  if(GET_FEATURE_IMPORTANCE)
  {
  #-- Get importance of features on the whole training set
  mod<-train(lrn_fast, tsk)
  
  #-- Get the feature importance matrix
  featImp <- as.data.table(getFeatureImportance(mod)$res)
  cols<-names(featImp)
  fimp<-data.table(features=cols,importance=as.vector(t(featImp[1,])))
  fimp <- fimp[order(-importance)]
  sel_feats <- c(fimp[importance>0,features],"TARGET") 
  
  #-- Update task with selected variables
  tsk <- makeClassifTask(id="FX",data=as.data.frame(dt_curr[,..sel_feats]), target="TARGET")
}
  
  
  #-- Select features
  sfeats = selectFeatures(learner = lrn_fast, task =
                            tsk, resampling = rsmpl_desc,
                          control = ctrl, show.info = TRUE, measures =list(auc))
                          #, measures=(sharpe_ratio))
  
  save_sel_feats <- data.table(features=sfeats$x,avg_sharpe=sfeats$y)
  save_sel_feats<-merge(save_sel_feats,fimp,all.x=T,all.y=F)
  save_sel_feats<-save_sel_feats[order(-importance)]
  
  
  #-- Save the features
  fwrite(save_sel_feats,paste0(data_output_dir,instrument,"/SELECTED_FEATURES_SL_",SL,"_PF_",PF,".csv"))
  
  
  
  
  
}


#--- END OF CODE ------------------


