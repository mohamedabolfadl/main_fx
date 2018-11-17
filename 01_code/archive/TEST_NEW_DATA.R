




rm(list=ls())


library(data.table)
library(mlr)



set.seed(123)

library(data.table)
library(mlr)

#--- Directoriewa
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"

#-- General input
SL <- 15
PF <- 2
SPREAD <- 2
test_ratio <- 0.3
N_pairs<-4
windowType <- "GrowingWindowCV"
initial.window <- 0.5
horizon <- 1e3


getTotalRet=function(pred)
{
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  return(predTable[nrow(predTable), equity])
  #/((1+max(predTable$drawdown)))

  
}


getDD=function(pred)
{
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  #return(predTable[nrow(predTable), equity])
  return((max(predTable$drawdown)))
  
  
}


sharpe_ratio = function(task, model, pred, feats, extra.args) {
  
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  predTable[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  #print(max(predTable$iter))
  #print("-----------")
  (predTable[nrow(predTable), equity])/((1+max(predTable$drawdown)))
}
sharpe_ratio = makeMeasure(
  id = "sharpe_ratio", name = "sharpe_ratio",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE,  fun = sharpe_ratio
)



#-- Read the whole data, attach indicators and select the test sample
dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
#-- Bar size and distances
dt[,BAR_SIZE_USDJPY:=USDJPY_High-USDJPY_Low][,BAR_DIR_USDJPY:=USDJPY_Close-USDJPY_Open][,dist_High_USDJPY:=USDJPY_Close-USDJPY_High][,dist_Low_USDJPY:=USDJPY_Close-USDJPY_Low][,dist_Open_USDJPY:=USDJPY_Close-USDJPY_Open][,delta_Close_USDJPY:=USDJPY_Close-shift(USDJPY_Close)][,delta_Low_USDJPY:=USDJPY_Low-shift(USDJPY_Low)][,delta_High_USDJPY:=USDJPY_High-shift(USDJPY_High)]                      
dt[,BAR_SIZE_EURUSD:=EURUSD_High-EURUSD_Low][,BAR_DIR_EURUSD:=EURUSD_Close-EURUSD_Open][,dist_High_EURUSD:=EURUSD_Close-EURUSD_High][,dist_Low_EURUSD:=EURUSD_Close-EURUSD_Low][,dist_Open_EURUSD:=USDJPY_Close-EURUSD_Open][,delta_Close_EURUSD:=EURUSD_Close-shift(EURUSD_Close)][,delta_Low_EURUSD:=EURUSD_Low-shift(EURUSD_Low)][,delta_High_EURUSD:=EURUSD_High-shift(EURUSD_High)]                      
dt[,BAR_SIZE_GBPUSD:=GBPUSD_High-GBPUSD_Low][,BAR_DIR_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,dist_High_GBPUSD:=GBPUSD_Close-GBPUSD_High][,dist_Low_GBPUSD:=GBPUSD_Close-GBPUSD_Low][,dist_Open_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,delta_Close_GBPUSD:=GBPUSD_Close-shift(GBPUSD_Close)][,delta_Low_GBPUSD:=GBPUSD_Low-shift(GBPUSD_Low)][,delta_High_GBPUSD:=GBPUSD_High-shift(GBPUSD_High)]                      
dt[,BAR_SIZE_AUDUSD:=AUDUSD_High-AUDUSD_Low][,BAR_DIR_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,dist_High_AUDUSD:=AUDUSD_Close-AUDUSD_High][,dist_Low_AUDUSD:=AUDUSD_Close-AUDUSD_Low][,dist_Open_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,delta_Close_GBPUSD:=AUDUSD_Close-shift(AUDUSD_Close)][,delta_Low_AUDUSD:=AUDUSD_Low-shift(AUDUSD_Low)][,delta_High_AUDUSD:=AUDUSD_High-shift(AUDUSD_High)]                      
dt[,BAR_SIZE_NZDUSD:=NZDUSD_High-NZDUSD_Low][,BAR_DIR_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,dist_High_NZDUSD:=NZDUSD_Close-NZDUSD_High][,dist_Low_NZDUSD:=NZDUSD_Close-NZDUSD_Low][,dist_Open_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,delta_Close_NZDUSD:=NZDUSD_Close-shift(NZDUSD_Close)][,delta_Low_NZDUSD:=NZDUSD_Low-shift(NZDUSD_Low)][,delta_High_NZDUSD:=NZDUSD_High-shift(NZDUSD_High)]                      
dt[,BAR_SIZE_USDCHF:=USDCHF_High-USDCHF_Low][,BAR_DIR_USDCHF:=USDCHF_Close-USDCHF_Open][,dist_High_USDCHF:=USDCHF_Close-USDCHF_High][,dist_Low_USDCHF:=USDCHF_Close-USDCHF_Low][,dist_Open_USDCHF:=USDCHF_Close-USDCHF_Open][,delta_Close_USDCHF:=USDCHF_Close-shift(USDCHF_Close)][,delta_Low_USDCHF:=USDCHF_Low-shift(USDCHF_Low)][,delta_High_USDCHF:=USDCHF_High-shift(USDCHF_High)]                      
dt[,BAR_SIZE_USDCAD:=USDCAD_High-USDCAD_Low][,BAR_DIR_USDCAD:=USDCAD_Close-USDCAD_Open][,dist_High_USDCAD:=USDCAD_Close-USDCAD_High][,dist_Low_USDCAD:=USDCAD_Close-USDCAD_Low][,dist_Open_USDCAD:=USDCAD_Close-USDCAD_Open][,delta_Close_USDCAD:=USDCAD_Close-shift(USDCAD_Close)][,delta_Low_USDCAD:=USDCAD_Low-shift(USDCAD_Low)][,delta_High_USDCAD:=USDCAD_High-shift(USDCAD_High)]                      
#-- lags

lags_vec <- c(1,2,4)
inds_to_lag <- c("BAR_SIZE","BAR_DIR","dist_High","dist_Low","dist_Open","RSI")
for (ind in inds_to_lag)
{
  feats_to_lag <- names(dt)[grepl(ind,names(dt))]
  for (feat in feats_to_lag)
  {
    for (lag in lags_vec)
    {
      dt[,paste0(feat,"_",lag):=shift(get(feat),lag)]
    }
  }
}
#-- Cut first part
dt<-dt[1e3:nrow(dt),]
#-- Exclude prices
logical_feats <- names(dt)[grepl("chaikin",names(dt))]
overfit_feats <- names(dt)[grepl("_Open$",names(dt)) | grepl("_High$",names(dt)) | grepl("_Close$",names(dt)) | grepl("_Low$",names(dt))]
non_feat_cols <- c(logical_feats,overfit_feats,"Time",names(dt)[grepl("buy_profit",names(dt))| grepl("^bs_",names(dt)) | grepl("buy_loss",names(dt)) | grepl("sell_loss",names(dt)) | grepl("sell_profit",names(dt))  ] )
feat_cols <- setdiff(names(dt),non_feat_cols)
dt_sel <- dt[,..feat_cols]
#-- Split train and test
dt_test <- dt_sel[floor(nrow(dt_sel)*(1-test_ratio)):nrow(dt),]
dt_sel <- dt_sel[1:(floor(nrow(dt_sel)*(1-test_ratio))-1),]



#-- Read the training parameters

bst_params <- fread(paste0(data_output_dir,"BEST_PARAMS_SL_",SL,"_PF_",PF,".csv"))

instruments <- c("BUY_RES_EURUSD","SELL_RES_GBPUSD","BUY_RES_GBPUSD","SELL_RES_EURUSD",
                 "SELL_RES_USDJPY","BUY_RES_USDJPY","SELL_RES_AUDUSD","BUY_RES_AUDUSD",
                 "BUY_RES_USDCAD","SELL_RES_USDCAD","BUY_RES_NZDUSD","SELL_RES_NZDUSD",
                 "SELL_RES_USDCHF","BUY_RES_USDCHF")

i<-1

for (curr_instrument in instruments)
{

  print(curr_instrument)
  #-- Train on the whole training part
#curr_instrument <- "BUY_RES_USDCAD"
dt_curr<-copy(dt_sel)
dt_curr[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
dt_curr$TARGET <- as.factor(dt_curr$TARGET)
results_cols <- c(names(dt_curr)[grepl("BUY_RES",names(dt_curr)) | grepl("SELL_RES",names(dt_curr)) ] )
feat_cols <- setdiff(names(dt_curr),results_cols)
dt_curr <- dt_curr[,..feat_cols]

#--- Creating a learner
lrnr<-makeLearner("classif.xgboost", predict.type = "prob", nrounds=bst_params[instrument==curr_instrument,nrounds],
                                                            eta=bst_params[instrument==curr_instrument,eta],
                                                            max_depth=bst_params[instrument==curr_instrument,max_depth])

#-- Make task
tsk <- makeClassifTask(id="FX",data=as.data.frame(dt_curr), target="TARGET")


#-- Train on the trainset

mod <- train(lrnr,tsk)

#-- Test on the test part
dt_curr_test<-copy(dt_test)
dt_curr_test[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
dt_curr_test$TARGET <- as.factor(dt_curr_test$TARGET)
results_cols <- c(names(dt_curr_test)[grepl("BUY_RES",names(dt_curr_test)) | grepl("SELL_RES",names(dt_curr_test)) ] )
feat_cols <- setdiff(names(dt_curr_test),results_cols)
dt_curr_test <- dt_curr_test[,..feat_cols]




res <- predict(mod, newdata =as.data.frame(dt_curr_test))

dt_res <- res$data
dt_res$response<-FALSE
dt_res<-as.data.table(dt_res)
dt_res[prob.TRUE>bst_params[instrument==curr_instrument,threshold],response:=TRUE]


#res_dt <- res_th$data





dt_stat <- dt_res[,.N,by=.(truth,response)]
dt_stat$instrument <- curr_instrument
dt_stat$DD<-getDD(dt_res)
dt_stat$total_ret<-getTotalRet(dt_res)

if(i==1)
{
  dt_all <- dt_stat
}else{
  
  dt_all <- rbind(dt_all, dt_stat)
  
}

print(dt_stat)

i<-i+1


}


print("Total returns")
dt_all[truth==T & response==T,sum(N)]

print("Total loss")
dt_all[truth==F & response==T,sum(N)]

print("Accuracy")
dt_all[truth==T & response==T,sum(N)]/(dt_all[truth==T & response==T,sum(N)]+dt_all[truth==F & response==T,sum(N)])



