##------ MLR


gc()

rm(list=ls())

library(mlr)
library(data.table)
library(FSelector)
library(ggplot2)


#--- Directory definitoins
data_input_dir <- "02_data/input/"
data_intermediate_dir <- "02_data/intermediate/"
data_output_dir <- "02_data/output/"
models_prod_dir <- "03_models/prod/"
models_archive_dir <- "03_models/archive/"

SL <- 15
PF <- 2
SPREAD <- 2
test_ratio <- 0.3
#method_CV <- "FixedWindowCV"
initial.window<-0.5
horizon <- 1e3


getDrawDownTotalRet <- function(dt)
{
  
  res_sel <- dt[response==T]
  total_ret <-  (nrow(res_sel[ truth==T])-nrow(res_sel[truth==F]))
  res_sel[,equity:=2*(as.numeric(truth)-1.5)][,equity:=cumsum(equity)][equity>0,equity:=PF][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  
  return( list(max(res_sel$drawdown),total_ret, nrow(res_sel[equity<0]) ,  total_ret/(1+max(res_sel$drawdown)) ,res_sel))
}
  


getBestThresh <- function(dt)
{
  res_orig <- as.data.table(dt)
  thresh_vec <- seq(0.01,0.99,0.01)
  bst_thresh <-0
  max_sharpe_ratio <- -991
  bst_drawdown <- -9999
  max_avg_ret <- -999
  for (th in thresh_vec)
  {
    res_sel <- copy(res_orig)
    res_sel[,response:=prob.TRUE>th]
    res_sel <- res_sel[response==T]
    #print(nrow(res_sel))
    if(nrow(res_sel)>10)
    {  
      
      #-- Compute the sharpe ratio as average ret per tra over the variance
      #-- Net equity
      res_sel[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
      
      #total_ret <-  (nrow(res_sel[ truth==T])-nrow(res_sel[truth==F]))#/nrow(res_sel)
      total_ret <-  res_sel[nrow(res_sel),equity]
      
      std_ret <- sqrt(var(res_sel$equity))
      min_drawdown <- max(res_sel$drawdown)
      #sharpe_ratio <- total_ret/(1+std_ret)
      sharpe_ratio <- total_ret/((1+min_drawdown)*max(res_sel$iter))
      
      if(sharpe_ratio>max_sharpe_ratio)
        #   if(avg_ret>max_avg_ret)
      {
        #      print(th)
        #      print(curr_acc)
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

drawdown = function(task, model, pred, feats, extra.args) {
  
  predTable <- as.data.table(pred)
  predTable <- predTable[response==T]
  predTable[,equity:=2*(as.numeric(truth)-1.5)][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
  max(predTable$drawdown)
}
drawdown = makeMeasure(
  id = "drawdown", name = "Drawdown",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = TRUE,  fun = drawdown
)

total_ret = function(task, model, pred, feats, extra.args) {
  tb = table(getPredictionResponse(pred),
             getPredictionTruth(pred))
  tb[1,1]-tb[1,2]
  #1 - sum(diag(tb)) / sum(tb)
}
total_ret = makeMeasure(
  id = "total_ret", name = "Total returns",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE, fun = total_ret
)



#-- Read data

dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
#-- Bar size and distances
dt[,BAR_SIZE_USDJPY:=USDJPY_High-USDJPY_Low][,BAR_DIR_USDJPY:=USDJPY_Close-USDJPY_Open][,dist_High_USDJPY:=USDJPY_Close-USDJPY_High][,dist_Low_USDJPY:=USDJPY_Close-USDJPY_Low]
dt[,BAR_SIZE_EURUSD:=EURUSD_High-EURUSD_Low][,BAR_DIR_EURUSD:=EURUSD_Close-EURUSD_Open][,dist_High_EURUSD:=EURUSD_Close-EURUSD_High][,dist_Low_EURUSD:=EURUSD_Close-EURUSD_Low]
dt[,BAR_SIZE_GBPUSD:=GBPUSD_High-GBPUSD_Low][,BAR_DIR_GBPUSD:=GBPUSD_Close-GBPUSD_Open][,dist_High_GBPUSD:=GBPUSD_Close-GBPUSD_High][,dist_Low_GBPUSD:=GBPUSD_Close-GBPUSD_Low]
dt[,BAR_SIZE_AUDUSD:=AUDUSD_High-AUDUSD_Low][,BAR_DIR_AUDUSD:=AUDUSD_Close-AUDUSD_Open][,dist_High_AUDUSD:=AUDUSD_Close-AUDUSD_High][,dist_Low_AUDUSD:=AUDUSD_Close-AUDUSD_Low]
dt[,BAR_SIZE_NZDUSD:=NZDUSD_High-NZDUSD_Low][,BAR_DIR_NZDUSD:=NZDUSD_Close-NZDUSD_Open][,dist_High_NZDUSD:=NZDUSD_Close-NZDUSD_High][,dist_Low_NZDUSD:=NZDUSD_Close-NZDUSD_Low]
dt[,BAR_SIZE_USDCHF:=USDCHF_High-USDCHF_Low][,BAR_DIR_USDCHF:=USDCHF_Close-USDCHF_Open][,dist_High_USDCHF:=USDCHF_Close-USDCHF_High][,dist_Low_USDCHF:=USDCHF_Close-USDCHF_Low]
dt[,BAR_SIZE_USDCAD:=USDCAD_High-USDCAD_Low][,BAR_DIR_USDCAD:=USDCAD_Close-USDCAD_Open][,dist_High_USDCAD:=USDCAD_Close-USDCAD_High][,dist_Low_USDCAD:=USDCAD_Close-USDCAD_Low]



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

instruments <- c("BUY_RES_EURUSD","SELL_RES_GBPUSD","BUY_RES_GBPUSD","SELL_RES_EURUSD",
                  "SELL_RES_USDJPY","BUY_RES_USDJPY","SELL_RES_AUDUSD","BUY_RES_AUDUSD",
                  "BUY_RES_USDCAD","SELL_RES_USDCAD","BUY_RES_NZDUSD","SELL_RES_NZDUSD",
                  "SELL_RES_USDCHF","BUY_RES_USDCHF")



for (specific_target in instruments)
{
  
  #-- Set the target and remove columns of other results
#  specific_target<-"BUY_RES_USDJPY"
  dt_curr<-copy(dt_sel)
  dt_curr[,TARGET:=.SD >  0.5,.SDcols = specific_target]
  dt_curr$TARGET <- as.factor(dt_curr$TARGET)
  results_cols <- c(names(dt_curr)[grepl("BUY_RES",names(dt_curr)) | grepl("SELL_RES",names(dt_curr)) ] )
  feat_cols <- setdiff(names(dt_curr),results_cols)
  dt_curr <- dt_curr[,..feat_cols]
  
  
  
  #-- Create weights
  
    #-- Make task
  tsk <- makeClassifTask(id="FX",data=as.data.frame(dt_curr), target="TARGET")
  

  
  
  params<-expand.grid(method = c("FixedWindowCV","GrowingWindowCV"),
                      initial.window = initial.window,
                      horizon = horizon,
    instrument=c(specific_target),
                      nrounds=c(100,200,400),
                      eta=c(0.01,0.1)
#                      max_depth=c(5,6)
  )
  
  
  params$drawdown <- -1
  params$total_prof <- -1
  params$neg_equity <- -1
  params$sharpe_ratio <- -1
  params$bst_thresh <- -1
  
  #params[,tree_fact:=nrounds*eta]

    params <- as.data.table(params)
  params<-params[sample(nrow(params)),]
  
  
  equities <- list()
  i<-1L
  while(i<(1+nrow(params)))
  {
    print(i)

    #-- Make the resampling strategy
    rsmpl_desc = makeResampleDesc(method=as.character(params[i,method]),initial.window=initial.window,horizon=horizon, skip =horizon)
    
      #-- Make learner with current parameters
  #lrn = makeLearner("classif.xgboost", predict.type = "response",eta=params[i,eta],nrounds=params[i,nrounds])
  lrn = makeLearner("classif.xgboost", predict.type = "prob",eta=params[i,eta],nrounds=params[i,nrounds])
  
  #-- Do the cross validation
  r = resample(learner = lrn, task = tsk, resampling = rsmpl_desc, show.info = FALSE, measures=list(acc))

  #-- Get the specs
#  opt_res <- getDrawDownTotalRet(as.data.table(r$pred$data))
  opt_res <- getBestThresh(as.data.table(r$pred$data))
  

  equities[[i]]<-opt_res[[6]]
  #-- Update the table
  set(params,i,"drawdown",opt_res[[1]])
  set(params,i,"total_prof",opt_res[[2]])
  set(params,i,"neg_equity",opt_res[[3]])
  set(params,i,"sharpe_ratio",opt_res[[4]])
  set(params,i,"bst_thresh",opt_res[[5]])
  print(params[order(-sharpe_ratio)])
  
  print("-------------------------")
    i<-i+1L
  
  }


fwrite(params, paste0(data_output_dir,"params_",specific_target,"_CV_type_",as.character(params[i,method]),"_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,".csv"))    
  
}
  
  
  