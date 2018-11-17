

rm(list = ls())


set.seed(123)

library(data.table)
library(mlr)
library(ggplot2)


#--- Directoriewa
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"

#-- General input
conf <- fread("config_file.csv")

#-- General input
SL <- conf$SL[1]
PF <- conf$PF[1]
SPREAD <- 2
test_ratio <- conf$test_portion[1]
#method_CV <- "FixedWindowCV"
initial.window<-conf$TRAIN_initial.window[1]
horizon <- conf$TRAIN_horizon[1]
wind <- conf$TRAIN_window_type[1]

sharpe_ratio = function(task, model, pred, feats, extra.args) {
  
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
sharpe_ratio = makeMeasure(
  id = "sharpe_ratio", name = "sharpe_ratio",
  properties = c("classif", "classif.multi", "req.pred",
                 "req.truth"),
  minimize = FALSE,  fun = sharpe_ratio
)


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
    #print(nrow(res_sel))
    if(nrow(res_sel)>10)
    {  
      
      #-- Compute the sharpe ratio as average ret per tra over the variance
      #-- Net equity
      res_sel[,equity:=2*(as.numeric(truth)-1.5)][equity>0,equity:=PF][,equity:=cumsum(equity)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
      
      #total_ret <-  (nrow(res_sel[ truth==T])-nrow(res_sel[truth==F]))#/nrow(res_sel)
      total_ret <-  res_sel[nrow(res_sel), equity]
      std_ret <- sqrt(var(res_sel$equity))
      min_drawdown <- max(res_sel$drawdown)
      #sharpe_ratio <- total_ret/(1+std_ret)
      sharpe_ratio <- total_ret/((1+min_drawdown)*iters)
      
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


#-- Read data and
dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
#-- Bar size and distances
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
#     }
#   }
# }
# #-- Cut first part
# dt<-dt[1e3:nrow(dt),]
#-- Exclude prices
logical_feats <- names(dt)[grepl("chaikin",names(dt))]
overfit_feats <- names(dt)[grepl("_Open$",names(dt)) | grepl("_High$",names(dt)) | grepl("_Close$",names(dt)) | grepl("_Low$",names(dt))]
non_feat_cols <- c(logical_feats,overfit_feats,"Time",names(dt)[grepl("buy_profit",names(dt))| grepl("^bs_",names(dt)) | grepl("buy_loss",names(dt)) | grepl("sell_loss",names(dt)) | grepl("sell_profit",names(dt))  ] )
feat_cols <- setdiff(names(dt),non_feat_cols)
dt_sel <- dt[,..feat_cols]
#-- Split train and test
dt_test <- dt_sel[floor(nrow(dt_sel)*(1-test_ratio)):nrow(dt),]
dt_sel <- dt_sel[1:(floor(nrow(dt_sel)*(1-test_ratio))-1),]





dt_bst_features <- fread(paste0(data_output_dir,"SELECTED_FEATURES_SL_",SL,"_PF_",PF,"_ALL.csv"))
dt_bst_features<- dt_bst_features[order(avg_sharpe)]


#dt_bst_features[grepl("_50",features),features:=gsub("_50","_2000",features)]


#dt_bst_features[grepl("_50",features),]

instruments <- unique(dt_bst_features$instrument)



i<-1

params<- as.data.table(expand.grid(
  nrounds=conf$TRAIN_nrounds,
  eta = conf$TRAIN_eta,
  max_depth=conf$TRAIN_max_depth
))



for (curr_instrument in instruments)
{

  print(curr_instrument)
  #curr_instrument <- "BUY_RES_EURUSD"
    #-- Get the best features of the current instrument
  selected_features <- dt_bst_features[curr_instrument==instrument,features]
  
  
  #-- Set the target column
  dt_curr<-copy(dt_sel)
  dt_curr[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
  dt_curr$TARGET <- as.factor(dt_curr$TARGET)
  results_cols <- c(names(dt_curr)[grepl("BUY_RES",names(dt_curr)) | grepl("SELL_RES",names(dt_curr)) ] )
  feat_cols <- setdiff(c(selected_features,"TARGET"),results_cols)
  dt_curr <- dt_curr[,..feat_cols]
  
  for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.infinite(dt_curr[[j]])), j, NA)
  
  
curr_params <- copy(params)  


#-- Parameter set
discrete_ps = makeParamSet(
  makeDiscreteParam("nrounds", values = unique(curr_params$nrounds)),
  makeDiscreteParam("eta", values =unique(curr_params$eta)),
  makeDiscreteParam("max_depth", values = unique(curr_params$max_depth))
)
curr_params$sharpe <-   -99
curr_params$instrument <- curr_instrument
  
#--- Creating a learner
lrnr<-makeLearner("classif.xgboost", predict.type = "prob")


#-- Make grid search
ctrl = makeTuneControlGrid()

#-- Make task
tsk <- makeClassifTask(id="FX",data=as.data.frame(dt_curr), target="TARGET")

#-- Make the resampling strategy
rsmpl_desc = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)

#-- Do the hypersearch
res = tuneParams(learner = lrnr, task = tsk, control = ctrl, 
                 resampling = rsmpl_desc, par.set = discrete_ps, show.info = TRUE, measures = list(auc))
                 #, measures = list(sharpe_ratio))


#res$learner

#lrnr_bst <- makeLearner("classif.xgboost", predict.type = "prob", nrounds = 20, eta = 0.1) 
#lrnr_bst <- res

lrnr_bst = makeLearner("classif.xgboost", predict.type = "prob", nrounds = res$x$nrounds, eta =res$x$eta , max_depth = res$x$max_depth)
r = resample(learner = lrnr_bst, task = tsk, resampling = rsmpl_desc, show.info = TRUE, measures=list(auc), models = F)

             #, measures=list(sharpe_ratio), models = F)


opt_res <- getBestThresh(as.data.table(r$pred$data))

dt_plot <- opt_res[[6]]
dt_plot$iter <- as.factor(dt_plot$iter)
ggplot(dt_plot)+geom_point(aes(x=id,y=equity,fill=iter))
ggsave(paste0(data_output_dir,curr_instrument,"_equity.png"))

dt_curr_res = data.table(instrument = curr_instrument,nrounds = res$x$nrounds, eta = res$x$eta,max_depth = res$x$max_depth, threshold = opt_res[[5]], total_res = opt_res[[2]] , drawdown = opt_res[[1]], neg_equity = opt_res[[3]], old_sharpe = mean(dt_bst_features[curr_instrument==instrument,avg_sharpe]),  sharpe_ratio = opt_res[[4]])
dt_curr_res[,IMPROVEMENT:=sharpe_ratio/old_sharpe]

  if(i==1)
{
    dt_res <-dt_curr_res
  }else{
    
    dt_res <-rbind(dt_res,dt_curr_res)
  }
i<-i+1  
  print(dt_res)
  
}


fwrite(dt_res, paste0(data_output_dir,"BEST_PARAMS_SL_",SL,"_PF_",PF,".csv"))

