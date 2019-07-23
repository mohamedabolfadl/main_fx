

# TODO:
# 1. use regression prediction of profit/loss time
# 2. Use prediction of buy and sell models
# 3. Weights to latest models and models with fast winns


library(data.table)
library(lubridate)
#-- Quick mlr 
library(mlr)
library(roll)

set.seed(15042017)
BASE_PERFORMANCE <- 0.5168695



#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"


N_MAX_CONSECUTIVE_TRADES<<-0 # 1 -> -0.004 default 2 10 "0.0082892609104559 % over base performance"
wind<-"FixedWindowCV" # "GrowingWindowCV" "FixedWindowCV"  GrowingWindowCV-> -0.0094
initial.window = 1e3   
horizon = 1e3 # 1e3 "0.00177425934484554 % over base performance"
limit_hours<-c(7,8,9,10,11,12,13,14,15,16)
#limit_hours<-seq(0,23)

PF = 1
SL <- 15
SPREAD <- 3
nrounds = 3
eta =	0.1
colsample_bytree = 0.749399129
max_depth = 3L
subsample = 0.79023482




#-- Functions
if(T)
{
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
      
      
      if(N_MAX_CONSECUTIVE_TRADES>0)
        {
      rolls <- roll_sum(as.matrix(dt_curr[,decision]),width=N_MAX_CONSECUTIVE_TRADES)
      rolls<-as.data.table(rolls)
      names(rolls)<-"triggered_trades"
      dt_curr<-cbind(dt_curr,rolls)
      dt_curr[triggered_trades>1 & !is.na(triggered_trades),decision:=0]
      }
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
    
    #print(bst_mean_ret)
    #print(bst_thr)
    return(bst_sharpe)
  }
  prob_pos_ret = makeMeasure(
    id = "prob_pos_week", name = "prob_pos_week",
    properties = c("classif", "classif.multi", "req.pred",
                   "req.truth"),
    minimize = FALSE,  fun = prob_pos_ret
  )
  
  get_sharpe=function(dt_curr,dt_time_lut_prediction_period,PF)
  {
    dt_portfolio <-  merge(dt_time_lut_prediction_period,dt_curr,all.x = T,by="index")
    #-- Add equity, returns and drawdown
    dt_portfolio[TARGET==1 & decision==1,returns:=PF][TARGET==0 & decision==1,returns:=-1][is.na(returns),returns:=0][,equity:=cumsum(returns)][,drawdown:=cummax(equity)][,drawdown:=(drawdown-equity) ]
    mean_returns <- dt_portfolio[,.(mean_returns=sum(returns)),by="ret_per"]
    return(list(mean_returns,mean(mean_returns$mean_returns),var(mean_returns$mean_returns),max(dt_portfolio$drawdown)))
  }
  
  
  
}




TARGET <- "SELL_RES_EURUSD"

# nrounds = 314
# eta =	0.033127486
# colsample_bytree = 0.849399129
# max_depth = 4L
# subsample = 0.559023482


dt <- fread(paste0("02_data/intermediate/ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
dt[,hour:=as.numeric(lubridate::hour(Time))]
#-- Limit trades
dt<-dt[hour%in%limit_hours]
dt[,index:= seq(1,nrow(dt))]


#-- Create the index, time lookup table
dt_time_lut <<- dt[,.(index,Time)]
dt_time_lut[,ret_per:=paste0(year(Time),"_",week(Time))]


dt_res <- data.table(pair=numeric(14),top_hour=numeric(14),accuracy=numeric(14),lift_hour=numeric(14),prob_pos_ret=numeric(14))


pairs <- c("EURUSD","AUDUSD","USDJPY","USDCAD","GBPUSD","NZDUSD","USDCHF")
#pairs <- c("EURUSD")
pairs<- c(paste0("BUY_RES_",pairs),paste0("SELL_RES_",pairs))
j<-1


#TARGET=pairs[1]


for(TARGET in pairs)
{
setnames(dt,TARGET,"TARGET")

time <- names(dt)[grepl("ime",names(dt))]
closes <- names(dt)[grepl("Close$",names(dt))]
opens <- names(dt)[grepl("Open$",names(dt))]
low <- names(dt)[grepl("Low$",names(dt))]
high <- names(dt)[grepl("High$",names(dt))]
bss <- names(dt)[grepl("bs",names(dt))]
ress <- names(dt)[grepl("RES",names(dt))]
pl <- names(dt)[grepl("(profit|loss)",names(dt))]
nonFeats <- c(time,bss,ress,pl)
relcols <- setdiff(names(dt),nonFeats)
dt_sel <- dt[,..relcols]


#-- parsing target as character so that H2O makes a classification task

#dt_sel <- na.omit(dt_sel)

tsk <- makeClassifTask(id=TARGET,data=as.data.frame(dt_sel), target="TARGET")
rsmpl = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)
lrnr <- makeLearner("classif.xgboost",predict.type="prob",nrounds=nrounds,eta=eta, colsample_bytree = colsample_bytree, max_depth = max_depth, subsample =subsample )
#lrnr <- makeLearner("classif.h2o.glm",predict.type="prob" )
res<-resample(task = tsk,learner=lrnr,measures = auc, resampling = rsmpl, model=T)
preds<-as.data.table(res$pred$data)

#-- Variable importance
#vimp<-mlr::getFeatureImportance(res$models[[1]])
#View(t(vimp$res))
preds_to_check <- merge(preds,dt_time_lut,all.x = T,by.x = "id",by.y="index")
thresh<-c(quantile(preds$prob.1,.5))
preds_to_check[,truth:=as.numeric(truth)][,truth:=truth-1]
preds_to_check<-preds_to_check[prob.1>thresh][truth<1]
preds_to_check<-preds_to_check[order(-prob.1)]

#-- Save the predictions to stud< the reasons for failed predictions
fwrite(preds_to_check,paste0(data_intermediate_dir,"XGB_PREDICTIONS_TO_CHECK_",TARGET,".csv"))

#-- Join back the hour ot the results
preds <- merge(preds,dt_sel[,.(index,hour)],all.x = T,by.x = "id",by.y="index")



#th_vec<-seq(0.05,0.775,0.05)
th_vec<-c(quantile(preds$prob.1,.5))
bas_pl_hour <- dcast(preds,hour~truth,value.var = "hour")
setnames(bas_pl_hour,c("0","1"),c("L","P"))
bas_pl_hour<-bas_pl_hour[,base_acc:=100*P/(P+L)][order(-base_acc)]
base_accuracy = nrow(preds[truth==1]) / nrow(preds)
i <-1
while(i<(length(th_vec)+1) )
{
  preds[,response:=as.numeric(prob.1>th_vec[i])]
  preds_dec <- preds[response>0]
  print(nrow(preds_dec)/nrow(preds))
  preds_dec[,PL:=truth==1]
  pl_hr_cnt <- dcast(preds_dec,hour~PL,fun.aggregate = length,value.var = "hour")
  setnames(pl_hr_cnt,c("TRUE","FALSE"),c("P","L"))
  pl_hr_cnt<-pl_hr_cnt[,accuracy:=100*P/(P+L)][order(-accuracy)]
  pl_hr_cnt<-pl_hr_cnt[,PL:=P-L][order(-accuracy)]
  pl_hr_cnt<-merge(pl_hr_cnt,bas_pl_hour[,.(hour,base_acc)])
  pl_hr_cnt<-pl_hr_cnt[,lift_base:=100*((accuracy/(100*base_accuracy) )-1)][order(-lift_base)]
  pl_hr_cnt<-pl_hr_cnt[,lift_hour:=100*(accuracy-base_acc)/base_acc][order(-accuracy)]
  i<-i+1
}

best_lift_hour <- pl_hr_cnt[1,lift_hour]
best_accuracy_hour <- pl_hr_cnt[1,accuracy]
  
top_hour <- pl_hr_cnt[1,hour]
dt_sel_hr <- dt_sel[hour %in% top_hour]
tsk <- makeClassifTask(id=TARGET,data=as.data.frame(dt_sel_hr), target="TARGET")
rsmpl = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)
lrnr <- makeLearner("classif.xgboost",predict.type="prob",nrounds=nrounds,eta=eta, colsample_bytree = colsample_bytree, max_depth = max_depth, subsample =subsample )
#lrnr <- makeLearner("classif.h2o.glm",predict.type="prob" )
res<-resample(task = tsk,learner=lrnr,measures = prob_pos_ret, resampling = rsmpl)
preds<-as.data.table(res$pred$data)
#-- Assign the hour
#preds <- merge(preds,dt_sel[,.(index,hour)],all.x = T,by.x = "id",by.y="index")
preds[,hour:=top_hour]
#th_vec<-seq(0.05,0.775,0.05)
th_vec<-c(quantile(preds$prob.1,.5))
bas_pl_hour <- dcast(preds,hour~truth,value.var = "hour")
setnames(bas_pl_hour,c("0","1"),c("L","P"))
bas_pl_hour<-bas_pl_hour[,base_acc:=100*P/(P+L)][order(-base_acc)]
base_accuracy = nrow(preds[truth==1]) / nrow(preds)
i <-1
while(i<(length(th_vec)+1) )
{
  preds[,response:=as.numeric(prob.1>th_vec[i])]
  preds_dec <- preds[response>0]
  print(nrow(preds_dec)/nrow(preds))
  preds_dec[,PL:=truth==1]
  pl_hr_cnt <- dcast(preds_dec,hour~PL,fun.aggregate = length,value.var = "hour")
  setnames(pl_hr_cnt,c("TRUE","FALSE"),c("P","L"))
  pl_hr_cnt<-pl_hr_cnt[,accuracy:=100*P/(P+L)][order(-accuracy)]
  pl_hr_cnt<-pl_hr_cnt[,PL:=P-L][order(-accuracy)]
  pl_hr_cnt<-merge(pl_hr_cnt,bas_pl_hour[,.(hour,base_acc)])
  pl_hr_cnt<-pl_hr_cnt[,lift_base:=100*((accuracy/(100*base_accuracy) )-1)][order(-lift_base)]
  pl_hr_cnt<-pl_hr_cnt[,lift_hour:=100*(accuracy-base_acc)/base_acc][order(-accuracy)]
  i<-i+1
}


pl_hr_cnt




#dt_res[j,"pair" ]<- TARGET
dt_res[j,"top_hour" ]<- top_hour
dt_res[j,"accuracy" ]<- pl_hr_cnt[1,accuracy]
dt_res[j,"lift_hour" ]<- pl_hr_cnt[1,lift_hour]
dt_res[j,"prob_pos_ret" ]<- mean(res$measures.test$prob_pos_week)
dt_res[j,"initial_best_accuracy" ]<- best_accuracy_hour
dt_res[j,"initial_best_lift_hour" ]<- best_lift_hour




j<-j+1
}

dt_res$pair <- pairs
dt_res[,best_lift:=pmax(initial_best_lift_hour,lift_hour)]
dt_res[,best_accuracy:=pmax(initial_best_accuracy,accuracy)]
View(dt_res)



