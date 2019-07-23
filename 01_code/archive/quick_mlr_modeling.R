



library(data.table)
library(lubridate)

BASE_PERFORMANCE <- 0.5168695

N_MAX_CONSECUTIVE_TRADES<<-20 # 1 -> -0.004 default 2 10 "0.0082892609104559 % over base performance"
wind<-"FixedWindowCV" # "GrowingWindowCV" "FixedWindowCV"  GrowingWindowCV-> -0.0094
initial.window = 1e3   
horizon = 1e3 # 1e3 "0.00177425934484554 % over base performance"
limit_hours<-c(7,8,9,10,11,12,13,14,15,16)

#-- Model parameters
nrounds = 10
eta =	0.3
colsample_bytree = 0.749399129
max_depth = 3L
subsample = 0.49023482


#-- Trade parameters
TARGET <- "BUY_RES_EURUSD"
TP <- 50
SPREAD<-0
PF = 1


dt <- fread(paste0("02_data/intermediate/ML_SL_",TP,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))
dt[,index:= seq(1,nrow(dt))]
dt[,hour:=as.numeric(lubridate::hour(Time))]

#-- Limit trades
dt<-dt[hour%in%limit_hours]

#-- Create the index, time lookup table
dt_time_lut <<- dt[,.(index,Time)]
dt_time_lut[,ret_per:=paste0(year(Time),"_",week(Time))]


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


#dt_sel[,TARGET:=as.character(TARGET)]
#fwrite(dt_sel,paste0("02_data/intermediate/",TARGET,"_h2o_ML_SL_15_PF_1_SPREAD_3_ALL.csv"))



#-- Quick mlr 
library(mlr)
library(roll)

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


#dt_sel <- na.omit(dt_sel)

tsk <- makeClassifTask(id=TARGET,data=as.data.frame(dt_sel), target="TARGET")
rsmpl = makeResampleDesc(method=wind,initial.window=initial.window,horizon=horizon, skip =horizon)
lrnr <- makeLearner("classif.xgboost",predict.type="prob",nrounds=nrounds,eta=eta, colsample_bytree = colsample_bytree, max_depth = max_depth, subsample =subsample )
res<-resample(task = tsk,learner=lrnr,measures = prob_pos_ret, resampling = rsmpl)
preds<-as.data.table(res$pred$data)
paste0( (mean(res$measures.test$prob_pos_week)/BASE_PERFORMANCE) - 1," % over base performance")
#-- Join back the hour ot the results
preds <- merge(preds,dt_sel[,.(index,hour)],all.x = T,by.x = "id",by.y="index")
head(preds)
#th_vec<-seq(0.05,0.775,0.05)
th_vec<-c(quantile(preds$prob.1,.5))

bas_pl_hour <- dcast(preds,hour~truth)
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
pl_hr_cnt <- dcast(preds_dec,hour~PL)
setnames(pl_hr_cnt,c("TRUE","FALSE"),c("P","L"))
pl_hr_cnt<-pl_hr_cnt[,accuracy:=100*P/(P+L)][order(-accuracy)]
pl_hr_cnt<-pl_hr_cnt[,PL:=P-L][order(-accuracy)]

# Join base accuracy
pl_hr_cnt<-merge(pl_hr_cnt,bas_pl_hour[,.(hour,base_acc)])

pl_hr_cnt<-pl_hr_cnt[,lift_base:=100*((accuracy/(100*base_accuracy) )-1)][order(-lift_base)]
pl_hr_cnt<-pl_hr_cnt[,lift_hour:=100*(accuracy-base_acc)/base_acc][order(-accuracy)]

print(th_vec[i])
print(paste("Accuracy ",pl_hr_cnt[1,accuracy]))
print("##############################")
i<-i+1
}

View(pl_hr_cnt)
















