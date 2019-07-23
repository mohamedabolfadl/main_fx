

rm(list=ls())
set.seed(456)

library(data.table)
library(mlr)


SL = 10
SPREAD = 1
MAXTRADETIME = 4
TRAIN_RATIO = 0.7

#dt <- fread("02_data/output/ml_ready_eurusd.csv")

dt <- fread(paste0("02_data/output/ml_ready_eurusd_SL_",SL,"_SPREAD_",SPREAD,"_MAXTRADETIME_",MAXTRADETIME,".csv"))


#-- Number of features to use
FEATS_TO_SELECT = 3.9e3
SELECT_FEATS <- F
#-- Choose target
#target="buy_betsize"
#target="sell_betsize"
#target="buy"
target="sell"
#-- Tune the parameters
tune_parameters = T
#-- Predicting binary target or trade time
is_regr_task = grepl("(sl|tp)",target)

#-- Non predictoes
exclude = c( names(dt)[ (length(names(dt))-10) :length(names(dt))] , names(dt)[1:2] )
# [1] "Unnamed: 0"   "index"        "seq"          "buy_tp"       "sell_tp"      "buy_sl"      
# [7] "sell_sl"      "buy"          "sell"         "buy_betsize"  "sell_betsize" "V1"  [13] "id"         
  

#-- Get predictors
vars = setdiff(names(dt),exclude)

#-- Change column names to var_xx
var_lut = data.table(vars = vars, cl = as.character(seq(1:length(vars))))

#-- predictors + target
rel_cols = c(target,setdiff(names(dt),exclude))

#-- Select only the relevant columns
dt_sel = dt[,..rel_cols]

#-- Rename target
setnames(dt_sel,target,"target")

#-- Set column names to var_xx
setnames(dt_sel,vars, paste0("v_",as.character(seq(1:length(vars))))  )

#-- Change target to factor for regression
if(!is_regr_task)
{
  dt_sel[,target:=ifelse(target ==T,1,0)]
  dt_sel[,target:=as.factor(target)]
}

#-- Create learner
if(is_regr_task)
{
  #-- Regression
  lrn = makeLearner(cl ="regr.xgboost",
                    nrounds=56, max_depth=3, eta=0.245)
}else{
  #-- Classificatoin

  #-- XGB
  lrn = makeLearner(cl ="classif.xgboost",
                    predict.type = "prob",
                    nrounds=15, max_depth=3, eta=0.238)
  
  #-- RF
  #lrn = makeLearner(cl ="classif.randomForest",
  #                  predict.type = "prob",
  #                  ntree=500, mtry=floor(sqrt(ncol(dt_sel))) )
  
}

#-- Convert all predictors to numerics
cols = setdiff(names(dt_sel),"target")
dt_sel[,(cols):=as.numeric(get(cols))]

#-- Assign row_id
dt_sel[,row_id:=seq(1,nrow(dt))]


#-- Select subset of the features
if(SELECT_FEATS)
{
  sel_nms = names(dt_sel)[1:FEATS_TO_SELECT]
  sel_nms = c(sel_nms,"row_id")
}

sel_nms = names(dt_sel)

#-- Choose rows where we have features
dt_sel = dt_sel[!is.na(target) & !is.na(v_1),..sel_nms]
dt_trn = dt_sel[1:floor( TRAIN_RATIO  * nrow(dt_sel)),]
dt_test = dt_sel[ (1+floor( TRAIN_RATIO  * nrow(dt_sel)) ): nrow(dt_sel) ,]

#-- Get a LUT
row_id_lut = dt_trn$row_id



if(!is_regr_task)
{
  tsk = makeClassifTask(data = as.data.frame(dt_trn) , target = "target")
}else{
  tsk = makeRegrTask(data = as.data.frame(dt_trn) , target = "target")
}


#-- CV
#rdesc = makeResampleDesc("CV", iters = 5)
rdesc = makeResampleDesc("GrowingWindowCV",horizon = 1e2L, initial.window = 0.01)
#rdesc = makeResampleDesc("FixedWindowCV",horizon = 1e2L, initial.window = 0.01)



#-- Resampling
if(!is_regr_task)
{
  res = resample(lrn,tsk,measures = mlr::auc, rdesc , models = T)
}else{
  res = resample(lrn,tsk, measures = kendalltau , rdesc )
}


mdls_all <- res$models

mdl <- mdls_all[[ length(mdls_all) ]]$learner.model
feats <- mdls_all[[ length(mdls_all) ]]$features


preds <- predict(mdl, as.matrix(dt_test[,..feats]) )



library(pROC)
library(gains)

gains( as.numeric(dt_test$target), preds , groups = 10)

auc( as.numeric(dt_test$target), preds)









if(F)
{










preds = as.data.table(res$pred$data)
dt_trn[,id:=seq(1,nrow(dt_trn))]

#-- Join bet size predictions
dt_trn<-merge(dt_trn,preds[,.(id,prob.1)],all.x=T,by="id")
dt_trn[,id:=NULL][,target:=NULL]





#-- Set new target
dt_trn[,row_id:=row_id_lut]

dt[,row_id:=seq(1,nrow(dt))]
dt_trn <- merge(dt_trn,dt[,.(row_id,buy)],all.x=T,by="row_id")


dt_trn[,row_id:=NULL]
setnames(dt_trn,"buy","target")

#-- Prepate target
dt_trn[,target:=ifelse(target ==T,1,0)]


dt_trn[,target:=as.factor(target)]



st = min(dt_trn$prob.1)
en = max(dt_trn$prob.1)

N=10
step = (en-st)/N

th = st+step


while(th<en)
{
  #-- Make task
  tsk = makeClassifTask(data = as.data.frame(dt_trn[prob.1>th]) , target = "target")
  
  
  #-- Make learner
  lrn = makeLearner(cl ="classif.xgboost",
                    predict.type = "prob",
                    nrounds=15, max_depth=2, eta=0.238
  )
  
  
  
  #-- CV
  res = resample(lrn,tsk,measures = auc,rdesc,models = T )
  
  th = th + step
  
}


if("RANDOM_SELECT_FEATURES"!="RANDOM_SELECT_FEATURES")
{
  ctrl = makeFeatSelControlRandom(prob=0.8,maxit = 10)
  
  sfeats = selectFeatures(learner = lrn, task =
                            tsk, resampling = rdesc,
                          control = ctrl, show.info = T, measures = auc)
  
}








mdl = res$models[[1]]$learner.model
feats = res$models[[1]]$features
xgb.importance(model = mdl,feature_names = feats)


tst = res$models[[1]]

#dt_trn[,.(mean(prob.1)),by=target]


if(tune_parameters)
{
  ps = makeParamSet(
    makeIntegerParam("nrounds", lower = 10, upper = 100),
    makeDiscreteParam("max_depth", values = c(2,3,4)),
    makeNumericParam("eta", lower = 0.01, upper = 0.3)
    
  )
  ctrl = makeTuneControlRandom(maxit = 20L)
  rdesc = makeResampleDesc("CV", iters = 3L)
  
  if(!is_regr_task)
  {
    res = tuneParams(lrn, task = tsk, resampling =
                       rdesc, measures = auc,
                     par.set = ps, control = ctrl)
    
  }else{
    res = tuneParams(lrn, task = tsk, resampling =
                       rdesc, measures = kendalltau,
                     par.set = ps, control = ctrl)
  }  
  
  
  
}



























}