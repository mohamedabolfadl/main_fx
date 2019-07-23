

rm(list=ls())
set.seed(456)

library(data.table)
library(mlr)

dt <- fread("02_data/output/ml_ready_eurusd.csv")



FEATS_TO_SELECT = 1e3

target="buy_betsize"
#target="buy_tp"
#target="buy"

tune_parameters = T

is_regr_task = grepl("(sl|tp)",target)

exclude = names(dt)[1:14]

vars = setdiff(names(dt),exclude)
var_lut = data.table(vars = vars, cl = as.character(seq(1:length(vars))))
rel_cols = c(target,setdiff(names(dt),exclude))



dt_sel = dt[,..rel_cols]

setnames(dt_sel,target,"target")

setnames(dt_sel,vars, paste0("v_",as.character(seq(1:length(vars))))  )

#-- Assign row_id
dt_sel[,row_id:=seq(1,nrow(dt))]


if(!is_regr_task)
{
  dt_sel[,target:=ifelse(target ==T,1,0)]
  dt_sel[,target:=as.factor(target)]
}

if(is_regr_task)
{
  #-- Regression
  lrn = makeLearner(cl ="regr.xgboost",
                    nrounds=56, max_depth=3, eta=0.245
  )
  # target="buy"
  # [Tune] Result: nrounds=56; max_depth=3; eta=0.245 : auc.test.mean=0.5335454
}else{
  #-- Classificatoin
  #lrn = makeLearner(cl ="classif.xgboost", predict.type = "prob")
  lrn = makeLearner(cl ="classif.xgboost",
                    predict.type = "prob",
                    nrounds=15, max_depth=2, eta=0.238
  )
  
  # target="buy_betsize"
  # [Tune] Result: nrounds=15; max_depth=2; eta=0.238 : auc.test.mean=0.8104307
}



cols = setdiff(names(dt_sel),"target")
#cols = c("v_561","v_800")
dt_sel[,(cols):=as.numeric(get(cols))]


dt_sel[,row_id:=seq(1,nrow(dt))]



if(F)
{
  sel_nms = names(dt_sel)[1:FEATS_TO_SELECT]
  sel_nms = c(sel_nms,"row_id")
}

sel_nms = names(dt_sel)


dt_trn = dt_sel[!is.na(target) & !is.na(v_1),..sel_nms]
#dt_trn = dt_sel[!is.na(target) & !is.na(v_1),..sel_nms]


row_id_lut = dt_trn$row_id

dt_trn[,row_id:=NULL]


if(!is_regr_task)
{
  tsk = makeClassifTask(data = as.data.frame(dt_trn) , target = "target")
}else{
  tsk = makeRegrTask(data = as.data.frame(dt_trn) , target = "target")
}


#-- CV
rdesc = makeResampleDesc("CV", iters = 5)

#-- Resampling
if(!is_regr_task)
{
  res = resample(lrn,tsk,measures = auc,rdesc )
}else{
  res = resample(lrn,tsk, measures = kendalltau , rdesc )
}


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



























