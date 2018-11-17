rm(list=ls())


library(data.table)
library(mlr)

library(caret)

set.seed(123)
#-- General input
SL <- 15
PF <- 2
SPREAD <- 2
test_ratio <- 0.3


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

#--- Directoriewa
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"


lrners<-as.data.table(listLearners())

classif_algos <- lrners[grepl("^classif",class) & installed==T & prob==T,class]


#-- Read data
dt<-fread(paste0(data_intermediate_dir,"ML_SL_",SL,"_PF_",PF,"_SPREAD_",SPREAD,"_ALL.csv"))

curr_instrument <- "BUY_RES_EURUSD"

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

dt_curr<-copy(dt_sel)
dt_curr[,TARGET:=.SD >  0.5,.SDcols = curr_instrument]
dt_curr$TARGET <- as.factor(dt_curr$TARGET)
results_cols <- c(names(dt_curr)[grepl("BUY_RES",names(dt_curr)) | grepl("SELL_RES",names(dt_curr)) ] )
feat_cols <- setdiff(names(dt_curr),results_cols)

#-- Selected features
#feat_cols <- c(feats,"TARGET")
dt_curr <- dt_curr[,..feat_cols]

dt_curr$TARGET <- as.factor(dt_curr$TARGET)
targ <- as.factor(dt_curr$TARGET)
for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.infinite(dt_curr[[j]])), j, NA)

inp <- as.data.frame(dt_curr)
inp <- inp[,setdiff(names(inp),"TARGET")]
prep <- preProcess( inp, method = c("center","scale","BoxCox","medianImpute","nzv","pca"))
dt_curr_comp<-predict(prep,inp)
dt_curr<-as.data.table(dt_curr_comp)
dt_curr$TARGET<-targ


#for (j in 1:ncol(dt_curr)) set(dt_curr, which(is.na(dt_curr[[j]])), j, 0)


#-- Create Classification task
tsk <- makeClassifTask(id="TEST",data=as.data.frame(dt_curr), target="TARGET")

#tsk = filterFeatures(tsk, method = "information.gain", abs = 40)

#--- Create cost measure
costs = matrix(c(0, 1, 5, 0), 2)
colnames(costs) = rownames(costs) =
  getTaskClassLevels(tsk)
fx.costs = makeCostMeasure(id = "fx.costs", name =
                             "fx costs", costs = costs,
                           best = 0, worst = 5)




#-- collecting the algos
#algs <- c("classif.qda","classif.logreg","classif.rpart",
#          "classif.randomForest","classif.ksvm","classif.gbm",
#          "classif.xgboost","classif.h2o.deeplearning")
algs <- classif_algos
#algs <- setdiff(algs,c("classif.rda","classif.rFerns","classif.rknn","classif.RRF","classif.rrlda",
#                       "classif.sda","classif.clusterSVM","classif.bartMachine","classif.blackboost",
#                       "classif.extraTrees","classif.fdausc.glm","classif.fdausc.kernel","classif.fdausc.knn",
#                       "classif.fdausc.np",classif.gausspr"))     #,"classif."
algs <- setdiff(algs,c("classif.rda","classif.rFerns","classif.rknn","classif.RRF","classif.rrlda",
                       "classif.sda","classif.clusterSVM","classif.bartMachine","classif.blackboost",
                       "classif.extraTrees","classif.ada","classif.adaboostm1","classif.binomial","classif.C50",
                       "classif.cforest","classif.ctree","classif.cvglmnet","classif.dbnDNN","classif.earth","classif.evtree",
                       "classif.extraTrees","classif.fdausc.glm","classif.fdausc.kernel","classif.fdausc.knn","classif.fdausc.np",
                       "classif.gausspr","classif.featureless","classif.gamboost"))     #,"classif."


algs <- setdiff(algs, c("classif.neuralnet","classif.qda"))

#algs<-algs[23:length(algs)]

algs <- rev(algs)

lrns = lapply(algs, makeLearner)

#lrns = lapply(lrns, setPredictType, "prob")


#-- Create a resampling task
rsmpl_desc = makeResampleDesc(method="FixedWindowCV",initial.window=0.5,horizon=1e4)



#gc()
#-- Conducting the benchmarking
#bmr = benchmark(lrns, tsk, rsmpl_desc, measures = auc)
bmr = benchmark(lrns, tsk, rsmpl_desc, measures=list(sharpe_ratio,fx.costs))


plotBMRBoxplots(bmr=bmr)
plotBMRSummary(bmr=bmr)
plotBMRRanksAsBarChart(bmr=bmr)


bmr_dt<-as.data.table(as.data.frame(bmr))
bmr_dt <- bmr_dt[order(sharpe_ratio)]
fwrite(bmr_dt,paste0(data_output_dir,"benchmark_results_sharpe.csv"))


################################################################################
################################################################################
##################   E    N    D    ############################################
################################################################################
################################################################################


#lrnr<-makeLearner("classif.h2o.deeplearning", predict.type = "prob")
lrnr<-makeLearner("classif.svm",predict.type = "prob")
r = resample(learner = lrnr, task = tsk, resampling = rsmpl_desc, show.info = TRUE)


opt_res <- getBestThresh(as.data.table(r$pred$data))
dt_plot <- opt_res[[6]]
dt_plot$iter <- as.factor(dt_plot$iter)
ggplot(dt_plot)+geom_point(aes(x=id,y=equity,fill=iter))

plot(opt_res[[6]]$equity)

#-- Random search in the space with 100 iterations
ctrl = makeTuneControlRandom(maxit = 5L, tune.threshold = TRUE)
#ctrl = makeTuneControlGenSA(tune.threshold = TRUE)
#ctrl = makeTuneControlMBO(tune.threshold = TRUE)


mod <- train()

#-- XGB parameters
# ps <- makeParamSet(
#   # The number of trees in the model (each one built sequentially)
#   makeIntegerParam("nrounds", lower = 10, upper = 100),
#   # number of splits in each tree
#   makeIntegerParam("max_depth", lower = 1, upper = 10)
#   # "shrinkage" - prevents overfitting
#   #makeNumericParam("eta", lower = .1, upper = .5),
#   # L2 regularization - prevents overfitting
#   #makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
# )

#-- SVM parameters
#ps = makeParamSet(
#  makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
#  makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x)
#)
#-- Random forest
ps = makeParamSet(
  makeIntegerParam("ntree",lower = 10, upper = 200),
  makeIntegerParam("mtry", lower = 3, upper = 10)
  #makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#-- Train the object
res = tuneParams(learner = lrnr, task = tsk, control = ctrl, 
                 resampling = rsmpl_desc, par.set = ps, show.info = TRUE, measures = list(auc))



#-- Plotting the 2D search space
print(res)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "nrounds", y = "auc.test.mean")
plotHyperParsEffect(data, x = "max_depth", y = "auc.test.mean")
plotHyperParsEffect(data, x = "nrounds", y = "max_depth", z = "auc.test.mean",
                    plot.type = "heatmap", interpolate = "regr.earth", show.experiments = TRUE)
plotHyperParsEffect(data, x = "iteration", y = "auc.test.mean")


#-- Getting the optimal threshold
#tr <- train(res$learner,task = tsk)



task.pred = predict(tr, task = tsk)
th_perf <- generateThreshVsPerfData(obj = task.pred, measures=list(fpr,fnr,f1))
plotThreshVsPerf(th_perf)



#------ END ------------------------#



#-- Stacked learners
base = c("classif.rpart", "classif.lda", "classif.svm")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "prob")
m = makeStackedLearner(base.learners = lrns,
                       predict.type = "prob", method = "hill.climb")
tmp = train(m, tsk)
res = predict(tmp, tsk)









#-- Make a paramset to search in
ps = makeParamSet(
  makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x)
)
#-- Random search in the space with 100 iterations
ctrl = makeTuneControlRandom(maxit = 100L)
# 3-fold CV
rdesc = makeResampleDesc("CV", iters = 2L)
# run the hyperparameter tuning process
res = tuneParams("classif.ksvm", task = pid.task, control = ctrl, 
                 resampling = rdesc, par.set = ps, show.info = FALSE)
print(res)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "C", y = "mmce.test.mean")
plotHyperParsEffect(data, x = "iteration", y = "mmce.test.mean")


#-- Multiple parameter search
# create the C and sigma parameter in continuous space: 2^-5 : 2^5
ps = makeParamSet(
  makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -5, upper = 5, trafo = function(x) 2^x)
)
# random search in the space with 100 iterations
ctrl = makeTuneControlRandom(maxit = 100L)
# 3-fold CV
rdesc = makeResampleDesc("CV", iters = 2L)
# run the hyperparameter tuning process
res = tuneParams("classif.ksvm", task = pid.task, control = ctrl, 
                 resampling = rdesc, par.set = ps, show.info = FALSE)
print(res)
# collect the hyperparameter data
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "C", y = "sigma", z = "mmce.test.mean",
                    plot.type = "heatmap", interpolate = "regr.earth")
plotHyperParsEffect(data, x = "C", y = "sigma", z = "mmce.test.mean",
                    plot.type = "heatmap", interpolate = "regr.earth", show.experiments = TRUE)
#-- Beautifying using ggplot
plt = plotHyperParsEffect(data, x = "C", y = "sigma", z = "mmce.test.mean",
                          plot.type = "heatmap", interpolate = "regr.earth", show.experiments = TRUE)
min_plt = min(plt$data$mmce.test.mean, na.rm = TRUE)
max_plt = max(plt$data$mmce.test.mean, na.rm = TRUE)
mean_plt = mean(c(min_plt, max_plt))
plt + scale_fill_gradient2(breaks = seq(min_plt, max_plt, length.out = 4),
                           low = "red", mid = "white", high = "blue", midpoint = mean_plt)


#ps = makeParamSet(
#  makeNumericParam("C", lower = -5, upper = 5, trafo = function(x) 2^x)
#)
#ctrl = makeTuneControlGrid()
#rdesc = makeResampleDesc("Holdout")
lrn = makeTuneWrapper("classif.ksvm", control = ctrl,
                      measures = list(acc, mmce), resampling = rdesc, par.set = ps, show.info = FALSE)

#res = resample(lrn, task = pid.task, resampling = cv2, extract = getTuneResult, show.info = FALSE)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "C", y = "acc.test.mean", plot.type = "line")






















