##------ MLR




rm(list=ls())

library(mlr)
library(data.table)


#-- All available learners and install the missing packages
#lrners<-as.data.table(listLearners())
#lrners_not_installed <- lrners[installed==F]
#packages_to_install <- unique(lrners_not_installed$package)
#lapply(packages_to_install,install.packages)


#-- Read data
dt<-fread("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/org/02_data/intermediate/010718/BUY_SL_15_PF_2_SPREAD_1.csv")
dt$BUY_RES <- as.factor(dt$BUY_RES)


#-- Create Classification task
tsk <- makeClassifTask(id="TEST",data=as.data.frame(dt), target="BUY_RES")

#-- Create a resampling task
rsmpl_desc = makeResampleDesc(method="GrowingWindowCV",initial.window=0.5,horizon=1e4)
#rsmpl_desc = makeResampleDesc("CV", iters = 3L)

#rin = makeResampleInstance(rsmpl_desc, task = tsk)


#--- Creating a learner
#lrnr<-makeLearner("classif.xgboost", predict.type = "prob")
#lrnr<-makeLearner("classif.ksvm", predict.type = "prob")
lrnr<-makeLearner("classif.randomForest", predict.type = "prob")



#-- Random search in the space with 100 iterations
ctrl = makeTuneControlRandom(maxit = 50L, tune.threshold = TRUE)
#ctrl = makeTuneControlGenSA(tune.threshold = TRUE)
#ctrl = makeTuneControlMBO(tune.threshold = TRUE)

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
tr <- train(res$learner,task = tsk)
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






















