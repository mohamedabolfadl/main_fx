



#--- QDA

#load qda 
qda.learner <- makeLearner("classif.qda", predict.type = "prob")


#-- Logistic regression
logistic.learner <- makeLearner("classif.logreg",predict.type = "prob")


#-- Decision tree
makeatree <- makeLearner("classif.rpart", predict.type = "prob")
dt_par <- makeParamSet(
makeIntegerParam("minsplit",lower = 10, upper = 50),
makeIntegerParam("minbucket", lower = 5, upper = 50),
makeNumericParam("cp", lower = 0.001, upper = 0.2)
)


#-- Random forest
makeatree <- makeLearner("classif.randomForest", predict.type = "prob")
rf_par<- makeParamSet(
makeIntegerParam("ntree",lower = 50, upper = 500),
makeIntegerParam("mtry", lower = 3, upper = 10),
makeIntegerParam("nodesize", lower = 10, upper = 50)
)


#-- SVM
ksvm <- makeLearner("classif.ksvm", predict.type = "prob")
svm_par <- makeParamSet(
makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)


#-- GBM
 g.gbm <- makeLearner("classif.gbm", predict.type = "prob")
gbm_par<- makeParamSet(
makeDiscreteParam("distribution", values = "bernoulli"),
makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)


#-- XGB
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_ps <- makeParamSet(
makeIntegerParam("nrounds",lower=200,upper=600),
makeIntegerParam("max_depth",lower=3,upper=20),
makeNumericParam("lambda",lower=0.55,upper=0.60),
makeNumericParam("eta", lower = 0.001, upper = 0.5),
makeNumericParam("subsample", lower = 0.10, upper = 0.80),
makeNumericParam("min_child_weight",lower=1,upper=5),
makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)


#-- DeepNet
nn_set <- makeLearner("classif.h2o.deeplearning", predict.type = "prob")
nn_ps <- makeParamSet(

	)

#-- All available learners
lrners<-as.data.table(listLearners())

#-- TO get the ps of a model
getLearnerParamSet("classif.randomForest")














