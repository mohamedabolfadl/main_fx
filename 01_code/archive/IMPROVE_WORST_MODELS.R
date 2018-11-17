#-- Improve the worst performing models



rm(list=ls())


library(data.table)
library(mlr)

#--- Directoriewa
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"

#-- General input
SL <- 10
PF <- 3
SPREAD <- 2
test_ratio <- 0.3
windowType <- "GrowingWindowCV"
initial.window <- 0.5
horizon <- 2e3
eta     <-0.1
nrounds  <-20
randiters <- 2e2




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










