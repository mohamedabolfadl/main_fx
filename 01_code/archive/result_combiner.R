






rm(list=ls())

set.seed(123)



library(data.table)
library(lubridate)
library(mlr)
library(ggplot2)
library(xgboost)
library(crayon)
library(plotly)
library(caret)
library(parallelMap)
#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"



"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}



all_pairs <- c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCHF","NZDUSD","USDCAD")



folds <- c(paste0("BUY_RES_",all_pairs),paste0("SELL_RES_",all_pairs))


i<-1
for (fold in folds)
{
  
  fls <- list.files(data_output_dir+fold)
  res_fl <- fls[grepl("res_sha",fls)]
  
  res_sharpe <- fread(data_output_dir+fold+"/"+res_fl,select=c("learner.id","mean_v"))
  setnames(res_sharpe,"mean_v",fold)
  
  if(i==1)
  {
    dt_all <- res_sharpe
  }else{
    
    dt_all<-merge(dt_all,res_sharpe)
    
  }
  
  i<-i+1
  
}


fwrite(dt_all,data_output_dir+"total_performance.csv")


#-- Getting best pair
auc_per_pair <- transpose(dt_all[,lapply(.SD,mean),.SDcols=setdiff(names(dt_all),"learner.id")])
auc_per_pair$pairs <- setdiff(names(dt_all),"learner.id")


#-- Getting best models
auc_per_model <- transpose(dt_all)
names(auc_per_model) <- as.vector(unlist(auc_per_model[1,]))
auc_per_model<-auc_per_model[2:nrow(auc_per_model),]
cols<-names(auc_per_model)
auc_per_model[,(cols):=lapply(.SD,as.numeric),.SDcols = cols]
dt_auc_per_model<-transpose(auc_per_model[,lapply(.SD,mean),.SDcols = names(auc_per_model)])
dt_auc_per_model$models <- names(auc_per_model)


setroworder <- function(x, neworder) {
  .Call(data.table:::Creorder, x, as.integer(neworder), PACKAGE = "data.table")
  invisible(x)
}


getPairs <- function(x)
{
  
  
  mat <- as.matrix(x)
  j <- 1
  #print(ncol(x))
   while(j<ncol(x))
     {
     i<-1
     
       while(i<(nrow(x)+1))
  {
    
#    print(i)
#         print(j)
  if(i==1 & j==1)  
  {
    
    res <- data.table(model_A=names(x)[j],model_B=x[i,model],score=mat[i,j])
    #print(res)
  }else{
    
    res<-rbind(res,data.table(model_A=names(x)[j],model_B=x[i,model],score=mat[i,j] ) )
  }
    
    i<-i+1
  }
  j<-j+1
}
  #print("Heyy")
  #print(res)
  res<-unique(res,by="score")
  res<-res[order(-score)]
  return(res)
}







for(fold in folds)
{
  
  fls <- list.files(data_output_dir+fold)
  res_fl <- fls[grepl("correla",fls)]
  corr_file <- fread(data_output_dir+fold+"/"+res_fl)
  model_order<-setdiff(names(corr_file),"learner.id")
  corr_file$learner.id<-NULL
  dt_sel <- dt_all[,.(learner.id,get(fold))]

  
  to_ord <- match(model_order,dt_sel$learner.id)
  
  setroworder(x=dt_sel, neworder=to_ord)
  
  
  

  names(dt_sel)[2]<-fold
  perf_mat <- as.matrix(as.vector(dt_sel[,c(2)]))%*%as.matrix(as.vector(transpose(dt_sel[,c(2)])))
  
  
  res <- perf_mat / (1+abs(corr_file))
  cat(fold+"\n")
  res$model <- model_order
pair_view <-getPairs(res) 
    cat(pair_view[1,]+"\n")
  cat("\n################\n\n")
  fwrite(pair_view,data_output_dir+fold+"/model_to_model_score.csv")
}







BUY_RES_AUDUSD
classif.LiblineaRL2LogReg
classif.OneR
0.4628998293
(0.497951099*0.521823423)/(1+0.43866355)
0.4979510986
0.5218234234



