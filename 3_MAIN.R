

library(data.table)

config_file <- data.table(
  SL = 10,
  PF = 3,
  test_portion = 0.3,
  FEAT_SEL_get_feature_importance = T,
  featSelProb=0.8,
  FEAT_SEL_nrounds = 10,
  FEAT_SEL_eta = 0.1,
  FEAT_SEL_initial.window = 0.2,
  FEAT_SEL_horizon = 1e3,
  FEAT_SEL_window_type = "FixedWindowCV",# "GrowingWindowCV", # "FixedWindowCV",
  FEAT_SEL_max_iterations = 2e2,
  TRAIN_nrounds = c(50,100,200),
  TRAIN_eta = c(0.01,0.1),
  TRAIN_max_depth = c(4,6,8),
  TRAIN_window_type =  "FixedWindowCV", #"GrowingWindowCV",
  TRAIN_initial.window = 0.2,
  TRAIN_horizon = 1e3,
  notes="Using AUC as criteria"
)
fwrite(config_file,"config_file.csv")

curr_list <- data.table(currs = c("BUY_RES_EURUSD","BUY_RES_USDJPY","BUY_RES_GBPUSD","SELL_RES_AUDUSD"))
fwrite(curr_list,"curr_list.csv")

#-- Run scripts
source("01_code/prod/FEAT_SELECT_AND_SAVE.R")

source("01_code/prod/COMBINE_FEAT_SEL_RESULTS.R")

source("01_code/prod/TRAIN_AND_SAVE.R")


#--- Directories
data_output_dir<-"02_data/output/"
data_input_dir<-"02_data/input/"
data_intermediate_dir<-"02_data/intermediate/"


#-- Copy config and results to the run folder
tm <- Sys.time()
save_dir <- paste0("05_runs/",substr(tm,1,13))
dir.create(file.path(save_dir))

file.copy(from="curr_list.csv", to=save_dir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="config_file.csv", to=save_dir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



file.copy(from=paste0(data_output_dir,"SELECTED_FEATURES_SL_",SL,"_PF_",PF,"_ALL.csv"), to=save_dir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



file.copy(from=paste0(data_output_dir,"BEST_PARAMS_SL_",SL,"_PF_",PF,".csv"), to=save_dir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



curr_list <- fread("curr_list.csv")


for(curr in curr_list$currs)
{
file.copy(from=paste0(data_output_dir,curr,"_equity.png"), to=save_dir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
}
#save_dir <- paste0("05_runs/","2018-07-28 15")
