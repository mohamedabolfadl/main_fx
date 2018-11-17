



old <- fread("02_data/intermediate/ML_SL_15_PF_1_SPREAD_2_ALL_prev.csv")
new <- fread("02_data/intermediate/ML_SL_15_PF_1_SPREAD_2_ALL.csv")




names(old) %in% names(new)

flds <- names(old)
new_sel <- new[,..flds]


sel_cols <- c("Time",  "BUY_RES_USDJPY" ,    "SELL_RES_USDJPY"  ,"buy_profit_USDJPY" , "buy_loss_USDJPY" ,"sell_profit_USDJPY" ,"sell_loss_USDJPY","USDJPY_Open", "USDJPY_High", "USDJPY_Low" ,"USDJPY_Close" ,"USDJPY_RSI" ,"USDJPY_TMS_green" ,"USDJPY_TMS_red")



tail(old[,..sel_cols],100)
View(tail(new_sel[,..sel_cols],100))


