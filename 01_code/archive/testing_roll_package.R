

n_vars <- 3
n_obs <- 15
x <- matrix(rnorm(n_obs * n_vars), nrow = n_obs, ncol = n_vars)
x<-cbind(x,x)
# rolling correlation matrices
result <- roll_cor(x, width = 5)



names(dt_results_all)


sel <- as.matrix(dt_results_all[1e3:nrow(dt_results_all),.(EURUSD_Close,GBPUSD_Close,USDJPY_Close,AUDUSD_Close,NZDUSD_Close,USDCHF_Close,XAUUSD_Close,USDCAD_Close)])



result <- roll_cor(na.omit(sel), width = 1e2)


dim(result)

hist(result[1,2,])


mean(result[1,2,],na.rm = T)


mean(result[1,3,],na.rm = T)


mean(result[1,4,],na.rm = T)


mean(result[1,5,],na.rm = T)


mean(result[1,6,],na.rm = T)


mean(result[1,7,],na.rm = T)

mean(result[1,8,],na.rm = T)






result_sel <- result[,,1e3:1020]


vec<-unlist(lapply(result_sel,as.vector))



vec[1:64]

result_sel[,,1]

