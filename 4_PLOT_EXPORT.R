









logs_dir <- "05_logs/"
data_output_dir<-"02_data/output/"


Sys.setenv("plotly_username" = "mabolfadl")
Sys.setenv("plotly_api_key" = "V4x9Bjg3EzM0svwyffRS")


bst_param<-fread(paste0(data_output_dir,"best_parameters.csv"))
bst_param[grepl("BUY",instrument),instrument:=paste0(substr(instrument,9,14),"_",substr(instrument,1,3))][grepl("SELL",instrument),instrument:=paste0(substr(instrument,10,15),"_",substr(instrument,1,4))]






#-- Read all the logs



lgs <- list.files((logs_dir))

i<-1
while(i<(length(lgs)+1))
{
  
dt<-  fread(paste0(logs_dir,lgs[i]))
  
if(i==1)
{
dt_all <- dt  
}else{
  
  dt_all <- rbind(dt_all,dt)
}


  i<-i+1
}


#dt_all[,Time_sub:=as.POSIXct(gsub("T"," ",substr(Time,1,19)))]
dt_all[,Time_sub:=gsub("T"," ",substr(Time,9,16))]


instruments <- unique(bst_param$instrument)
for (inst in instruments)
{

thr<-bst_param[instrument==c(inst),threshold]
cl_name <- paste0(inst,"_thresh")

dt_all[,c(cl_name):=thr]


}



#-- USDJPY
p_USDJPY <- plot_ly(dt_all, x = ~Time_sub, y = ~USDJPY_BUY,type="scatter", name = 'USDJPY Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~USDJPY_SELL, name = 'USDJPY SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~USDJPY_BUY_thresh, name = 'USDJPY BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
add_trace(y = ~USDJPY_SELL_thresh, name = 'USDJPY SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
layout(title = "USDJPY",
       xaxis = list(title = "Time"),
       yaxis = list (title = "USDJPY"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_USDJPY, filename="USDJPY")

#-- EURUSD
p_EURUSD <- plot_ly(dt_all, x = ~Time_sub, y = ~EURUSD_BUY,type="scatter", name = 'EURUSD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~EURUSD_SELL, name = 'EURUSD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~EURUSD_BUY_thresh, name = 'EURUSD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~EURUSD_SELL_thresh, name = 'EURUSD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "EURUSD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "EURUSD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_EURUSD, filename="EURUSD")


#-- GBPUSD
p_GBPUSD <- plot_ly(dt_all, x = ~Time_sub, y = ~GBPUSD_BUY,type="scatter", name = 'GBPUSD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~GBPUSD_SELL, name = 'GBPUSD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~GBPUSD_BUY_thresh, name = 'GBPUSD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~GBPUSD_SELL_thresh, name = 'GBPUSD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "GBPUSD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "GBPUSD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_GBPUSD, filename="GBPUSD")


#-- AUDUSD
p_AUDUSD <- plot_ly(dt_all, x = ~Time_sub, y = ~AUDUSD_BUY,type="scatter", name = 'AUDUSD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~AUDUSD_SELL, name = 'AUDUSD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~AUDUSD_BUY_thresh, name = 'AUDUSD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~AUDUSD_SELL_thresh, name = 'AUDUSD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "AUDUSD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "AUDUSD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_AUDUSD, filename="AUDUSD")


#-- NZDUSD
p_NZDUSD <- plot_ly(dt_all, x = ~Time_sub, y = ~NZDUSD_BUY,type="scatter", name = 'NZDUSD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~NZDUSD_SELL, name = 'NZDUSD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~NZDUSD_BUY_thresh, name = 'NZDUSD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~NZDUSD_SELL_thresh, name = 'NZDUSD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "NZDUSD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "NZDUSD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_NZDUSD, filename="NZDUSD")


#-- USDCAD
p_USDCAD <- plot_ly(dt_all, x = ~Time_sub, y = ~USDCAD_BUY,type="scatter", name = 'USDCAD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~USDCAD_SELL, name = 'USDCAD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~USDCAD_BUY_thresh, name = 'USDCAD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~USDCAD_SELL_thresh, name = 'USDCAD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "USDCAD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "USDCAD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_USDCAD, filename="USDCAD")

#-- USDCHF
p_USDCHF <- plot_ly(dt_all, x = ~Time_sub, y = ~USDCHF_BUY,type="scatter", name = 'USDCHF Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~USDCHF_SELL, name = 'USDCHF SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~USDCHF_BUY_thresh, name = 'USDCHF BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~USDCHF_SELL_thresh, name = 'USDCHF SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "USDCHF",
         xaxis = list(title = "Time"),
         yaxis = list (title = "USDCHF"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_USDCHF, filename="USDCHF")


#-- XAUUSD
p_XAUUSD <- plot_ly(dt_all, x = ~Time_sub, y = ~GOLD_BUY,type="scatter", name = 'XAUUSD Buy',mode = 'lines+markers',line=list(color = 'rgba(3, 255, 3, .9)')) %>%
  add_trace(y = ~GOLD_SELL, name = 'XAUUSD SELL', mode = 'lines+markers',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  add_trace(y = ~XAUUSD_BUY_thresh, name = 'XAUUSD BUY threshold', mode = 'lines',line=list(color = 'rgba(3, 255, 3, .9)'))%>%
  add_trace(y = ~XAUUSD_SELL_thresh, name = 'XAUUSD SELL threshold', mode = 'lines',line=list(color = 'rgba(255, 3, 3, .9)'))%>%
  layout(title = "XAUUSD",
         xaxis = list(title = "Time"),
         yaxis = list (title = "XAUUSD"))%>%
  layout( yaxis = list(range = c(0.05,0.9)))
chart_link = api_create(p_XAUUSD, filename="GOLD")





p_all <- subplot(p_EURUSD,
                 p_USDCAD,
                 p_AUDUSD,
                 p_USDJPY,
                 p_NZDUSD,
                 p_GBPUSD,
                 p_USDCHF,
                 p_XAUUSD,titleY = T,nrows = 2)%>%
  layout(showlegend = FALSE)


chart_link = api_create(p_all,filename="fullview")



#-- Current snap shot

#dt_all[nrow(dt_all),':='(USDJPY_SELL_dist)]





