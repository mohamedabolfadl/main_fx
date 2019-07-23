






cur = "EURUSD"







library(xts)
library(PerformanceAnalytics)


get_Res=function(x,span=12)
{
  x=as.vector(x$dt_xts.High)
    y = list()
  if(length(x)>2*span)
{  
  i=span+1
  #print(i)
  #print(length(x)-span)
  while(i<(1+length(x)-span))
  {
  if( sum(x[i]>(x[(i-span):(i-1)]))==span  &  sum(x[i]>(x[(i+1):(i+span)]))==span)
    {    
    y<-x[i]
    }
    i=i+1
  }

  return(y)
  }else{
  
    return(-1)
}    }

get_Supp=function(x,span=12)
{
  x=as.vector(x$dt_xts.Low)
  y = list()
  if(length(x)>2*span)
  {  
    i=span+1
    #print(i)
    #print(length(x)-span)
    while(i<(1+length(x)-span))
    {
      if( sum(x[i]<(x[(i-span):(i-1)]))==span  &  sum(x[i]<(x[(i+1):(i+span)]))==span)
      {    
        y<-x[i]
      }
      i=i+1
    }
    
    return(y)
  }else{
    
    return(-1)
  }    }



collapse_sr=function(x)
{
print(nrow(x))
   # print(na.omit(x$supp))
 #y = as.vector(na.omit(x$supp))
 y = as.vector(x$supp)
 
 # y = as.vector(na.omit(x))
# print(length(y))
print(length(y))
return(paste0(y,collapse = "_"))
      }



span=80
N=1e4
dt_sel <- dt[1:N]
dt_sel$dt_xts.Open<-NULL
dt_sel$dt_xts.Close<-NULL

yy=rollapply(dt_sel$dt_xts.Low,width = 1+span*2,get_Supp,span=span)
dt_sel$supp <- yy

supps=rollapply(dt_sel$supp,width = 1e4,collapse_sr)



#dt_sel$dt_xts.Low<-NULL

yy=rollapply(dt_sel$dt_xts.High,width = 1+span*2,get_Res,span=span)
dt_sel$res <- yy
#dt_sel$dt_xts.Low<-NULL


#dt_sel$dt_xts.High<-NULL

plot(dt_sel)


library(ggplot2)
ggplot(dt_sel)+geom_line()




supps<-na.omit(dt_sel$supp)
supps_indices = index(supps)

i=1

while(i<(nrow(supps_indices)+1))
{
supps_list = paste0(supps[1:i],collapse = "_")  
  

  i=i+1
}

ret <- apply.fromstart(dt_sel[1e3:2e3]$supp, FUN="collapse_sr")


#ret <- rollapplyr(dt_sel[1e3:2e3,3], seq_along(dt_sel[1e3:2e3]$supp), collapse_sr)
ret <- rollapplyr(dt_sel[1e3:2e3,3], seq_along(dt_sel[1e3:2e3]$supp), collapse_sr)




