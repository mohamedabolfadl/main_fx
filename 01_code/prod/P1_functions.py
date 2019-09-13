# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 18:25:22 2019

@author: Mohamed Ibrahim

Purpose:
    
    Includes all functions related to P1
"""

import numpy as np
import pandas as pd



#-- Clean all variables
def rm_all():
    for name in dir():
        if not name.startswith('_'):
            del globals()[name]

#-- Get Fractionally differentiated based on Marcos Prado book
def getWeights(d,size):
    # thres>0 drops insignificant weights
    w=[1.]
    for k in range(1,size):
        w_=-w[-1]/k*(d-k+1)
        w.append(w_)
    w=np.array(w[::-1]).reshape(-1,1)
    #print("here")
    return w

def fracDiff(series,d,thres=.01):
    #1) Compute weights for the longest series
    w=getWeights(d,series.shape[0])
    
    #2) Determine initial calcs to be skipped based on weight-loss threshold
    w_=np.cumsum(abs(w))
    w_/=w_[-1]
    skip=w_[w_>thres].shape[0]
    #3) Apply weights to values
    df={}
    for name in series.columns:
        seriesF,df_=series[[name]].fillna(method='ffill').dropna(),pd.Series()
        
        df_ = np.zeros(seriesF.shape[0])
        df_ [:] = np.nan 
        df_ = pd.Series(df_)
        
        for iloc in range(skip,seriesF.shape[0]):
                loc=seriesF.index[iloc]
                if not np.isfinite(series.loc[loc,name]):continue # exclude NAs
                #df_[loc]=np.dot(w[-(iloc+1):,:].T,seriesF.loc[:loc])[0,0]
                df_[loc]=np.dot(w[-(iloc+1):,:].T,seriesF.loc[:loc])[0]
                
        df[name]=df_.copy(deep=True)
    df=pd.concat(df,axis=1)
    return df

def getWeights_FFD(d,size=1000,thres=0.01):
    # thres>0 drops insignificant weights
    w=[1.]
    
    for k in range(1,size):
        w_=-w[-1]/k*(d-k+1)
        w.append(w_)
    w=np.array(w[::-1]).reshape(-1,1)
    w = w[abs(w)>thres]
    return w

def fracDiff_FFD(series,d,thres=0.01):
    #1) Compute weights for the longest series
    w=getWeights_FFD(d,size=series.shape[0],thres=0.01)
    width=len(w)-1
    #2) Apply weights to values
    df={}
    for name in series.columns:
        seriesF,df_=series[[name]].fillna(method='ffill').dropna(),pd.Series()
        df_ = np.zeros(seriesF.shape[0])
        df_ [:] = np.nan 
        df_ = pd.Series(df_)
        
        for iloc1 in range(width,seriesF.shape[0]):
            loc0,loc1=seriesF.index[iloc1-width],seriesF.index[iloc1]
            if not np.isfinite(series.loc[loc1,name]):continue # exclude NAs
            df_[loc1]=np.dot(w.T,seriesF.loc[loc0:loc1])[0]
        df[name]=df_.copy(deep=True)
    
    df=pd.concat(df,axis=1)
    return df

def plotMinFFD(df0):
    from statsmodels.tsa.stattools import adfuller
    #path,instName='./','ES1_Index_Method12'
    out=pd.DataFrame(columns=['adfStat','pVal','lags','nObs','95% conf','corr'])
    #df0=pd.read_csv(path+instName+'.csv',index_col=0,parse_dates=True)
    for d in np.linspace(0,1,11):
        #df1=np.log(df0[['Close']]).resample('1D').last() # downcast to daily obs
        #df0=df0.last() # downcast to daily obs
        df2=fracDiff_FFD(df0,d,thres=.01)
        corr=np.corrcoef(df0.loc[df2.index,'Close'],df2['Close'])[0,1]
        df2=adfuller(df2['Close'],maxlag=1,regression='c',autolag=None)
        out.loc[d]=list(df2[:4])+[df2[4]['5%']]+[corr] # with critical value
        #out.to_csv(path+instName+'_testMinFFD.csv')
    out[['adfStat','corr']].plot(secondary_y='adfStat')
    return out
