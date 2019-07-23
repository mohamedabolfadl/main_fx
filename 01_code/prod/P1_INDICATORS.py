# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 23:07:01 2019

@author: Mohamed Ibrahim
"""

# -*- coding: utf-8 -*-
"""
Created on Fri Mar  1 16:10:26 2019

@author: Mohamed Ibrahim
"""


import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from tsfresh import extract_features
import os



def rm_all():
    for name in dir():
        if not name.startswith('_'):
            del globals()[name]

#-- Fractionally differentiated
def getWeights(d,size):
    # thres>0 drops insignificant weights
    w=[1.]
    #print("here a")
    for k in range(1,size):
        w_=-w[-1]/k*(d-k+1)
        w.append(w_)
    w=np.array(w[::-1]).reshape(-1,1)
    #print("here")
    return w

def fracDiff(series,d,thres=.01):
    #1) Compute weights for the longest series
    w=getWeights(d,series.shape[0])
    #w=getWeights(d,thres)
    
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
#    mpl.axhline(out['95% conf'].mean(),linewidth=1,color='r',linestyle='dotted')
#    mpl.savefig(path+instName+'_testMinFFD.png')
    return out



rm_all()




data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"

path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'
os.chdir(path)

#-- Run inputs
#curs = ["USDJPY","GBPUSD","USDCHF","USDCAD","NZDUSD","AUDUSD","XAUUSD","GBPUSD"]
#pipsize=0.0001
#decimal_plc = int(np.log(1/pipsize)/np.log(10))
#SL_vec=100
#PF_vec=1
#SPREAD_VAL = 0
#MAX_PERIOD=50
N = 5e6 #-- Number of columns to read
USE_ALL_COLS = False
START_HOUR = 12
brick_size = 5 
DO_FRAC_DIFF = False

print('Reading minute data...')
df = pd.read_csv(data_input_dir+"dt_all_min.csv")

#pairs = ['eurusd','gbpusd','audusd','usdjpy','usdcad','usdchf','xauusd','nzdusd']
pairs = ['eurusd','gbpusd']


for pair in pairs:
    print(70*'#')
    print(pair)
    pair_cap = pair.upper()
    #d = df.loc[1:20000]
    d = df.loc[0:df.shape[0]-10]
    #d = df.loc[1000:10000]
    
    if pair == 'usdjpy':
        pipsize = 0.01
    else:
        pipsize = 0.0001
    
    # Select certain columns
    d=d[['Time','Open_'+pair_cap,'High_'+pair_cap,'Low_'+pair_cap,'Close_'+pair_cap]]
    
    # Convert Time to datetime
    d['Time']  = pd.to_datetime(d['Time'])
    
    # Set the index to be the time
    d           = d.set_index('Time')
    
     # Resample by labeling OHLC for specific columns
    if USE_ALL_COLS:
        d = d.resample('15min').ohlc()
    else:
        d = d.resample('15min').agg({'Open_'+pair_cap: 'first', 
                                         'High_'+pair_cap: 'max', 
                                         'Low_'+pair_cap: 'min', 
                                         'Close_'+pair_cap: 'last'})
        d.columns = ['Open_'+pair_cap,'High_'+pair_cap,'Low_'+pair_cap,'Close_'+pair_cap]
    
    
    #-- Select weekdays
    d=d[d.index.dayofweek < 5]
    
    # Label weekdays
    # d['WEEKDAY'] = ((pd.DatetimeIndex(d.index).dayofweek) // 5 == 1).astype(float)
    # d.loc[d['WEEKDAY'] < 1.0]
     
    #-- Get stationary features
    print('Getting differences...')
    
    d[pair+'_diff'] = (d['Close_'+pair_cap]-d['Close_'+pair_cap].shift(1))
    print('Getting returns...')
    
    d[pair+'_ret'] = (d['Close_'+pair_cap]/d['Close_'+pair_cap].shift(1))
    print('Getting log returns...')
    
    d[pair+'_log_ret'] = np.log(d['Close_'+pair_cap]/d['Close_'+pair_cap].shift(1))
    
    #-- Takes long time
    if DO_FRAC_DIFF:
        print('Getting fracdiff_ffd...')
    
        d[pair+'_fracdiff_ffd']  = fracDiff_FFD(pd.DataFrame(d['Close_'+pair_cap]),d=0.8)
        print('Getting fracdiff...')
    
        d[pair+'_fracdiff']  = fracDiff(pd.DataFrame(d['Close_'+pair_cap]),0.9)
        d[pair+'_fracdiff'] = d[pair+'_fracdiff'].astype('float')
    
    if DO_FRAC_DIFF:
        stnry_fts = [pair+'_diff',pair+'_ret',pair+'_log_ret',pair+'_fracdiff_ffd',pair+'_fracdiff']
    else:
        stnry_fts = [pair+'_diff',pair+'_ret',pair+'_log_ret']
    
    
    
    #-- Shift hours by starting hour
    d['Time']  = pd.to_datetime(d.index)
    
    #d['Time_lag'] = d['Time'].shift(periods = 96)
    
    #d['hour'] = d.index.hour
    #d['minute'] = d.index.minute
    d['entry'] = 0
    
    #-- Define entry time
    #d.loc[ (d['hour'] == 9) & (d['minute'] <15) ,'entry'] =1
    d.loc[ (d.index.hour == 9) & (d.index.minute <15) ,'entry'] =1
    
    #-- Label the period ID
    d['id'] = d['entry'].cumsum()
    
    
    #-- Renko
    print('Computing rekno...')
    
    d_rnk = pd.DataFrame((brick_size*pipsize)*np.floor(d['Close_'+pair_cap]/(brick_size*pipsize)))
    d_rnk['id'] = d['id']
    d_rnk.columns = ['tmp','id']
    d_rnk['renko_shift'] = d_rnk['tmp'] - d_rnk['tmp'].shift(1)
    d_rnk.dropna(inplace=True)
    d_rnk = d_rnk.reset_index()
    #d_rnk = d_rnk.drop(['Time'],axis = 1)
    d_rnk = d_rnk.loc[d_rnk['renko_shift'] != 0  ,['Time','renko_shift','id']]
    d_rnk['renko_shift'] = d_rnk['renko_shift'] / (pipsize*brick_size)
    deltas = []
    ids = []
    i = 0
    for i in d_rnk.index:
        deltas.extend(np.sign(d_rnk.loc[i,'renko_shift'])*np.ones( int(round(abs(d_rnk.loc[i,'renko_shift']) ))))
        ids.extend(d_rnk.loc[i,'id'] * np.ones( int(round(abs(d_rnk.loc[i,'renko_shift']) ))))
        i+=1
    d_rnk_unt = pd.DataFrame({pair+'_renko_unit':deltas,
                              'id':ids})
    
    d_rnk_unt['id'] = d_rnk_unt['id'].astype(int) 
    d_rnk_unt['Time'] = d_rnk_unt.index
    # Resulting renkos
        # d_rnk
        # d_rnk_unt
    
    #-- ID time lookup
    lut = d.loc[d['entry']>0,['entry','Time'] ]
    lut = lut.reset_index(drop=True)
    
    
    
    #-- Get features from TSFRESH
    print('Getting tsfresh features...')
    
    i = 1
    for ft in stnry_fts:
        print(ft)
        d_sel = d[['id','Time',ft]].dropna()
        x = extract_features(d_sel, column_id="id", column_sort="Time")
        #x.columns = ft +"_"+ x.columns
        if i==1:
            rs = x
        else:
            rs = rs.join(x, how='outer')
        i = i + 1
    
    print("Done!")
    rs = rs.join( lut , how='left' )
    
    print('Getting renko features')
    
    #-- Get features from the renko
    x = extract_features(d_rnk.dropna(), column_id="id", column_sort="Time")
    x.columns = "renko_"+x.columns
    rs = rs.join(x, how='outer')
    
    x = extract_features(d_rnk_unt, column_id="id", column_sort="Time")
    x.columns = "renko_unit_"+ x.columns
    rs = rs.join(x, how='outer')
        
        
        
    print('Saving csv')
    #-- Write to csv
    rs.to_csv(data_output_dir + pair+'_tsfresh_feats_STARTHOUR_'+str(START_HOUR)+'.csv')


if False:
    
    d['gbpusd_fracdiff_ffd'].plot()
    d['gbpusd_fracdiff'].plot()
    
    r = fracDiff_FFD(pd.DataFrame(d['Close_GBPUSD']),d=0.5)
    r = fracDiff(pd.DataFrame(d['gbpusd_log_ret']),d=0.5)
    
    
    d_ = pd.DataFrame(d['Close_GBPUSD'])
    
    d_.columns = ['Close']
    
    
    
    
    r = plotMinFFD(d_)
    
    
    # Convert to 15 min 
    
    
    d['gbpusd_diff'].plot()
    d['gbpusd_frac'].plot()
    d['gbpusd_log_frac'].plot()
    r.plot()
    adfuller(d['gbpusd_log_ret'].dropna(),maxlag=1,regression='c',autolag=None)
    
    # Get log(P_t/P_t-1)
    
    
    
    
    # Try fractional differencing
    
