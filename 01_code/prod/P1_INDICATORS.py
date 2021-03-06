# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 23:07:01 2019

@author: Mohamed Ibrahim

Purpose:
    Compute all features from minute data
    
Input: 
    Minute data: dt_all_min.csv in input folder

Output:
    data_output_dir + pair+'_tsfresh_feats_STARTHOUR_'+str(START_HOUR)+'.csv'

"""

####################################
#--       INPUT VARIABLES       
####################################

data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"

#-- Change to project directory
path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'


#-- Compute OHLC for all pairs before extracting features
USE_ALL_COLS = False
#-- Start trade hour
START_HOUR = 9
#-- Brick size in pips of the Renko
brick_size = 5 
#-- Flag whether to do slow fractional differentiation
DO_FRAC_DIFF = False
#-- Pairs to extract the features
pairs = ['eurusd','gbpusd']

####################################
#--       IMPORT LIBRARIES       
####################################

import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from tsfresh import extract_features
import os


#-- Change to project home directory
os.chdir(path)

####################################
#--       DEFINE FUNCTIONS       
####################################

exec(open("01_code/prod/P1_functions.py").read())

####################################
#--       LOAD DATA       
####################################

#-- Read minute data
print('Reading minute data...')
df = pd.read_csv(data_input_dir+"dt_all_min.csv")

####################################
#--       MAIN ENTRY       
####################################


#-- Loop over the pairs to extract the features
for pair in pairs:

    print(70*'#')
    print(pair)
    pair_cap = pair.upper()
    d = df.loc[0:df.shape[0]-10]
    
    #-- Set pipsize
    if pair == 'usdjpy':
        pipsize = 0.01
    else:
        pipsize = 0.0001
    
    #-- Select the current pairs columns
    d=d[['Time','Open_'+pair_cap,'High_'+pair_cap,'Low_'+pair_cap,'Close_'+pair_cap]]
    
    #-- Convert Time to datetime
    d['Time']  = pd.to_datetime(d['Time'])
    
    #-- Set the index to be the time
    d = d.set_index('Time')
    
     #-- Resample by labeling OHLC for specific columns
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
        stnry_fts = [pair+'_diff',pair+'_ret',pair+'_log_ret',pair+'_fracdiff_ffd',pair+'_fracdiff']
    else:
        stnry_fts = [pair+'_diff',pair+'_ret',pair+'_log_ret']
    
    
    
    #-- Get time as a feature form index
    d['Time']  = pd.to_datetime(d.index)
    
    #-- Initialize entry hour
    d['entry'] = 0
    
    #-- Label entry time as input START_HOUR
    d.loc[ (d.index.hour == START_HOUR) & (d.index.minute <15) ,'entry'] =1
    
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

    
    #-- ID time lookup
    lut = d.loc[d['entry']>0,['entry','Time'] ]
    lut = lut.reset_index(drop=True)
    
    
 
    #-- Get features from TSFRESH
    print('Getting tsfresh features...')
    
    i = 1
    for ft in stnry_fts:
        print(ft)
        d_sel = d[['id','Time',ft]].dropna()

        #-- Get TSFRESH features
        x = extract_features(d_sel, column_id="id", column_sort="Time")

        if i==1:
            rs = x
        else:
            rs = rs.join(x, how='outer')
        i = i + 1
    

    #-- Join entry and time
    print("Done!")
    rs = rs.join( lut , how='left' )
    
    
    #-- Get features from the renko chart
    print('Getting renko features')
    x = extract_features(d_rnk.dropna(), column_id="id", column_sort="Time")
    x.columns = "renko_"+x.columns
    rs = rs.join(x, how='outer')
    
    x = extract_features(d_rnk_unt, column_id="id", column_sort="Time")
    x.columns = "renko_unit_"+ x.columns
    rs = rs.join(x, how='outer')
        
        
        
    print('Saving csv')
    #-- Write to csv
    rs.to_csv(data_output_dir + pair+'_tsfresh_feats_STARTHOUR_'+str(START_HOUR)+'.csv')


