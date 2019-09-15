# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 23:07:48 2019
@author: Mohamed Ibrahim

Purpose:
    Label buy and sell trades
    
Input: 
    Minute data: dt_all_min.csv in input folder

Output:
    data_output_dir+pair+'_labels.csv'

"""

####################################
#--       INPUT VARIABLES       
####################################

data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"

path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'

#-- Pairs to compute the labels for
pairs = ['eurusd','gbpusd','audusd','usdjpy','usdcad','usdchf','xauusd','nzdusd']
pipsize=0.0001
#decimal_plc = int(np.log(1/pipsize)/np.log(10))
#SL_vec=100
#PF_vec=1
#SPREAD_VAL = 0
#MAX_PERIOD=50
N = 5e6 #-- Number of columns to read
#USE_ALL_COLS = False
#START_HOUR = 9
#brick_size = 5 
#DO_FRAC_DIFF = False
MAX_TRADE_TIME = 4*60  # Maximum time of the observation in minutes
SL = 30
TP_fac = 1
buy_tp = SL*TP_fac
sell_tp = SL*TP_fac
buy_sl = SL
sell_sl = SL
SPREAD = 1.

####################################
#--       IMPORT LIBRARIES       
####################################

import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from tsfresh import extract_features
import os

#-- Change to project directory
os.chdir(path)

pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 20)
pd.set_option('display.width', 1000)

####################################
#--       DEFINE FUNCTIONS       
####################################

exec(open("01_code/prod/P1_functions.py").read())


####################################
#--       MAIN ENTRY       
####################################


print('Reading minute data...')
df = pd.read_csv(data_input_dir+"dt_all_min.csv")


#-- Loop over the pairs
for pair in pairs:

    print(70*'#')
    print(pair)
    
    pair_cap = pair.upper()
  
    d = df[['Time','High_'+pair_cap,'Low_'+pair_cap,'Close_'+pair_cap]]
    
    #-- Convert time to date
    d['Time']  = pd.to_datetime(d['Time'])
    
    # Set the index to be the time
    d = d.set_index('Time')
    
    #-- Exclude weekends
    d=d[d.index.dayofweek < 5]
    
    #-- Initialize entry entry
    d['entry'] = 0
        
    #-- Define entry time as all hour times    
    d.loc[  (d.index.minute <1) ,'entry'] =1
         
    #-- Label the period ID
    d['id'] = d['entry'].cumsum()
    
    #-- Set the row to index rather than Time
    d = d.reset_index()
    
    d['seq'] = np.arange(0,d.shape[0],1)
    
    iter_ = d.loc[d['entry']>0,['Time','seq']]
    iter_ = iter_.reset_index()
    iter_['buy_tp'] = np.nan * np.ones(iter_.shape[0])
    iter_['sell_tp'] = np.nan * np.ones(iter_.shape[0])
    iter_['buy_sl'] = np.nan * np.ones(iter_.shape[0])
    iter_['sell_sl'] = np.nan * np.ones(iter_.shape[0])
    
    i = 0
    
    while i<iter_.shape[0]:
        entry = d.loc[ iter_.loc[i,'seq'] ,'Close_'+pair_cap]
        rng = d.loc[iter_.loc[i,'seq']+1:iter_.loc[i,'seq']+MAX_TRADE_TIME].copy()
        rng = rng.reset_index()
        rng['High_'+pair_cap] = rng['High_'+pair_cap]-entry
        rng['Low_'+pair_cap] = entry-rng['Low_'+pair_cap]
        iter_.loc[i , 'buy_tp'] = rng[ (rng['High_'+pair_cap]-SPREAD*pipsize) >pipsize*buy_tp].index.min()
        iter_.loc[i , 'sell_tp'] =rng[(rng['Low_'+pair_cap]-SPREAD*pipsize) >pipsize*sell_tp].index.min()
        iter_.loc[i , 'sell_sl'] =rng[(rng['High_'+pair_cap]+SPREAD*pipsize) >pipsize*buy_tp].index.min()
        iter_.loc[i , 'buy_sl'] = rng[(rng['Low_'+pair_cap]+SPREAD*pipsize) >pipsize*sell_tp].index.min()
        
        
        
        i+=1
    
    iter_.loc[:,['Time','buy_tp','buy_sl','sell_tp','sell_sl']]
    
    #-- Label buy
    iter_['buy'] = iter_['buy_tp']<iter_['buy_sl']
    iter_.loc[ np.isnan(iter_['buy_sl']) & np.isreal(iter_['buy_tp']), 'buy'] = True
    iter_.loc[ np.isnan(iter_['buy_sl']) & np.isnan(iter_['buy_tp']), 'buy'] = False
    iter_.loc[:,['buy_tp','buy_sl','buy']]
    
    #-- Label sell
    iter_['sell'] = iter_['sell_tp']<iter_['sell_sl']
    iter_.loc[ np.isnan(iter_['sell_sl']) & np.isreal(iter_['sell_tp']), 'sell'] = True
    iter_.loc[ np.isnan(iter_['sell_sl']) & np.isnan(iter_['sell_tp']), 'sell'] = False
    iter_.loc[:,['sell_tp','sell_sl','sell']]
    
    #-- Label betsize
    iter_['buy_betsize'] =False
    iter_.loc[ np.isnan(iter_['buy_sl']) & np.isnan(iter_['buy_tp']), 'buy_betsize'] = True
    iter_['sell_betsize'] =False
    iter_.loc[ np.isnan(iter_['sell_sl']) & np.isnan(iter_['sell_tp']), 'sell_betsize'] = True
    
    
    iter_.to_csv(data_output_dir+pair+'_labels.csv')




