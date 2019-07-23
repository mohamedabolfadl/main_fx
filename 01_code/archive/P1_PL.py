# -*- coding: utf-8 -*-
"""
Created on Sun Mar 24 13:02:06 2019

save multiple csv files according to 
SPREAD
SL
MAX_TRADE_TIME


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



rm_all()




data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"

path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'
os.chdir(path)



SPREAD_vec = [1,2,3]
SL_vec = [10,20,30]
MAX_TRADE_TIME_vec =[4,6,8]
pairs = ['eurusd']
START_HOUR_curr = 9

print('Reading minute data...')
df = pd.read_csv(data_input_dir+"dt_all_min.csv")

for pair in pairs:
                    if pair =='usdjpy':
                        pipsize=0.01
                    else:
                        pipsize = 0.0001
                    decimal_plc = int(np.log(1/pipsize)/np.log(10))
                
                    pair_cap = pair.upper()
                    
                    d = df[['Time','High_'+pair_cap,'Low_'+pair_cap,'Close_'+pair_cap]]
                    
                    #-- Convert time to date
                    d['Time']  = pd.to_datetime(d['Time'])
                    
                    # Set the index to be the time
                    d           = d.set_index('Time')
                    
                    #-- Exclude weekends
                    d=d[d.index.dayofweek < 5]
                    
                    #-- Init entry
                    d['entry'] = 0
                        
                    #-- Define entry time
                    #d.loc[ (d.index.hour == 9) & (d.index.minute <1) ,'entry'] =1
                    d.loc[  (d.index.minute <1) ,'entry'] =1
                         
                    #-- Label the period ID
                    d['id'] = d['entry'].cumsum()
                    
                    #-- Set the row to index rather than Time
                    d = d.reset_index()
                    
                    d['seq'] = np.arange(0,d.shape[0],1)

                    for sprd_curr in SPREAD_vec:
                        for sl_curr in SL_vec:
                            for MAX_TRADE_TIME_curr in MAX_TRADE_TIME_vec:
                                
                           
                                    
                                        MAX_TRADE_TIME = MAX_TRADE_TIME_curr*60  # Maximum time of the observation in minutes
                                        SL = sl_curr
                                        SPREAD = sprd_curr
                                        TP_fac = 1
                                        buy_tp = SL*TP_fac
                                        sell_tp = SL*TP_fac
                                        buy_sl = SL
                                        sell_sl = SL
                                        START_HOUR  = START_HOUR_curr
                                    
                                    
                                        print(70*'#')
                                        print('SL_'+str(SL)+' MAX_TRADE_TIME: '+str(MAX_TRADE_TIME)+' SPREAD: '+str(SPREAD))
                                    
                                    
                                    
                                        
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
                                            
                                        print("Labeling PL")
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
                                        print("Done")
                                        
                                        
                                        iter_.to_csv(data_output_dir+pair+'_labels_SL_'+str(sl_curr)+'_SPREAD_'+str(sprd_curr)+'_MAXTRADETIME_'+str(MAX_TRADE_TIME_curr)+'.csv')
                
                
                
                
