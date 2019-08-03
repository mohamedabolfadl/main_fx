# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 16:24:44 2019

@author: Mohamed Ibrahim

Purpose:
    Join feature and labeled data
    
Input: 
    Feature data
        data_output_dir+pair+'_tsfresh_feats_STARTHOUR_'+str(STARTHOUR)+'.csv'
    Labeled data
        data_output_dir + pair+'_labels_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv'

Output:
    Feature + Labled data
    data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv'

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

#-- Run inputs
SPREAD_VEC = [1,2,3]
MAXTRADETIME_VEC = [4,6,8]
SL_VEC = [10,20,30]
STARTHOUR = 9
pairs = ['eurusd']



####################################
#--       IMPORT LIBRARIES       
####################################

import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from tsfresh import extract_features
import os



####################################
#--       ENTRY POINT       
####################################

#-- Change to project directory
os.chdir(path)



for pair in pairs:
    print('Reading features...')
    d_f = pd.read_csv(data_output_dir+pair+'_tsfresh_feats_STARTHOUR_'+str(STARTHOUR)+'.csv')
    for SL in SL_VEC:
            for SPREAD in SPREAD_VEC:
                for MAXTRADETIME in MAXTRADETIME_VEC:
                
                
                    print('Reading labels...')
                    d_l = pd.read_csv(data_output_dir + pair+'_labels_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')
                    
                    #-- Join on time
                    d = d_f.merge(d_l,on ="Time", how = "left")

                    #-- Write to csv
                    d.to_csv(data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')


