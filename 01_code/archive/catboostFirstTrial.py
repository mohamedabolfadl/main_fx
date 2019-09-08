# -*- coding: utf-8 -*-
"""
Created on Sun Sep  8 15:42:33 2019

@author: Mohamed Ibrahim
"""

import numpy as np

from catboost import CatBoostClassifier, Pool
import pandas as pd
import os
import seaborn as sns
import matplotlib.pyplot as plt

####################################
#--       INPUT VARIABLES       
####################################

data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"

DO_DATA_EXPLORATION = False
TEST_FEATURE_SELECTION =False

path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'

#-- Run inputs
SPREAD = 1 # [1,2,3]
MAXTRADETIME = 8 # [4,6,8]
SL = 30 # [10,20,30]
STARTHOUR = 9
pair = 'eurusd'
target = 'sell'


# initialize data
train_data = np.random.randint(0,
                               100, 
                               size=(100, 10))

train_labels = np.random.randint(0,
                                 2,
                                 size=(100))

test_data = catboost_pool = Pool(train_data, 
                                 train_labels)

model = CatBoostClassifier(iterations=2,
                           depth=2,
                           learning_rate=1,
                           loss_function='Logloss',
                           verbose=True)
# train the model
model.fit(train_data, train_labels)
# make the prediction using the resulting model
preds_class = model.predict(test_data)
preds_proba = model.predict_proba(test_data)
print("class = ", preds_class)
print("proba = ", preds_proba)




############ TRIAL with real data ###########################
from catboost import cv, randomized_search

params = {}
params['loss_function'] = 'Logloss'
params['iterations'] = 80
params['custom_loss'] = 'AUC'
params['random_seed'] = 63
params['learning_rate'] = 0.5

df= pd.read_csv(data_output_dir+'preprop_featsel_eurusd_targ_sell_SL_20_SPREAD_1_MAXTRADETIME_8.csv')

y = df['target']
train_labels = y
X = df.drop(columns = ['target'])

cv_data = cv(
    params = params,
    pool = Pool(X, label=y),
    fold_count=5,
    shuffle=True,
    partition_random_seed=0,
    plot=False,
    stratified=False,
    verbose=True
)

if False: # Incomplete hyper parameters search
    param_distributions
    
    randomized_search(param_distributions,
                      X,
                      y=None,
                      cv=3,
                      n_iter=10,
                      partition_random_seed=0,
                      calc_cv_statistics=True, 
                      search_by_train_test_split=True,
                      refit=True, 
                      shuffle=True, 
                      stratified=None, 
                      train_size=0.8, 
                      verbose=True)


import xgboost

data_dmatrix = xgboost.DMatrix(data=X,label=y.values)
   
params_xgb = {"objective":"binary:logistic",
#              'colsample_bytree': 0.05,
#              'learning_rate': 0.01,
#                    'max_depth': 3,
                    'nrounds':80,
                    'learning_rate':0.5}
    
cv_results = xgboost.cv(dtrain=data_dmatrix, params=params_xgb, nfold=5,
                        num_boost_round=80,metrics="auc",
                        as_pandas=True, seed=123)
    
print((cv_results["test-auc-mean"]).tail(1))


