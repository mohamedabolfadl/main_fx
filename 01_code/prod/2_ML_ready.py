# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 16:24:44 2019

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

#-- Run inputs
SPREAD_VEC = [1,2,3]
MAXTRADETIME_VEC = [4,6,8]
SL_VEC = [10,20,30]

STARTHOUR = 9


pairs = ['eurusd']

for pair in pairs:
    print('Reading features...')
    d_f = pd.read_csv(data_output_dir+pair+'_tsfresh_feats_STARTHOUR_'+str(STARTHOUR)+'.csv')
    for SL in SL_VEC:
            for SPREAD in SPREAD_VEC:
                for MAXTRADETIME in MAXTRADETIME_VEC:
                
                
                    print('Reading labels...')
                #   d_f = pd.read_csv(data_output_dir + pair+'_tsfresh_feats.csv')
                    d_l = pd.read_csv(data_output_dir + pair+'_labels_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')
                    
                    #-- Join on time
                    d = d_f.merge(d_l,on ="Time", how = "left")
                    
                    
                    
                    #-- Write to csv
                    d.to_csv(data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')


if False:
    pipsize=0.0001
    decimal_plc = int(np.log(1/pipsize)/np.log(10))
    SL_vec=100
    PF_vec=1
    SPREAD_VAL = 0
    MAX_PERIOD=50
    N = 5e6 #-- Number of columns to read
    USE_ALL_COLS = False
    START_HOUR = 9
    brick_size = 5 
    DO_FRAC_DIFF = False
    MAX_TRADE_TIME = 8*60  # Maximum time of the observation in minutes
    SL = 20
    TP_fac = 1
    buy_tp = SL*TP_fac
    sell_tp = SL*TP_fac
    buy_sl = SL
    sell_sl = SL
    
    d['buy'] = (d['buy'] == True).astype(int)
    d = d[np.isfinite(d['buy'])]
    
    #tst = d.iloc[:,14: ]
    #tst = d.iloc[:,-3: ]
    
    #-------------------------------------#
    #----    MODEL TRAINING           ----#
    #-------------------------------------#
    
    #vars_ = d.columns
    #tst.columns
    
    
    X = d.iloc[:, 14:].values
    y = d.iloc[:, 8].values #  8 Buy, 9 sell
    
    # Splitting the dataset into the Training set and Test set
    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)
    
    
    # Fitting XGBoost to the Training set
    from xgboost import XGBClassifier
    classifier = XGBClassifier()
    classifier.fit(X_train, y_train)
    
    #-- predict on test set
    y_pred = classifier.predict(X_test)
    
    # Making the Confusion Matrix
    from sklearn.metrics import confusion_matrix
    cm = confusion_matrix(y_test, y_pred)
    
    
    # Applying k-Fold Cross Validation
    from sklearn.model_selection import cross_val_score
    accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 5)
    accuracies.mean()
    accuracies.std()
    
    
    unq, cnt = np.unique(y_test, return_counts=True)
    
    print(classifier.feature_importances_)
    
    
    from matplotlib import pyplot
    from xgboost import plot_importance
    plot_importance(classifier)
    pyplot.show()
    
    fig, ax = pyplot.subplots(figsize=(12,18))
    plot_importance(classifier, max_num_features=50, height=0.8, ax=ax)
    pyplot.show()
    
    
    d.iloc[:,3452]
    d.columns[3452+14-1]
    
    classifier.get_score(importance_type='gain')
    
    
    
    # Predicting the Test set results
    y_pred = classifier.predict(X_test)
    
    # Making the Confusion Matrix
    from sklearn.metrics import confusion_matrix
    cm = confusion_matrix(y_test, y_pred)
    
    
    
    
    
    
    
    # Import `Sequential` from `keras.models`
    from keras.models import Sequential
    
    # Import `Dense` from `keras.layers`
    from keras.layers import Dense
    
    # Initialize the constructor
    model = Sequential()
    
    # Add an input layer 
    model.add(Dense(12, activation='relu', input_shape=(11,)))
    
    # Add one hidden layer 
    model.add(Dense(8, activation='relu'))
    
    # Add an output layer 
    model.add(Dense(1, activation='sigmoid'))
    d_l.columns
    d_f.columns
    
    
    d_l.head()
    d_f.head()
    d_l.Time
    d_f.Time
    
    d_l.index
    d_f.index
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
