# -*- coding: utf-8 -*-
"""
Created on Sun Jul 14 20:26:10 2019

@author: Mohamed Ibrahim
"""


#-- Clear variables
from IPython import get_ipython
ipython = get_ipython()
ipython.magic("%reset  -f")

####################################
#--      INPUT PARAMETERS        --#
####################################

proj_dir = "C://Users//Mohamed Ibrahim//Box Sync//FX_DATASCIENCE//main_fx"
MIN_OFFERS_IN_TOWER = 50
USE_XGB_CV = False
DO_HYPERPARAM_SEARCH = True
USE_XGB_MODEL = True # If false uses RF

####################################
#--      IMPORT LIBRARIES        --#
####################################


import time
import math
import pandas as pd
import re
import statistics as st
import os
import numpy as np
import xgboost
#import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from sklearn import  tree, linear_model
from sklearn.model_selection import cross_val_predict, cross_validate,train_test_split, KFold
from sklearn.metrics import explained_variance_score
from sklearn.preprocessing import StandardScaler



from scipy.stats import randint as sp_randint
import scipy.stats as st

from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
from sklearn.datasets import load_digits
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier

import seaborn as sns
import matplotlib.pyplot as plt

#-- Display settings
pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 20)
pd.set_option('display.width', 1000)




os. chdir(proj_dir)


#-- Read csv
df = pd.read_csv("02_data/intermediate/ML_SL_30_PF_1_SPREAD_3_ALL.csv")

#-- Put all col names in a dataframe
col_nms = pd.DataFrame(df.columns,columns = ['col_names'])


#-- Select on the EU cols
eu_colnames=col_nms.loc[col_nms.col_names.str.contains("EURUSD"),'col_names']
eu_colnames = pd.DataFrame(eu_colnames,columns = ['col_names'])

df_sel = df.loc[:,eu_colnames.col_names.values]


sns.set()
plt.figure()
sns.boxplot(x="BUY_RES_EURUSD", y = "USDJPY_EURUSD_corr_close_long", data = df_sel)

sns.barplot(x="BUY_RES_EURUSD", y = "USDJPY_EURUSD_corr_close_long", data = df_sel)

sns.pairplot(df_sel.loc[:,['BUY_RES_EURUSD','EURUSD_EMA_1000','buy_loss_EURUSD','AUDUSD_EURUSD_corr_diff_short']])

sns.boxenplot(x="BUY_RES_EURUSD", y = "USDJPY_EURUSD_corr_close_long", data = df_sel)


g = sns.scatterplot("EURUSD_DIFF_1_1","EURUSD_EMA_100",
                data = df_sel 
                ,hue = "BUY_RES_EURUSD")


sns.catplot(x="EURUSD_DIFF_1_1", y = "EURUSD_EMA_100", data = df_sel)


sns.distplot(x = "EURUSD_DIFF_1_1",
                data = df_sel)

sns.distplot(np.log1p(1+df_sel.NZDUSD_EURUSD_corr_close_long.dropna()))


sns.kdeplot(df_sel.EURUSD_DIFF_1_1)



predictors = [x if (x!="buy_profit_EURUSD" and
                   x!="buy_loss_EURUSD" and
                   x!="sell_profit_EURUSD" and
                   x!="sell_loss_EURUSD" and
                   x!="bs_EURUSD" and
                   x!="SELL_RES_EURUSD" and
                   x!="EURUSD_Open" and
                   x!="EURUSD_High" and
                   x!="EURUSD_Low" and
                   x!="EURUSD_Close") else ""  for x in df_sel.columns]

predictors = [x   for x in df_sel.columns if (x!="buy_profit_EURUSD" and
                   x!="buy_loss_EURUSD" and
                   x!="sell_profit_EURUSD" and
                   x!="sell_loss_EURUSD" and
                   x!="bs_EURUSD" and
                   x!="SELL_RES_EURUSD" and
                   x!="EURUSD_Open" and
                   x!="EURUSD_High" and
                   x!="EURUSD_Low" and
                   x!="EURUSD_Close") and
                    x!= "BUY_RES_EURUSD"]
target = "BUY_RES_EURUSD"

X = df_sel.dropna().loc[:,predictors]
y = df_sel.dropna().loc[:,target]



def report(results, n_top=3):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                  results['mean_test_score'][candidate],
                  results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))
            print("")


mdl = RandomForestClassifier()
param_dist = {"n_estimators":sp_randint(20, 100),
              #"max_depth": [3, None],
              #"max_features": sp_randint(1, 11),
              #"min_samples_split": sp_randint(2, 11),
              "bootstrap": [True, False]}
n_iter_search = 10
random_search = RandomizedSearchCV(mdl, 
                                   param_distributions=param_dist,
                                   n_iter=n_iter_search, 
                                   cv=5,
                                   scoring='roc_auc', #neg_mean_absolute_error
                                   iid=False)
random_search.fit(X, y)
report(random_search.cv_results_)





mdl = xgboost.XGBClassifier()
param_dist =  {"eta":[0.10,0.2]
 #        ,"gamma":0
 #        ,"max_depth":4
 #        ,"min_child_weight":1
 #        ,"subsample":0.8
 #        ,"colsample_bytree":0.4
         ,"nrounds":[100,200]
            }

n_iter_search = 4
random_search = RandomizedSearchCV(mdl, 
                                   param_distributions=param_dist,
                                   n_iter=n_iter_search, 
                                   cv=3,
                                   scoring='roc_auc', #neg_mean_absolute_error
                                   iid=False)

random_search.fit(X, y)
report(random_search.cv_results_)





import sklearn


import statistics as st


st.pvariance(df_sel.EURUSD_EMA_100)


#-- Opt 1
[x if "EURUSD" in x else "" for x in df.columns ]

#-- Opt 2
col_nms['has_EU'] = col_nms.col_names.apply(lambda x: "EURUSD" in x)

#-- Opt 3
eu_colnamescol_nms.loc[col_nms.col_names.str.contains("EURUSD"),'col_names']

df.BUY_RES_EURUSD.describe()
df.BUY_RES_EURUSD.summary()

plt.figure()
sns.distplot(df.price)

g = sns.regplot("area","price",
                data = df[(df.price<150000) & (df.area<3000) ])

g = sns.scatterplot("area","price",
                data = df[(df.price<150000) & (df.area<3000) ]
                ,hue = "furnished")

g = sns.boxplot("bedrooms","price"
                    ,data = df)ge

g = sns.boxplot("privatepool","price"
                    ,data = df)

g = sns.boxplot("bathrooms","price"
                    ,data = df)

g = sns.boxplot("privategarden","price"
                    ,data = df)


plt.show()
