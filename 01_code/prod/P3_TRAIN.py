# -*- coding: utf-8 -*-
"""
Created on Fri Aug  2 17:04:31 2019

@author: Mohamed Ibrahim

Purpose:
    Load train models based on feature + labels
    
Input:
    Labeled data
         data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv'
    
Output:
    

"""

####################################
#--       IMPORT LIBRARIES       
####################################

import numpy as np
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


####################################
#--       FUNCTION DEFINITIONS
####################################

def eda(data):
    print("----------Top-5- Record----------")
    print(data.head(5))
    print("-----------Information-----------")
    print(data.info())
    print("-----------Data Types-----------")
    print(data.dtypes)
    print("----------Missing value-----------")
    print(data.isnull().sum())
    print("----------Null value-----------")
    print(data.isna().sum())
    print("----------Shape of Data----------")
    print(data.shape)

#-- Box plot discrete vs contunuous
def boxplot(df, discVar, contVar, showfliers=False):
	data = pd.concat([df[contVar], df[discVar]], axis=1).dropna()
	f, ax = plt.subplots(figsize=(8, 6))
	sns.boxplot(x=discVar, y=contVar, data=data,showfliers=False)
	f.show()

#-- Scatterplot
def scatter(df, contVar1, contVar2):
	data = pd.concat([df[contVar2], df[contVar1]], axis=1)
	data.plot.scatter(x=contVar1, y=contVar2)


#-- Target should be boolean
def getPvalStats(df, target):
	import scipy.stats as stats
	df_res = pd.DataFrame(df.columns,columns=['column_name'])
	df_res['pval'] = 0
	for cl in df.columns:
	    try:
	        res = stats.ttest_ind(a=df.loc[df[target]=="True",cl].dropna(),b=df.loc[df[target]=="False",cl].dropna(),equal_var=False)
	        df_res.loc[df_res.column_name==cl,'pval'] = res[1]
	    except:
	        pass
	feat_rank = df_res.dropna().sort_values(by='pval').reset_index()
	return feat_rank


def correlationHeatMap(df):
	#correlation matrix
	corrmat = df.corr()
	f, ax = plt.subplots(figsize=(12, 9))
	sns.heatmap(corrmat, vmax=.8, square=True);


#-- Plot top k correlated to column x
def topCorrelatedHeatmap(df, x, k=10):
	corrmat = df.corr()
	#saleprice correlation matrix
	k = min(k,len(df.columns)) #number of variables for heatmap
	cols = corrmat.nlargest(k, x)[x].index
	cm = np.corrcoef(df[cols].values.T)
	sns.set(font_scale=1.25)
	hm = sns.heatmap(cm, cbar=True, annot=True, square=True, fmt='.2f', annot_kws={'size': 10}, yticklabels=cols.values, xticklabels=cols.values)
	plt.show()


def plotNormalityTest(df,col):
	sns.distplot(df[col], fit=norm);
	fig = plt.figure()
	res = stats.probplot(df[col], plot=plt)
    
    
def plot2dHist(df, col1, col2):
	with sns.axes_style('white'):
	    sns.jointplot(col1, col2, df, kind='hex');  
    

####################################
#--       ENTRY POINT       
####################################

#-- Change to project directory
os.chdir(path)

#-- Load data
df = pd.read_csv(data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv', low_memory=False)

#-- Get target column
targ_col = df[target]

#-- Define target columns
targ_cols = ['Unnamed: 0', 'id','index', 'seq', 'buy_tp', 'sell_tp', 'buy_sl', 'sell_sl', 'buy', 'sell',
       'buy_betsize', 'sell_betsize','Unnamed: 0.1']

#-- Features
feats = [x  for x in df.columns if x not in targ_cols]

#-- Giving shorter names for the features
new_feat_names = ['feat_'+str(i) for i in np.arange(1,len(feats)+1,1)]

#-- Create look up of feature names
feat_lut = pd.DataFrame(zip(feats,new_feat_names), columns = ["old_feat_name","new_feat_name"])

#-- Rename column names
df = df[feats]
df.columns = new_feat_names


#-- Set target



df['target'] = targ_col
#df = df[ (df['target'] == True) | (df['target'] == False)]


df['target'] = df['target'].astype(str)
df = df[ (df['target'] == "True") | (df['target'] == "False")]


#-- Dump csv before feature selection
df.to_csv(data_output_dir+'preprop_'+pair+'_targ_'+target+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')

          
          
          
          
####################################
#--       FEATURE SELECTION
####################################

from feature_selector import FeatureSelector

#-- Separate features from labels
y = df['target']
train_labels = y
df_feats = df.drop(columns = ['target'])

#-- Create an instance
fs = FeatureSelector(data = df_feats, labels = train_labels)

#-- Identify useless features
fs.identify_all(selection_params = {'missing_threshold': 0.6, 'correlation_threshold': 0.98, 
                                    'task': 'classification', 'eval_metric': 'auc', 
                                     'cumulative_importance': 0.99})
    
#-- Get valuable features   
X = fs.remove(methods = 'all', keep_one_hot = True)
#-- Drop time
X.drop(columns=["feat_2384"],inplace =True)
filt_feat_names = X.columns

y = df['target'] 

y_int = [int(x=="True") for x in y]

y_int = pd.Series(data=y_int)

y_int.name = 'target'


df_preprop = pd.concat([X,y_int],axis=1)

#-- Dump csv after feature selection
df_preprop.to_csv(data_output_dir+'preprop_featsel_'+pair+'_targ_'+target+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv')

####################################
#--       PREPROCESSING
####################################
from sklearn.preprocessing import StandardScaler

scaler = StandardScaler()
scaler.fit(X)

X = scaler.transform(X)

#-- Convert to boolean
y = y == "True"


####################################
#--       MODELING
####################################


#---------------------------------------------------------------------------

import xgboost

data_dmatrix = xgboost.DMatrix(data=X,label=y.values)
   
params_xgb = {"objective":"binary:logistic",
              'colsample_bytree': 0.05,
              'learning_rate': 0.01,
                    'max_depth': 3,
                    'nrounds':100}
    
cv_results = xgboost.cv(dtrain=data_dmatrix, params=params_xgb, nfold=20,
                        num_boost_round=500,early_stopping_rounds=30,metrics="auc",
                        as_pandas=True, seed=123)
    
print((cv_results["test-auc-mean"]).tail(1))
#-- 0.5326

#---------------------------------------------------------------------------

import lightgbm as lgb
dftrainLGB = lgb.Dataset(data = X, label = [int(x) for x in y ], feature_name = list(filt_feat_names))


params_lgb = {   'application': 'binary',
    'objective': 'binary',
    'metric': 'auc',
    'feature_fraction': 0.95,
    'learning_rate': 0.05,
    'verbose': 0}



cv_results = lgb.cv(
        params_lgb,
        dftrainLGB,
        num_boost_round=500,
        nfold=5,
        metrics='auc',
        early_stopping_rounds=300
        )
print((np.mean(cv_results["auc-mean"])))


#---------------------------------------------------------------------------
from tpot import TPOTClassifier


pipeline_optimizer = TPOTClassifier()


pipeline_optimizer = TPOTClassifier(generations=5, population_size=20, cv=5,
                                    random_state=42, verbosity=2)

pipeline_optimizer.fit(X, y_int)

pipeline_optimizer.export('tpot_exported_pipeline.py')





if TEST_FEATURE_SELECTION:
    fs.identify_missing(missing_threshold=0.6)
    missing_features = fs.ops['missing']
    missing_features[:10]
    
    fs.plot_missing()
    
    fs.missing_stats.head(10)
    
    
    fs.identify_single_unique()
    single_unique = fs.ops['single_unique']
    single_unique
    fs.plot_unique()
    
    
    
    fs.identify_collinear(correlation_threshold=0.975)
    correlated_features = fs.ops['collinear']
    correlated_features[:5]
    fs.plot_collinear()
    fs.record_collinear.head()
    
    fs.identify_zero_importance(task = 'classification', eval_metric = 'auc', 
                                n_iterations = 10, early_stopping = True)
    one_hot_features = fs.one_hot_features
    base_features = fs.base_features
    print('There are %d original features' % len(base_features))
    print('There are %d one-hot features' % len(one_hot_features))
    fs.plot_feature_importances(threshold = 0.99, plot_n = 12)
    fs.feature_importances.head(10)





if DO_DATA_EXPLORATION:
    discVar = 'target'
    contVar = 'feat_3766'
    boxplot(df, discVar,contVar)
    
    contVar1 = 'feat_1717'
    contVar2 = 'feat_3766'
    scatter(df,contVar1,contVar2)
    plot2dHist(df,contVar1,contVar2)
    
    
    rnk = getPvalStats(df, 'target')
    
    corrmat = df[rnk.iloc[3:30,1]].corr()
    
    correlationHeatMap(df[rnk.iloc[3:90,1]])
    
    
    plotNormalityTest(df, contVar1)
    
    



#-------------------------------   JUNK   -------------------------------------

from feature_selector import FeatureSelector

#-- Separate features from labels
y = df['target']
train_labels = y
df_feats = df.drop(columns = ['target'])

#-- Create an instance
fs = FeatureSelector(data = df_feats, labels = train_labels)


#-- Missing value features
fs.identify_missing(missing_threshold=0.6)
missing_features = fs.ops['missing']


#-- Single unique
fs.identify_single_unique()
single_unique = fs.ops['single_unique']


#-- Correlated
fs.identify_collinear(correlation_threshold=0.8)
correlated_features = fs.ops['collinear']

correlated_features = fs.record_collinear.loc[:,"drop_feature"]

#-- Features to exclude
exc_feats =[]
exc_feats.extend(missing_features)
exc_feats.extend(single_unique)
exc_feats.extend(correlated_features)

sel_cols = list(set(df.columns)-set(exc_feats))
#sel_cols.extend(["target"])
df_cl = df[sel_cols]

#-- Trying to select the top N features with pval for training
import xgboost
df_cl.target = df_cl.target.astype(str)

r = getPvalStats(df_cl , "target")
#-- Skip top 2 feats which are target and time
skipFeats = 2 





#-- XGB params
params_xgb = {"objective":"binary:logistic",
                  'colsample_bytree': 0.1,
                  'learning_rate': 0.01,
                        'max_depth': 3,
#                        'nrounds':50,
                        'n_estimators':10}


nFeatSelectVec = range(5,15)

auc = []

for nFeatsSelect in nFeatSelectVec:
    print(nFeatsSelect)
    #-- Number of features to select
    #nFeatsSelect = 20
    
    #-- Get top feats
    selFeats = r.iloc[skipFeats :nFeatsSelect,:]
    
    
    #-- Select columns (features + target)
    selCols = list(selFeats.column_name)
    selCols.extend(["target"])
    df_sel = df[selCols]
    
    #-- Convert String to int
    df_sel.target  = df_sel.target.map(lambda x: int(x=="True"))
    
    
    
    #-- Split features from target
    X = df_sel[list(selFeats.column_name)]
    y = df_sel.target
    
    
    
    data_dmatrix = xgboost.DMatrix(data=X,label=y.values)
       
        
    cv_results = xgboost.cv(dtrain=data_dmatrix, params=params_xgb, nfold=20,
                            metrics="auc",
                            as_pandas=True, seed=123)
        
    #num_boost_round=5000,early_stopping_rounds=30,
    
    print( ( (cv_results["test-auc-mean"]).tail(1) ))

    auc.append(cv_results["test-auc-mean"].mean())



#-- Checking the name of the features
selFeats.drop("index",inplace = True, axis =1)
selFeats.columns = ["new_feat_name","pval"]
t = selFeats.merge(feat_lut, how = 'left',on='new_feat_name')





df_cl.target = df_cl.target == 1
#df_cl.target = df_cl.target.map(lambda x: if x: "True" else "False")

X = df_cl[list(set(df_cl.columns)-set("target"))]
X.drop("feat_2384", inplace=True, axis = 1)
X.drop("target", inplace=True, axis = 1)

y = df_cl.target.map(lambda x: int(x))






#correlated_features[:5]
#fs.plot_collinear()
#fs.record_collinear.head()


