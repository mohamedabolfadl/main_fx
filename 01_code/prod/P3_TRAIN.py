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
from feature_selector import FeatureSelector
from sklearn.preprocessing import StandardScaler
import xgboost

pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 20)
pd.set_option('display.width', 1000)

path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'


####################################
#--       INPUT VARIABLES       
####################################

data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"


USE_LEARNER_FOR_FEATURE_SELECTION =False
DO_DATA_EXPLORATION = False
TEST_FEATURE_SELECTION =False


#-- Combinatorial CV parameters
#-- N groups of CV
N_CV = 6
k_CV = 2
DECISION_THRESHOLD = 0.6
NFEAT_PVAL = 800

#-- Feature selection maximum missing values
MISSING_VALUE_THRESHOLD = 0.10
#-- Feature selection maximum correlation between variables
CORRELATION_THRESHOLD = 0.8

#-- Portion to test the model
TEST_PORTION = 0.1


#-- Run inputs
SPREAD = 1 # [1,2,3]
MAXTRADETIME = 8 # [4,6,8]
SL = 30 # [10,20,30]
STARTHOUR = 9
pair = 'eurusd'
target = 'sell'
start_date = '2011-01-01 00:00:00'

####################################
#--       FUNCTION DEFINITIONS
####################################

exec(open("01_code/prod/P3_functions.py").read())

####################################
#--       ENTRY POINT       
####################################

#-- Change to project directory
os.chdir(path)

#-- Load data
df = pd.read_csv(data_output_dir+'ml_ready_'+pair+'_SL_'+str(SL)+'_SPREAD_'+str(SPREAD)+'_MAXTRADETIME_'+str(MAXTRADETIME)+'.csv', low_memory=False)

#-- Filter dates
df = df.loc[df.Time>start_date,:]


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
feat_lut = pd.DataFrame(list(zip(feats,new_feat_names)), columns = ["old_feat_name","new_feat_name"])

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



#-- Separate features from labels
y = df['target']
train_labels = y
df_feats = df.drop(columns = ['target'])

#-- Create an instance
fs = FeatureSelector(data = df_feats, labels = train_labels)

#-- Identify redundant features
if(USE_LEARNER_FOR_FEATURE_SELECTION):
    # NOT COMPLETE
    fs.identify_all(selection_params = {'missing_threshold': 0.6, 'correlation_threshold': 0.98, 
                                    'task': 'classification', 'eval_metric': 'auc', 
                                     'cumulative_importance': 0.99})
    #-- Get valuable features   
    X = fs.remove(methods = 'all', keep_one_hot = True)

else:
    #-- Features with missing values greater than threshold 
    fs.identify_missing(missing_threshold = MISSING_VALUE_THRESHOLD)
    #-- Correlated features
    fs.identify_collinear(correlation_threshold = CORRELATION_THRESHOLD)
    #-- Single unique value
    fs.identify_single_unique()
    
    #-- TO get keys fs.ops.keys()
    missing_features = list(fs.ops['missing'])
    corelated_features = list(fs.ops['collinear'])
    single_value = list(fs.ops['single_unique'])
    
    r = set(flatten([missing_features,corelated_features,single_value]))
    #X = df_feats.drop(r, axis=1)    
    

     
rnk_pval = getPvalStats(df, 'target')    


feat_types = pd.DataFrame(df_feats.dtypes)
feat_types.columns = ['type']
feat_types = feat_types.reset_index()
drp_cls = feat_types.loc[feat_types.type!='float64','index']

#-- Drop time
#df_feats.drop(columns=["feat_2384"],inplace =True)
df_feats.drop(columns=drp_cls.values,inplace =True)

#-- Drop low pval
cols = list(rnk_pval.iloc[2:NFEAT_PVAL+2,1])
X = df_feats[df_feats.columns.intersection(cols)]




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

#-- Scale features
scaler = StandardScaler()
scaler.fit(X)
X_scaled = pd.DataFrame(scaler.transform(X), columns = X.columns)

#-- Convert target to boolean
y_boolean = y == "True"




####################################
#--       MODELING
####################################
#-- Inputs
#
# X: Feature selected unscaled features
# X_scaled: Feature selected scaled features
#
# y_int : Target in integer form (0,1)
# y_boolean: Target in boolean form
# y: Target in string form



#-- Splitting CV and test
#-- Total rows
N_tot = len(X)
#-- Test size
N_test = round(TEST_PORTION*N_tot)
#-- Fune tuning for CV size
N_trn = N_CV*round((N_tot-N_test)/N_CV)
#-- Updating test size
N_test = N_tot-N_trn

#-- Splitting train and test
X_trn = X.iloc[0:N_trn,:]
X_tst = X.iloc[N_trn:X.shape[0],:]

X_scaled_trn = X_scaled.iloc[0:N_trn,:]
X_scaled_tst = X_scaled.iloc[N_trn:X.shape[0],:]

y_trn = y[0:N_trn]
y_tst = y[N_trn:X.shape[0]]

y_int_trn = y_int[0:N_trn]
y_int_tst = y_int[N_trn:X.shape[0]]

y_boolean_trn = y_boolean[0:N_trn]
y_boolean_tst = y_boolean[N_trn:X.shape[0]]




#-- Create cv instance
cvsett = CvSettings(N_CV,k_CV)
#-- Get combinatoral matrices
cvFlags, nPaths, cvMask = cvsett.getCVLabel(N_trn,N_CV,k_CV)
#-- Set index to linspace
cvMask.index = range(cvMask.shape[0])

cvProb = cvMask.copy()
cvTruth = cvProb.copy()


exec(open("01_code/prod/P3_functions.py").read())
xgb_mdl = FxModels("xgboost")
lgb_mdl = FxModels("lightgbm")
cat_mdl = FxModels("catboost")
rfo_mdl = FxModels("randomforest")
glm_mdl = FxModels("glm")
dl_mdl = FxModels("deeplearning")

i=0
while (i<len(cvMask.columns)):
    
    print(i)
    #-- Train test indicies
    tst_inds = cvMask.iloc[:,i]>0
    trn_inds = cvMask.iloc[:,i]<1
    
    #-- Test
    x_cv_tst = X_scaled_trn.loc[ tst_inds ,:]
    y_cv_tst = y_int_trn[ tst_inds ]
    
    #-- Train
    x_cv_trn = X_scaled_trn.loc[ trn_inds ,:]
    y_cv_trn = y_int_trn[ trn_inds ]
    
    #-- XGboost
    clf = xgb_mdl.xgb_model( x_cv_trn, y_cv_trn)
    y_pred = xgb_mdl.xgb_predict(clf, x_cv_tst)

    #-- LGB    
    #clf = lgb_mdl.lgb_model( x_cv_trn, y_cv_trn)
    #y_pred = lgb_mdl.lgb_predict(clf, x_cv_tst)
    
    #-- Catboost
    #clf = cat_mdl.cat_model( x_cv_trn, y_cv_trn)
    #y_pred = cat_mdl.cat_predict(clf, x_cv_tst)
    
    
    #-- RandomForest
    #clf = rfo_mdl.rfo_model( x_cv_trn, y_cv_trn)
    #y_pred = rfo_mdl.rfo_predict(clf, x_cv_tst)
    
    
    #-- GLM
    #clf = glm_mdl.glm_model( x_cv_trn, y_cv_trn)
    #y_pred = glm_mdl.rfo_predict(clf, x_cv_tst)


    #-- Deep learning
    #clf = dl_mdl.dl_model( x_cv_trn, y_cv_trn.values)
    #y_pred = dl_mdl.dl_predict(clf, x_cv_tst)

    #-- Setting predictions and truth
    cvProb.loc[tst_inds,i+1] = y_pred.reshape((len(y_pred),))
    cvTruth.loc[tst_inds,i+1] =y_cv_tst
    

    i=i+1


#-- Get path metrics
DECISION_THRESHOLD = 0.5
sharpe_vec,drawdown_vec,drawup_vec,pl_eq_vec= cvsett.getPathMetrics( cvProb, y_int_trn, DECISION_THRESHOLD , cvFlags)

print(np.mean(sharpe_vec))


plt.plot(pl_eq_vec[0])




#-- XGBoost sample code
    #-- Put in XGBoost format
    #xgtrain = xgboost.DMatrix(x_cv_trn, y_cv_trn)
    #xgtest = xgboost.DMatrix(x_cv_tst, y_cv_tst)
    
    #param = {'max_depth':2, 'silent':1, 'colsample_bytree':0.1, 'eta':0.01, 'subsample':0.2,  'objective':'binary:logistic'}
    #num_round = 1000
    #bst = xgboost.train(param, xgtrain, num_round)
    #y_pred = bst.predict(xgtest)



#---------------------------------------------------------------------------



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


