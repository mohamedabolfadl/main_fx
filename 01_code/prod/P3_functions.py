# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 18:25:22 2019

@author: Mohamed Ibrahim

Purpose:
    
    Includes all functions related to P1
"""

import numpy as np
import pandas as pd
import itertools
import seaborn as sns
import matplotlib.pyplot as plt
import xgboost as xgb
import lightgbm as lgb
import catboost as cat
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from keras.models import Sequential
from keras.layers import Dense,Activation,Layer,Lambda

#-- Clean all variables
def rm_all():
    for name in dir():
        if not name.startswith('_'):
            del globals()[name]


def flatten(l):
    return([item for sublist in l for item in sublist])


class FxModels:
    
    
    def __init__(self,mdl_type):
        if(mdl_type not in ["xgboost","lightgbm","glm","catboost","randomforest","glm","deeplearning"]):
            print("Model not yet supported")
        else:
            self.mdl_type = mdl_type
            if mdl_type == "xgboost":
                #-- Default XGB features
                self.xgb_params ={
                                  'max_depth':7,
                           'min_child_weight':1,
                           'learning_rate':0.1,
                           'n_estimators':20,
                           'silent':True,
                           'objective':'binary:logistic',
                           'gamma':0,
                           'max_delta_step':0,
                           'subsample':1,
                           'colsample_bytree':1,
                           'colsample_bylevel':1,
                           'reg_alpha':0,
                           'reg_lambda':0,
                           'scale_pos_weight':1,
                                  }
                
            if mdl_type =="lightgbm":
               self.lgb_params =  {"objective": "binary",
                                            "learning_rate": 0.1,
                                             "verbosity": 0,
                                             "drop_rate": 0.1,
                                              "is_unbalance": False,
                                                  "subsample": 0.9,
                                                  'num_boost_round':20
                                                  }

            if mdl_type =="catboost":
               self.cat_params = {'depth': 3,
                                  'learning_rate' : 0.1,
                                 'l2_leaf_reg': 1,
                                 'iterations': 20,
                                 'verbose':0}
               
            if mdl_type =="randomforest":
                self.rfo_params = {'n_estimators' : 50,
                                  'max_depth':5,
                                  'criterion':'gini', # 'entropy'
                                   'bootstrap': True,
                                   'class_weight':'balanced_subsample',
                                   'max_features': 'auto', #'sqrt', 'log2', None],
                                    'verbose':0}
                
            if mdl_type =="glm":
                self.glm_params = {'max_iter' : 10,
                                  #'l1_ratio':0.5,
                                  'class_weight':'balanced', # 'entropy'
                                   'penalty': 'elasticnet'}
            if mdl_type =="deeplearning":
                self.dl_params = {
                        'activation':'relu',
                        'arr':[40,10,1],
                        'loss':'binary_crossentropy',
                        'optimizer':'rmsprop',
                        'metrics':'accuracy',
                        'epochs':100}
                    
                

                    
#######################
#--      XGBOOST    --#                
#######################

    def setXGBmax_depth(self, max_depth):
        self.xgb_params['max_depth'] = max_depth            
    def setXGBmin_child_weight(self, min_child_weight):
        self.xgb_params['min_child_weight'] = min_child_weight            
    def setXGBlearning_rate(self, learning_rate):
        self.xgb_params['learning_rate'] = learning_rate            
    def setXGBn_estimators(self, n_estimators):
        self.xgb_params['n_estimators'] = n_estimators            
    def setXGBsubsample(self, subsample):
        self.xgb_params['subsample'] = subsample            
    def setXGBcolsample_bytree(self, colsample_bytree):
        self.xgb_params['colsample_bytree'] = colsample_bytree            
      
                
    def setXGBFeatures(self, params):
        self.xgb_params = params

    #-- Train xgb model        
    def xgb_model(self, train_data, train_label):
        self.xgb_mdl = xgb.XGBClassifier(max_depth=self.xgb_params['max_depth'],
                           min_child_weight=self.xgb_params['min_child_weight'],
                           learning_rate=self.xgb_params['learning_rate'],
                           n_estimators=self.xgb_params['n_estimators'] ,
                           silent=True,
                           objective='binary:logistic',
                           gamma=0,
                           max_delta_step=0,
                           subsample=self.xgb_params['subsample'] ,
                           colsample_bytree=self.xgb_params['colsample_bytree'],
                           colsample_bylevel=1,
                           reg_alpha=0,
                           reg_lambda=0,
                           scale_pos_weight=1,
                           seed=1,
                           missing=None)
        self.xgb_mdl.fit(train_data, train_label,  verbose=False)
        
        return(self.xgb_mdl)
        
    #-- Predict probabilities    
    def xgb_predict(self,mdl, x_cv_tst):
        y_pred = mdl.predict_proba(x_cv_tst)[:, 1]
        return(y_pred)

#######################
#--      Light gbm  --#                
#######################

    def setLGBFeatures(self, params):
        self.lgb_params = params

    #-- Train xgb model        
    def lgb_model(self, train_data, train_label):
        dtrain = lgb.Dataset(train_data, train_label)
        self.lgb_mdl = lgb.train(self.lgb_params, dtrain, self.lgb_params['num_boost_round'])
        return(self.lgb_mdl)
        
    #-- Predict probabilities    
    def lgb_predict(self,mdl, x_cv_tst):
        y_pred = mdl.predict(x_cv_tst)
        return(y_pred)

#######################
#--      catboost  --#                
#######################

    def setCATFeatures(self, params):
        self.cat_params = params

    #-- Train xgb model        
    def cat_model(self, train_data, train_label):
        cb = cat.CatBoostClassifier(depth=self.cat_params['depth'], iterations= self.cat_params['iterations'], l2_leaf_reg= self.cat_params['l2_leaf_reg'], learning_rate= self.cat_params['learning_rate'])
        self.cat_mdl = cb.fit(train_data, train_label, verbose = False)
        return(self.cat_mdl)
        
    #-- Predict probabilities    
    def cat_predict(self,mdl, x_cv_tst):
        y_pred = mdl.predict_proba(x_cv_tst)[:, 1]
        return(y_pred)

#######################
#--      RF         --#                
#######################
    def setRFOFeatures(self, params):
        self.rfo_params = params

    #-- Train xgb model        
    def rfo_model(self, train_data, train_label):
        train_data = train_data.fillna(train_data.median())
        self.rfo_mdl = RandomForestClassifier(n_estimators = self.rfo_params['n_estimators'],
                                  max_depth = self.rfo_params['n_estimators'],
                                  criterion = self.rfo_params['criterion'], # 'entropy'
                                   bootstrap = self.rfo_params['bootstrap'],
                                   max_features = self.rfo_params['max_features'], #'sqrt', 'log2', None],
                                   class_weight= self.rfo_params['class_weight'],
                                    verbose=0)
        self.rfo_mdl.fit(train_data, train_label)
        return(self.rfo_mdl)
        
    #-- Predict probabilities    
    def rfo_predict(self,mdl, x_cv_tst):
        x_cv_tst = x_cv_tst.fillna(x_cv_tst.median())
        y_pred = mdl.predict_proba(x_cv_tst)[:, 1]
        return(y_pred)

#######################
#--      GLM        --#                
#######################

                
    def setGLMFeatures(self, params):
        self.glm_params = params

    #-- Train xgb model        
    def glm_model(self, train_data, train_label):
        train_data = train_data.fillna(train_data.median())
        self.glm_mdl = LogisticRegression(max_iter = self.glm_params['max_iter'],
                                  #l1_ratio = self.glm_params['l1_ratio'],
                                  class_weight = self.glm_params['class_weight'], # 'entropy'
                                   #penalty = self.glm_params['penalty']
                                    )
        self.glm_mdl.fit(train_data, train_label)
        return(self.glm_mdl)
                                   
    #-- Predict probabilities    
    def glm_predict(self,mdl, x_cv_tst):
        x_cv_tst = x_cv_tst.fillna(x_cv_tst.median())
        y_pred = mdl.predict_proba(x_cv_tst)[:, 1]
        return(y_pred)

#######################
#--      DL        --#                
#######################
    def setDLFeatures(self, params):
        self.dl_params = params

    #-- Train xgb model        
    def dl_model(self, train_data, train_label):
         train_data = train_data.fillna(train_data.median())
         train_data = train_data.values
         model=Sequential()
         arr = self.dl_params['arr']
         for i in range(len(arr)):
             if i!=0 and i!=len(arr)-1:
                 if i==1:
                     model.add(Dense(arr[i],input_dim=arr[0],kernel_initializer='normal', activation='relu'))
             else:
                    model.add(Dense(arr[i],activation=self.dl_params['activation']))
         model.add(Dense(arr[-1],kernel_initializer='normal',activation="sigmoid"))
         model.compile(loss=self.dl_params['loss'],optimizer=self.dl_params['optimizer']  , metrics=['accuracy'] )
         model.fit(train_data,train_label,epochs=self.dl_params['epochs'], verbose = 0)
         self.dl_mdl = model
         return(self.dl_mdl)
                                   
    #-- Predict probabilities    
    def dl_predict(self,mdl, x_cv_tst):
        x_cv_tst = x_cv_tst.fillna(x_cv_tst.median())
        y_pred = mdl.predict(x_cv_tst.values)
        return(y_pred)
   




#-- Class hold ing the CV data
class CvSettings:

    def __init__(self,N=6,k=2):
        self.N = N
        self.k = k
        
        
        #-- For given data size T, N groups, k test groups get
    # cvmRes TxCV_length with each cell either 0 -> train or i-> test for path i
    # nPaths  scalar with the number of paths for the givien parameters
    # cvm_mask like cvmRes but with 1s instead of i           
    def getCVLabel(self,T,N=6,k=2):
    
        #-- List with indices of test groups
        testLabels = list(itertools.combinations(range(1,N+1),k))
        
        #-- Set the number of cv iterations
        self.cv_length = len(testLabels)
        
        #-- Number of paths
        self.nPaths = int(len(testLabels)*k/N)
        
        #-- Create dataframe with train test data
        cvm = pd.DataFrame(np.zeros((N, len(testLabels))), columns = range(1,1+len(testLabels)), index = range(1,1+N))
        
        #-- Fill test sets with 1s
        j=0
        for i in testLabels:
            cvm.iloc[[ii-1 for ii in list(i)],j]=1
            j=j+1
        
        #-- Save the mask
        cvm_mask = cvm
        cvm_mask = cvm_mask.iloc[np.repeat(np.arange(len(cvm_mask)), T/N)]
        
        #-- Label paths
        j=2
        while (j< (1+cvm.shape[1])):
            toChangeIndices = [ii-1 for ii in list(cvm.loc[  cvm.loc[:,j]>0   ,j].index)]
            cvm.loc[  cvm.loc[:,j]>0   ,j]= cvm.loc[  cvm.loc[:,j]>0   ,j] + cvm.iloc[toChangeIndices,range(j-1)].max(axis=1)
            
            j=j+1
            
        #-- Scale the table to the full number of rows
        cvmRes = cvm.iloc[np.repeat(np.arange(len(cvm)), T/N)]
        
        #-- Rename columns for readability
        cvmRes.columns = ['CV_'+str(i+1) for i in range(len(testLabels))]
        
        
        self.cvmRes = cvmRes
        self.cvm_maks = cvm_mask
        
        return [cvmRes, self.nPaths, cvm_mask]
    
    
    
    #-- Get metrics per path
    # simProb  T x CV_length matrix with probabilities in the cells of paths
    # simTruth T x 1 vector with 1 and -1
    # p        [0,1] scalar for threshold of decision
    # cvmRes   T x CV_length matrix with path id in path cells
    def getPathMetrics(self, simProb, simTruth, p, cvmRes):
    
        
        #-- Debugging
        #simProb = cvProb
        #simTruth = y_int_trn
        #p = 0.5
        #cvmRes = cvFlags
        #nPaths = cvFlags.max().max()
        
        
        #-- Convert truth to 1,-1
        simTruth = (simTruth-0.5)*2

        #-- Set number of paths
        nPaths = self.nPaths
        self.p = p
        
        
        #-- Threshold the probabilities
        simProb_dec = simProb.copy()
        simProb_dec[simProb_dec>p]=1
        simProb_dec[simProb_dec<1]=0
    
        #-- Set cvmRes for indexing in the loop
        cvmRes.columns = simProb_dec.columns
        cvmRes.index= simProb_dec.index
        
        #-- Initialize result    
        sharpe_vec = []
        drawdown_vec = []
        drawup_vec = []
        pl_eq_vec = []
        
        for pth_id in range(int(nPaths)):
            
            pth_id = pth_id + 1
            #-- Select the locations with path id
            pth = simProb_dec[ cvmRes==pth_id ]
            #-- Drop columns which have no predictions for this path
            pth.dropna(thresh=1,axis='columns',inplace=True)
            #-- Fill NAs with 0s
            pth.fillna(0, inplace=True)
            #-- Sum all columns
            pth = pth.sum(axis='columns')
            
            #-- Getting P&L
            pl = pd.DataFrame(pth)*simTruth
            pl = pth*simTruth
            #-- Getting equity over time
            pl_eq = pl.cumsum()
            
            #-- Computing sharpe ratio
            mean_returns = pl.mean()
            variance_returns = pl.var()
            
            sharpe = (mean_returns/(0.00001 + variance_returns))
            
            #sharpe = np.asscalar(mean_returns/variance_returns)
            
            #-- Computing maximum drawdown
            drawdown = np.asscalar((pl_eq-pl_eq.cummax()).min())
            
            #-- Computing maximum drawup
            drawup = np.asscalar((pl_eq-pl_eq.cummin()).max())
            
            #-- Append the result
            sharpe_vec.append(sharpe)
            drawdown_vec.append(drawdown)
            drawup_vec.append(drawup)
            pl_eq_vec.append(pl_eq) 
        
        
        
        self.sharpe_vec = sharpe_vec
        self.drawdown_vec = drawdown_vec
        self.drawup_vec = drawup_vec
        self.pl_eq_vec = pl_eq_vec
        
        return [sharpe_vec,drawdown_vec,drawup_vec,pl_eq_vec]
    



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
    

        
        
#####################################################################################################        
#-- For given data size T, N groups, k test groups get
# cvmRes TxCV_length with each cell either 0 -> train or i-> test for path i
# nPaths  scalar with the number of paths for the givien parameters
# cvm_mask like cvmRes but with 1s instead of i           
def getCVLabel(T,N=6,k=2):

    #-- List with indices of test groups
    testLabels = list(itertools.combinations(range(1,N+1),k))
    
    #-- Number of paths
    nPaths = int(len(testLabels)*k/N)
    
    #-- Create dataframe with train test data
    cvm = pd.DataFrame(np.zeros((N, len(testLabels))), columns = range(1,1+len(testLabels)), index = range(1,1+N))
    
    #-- Fill test sets with 1s
    j=0
    for i in testLabels:
        cvm.iloc[[ii-1 for ii in list(i)],j]=1
        j=j+1
    
    #-- Save the mask
    cvm_mask = cvm
    cvm_mask = cvm_mask.iloc[np.repeat(np.arange(len(cvm_mask)), T/N)]
    
    #-- Label paths
    j=2
    while (j< (1+cvm.shape[1])):
        toChangeIndices = [ii-1 for ii in list(cvm.loc[  cvm.loc[:,j]>0   ,j].index)]
        cvm.loc[  cvm.loc[:,j]>0   ,j]= cvm.loc[  cvm.loc[:,j]>0   ,j] + cvm.iloc[toChangeIndices,range(j-1)].max(axis=1)
        
        j=j+1
        
    #-- Scale the table to the full number of rows
    cvmRes = cvm.iloc[np.repeat(np.arange(len(cvm)), T/N)]
    
    #-- Rename columns for readability
    cvmRes.columns = ['CV_'+str(i+1) for i in range(len(testLabels))]
    
    return [cvmRes, nPaths, cvm_mask]



#-- Get metrics per path
# simProb  T x CV_length matrix with probabilities in the cells of paths
# simTruth T x 1 vector with 1 and -1
# p        [0,1] scalar for threshold of decision
# cvmRes   T x CV_length matrix with path id in path cells
def getPathMetrics(simProb, simTruth, p, cvmRes):

    #-- derive number of paths
    nPaths = int(cvmRes.max().max())
    
    #-- Threshold the probabilities
    simProb_dec = simProb.copy()
    simProb_dec[simProb_dec>p]=1
    simProb_dec[simProb_dec<1]=0

    #-- Set cvmRes for indexing in the loop
    cvmRes.columns = simProb_dec.columns
    cvmRes.index= simProb_dec.index
    
    #-- Initialize result    
    sharpe_vec = []
    drawdown_vec = []
    drawup_vec = []
    pl_eq_vec = []
    
    for pth_id in range(nPaths):
        
        pth_id = pth_id + 1
        #-- Select the locations with path id
        pth = simProb_dec[ cvmRes==pth_id ]
        #-- Drop columns which have no predictions for this path
        pth.dropna(thresh=1,axis='columns',inplace=True)
        #-- Fill NAs with 0s
        pth.fillna(0, inplace=True)
        #-- Sum all columns
        pth = pth.sum(axis='columns')
        
        #-- Getting P&L
        pl = pd.DataFrame(pth)*simTruth
        #-- Getting equity over time
        pl_eq = pl.cumsum()
        
        #-- Computing sharpe ratio
        mean_returns = pl.mean()
        variance_returns = pl.var()
        sharpe = np.asscalar(mean_returns/variance_returns)
        
        #-- Computing maximum drawdown
        drawdown = np.asscalar((pl_eq-pl_eq.cummax()).min())
        
        #-- Computing maximum drawup
        drawup = np.asscalar((pl_eq-pl_eq.cummin()).max())
        
        #-- Append the result
        sharpe_vec.append(sharpe)
        drawdown_vec.append(drawdown)
        drawup_vec.append(drawup)
        pl_eq_vec.append(pl_eq) 
    
    
    return [sharpe_vec,drawdown_vec,drawup_vec,pl_eq_vec]
