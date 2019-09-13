# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 19:36:11 2019

@author: Mohamed Ibrahim
"""




import itertools
import numpy as np
import pandas as pd


#-- Number of rows
T = 60
#-- Number of groups
N = 6
#-- Number of test sets within each group
k = 2


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


T=60
N=6
k=2

testLabels = list(itertools.combinations(range(1,N+1),k))
cv_len = len(testLabels )




x = getCVLabel(T)

#-- Get the mask
msk = x[2]
msk = msk.reset_index().drop('index',1)   
msk.index = range(T)
msk.columns = range(cv_len )

#-- Simulated model probabilites
simProb = pd.DataFrame(np.random.rand(T,len(testLabels)))*msk

#-- Simulated truth
simTruth = (round(pd.DataFrame(  (np.random.rand(T,1))  ))*2 -1)

#-- Threshold
p = 0.8

#-- Thresholding model probability




#-- Create paths
nPaths = x[1]
cvmRes = x[0]


#-- Probability of the model 
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
    
    
res = getPathMetrics(simProb, simTruth, p, cvmRes)