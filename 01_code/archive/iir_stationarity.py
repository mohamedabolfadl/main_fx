#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns
from scipy import stats
from scipy import signal
from statsmodels.tsa.stattools import adfuller
from  matplotlib import patches
from matplotlib.figure import Figure
from matplotlib import rcParams


# In[2]:


####################################
#--       INPUT VARIABLES       
####################################
data_input_dir = "02_data/input/"
data_intermediate_dir = "02_data/intermediate/"
data_output_dir = "02_data/output/"
models_prod_dir = "03_models/prod/"
models_archive_dir = "03_models/archive/"
#-- Change to project directory
path = 'C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx'
import os
#-- Change to project home directory
os.chdir(path)
exec(open("01_code/prod/P1_functions.py").read())


# In[3]:


def getWeights(d,size):
    # thres>0 drops insignificant weights
    w=[1.]
    for k in range(1,size):
        w_=-w[-1]/k*(d-k+1)
        w.append(w_)
    w=np.array(w[::-1]).reshape(-1,1)
    return w


# In[33]:


def zplane(b,a,filename=None):
    """Plot the complex z-plane given a transfer function.
    """

    # get a figure/plot
    ax = plt.subplot(111)

    # create the unit circle
    uc = patches.Circle((0,0), radius=1, fill=False,
                        color='black', ls='dashed')
    ax.add_patch(uc)

    # The coefficients are less than 1, normalize the coeficients
    if np.max(b) > 1:
        kn = np.max(b)
        b = b/float(kn)
    else:
        kn = 1

    if np.max(a) > 1:
        kd = np.max(a)
        a = a/float(kd)
    else:
        kd = 1
        
    # Get the poles and zeros
    p = np.roots(a)
    z = np.roots(b)
    k = kn/float(kd)
    
    # Plot the zeros and set marker properties    
    t1 = plt.plot(z.real, z.imag, 'go', ms=10)
    plt.setp( t1, markersize=10.0, markeredgewidth=1.0,
              markeredgecolor='k', markerfacecolor='g')

    # Plot the poles and set marker properties
    t2 = plt.plot(p.real, p.imag, 'rx', ms=10)
    plt.setp( t2, markersize=12.0, markeredgewidth=3.0,
              markeredgecolor='r', markerfacecolor='r')

    ax.spines['left'].set_position('center')
    ax.spines['bottom'].set_position('center')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

    # set the ticks
    r = 3; plt.axis('scaled'); plt.axis([-r, r, -r, r])
    ticks = [-1, -.5, .5, 1]; plt.xticks(ticks); plt.yticks(ticks)

    if filename is None:
        plt.show()
    else:
        plt.savefig(filename)
    

    return z, p, k


# In[30]:


def plotTFs(f1,f2):
    w, h1 = signal.freqz(f1)
    w, h2 = signal.freqz(f2)
    axes = plt.gca()
#axes.set_xlim([xmin,xmax])
    axes.set_ylim([-10,5])
    plt.plot(20*np.log10(abs(h1)),'r--',20*np.log10(abs(h2)),'b--')


# In[70]:


def plotIIR(b,a):
    w, h1 = signal.freqz(b, a)
    axes = plt.gca()
#axes.set_xlim([xmin,xmax])
    axes.set_ylim([-10,5])
    plt.plot(20*np.log10(abs(h1)),'r--')


# In[31]:


def getADF(x):
    return adfuller(x)[0]


# In[32]:


def autocorr(x):
    result = np.correlate(x, x, mode='full')
    return result[result.size // 2:]


# In[47]:


def acf(x, length=20):
    return np.array([1]+[np.corrcoef(x[:-i], x[i:])[0,1]          for i in range(1, length)])


# In[4]:


df = pd.read_csv(data_input_dir+"dt_all_min.csv")


# In[5]:


dd = df[['Time','Close_EURUSD','Close_GBPUSD','Close_USDJPY','Close_AUDUSD']]


# In[6]:


dd['Time'] = pd.to_datetime(dd['Time'])


# In[7]:


dd.set_index('Time', inplace=True)


# In[8]:


dd = dd.resample('B').agg({'Close_GBPUSD': 'last','Close_USDJPY': 'last', 
                                         'Close_AUDUSD': 'last', 
                                         'Close_EURUSD': 'last'})


# In[9]:


dd.head()


# In[57]:


z = signal.lfilter( b=[1,-1], a=1, x=dd['Close_USDJPY'], axis=0)


# In[58]:


x = acf(z[1:])


# In[56]:


plt.plot(20*np.log10(abs(x[0:4])))


# In[66]:


plotTFs(signal.firwin(9, 0.1, pass_zero=False),signal.firwin(9, 0.4, pass_zero=False) )


# In[77]:


b, a = signal.butter(4, [0.00001,0.1], 'bandstop')


# In[78]:


plotIIR(b,a)


# In[251]:


#w_v = [0.0001,0.005,0.01,0.025,0.05,0.1]
#w_v = [0.0001,0.1,0.15,0.2,0.25,0.3,0.35,0.4]
w_v = [0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85]



# In[348]:


cr = 'Close_EURUSD'
N_1 = 11
N_2 = 19
crr_1=[]
ad_1 = []
crr_2=[]
ad_2 = []
crr_f=[]
ad_f = []
N_fracdiff = 10
crr_d =[]
ad_d = []

for w in w_v:
    #-- FIR low order
    b = signal.firwin(N_1, w, pass_zero=False)
    #-- Filter the signal
    z = signal.lfilter( b=b, a=1, x=dd[cr], axis=0)[(N_1+10):]
    #-- Get corr
    crr_1.append(np.mean(abs(acf(z)[1:5])))
    #-- Get adf
    ad_1.append(getADF(z))
    
    #-- IIR high order
    b, a = signal.iirfilter(N_2,w, rs=1, btype = 'highpass', ftype = 'cheby2', analog =False)
    #b, a = signal.butter(N_2,[w,w+0.01], 'bandpass')
    #-- Filter the signal
    z = signal.lfilter( b=b, a=a, x=dd[cr], axis=0)[(N_2+10):]
    z = z[~np.isnan(z)]
    #-- Get corr
    crr_2.append( np.mean(abs(acf(z)[1:5]))  )
    #-- Get adf
    ad_2.append(getADF(z))
    
    #-- Fracdiff
    b = np.reshape(getWeights(w,N_fracdiff),N_fracdiff)
    #-- Filter the signal
    z = signal.lfilter( b=b, a=1, x=dd[cr], axis=0)[(N_fracdiff+10):]
    #-- Get corr
    crr_f.append( np.mean(abs(acf(z)[1:5]))  )
    #-- Get adf
    ad_f.append(getADF(z))
    
    #-- First order diff
    b = [1,-1]
    #-- Filter the signal
    z = signal.lfilter( b=b, a=1, x=dd[cr], axis=0)[(N_fracdiff+10):]
    #-- Get corr
    crr_d.append( np.mean(abs(acf(z)[1:5]))  )
    #-- Get adf
    ad_d.append(getADF(z))
    
    
    
    


# In[349]:


plt.plot(w_v,crr_1,'r',w_v,crr_2,'b', w_v, crr_f,'g',w_v, crr_d,'y' )


# In[350]:


#plt.plot(w_v,ad_1,'r',w_v,ad_2,'b', w_v, ad_f,'g',w_v, ad_d,'y' )
plt.plot(w_v,ad_1,'r',w_v,ad_2,'b', w_v, ad_f,'g' )


# In[357]:


b, a = signal.iirfilter(3,0.2, rs=1, btype = 'highpass', ftype = 'cheby2', analog =False)
#b, a = signal.butter(N_2,[w,w+0.01], 'bandpass')
#-- Filter the signal
z = signal.lfilter( b=b, a=a, x=dd[cr], axis=0)[(N_2+10):]
z = z[~np.isnan(z)]


# In[358]:


print(z)


# In[359]:


print(acf(z)[0:15])
print(acf(dd[cr])[0:15])


# In[360]:


plt.plot(z[50:])
plt.show()
plt.plot(dd[cr],'r')
plt.show()


# In[361]:


adfuller(z)


# In[228]:


acf(z)


# In[ ]:




