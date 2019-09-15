# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 23:07:01 2019

@author: Mohamed Ibrahim

Purpose:
    Compute all features from minute data
    
Input: 
    Minute data: dt_all_min.csv in input folder

Output:
    data_output_dir + pair+'_tsfresh_feats_STARTHOUR_'+str(START_HOUR)+'.csv'

"""

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

#-- Countries to get news from
relevant_countries = ['United States','United Kingdom','Japan','Euro Zone','Australia','Canada','Germany','France','New Zealand','China','Switzerland','Russia']
#relevant_countries = ['United States','United Kingdom','Japan','Euro Zone','Australia','Canada','Germany','New Zealand','Switzerland']
#-- Start trade hour
START_HOUR = 9

####################################
#--       IMPORT LIBRARIES       
####################################

import numpy as np
import pandas as pd
import os


pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 20)
pd.set_option('display.width', 1000)

#-- Change to project home directory
os.chdir(path)

####################################
#--       DEFINE FUNCTIONS       
####################################

exec(open("01_code/prod/P1_functions.py").read())

####################################
#--       LOAD DATA       
####################################

fls_inp = os.listdir(data_input_dir)

if 'combined_eco_news.csv' in fls_inp:
    df = pd.read_csv(data_input_dir+'combined_eco_news.csv')
else:
#-- Get the csvs
    fls = os.listdir(data_input_dir+'econews')
    fls_rel = [fl for fl in fls if "xls" in fl]
    #-- Append to one table
    i=1
    for fl in fls_rel:
        #f = pd.read_csv(data_input_dir+'econews/'+fl, delimiter=',')
        f = pd.read_excel(data_input_dir+'econews/'+fl, header=None)
        if i==1:
            df = f.copy()
        else:
            df = df.append(f)
        i=i+1
    df.columns = ['date','time','country','impact','news_label','actual_vs_real','units','actual','forecast','previous']
    df.to_csv(data_input_dir+'combined_eco_news.csv')



####################################
#--       MAIN ENTRY       
####################################

#-- Filter to only relevant countries
srch = '|'.join(relevant_countries)
srch = '('+srch+')'
dt = df.loc[df.country.str.contains(srch), :]


#-- Shift time from GMT-4:00 to GMT+2:00
dt.loc[:,'time_full'] = dt.loc[:,'date']+' '+dt.loc[:,'time']
dt.loc[:,'time_full'] =pd.to_datetime(dt.loc[:,'time_full']) + pd.DateOffset(hours=6)


#-- Get before and after trade time
dt.loc[:,'time_data_vailable'] =pd.to_datetime(dt.loc[:,'time_full']) + pd.DateOffset(hours=24-START_HOUR)
dt.loc[:,'date_data_available'] = dt.loc[:,'time_data_vailable'].dt.date
dt.loc[:,'date_data_upcoming'] = dt.loc[:,'date_data_available'] + pd.DateOffset(days=1)


#-- preprocess strings
dt.loc[:,'dumm_country'] = dt.loc[:,'country'].str.strip().str.lower().str.replace("\s+","_")
dt.loc[:,'dumm_impact'] = dt.loc[:,'impact'].str.strip().str.lower().str.replace("\s+","_").str.replace("_volatility_expected","")
dt.loc[:,'dumm_actual_vs_real'] = dt.loc[:,'actual_vs_real'].str.strip().str.lower().str.replace("\s+","_").str.replace("(_than_expected|_with_expectation)","")
dt.loc[:,'dumm_news_label'] = dt.loc[:,'news_label'].str.strip().str.lower().str.replace("(\s+|\.|\-|\/|\;)","_")

#-- Limiting news label
nws_lbl = dt.loc[:,'dumm_news_label'].value_counts()
nws_lbl = nws_lbl[0:5]
dt.loc[ ~dt.dumm_news_label.isin(nws_lbl.index) ,'dumm_news_label'] = 'other_nws'

#-- Clean previous
dt.loc[ dt.previous.str.contains("([a-zA-Z]|\')") & dt.previous.notnull(),'previous'] = np.NaN
dt.loc[ :,'previous'] = df.previous.apply(pd.to_numeric, args=('coerce',))
dt.loc[ :,'actual'] = df.actual.apply(pd.to_numeric, args=('coerce',))

#-- Fill empty actual vs real
dt.loc[ dt.actual_vs_real.isnull() & dt.actual.notnull() & dt.previous.notnull() ,'actual_vs_real'] = dt.loc[ dt.actual_vs_real.isnull() & dt.actual.notnull() & dt.previous.notnull(),'actual'] > dt.loc[ dt.actual_vs_real.isnull() & dt.actual.notnull() & dt.previous.notnull(),'previous']


#-- concat news results
dt.loc[:,'dumm_news_cnt'] =dt.loc[:,'dumm_country']+'_'+dt.loc[:,'dumm_impact']
dt.loc[:,'dumm_news_res'] =dt.loc[:,'dumm_country']+'_'+dt.loc[:,'dumm_impact']+'_'+dt.loc[:,'dumm_actual_vs_real'].astype(str)

#-- Dummify columns
#dt_tst = pd.get_dummies(dt, columns=['country','impact','actual_vs_real','news_label'])
dt_dumm = pd.get_dummies(dt, columns=['dumm_news_cnt','dumm_news_res','dumm_country'])



####################################
#--       AGGREGATION      
####################################

result_columns = pd.DataFrame(dt_dumm.columns[dt_dumm.columns.str.contains("res")].values, columns = ['id'])
result_columns['op']='sum'
result_columns = result_columns.set_index('id').to_dict()['op']

cnt_columns = pd.DataFrame(dt_dumm.columns[dt_dumm.columns.str.contains("cnt")].values, columns = ['id'])
cnt_columns['op']='sum'
cnt_columns = cnt_columns.set_index('id').to_dict()['op']

#-- Aggregate results available from previous day
dt_agg_res = dt_dumm.groupby('date_data_available').agg(result_columns).reset_index()
dt_agg_res = dt_agg_res.rename(columns = {'date_data_available':'date_join'})

#-- Aggregate count news available from previous day
dt_agg_cnt = dt_dumm.groupby('date_data_available').agg(cnt_columns).reset_index()
dt_agg_cnt = dt_agg_cnt.rename(columns = {'date_data_available':'date_join'})

#-- Aggregate count news available from previous day
dt_agg_planned = dt_agg_cnt.copy()
dt_agg_planned['date_data_available'] = dt_agg_planned['date_join']+ pd.DateOffset(days=-1)
#-- Rename columns to avoid conflict with actual data
dt_agg_planned.columns = dt_agg_planned.columns+'_planned'
dt_agg_planned = dt_agg_planned.rename(columns = {'date_join_planned':'date_join'})


####################################
#--       JOIN ALL      
####################################


res = pd.merge(pd.merge(dt_agg_res,dt_agg_cnt, how = 'outer'), dt_agg_planned, how = 'outer')


#-- Append hour to the date for joining
if(START_HOUR<10):
    res.loc[:,'Time'] = res.loc[:,'date_join'].map(str)+' 0'+str(START_HOUR)+':00:00'
else:
    res.loc[:,'Time'] = res.loc[:,'date_join'].map(str)+' '+str(START_HOUR)+':00:00'

####################################
#--       DUMP      
####################################

res.to_csv(data_intermediate_dir+'economic_news_STARTHOUR_'+str(START_HOUR)+'.csv')


