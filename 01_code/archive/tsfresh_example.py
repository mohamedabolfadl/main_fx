# -*- coding: utf-8 -*-
"""
Created on Sat Mar  2 13:43:02 2019

@author: Mohamed Ibrahim
"""

from tsfresh.examples.robot_execution_failures import download_robot_execution_failures, \
    load_robot_execution_failures
download_robot_execution_failures()
timeseries, y = load_robot_execution_failures()



print(timeseries.head())
print(y.head())












