# -*- coding: utf-8 -*-
"""
Created on Sun Nov 15 22:06:33 2020

@author: Jorge Caiado
"""

# Airline passengers dataset prvided by Kaggle
# https://www.kaggle.com/chirag19/air-passengers


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Read and plot data 
url = 'C:/Users/Jorge Caiado/Dropbox/Jorge Caiado/ISEG/Time Series & Forecasting/Python/AirPassengers.csv'

df = pd.read_csv(url,index_col='Month',parse_dates=True)
df.index.freq = 'MS'
print(df)
df['#Passengers'].plot(legend=True,label='US Passengers')

# Train and Test data Splitting
train = df.iloc[:108]
test = df.iloc[108:] #last 36

# Time series decomposition
from statsmodels.tsa.seasonal import seasonal_decompose
seasonal_decompose(train['#Passengers']).plot()

# Single method
from statsmodels.tsa.holtwinters import SimpleExpSmoothing
single = SimpleExpSmoothing(train['#Passengers']).fit(
                            smoothing_level=0.1, optimized=False)
single_predictions = single.forecast(36).rename('Holt Forecast')
print(single_predictions)

train['#Passengers'].plot(legend=True,label='Train')
test['#Passengers'].plot(legend=True,label='Test',figsize=(12,8))
single_predictions.plot(legend=True,label='Forecast')

# Compute forecast accuarcy measures
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_percentage_error 
from sklearn.metrics import mean_absolute_error

# Forecast Accuracy Measures
rmse = round(np.sqrt(mean_squared_error(test,single_predictions)),2)
print("Root Mean Squared Error (RMSE) =",rmse)

mae = round(mean_absolute_error(test,single_predictions),2)
print("Mean Absolute Error (MAE) =",mae)

mape = round(100*mean_absolute_percentage_error(test,single_predictions),2)
print("Mean Absolute Percentual Error (MAPE) =",mape,'%')

maen = round(mean_absolute_error(test[1:],test[:-1]),2)
print("Mean Absolute Error for Naive Forecast (MAEN) =",maen)

# Forecast the next 36 observations
singlef = SimpleExpSmoothing(df['#Passengers']).fit(
                            smoothing_level=0.1, optimized=False)
forecasts = singlef.forecast(36).rename('Forecast')
print(forecasts)
df['#Passengers'].plot(legend=True,label='Passengers',figsize=(12,8))
forecasts.plot(legend=True,label='Single Smoothing Forecast')