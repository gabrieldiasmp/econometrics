# -*- coding: utf-8 -*-
"""
Created on Mon Feb  7 09:27:15 2022

@author: Jorge Caiado
"""


import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import rcParams

rcParams['figure.figsize'] = 12,8
rcParams['lines.linewidth'] = 2.5


# Dataset
url = 'https://raw.githubusercontent.com/jbrownlee/Datasets/master/airline-passengers.csv'
df = pd.read_csv(url, index_col='Month', parse_dates=True)

# Visualize
plt.title('Airline Passengers', size=20)
plt.plot(df)

# Time series decomposition
from statsmodels.tsa.seasonal import seasonal_decompose
seasonal_decompose(df['Passengers'],model="multiplicative").plot()

