#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 27 15:55:09 2019

@author: gabemersy
"""

import pandas as pd

df = pd.read_csv('prices_march_0.csv')
label = df['closePrice']

def sellHold(arr, sInd, eInd):
    mx = arr[sInd]
    for i in range(sInd, eInd):
        if arr[i+1] > mx:
            mx = arr[i+1]
    
    for i in range(sInd, eInd+1):
        if arr[i] == mx:
            arr[i] = 1
        else:
            arr[i] = 0
    return arr[sInd:]

binArr = sellHold(label, 453, 496)

df['sellHold'] = binArr

df.to_csv('sell_hold_march_0.csv')