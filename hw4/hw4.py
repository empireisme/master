# -*- coding: utf-8 -*-
"""
Created on Fri Nov 23 11:02:15 2018

@author: User
"""

import pandas as pd
url = 'https://raw.githubusercontent.com/empireisme/datamining/master/data.csv'
df = pd.read_csv(url,index_col=0,parse_dates=[0])