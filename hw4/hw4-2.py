# -*- coding: utf-8 -*-
"""
Created on Fri Nov 23 11:02:15 2018

@author: User
"""


#url = 'https://github.com/empireisme/master/blob/master/hw4/training_label.txt'
#df = pd.read_csv(url,index_col=0,parse_dates=[0])


import pandas as pd
import numpy as np

from keras.datasets import imdb
from keras.preprocessing import sequence
from keras.preprocessing.text import Tokenizer



f=open('C:\\Users\\User\\Documents\\GitHub\\master\\hw4\\training_label.txt','r',encoding = 'utf8')

#注意這邊要用 r 還有編碼是 utf8
lines=f.readlines()

type(lines)


for i in range(0,len(lines)):
    lines[i]=lines[i].split('+++$+++')
    
df=pd.DataFrame(lines) #這會自動讓lines變成data frame

train_txt=df.iloc[:,1]

token = Tokenizer(num_words=3800) 

#使用Tokenizer模組建立token，建立一個3800字的字典
token.fit_on_texts(train_txt)  
#讀取所有訓練資料影評，依照每個英文字在訓練資料出現的次數進行排序，
#前3800名的英文單字會加進字典中
A=token.word_index
#可以看到它將英文字轉為數字的結果，例如:the轉換成1

x_train_seq = token.texts_to_sequences(train_text)
x_test_seq = token.texts_to_sequences(test_text)
#透過texts_to_sequences可以將訓練和測試集資料中的影評文字轉換為數字list



lines[0]

a=lines[0].split('+++$+++')

for i in range(200000):
    =lines[i].split('+++$+++')
    

big=lines.split('+++$+++')

a=[]
for line in f:
    a.append(line)

for 


for i in rane   
    
for i in lines:
    a.append()
A=[]