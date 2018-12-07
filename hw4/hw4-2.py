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

ftest=open('C:\\Users\\User\\Documents\\GitHub\\master\\hw4\\testing_label.txt','r',encoding = 'utf8' )
#注意這邊要用 r 還有編碼是 utf8
lines=f.readlines()
lines_test=ftest.readlines()

#readlines 用rstring

type(lines)

type(lines_test)


for i in range(0,len(lines)):
    lines[i]=lines[i].split('+++$+++')


for j in range(0,len(lines_test)):
    lines_test[j]=lines_test[j].rstrip().split('#####')

s=[ i for i in  lines_test if i[0] !=''  ]
s
df=pd.DataFrame(lines) #這會自動讓lines變成data frame
dftest=pd.DataFrame(s)
'''
dftest.iloc[95,:]

dftest.drop('None')
'''
train_txt=df.iloc[:,1]
y_train=df.iloc[:,0]
test_txt=dftest.iloc[:,1]
y_test=dftest.iloc[:,0]


token = Tokenizer(num_words=3800) 

#使用Tokenizer模組建立token，建立一個3800字的字典
token.fit_on_texts(train_txt)  
#讀取所有訓練資料影評，依照每個英文字在訓練資料出現的次數進行排序，
#前3800名的英文單字會加進字典中
A=token.word_index
#可以看到它將英文字轉為數字的結果，例如:the轉換成1

x_train_seq = token.texts_to_sequences(train_txt)
x_test_seq = token.texts_to_sequences(test_txt)
#透過texts_to_sequences可以將訓練和測試集資料中的影評文字轉換為數字list

x_train = sequence.pad_sequences(x_train_seq, maxlen=380)
x_test = sequence.pad_sequences(x_test_seq, maxlen=380)

from keras.models import Sequential
from keras.layers.core import Dense,Dropout,Activation,Flatten
from keras.layers.embeddings import Embedding
from keras.layers.recurrent import SimpleRNN

modelRNN = Sequential()  #建立模型
#Embedding層將「數字list」轉換成「向量list」
modelRNN.add(Embedding(output_dim=32,   #輸出的維度是32，希望將數字list轉換為32維度的向量
     input_dim=3800,  #輸入的維度是3800，也就是我們之前建立的字典是3800字
     input_length=380)) #數字list截長補短後都是380個數字


modelRNN.add(SimpleRNN(units=16))
 #建立16個神經元的RNN層

#建立隱藏層
modelRNN.add(Dense(units=256,activation='relu')) 
#建立256個神經元的隱藏層
#ReLU激活函數
modelRNN.add(Dropout(0.35))

#建立輸出層
modelRNN.add(Dense(units=1,activation='sigmoid'))
#建立一個神經元的輸出層
#Sigmoid激活函數

modelRNN.summary()

#定義訓練模型
modelRNN.compile(loss='binary_crossentropy',
     optimizer='adam',
     metrics=['accuracy']) 

train_history1 = modelRNN.fit(x_train,y_train, 
         epochs=2, 
         batch_size=100,
         verbose=2,
         validation_split=0.2)

scores = modelRNN.evaluate(x_test, y_test,verbose=1)
scores[1]



### LSTM

from keras.models import Sequential
from keras.layers.core import Dense,Dropout,Activation,Flatten
from keras.layers.embeddings import Embedding
from keras.layers.recurrent import LSTM
from keras.callbacks import History 
history = History()
modelLSTM = Sequential() 
modelLSTM.add(Embedding (output_dim=32,input_dim=3800, input_length=380))     

modelLSTM.add(Dropout(0.2)) #隨機在神經網路中放棄20%的神經元，避免overfitting

modelLSTM.add(LSTM(32)) 
#建立32個神經元的LSTM層

#建立隱藏層
modelLSTM .add(Dense(units=256,activation='relu')) 
#建立256個神經元的隱藏層
modelLSTM .add(Dropout(0.2))

#建立輸出層
modelLSTM .add(Dense(units=1,activation='sigmoid'))
 #建立一個神經元的輸出層

modelLSTM .summary()


modelLSTM.compile(loss='binary_crossentropy',
     optimizer='adam',
     metrics=['accuracy']) 

train_history2 = modelLSTM.fit(x_train,y_train, 
         epochs=2, 
         batch_size=100,
         verbose=2,
         validation_split=0.2)

type(train_history.history)

print(train_history.history.keys())

train_history.history

scores = modelLSTM .evaluate(x_test, y_test,verbose=1)
scores[1]

"""
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
"""