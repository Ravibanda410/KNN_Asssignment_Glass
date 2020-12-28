# -*- coding: utf-8 -*-
"""
Created on Sun May 24 01:31:22 2020

@author: RAVI
"""
# Importing Libraries 
import pandas as pd
import numpy as np

glass = pd.read_csv("C:/RAVI/Data science/Assignments/Module 18 KNN/KNN Assignment1 dataset/glass.csv/glass.csv")
glass.head()


#normalisation function
def norm_func(i):
    x=(i-i.min())/(i.max()-i.min())
    return(x)

#Normalized data frame(considering numerical part of data if have)
glass_n= norm_func(glass.iloc[ :, :9]) 
glass_n.describe()

X=np.array(glass_n.iloc[ :, : ])  #predictors
Y=np.array(glass['Type'])       #Target


# Training and Test data using 
from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test= train_test_split(X,Y,test_size = 0.2) # 0.2 => 20 percent of entire data

from sklearn.neighbors import KNeighborsClassifier

KNN = KNeighborsClassifier(n_neighbors = 4)
KNN.fit(X_train, Y_train)

pred = KNN.predict(X_test)
pred

#Evaluate the model
from sklearn.metrics import accuracy_score
pd.crosstab(Y_test,pred,rownames=['Actual'],colnames=['Predictions'] )
test_acc=print(accuracy_score(Y_test,pred))

#error on train data
pred_train=KNN.predict(X_train)
pd.crosstab(Y_train,pred_train,rownames=['Actual'],colnames=['Predictions'] )
train_acc=print(accuracy_score(Y_train,pred_train))


























