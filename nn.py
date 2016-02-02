# -*- coding: utf-8 -*-
"""
Created on Fri Jan 29 21:27:23 2016

__author__: Bishwarup

public LB: 84.3% - Not used in the final submission
"""


import os
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler

import theano
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from keras.layers.normalization import BatchNormalization
from keras.layers.advanced_activations import PReLU
from keras.utils import np_utils
from keras.layers.advanced_activations import PReLU
from keras.optimizers import SGD, Adadelta, Adagrad
from keras.optimizers import Adagrad,SGD,Adadelta
from keras.callbacks import Callback

need_validation = False
nb_epoch = 25
batch_size = 14

os.chdir("F:/AV/Last man standing")
np.random.seed(112)

train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")

train["train_flag"] = 1
test["train_flag"] = 0
test["Crop_Damage"] = -1

alldata = train.append(test, ignore_index = True)
alldata["weeks_since_pesticide"] = alldata["Number_Weeks_Used"] + alldata["Number_Weeks_Quit"]
alldata["insect_cnt_per_does"] = alldata["Estimated_Insects_Count"] / alldata["Number_Doses_Week"]
alldata["weeks_used_by_weeks_quit"] = alldata["Number_Weeks_Used"] / alldata["Number_Weeks_Quit"]
alldata["total_dosage"] = alldata["Number_Weeks_Used"] * alldata["Number_Doses_Week"]

alldata.fillna(-1, inplace = True)
alldata["insect_cnt_per_does"][alldata["insect_cnt_per_does"] == np.inf] = -1
alldata["weeks_used_by_weeks_quit"][alldata["weeks_used_by_weeks_quit"] == np.inf] = -1

alldata["Season"] = alldata["Season"].astype(str)
alldata["Pesticide_Use_Category"] = alldata["Pesticide_Use_Category"].astype(str)

dummies = pd.get_dummies(alldata[["Season", "Pesticide_Use_Category"]])
dummies[dummies == 0] = -1
alldata.drop(["Season", "Pesticide_Use_Category"], axis = 1, inplace = True)


alldata["Estimated_Insects_Count"] = np.log(alldata["Estimated_Insects_Count"])

normalize_cols = [x for x in alldata.columns if x not in["ID", "Crop_Damage", "train_flag", "Crop_Type", "Soil_Type"]]

normal_df = alldata[normalize_cols].copy()
sc = StandardScaler()
normal_df = pd.DataFrame(sc.fit_transform(normal_df))
normal_df.columns = normalize_cols

alldata.drop(normalize_cols, axis = 1, inplace = True)

alldata = pd.concat([alldata, dummies], axis = 1)
alldata = pd.concat([alldata, normal_df], axis = 1)

ptr = alldata[alldata["train_flag"] == 1]
pte = alldata[alldata["train_flag"] == 0]

def build_model():

    input_dim = ptr.shape[1] - 3
    classes = 3    
    
    model = Sequential()
    model.add(Dense(256, input_dim= input_dim)) 
    model.add(Dropout(0.1))
    model.add(PReLU())
#    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(256))
    model.add(PReLU())
#    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(56))
    model.add(PReLU())
#    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(classes))
    model.add(Activation("softmax"))
    
    model.compile(loss='categorical_crossentropy', optimizer="adam")
    return model    

def build_model2():
    input_dim = ptr.shape[1] - 3
    classes = 3    
    
    model = Sequential()
    model.add(Dense(6, input_dim= input_dim)) 
    model.add(Dropout(0.1))
    model.add(Activation("tanh"))
    model.add(Dropout(0.5))
    
    model.add(Dense(6))
    model.add(Activation("tanh"))
    
    model.add(Dense(classes))
    model.add(Activation("softmax"))
    
    model.compile(loss = "binary_crossentropy", optimizer = "Adagrad")
    return model

def fit_model():
    
    feature_names = [f for f in ptr.columns if f not in ["ID", "Crop_Damage", "train_flag"]]
    if need_validation:
        
        fold_ids = pd.read_csv("validation_ID.csv")
        
        nn_val = pd.DataFrame(columns = ["QuoteNumber", "nn_cl_0", "nn_cl_1", "nn_cl_2"])
        
        for i in xrange(5):
            
            print "\n--------------------------------------------"
            print "---------------- Fold %d --------------------" %i
            print "--------------------------------------------"            
            
            model = build_model()
            val_ids = fold_ids.ix[:, i].dropna()
            idx = ptr["ID"].isin(list(val_ids))
            
            trainingSet = ptr[~idx]
            validationSet = ptr[idx]        
            
            tr_X = np.matrix(trainingSet[feature_names])
            tr_Y = np_utils.to_categorical(np.array(trainingSet["Crop_Damage"]))
            val_X = np.matrix(validationSet[feature_names])
            val_Y = np_utils.to_categorical(np.array(validationSet["Crop_Damage"]))
            model.fit(tr_X, tr_Y, validation_data=(val_X, val_Y), nb_epoch = nb_epoch, batch_size= batch_size, show_accuracy=True)
            
            preds = model.predict_proba(val_X, batch_size=128)
            df = pd.DataFrame({"ID" : validationSet["ID"], "nn_cl_0" : preds[:,0], "nn_cl_1" : preds[:, 1], "nn_cl_2" : preds[:, 2]})
            nn_val = nn_val.append(df, ignore_index = True)
        
        return nn_val
    
    else:
        
        model = build_model()
        tr_X = np.matrix(ptr[feature_names])
        tr_Y = np_utils.to_categorical(np.array(train["Crop_Damage"]))
        test_X = np.matrix(pte[feature_names])
        model.fit(tr_X, tr_Y, nb_epoch = nb_epoch, batch_size = batch_size)
        preds = model.predict_proba(test_X, batch_size = 128)
        test_df = pd.DataFrame({"ID" : pte["ID"], "nn_cl_0" : preds[:, 0], "nn_cl_1" : preds[:, 1], "nn_cl_2" : preds[:, 2]})
        return test_df
        
##########################################
model = build_model()
#nn_val= fit_model()
#nn_val.to_csv("nn1_eval.csv", index = False)
nn_test = fit_model()
nn_test.to_csv("nn1_test.csv", index = False)
