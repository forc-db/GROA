import subprocess
import argparse
import learn2map.raster_tools as rt
from azureml.core import Run
import gdal
import argparse
import pandas as pd
import numpy as np
import glob
import rasterio
import gdal
import os
import datetime
import csv
import random

from sklearn.model_selection import cross_val_score
from sklearn import metrics
from sklearn import svm
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.pipeline import FeatureUnion
from sklearn.preprocessing import StandardScaler, OneHotEncoder, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import mean_squared_error
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.decomposition import PCA
from sklearn.feature_selection import SelectFromModel
from sklearn.svm import LinearSVR
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import validation_curve
from sklearn.neural_network import MLPClassifier
from sklearn.utils import resample
from sklearn.metrics import confusion_matrix
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.metrics import accuracy_score
from sklearn.metrics import roc_auc_score

import xgboost as xgb
import pickle
import math
import datetime
import warnings
from shutil import copyfile
import csv
import subprocess
import argparse
import os
import numpy as np
import learn2map.raster_tools as rt
import pandas as pd
import glob
import rasterio
from osgeo import gdal
from azureml.core import Run
from sklearn.model_selection import train_test_split
import multiprocessing as mp
from sklearn.utils import resample
from multiprocessing import Pool #  Process pool
from multiprocessing import sharedctypes
import tqdm
import sys
import math
import shutil

class ForestLearn(object):
    """
    Build machine learning object that can find the best parameters for final run.

    """
    def __init__(self, predictors=[], y_column=None, xy = ['x','y'], cat_feats = [], one_hot_feats = []):
        """
        :param in_file: input h5 file that contains the training data
        :param y_column: column index for response variable y
        :param mask_column: column index for mask (if exist, and should be after popping y_column)
        :param: best_params: param set that can be used in further learning
        """
        
        self.y_column = y_column
        
        if self.y_column is None:
            sys.exit('"y_column" must be defined in training process...')
        
        numeric_features = [x for x in predictors if x not in cat_feats]
            
        self.predictors = numeric_features + cat_feats
        self.numeric_features = numeric_features
        self.categorical_features = cat_feats
        self.one_hot_features = one_hot_feats
        self.mdl = None
        self.xy = xy
        self.best_params = None
        self.true_negative = {}
        self.false_positive = {}
        self.false_negative = {}
        self.true_positive = {}
        self.precision = {}
        self.recall = {}
        self.f1 = {}
        self.accuracy = {}
        self.roc_auc = {}
        self.actual_negative = {}
        self.actual_positive = {}
        self.predicted_negative = {} 
        self.predicted_positive = {}

    def get_column_names_from_ColumnTransformer(self):    
        column_transformer = self.mdl.named_steps['preprocess']
        col_name = []
        for transformer_in_columns in column_transformer.transformers_[:-1]:#the last transformer is ColumnTransformer's 'remainder'
            raw_col_name = transformer_in_columns[2]
            raw_col_name_reverse = raw_col_name[::-1]
            if isinstance(transformer_in_columns[1],Pipeline): 
                transformer = transformer_in_columns[1].steps[-1][1]
            else:
                transformer = transformer_in_columns[1]
            try:
                names = transformer.get_feature_names()
                exchange_name = [(_.split("_")) for _ in column_transformer.transformers_[:-1][0][1].steps[-1][1].get_feature_names()]
                last_pre_name = ""
                last_raw_name = ""
                for pre_name,value in exchange_name:
                    if pre_name==last_pre_name:
                        col_name.append(last_raw_name+"_"+value)
                    if pre_name!=last_pre_name:
                        last_pre_name=pre_name
                        last_raw_name=raw_col_name_reverse.pop()
                        col_name.append(last_raw_name+"_"+value)
            except AttributeError: # if no 'get_feature_names' function, use raw column name
                names = raw_col_name
            if isinstance(names,np.ndarray): # eg.
                col_name += names.tolist()
            elif isinstance(names,list):
                col_name += names    
            elif isinstance(names,str):
                col_name.append(names)
        return col_name

    def tune_param_set(self, train, params, modelfilename, cv_results_filename, k=3, scoring='roc_auc', n_jobs=1,verbose=1): 
        
        grid_search = GridSearchCV(self.mdl, params, n_jobs=n_jobs, verbose=verbose, cv=k, scoring=scoring, 
                                   refit=True, return_train_score=True)    
        grid_search.fit(train[self.predictors], train[self.y_column])
        self.best_params = grid_search.best_params_
        self.mdl = grid_search.best_estimator_
        train = None
        print('Best parameters: {}'.format(grid_search.best_params_))
        print('Best score: {}'.format(grid_search.best_score_))
        
        print(self.get_column_names_from_ColumnTransformer())
        
        
        #print('HERE',transformers)
        pickle.dump(self.mdl, open(modelfilename, 'wb'))
        
        cv_results_df = pd.DataFrame.from_dict(grid_search.cv_results_)
        cv_results_df.to_csv(cv_results_filename,index=False)
        
        
    def fit_model_with_params(self, train, out_modelfilename, in_params=None, in_modelfilename=None): 
        
        if in_params:
            self.mdl.named_steps['learn'].set_params(**in_params)
        elif in_modelfilename:
            in_model = pickle.load(open(in_modelfilename, 'rb'))
            params = in_model.named_steps['learn'].get_params()
            self.mdl.named_steps['learn'].set_params(**params)
            
        self.mdl.fit(train[self.predictors], train[self.y_column])
        self.best_params = params
        train = None
        
        pickle.dump(self.mdl, open(out_modelfilename, 'wb'))
        
    def load_model_from_file(self, in_modelfilename): 
        self.mdl = pickle.load(open(in_modelfilename, 'rb'))
        self.best_params = self.mdl.named_steps['learn'].get_params()
        
    def save_feature_importances(self, feature_importance_filename): 
        #Save feature importances
        if self.one_hot_features:
            try:
                ohe = (self.mdl.named_steps['preprocess'].named_transformers_['cat'].named_steps['onehot'])
                one_hot_feature_names = ohe.get_feature_names(input_features=self.one_hot_features)
                other_categorical_features = [x for x in self.categorical_features if x not in self.one_hot_features]
                all_feature_names = np.r_[self.numeric_features, one_hot_feature_names, other_categorical_features]
            except:
                all_feature_names = self.predictors
        else:
            all_feature_names = self.predictors
        try:
            feature_importances = self.mdl.named_steps['learn'].feature_importances_
            feature_dictionary = dict(zip(all_feature_names, feature_importances))
            dictionary = pd.DataFrame(feature_dictionary, index=[0]).transpose()
            dictionary.to_csv(feature_importance_filename)
        except:
            print('No feature importances collected')
                        
    def setup_rf_model(self):
        mdl1 = RandomForestRegressor(
            n_estimators=500,
            max_features="sqrt",
            min_samples_split=5,
            oob_score=True,
        )

        estimators = [
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        
    
    def setup_rf_model_classify_scale(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

        mdl1 = RandomForestClassifier(
            n_estimators=500,
            max_features="sqrt",
            min_samples_split=5,
            oob_score=True,
        )
        estimators = [
            ('preprocess', preprocessor),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        
    
    def setup_xgb_model(self):
        mdl1 = xgb.XGBRegressor(
            learning_rate=0.1,
            n_estimators=50,
            objective='reg:squarederror',
            eval_metric='rmse',
            
            nthread=-1)

        estimators = [
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        
    
    def setup_xgb_model_classify_scale(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

        mdl1 = xgb.XGBClassifier(
            learning_rate=0.1,
            n_estimators=50,
            objective='reg:squarederror',
            eval_metric='rmse',
            nthread=-1)
        
        estimators = [
            ('preprocess', preprocessor),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        
    
    def setup_MLP_model_classify_scale(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

        mdl1 = MLPClassifier()
        
        estimators = [
            ('preprocess', preprocessor),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
    
    
    def predict_supervised_data(self, test_file, out_file, name='Train',chunksize=500000):
        """
        sklearn prediction for big data
        :param out_file_h5:
        :param test_file:
        :return:
        """
        y_hat_all = []
        y_true_all = []
        y_prob_all = []

        out_df = pd.DataFrame(columns=self.xy+['Y_true','Est','Prob'])
        out_df.to_csv(out_file,index=False)
        
        try:
            chunks = pd.read_hdf(test_file, chunksize=chunksize)
        except:
            chunks = pd.read_csv(test_file, chunksize=chunksize)
        for df in chunks:
            df = df.dropna()
            y_hat = self.mdl.predict(df[self.predictors])
            prob = self.mdl.predict_proba(df[self.predictors])
            prob = prob[:,1]
            
            y_true_all = np.append(y_true_all, df[self.y_column].values)
            y_hat_all = np.append(y_hat_all, y_hat)
            y_prob_all = np.append(y_prob_all, prob)
        
            dfY = pd.Series(df[self.y_column].values, name='Y_true').reset_index(drop=True)
            dfY_hat = pd.Series(y_hat, name='Est').reset_index(drop=True)
            dfProb = pd.Series(prob, name='Prob').reset_index(drop=True)
            dfCoords = df[self.xy].reset_index(drop=True)
            df0 = pd.concat([dfCoords, dfY, dfY_hat, dfProb], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+['Y_true','Est','Prob'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
            
        tn, fp, fn, tp = confusion_matrix(y_true_all, y_hat_all).ravel()
        
        self.actual_positive[name] = np.sum(y_true_all>0)
        self.actual_negative[name] = np.sum(y_true_all==0)
        self.predicted_positive[name] = np.sum(y_hat_all>0)
        self.predicted_negative[name] = np.sum(y_hat_all==0)
        self.true_negative[name] = tn
        self.false_positive[name] = fp
        self.false_negative[name] = fn
        self.true_positive[name] = tp
        self.precision[name] = precision_score(y_true_all, y_hat_all)
        self.recall[name] = recall_score(y_true_all, y_hat_all)
        self.f1[name] = f1_score(y_true_all, y_hat_all)
        self.accuracy[name] = accuracy_score(y_true_all, y_hat_all)
        self.roc_auc[name] = roc_auc_score(y_true_all, y_prob_all)
        

    def save_scores(self, out_file):
        dict_list = [self.actual_positive, self.actual_negative, self.predicted_positive, self.predicted_negative, 
                        self.true_negative, self.true_positive, self.false_positive, self.false_negative, self.precision, 
                        self.recall, self.f1, self.accuracy, self.roc_auc]
        df = pd.DataFrame(dict_list)
        df.insert(0, 'score', ['actual_positive','actual_negative','predicted_positive','predicted_negative',
                               'true_negative','true_positive', 'false_positive','false_negative','precision',
                               'recall','f1','accuracy','roc_auc'])
        df.to_csv(out_file)
        
    def predict_unsupervised_data(self, in_file, out_file, chunksize=500000):
        """
        sklearn prediction for big data
        :param out_file_h5:
        :param test_file:
        :return:
        """
        
        out_df = pd.DataFrame(columns=self.xy+['Est','Prob'])
        out_df.to_csv(out_file,index=False)
        try:
            chunks = pd.read_hdf(in_file, chunksize=chunksize)
        except:
            chunks = pd.read_csv(in_file, chunksize=chunksize)
        
        for df in chunks:
            df = df.dropna()
            y_hat = self.mdl.predict(df[self.predictors])
            prob = self.mdl.predict_proba(df[self.predictors])
            prob = prob[:,1]
            
            dfY_hat = pd.Series(y_hat, name='Est').reset_index(drop=True)
            dfProb = pd.Series(prob, name='Prob').reset_index(drop=True)
            dfCoords = df[self.xy].reset_index(drop=True)
            df0 = pd.concat([dfCoords, dfY_hat, dfProb], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+['Est','Prob'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
            

