import subprocess
import learn2map.raster_tools as rt
from azureml.core import Run
import gdal
import argparse
import pandas as pd
import numpy as np
import glob
import rasterio
import os
import datetime
import csv
import random


from math import sqrt
import xgboost as xgb
import pickle
import math
import datetime
import warnings
from shutil import copyfile
import csv

from sklearn.model_selection import cross_val_score
from sklearn import metrics
from sklearn import svm
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.decomposition import PCA
from sklearn.feature_selection import SelectFromModel
from sklearn.svm import LinearSVR
from sklearn.model_selection import StratifiedKFold, validation_curve
from sklearn.neural_network import MLPClassifier
from sklearn.utils import resample
from sklearn.metrics import mean_squared_error

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

    def __init__(self, predictors=[], y_column=None, xy = ['x','y'], cat_feats = [], one_hot_feats = [],
                    class_names=[]):
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
            
        self.xy = xy
        self.predictors = numeric_features + cat_feats
        self.numeric_features = numeric_features
        self.categorical_features = cat_feats
        self.one_hot_features = one_hot_feats
        self.predictors = predictors
        self.numeric_features = numeric_features
        self.best_params = {}
        self.categorical_features = cat_feats
        self.rmse = {}
        self.r2 = {}
        self.avg_res = {}
        self.avg_abs_res = {}
        
    def tune_param_set(self, train, params, modelfilename, cv_results_filename, k=5, scoring='neg_root_mean_squared_error', n_jobs=4,verbose=1,refit=True): 
        
        grid_search = GridSearchCV(self.mdl, params, n_jobs=n_jobs, verbose=verbose, cv=k, scoring=scoring, 
                                   refit=refit, return_train_score=True)    
        grid_search.fit(train[self.predictors], train[self.y_column])
        self.best_params = grid_search.best_params_
        self.mdl = grid_search.best_estimator_
        train = None
        print('Best parameters: {}'.format(grid_search.best_params_))
        
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
                all_feature_names = np.r_[self.numeric_features, one_hot_feature_names, self.categorical_features]
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
          

    def save_scores(self, out_file):
        dict_list = [self.rmse, self.r2, self.avg_res, self.avg_abs_res]
        df = pd.DataFrame(dict_list)
        df.insert(0, 'score', ['rmse','r2','avg_res','avg_abs_res'])
        print(df)
        df.to_csv(out_file)

    def predict_data(self, df, out_file, name):
        out_df = pd.DataFrame(columns=self.xy+['Y_true','Est','Residual'])
        out_df.to_csv(out_file,index=False)
        #df = pd.read_csv(in_file, encoding='utf8',engine='python')
        df = df.dropna()
        y_hat = self.mdl.predict(df[self.predictors])
        residual = df[self.y_column[0]].values - y_hat
        
        dfY = pd.Series(df[self.y_column[0]].values, name='Y_true')
        dfY_hat = pd.Series(y_hat, name='Est')
        dfResidual = pd.Series(residual, name='Residual')
        dfCoords = df[self.xy].reset_index(drop=True)
        df0 = pd.concat([dfCoords, dfY, dfY_hat, dfResidual], axis=1)
        out_df = pd.DataFrame(df0, columns=self.xy+['Y_true','Est','Residual'])
        out_df.to_csv(out_file, mode='a', header=False, index=False)
        
        self.rmse[name] = math.sqrt(metrics.mean_squared_error(df[self.y_column[0]], y_hat))
        self.r2[name] = metrics.r2_score(df[self.y_column[0]], y_hat)
        self.avg_res[name] = np.mean(df[self.y_column[0]] - y_hat)
        self.avg_abs_res[name] = np.mean(abs(df[self.y_column[0]] - y_hat))        
      
    def predict_unsupervised_data(self, in_file, out_file, chunksize=500000):
        """
        sklearn prediction for big data
        :param out_file_h5:
        :param test_file:
        :return:
        """
        
        out_df = pd.DataFrame(columns=self.xy+['Est'])
        out_df.to_csv(out_file,index=False)
        
        if '.csv' in in_file:
            chunks = pd.read_csv(in_file, chunksize=chunksize)
        else:
            chunks = pd.read_hdf(in_file, chunksize=chunksize)
        
        for df in chunks:
            df = df.dropna()
            y_hat = self.mdl.predict(df[self.predictors])
            
            dfY_hat = pd.Series(y_hat, name='Est').reset_index(drop=True)
            dfCoords = df[self.xy].reset_index(drop=True)
            df0 = pd.concat([dfCoords, dfY_hat, dfProb], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+['Est'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
            
                    
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
        return self.mdl
    

    def setup_rf_model_scale(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

        mdl1 = RandomForestRegressor(
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
        return self.mdl
        
    def setup_rf_model_PCA(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler()),
            ('PCA', PCA(0.95))])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

        mdl1 = RandomForestRegressor(
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
        return self.mdl

    
    def setup_rf_model_scale_SVR_FS(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')
                
        mdl1 = RandomForestRegressor(
            n_estimators=500,
            max_features="sqrt",
            min_samples_split=5,
            oob_score=True,
        )
        estimators = [
            ('preprocess', preprocessor),
            ('feature_selection', SelectFromModel(LinearSVR())),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        return self.mdl
    
    def setup_rf_model_scale_RF_FS(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')
                
        mdl1 = RandomForestRegressor(
            n_estimators=500,
            max_features="sqrt",
            min_samples_split=5,
            oob_score=True,
        )
        estimators = [
            ('preprocess', preprocessor),
            ('feature_selection', SelectFromModel(RandomForestRegressor(n_estimators=100))),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        return self.mdl
    
    
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
        return self.mdl

    def setup_xgb_model_scale(self):
        numeric_transformer = Pipeline(steps=[('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')
                
        mdl1 = xgb.XGBRegressor(
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
        return self.mdl
    
    def setup_xgb_model_PCA(self):
        numeric_transformer = Pipeline(steps=[
            ('scale', StandardScaler()),
            ('PCA', PCA(0.95))
        ])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')
                
        mdl1 = xgb.XGBRegressor(
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
        return self.mdl
    
    def setup_xgb_model_RF_FS(self):
        numeric_transformer = Pipeline(steps=[('scale', StandardScaler())])

        categorical_transformer = Pipeline(steps=[
            ('onehot', OneHotEncoder(handle_unknown='ignore'))])

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, self.numeric_features),
                ('cat', categorical_transformer, self.one_hot_features)],remainder='passthrough')

                
        mdl1 = xgb.XGBRegressor(
            learning_rate=0.1,
            n_estimators=50,
            objective='reg:squarederror',
            eval_metric='rmse',
            nthread=-1)

        estimators = [
            ('preprocess', preprocessor),
            ('feature_selection', SelectFromModel(RandomForestRegressor(n_estimators=100))),
            ('learn', mdl1)
        ]
        self.mdl = Pipeline(estimators)
        return self.mdl
           
             
     
     
     
     
     
     
     
     
     
     
        
#
# parser = argparse.ArgumentParser()
# parser.add_argument('--data-folder', type=str, dest='data_folder', help='data folder mounting point')
# args = parser.parse_args()
# data_folder = args.data_folder
# output_directory =  os.path.join(data_folder,'Final_Runs','split_0_bootstrap_xgboost_reduced_variables')
#
# random.seed(30)
#
#
# y_column = 'carbon_seqr_rate_Mg_ha_yr'
# cat_feats = ['BiomesMask_b1']
# # mask_columnz = ['Index','.geo','Biomes','system:index','data_source','stand_age','total_AGC_Mg_ha','system_ind',
# #                 'CM10_1975H_Bio36_V1_2_b1','CM10_1975H_Bio37_V1_2_b1', 'CM10_1975H_Bio38_V1_2_b1',
# #                 'CM10_1975H_Bio39_V1_2_b1','CM10_1975H_Bio40_V1_2_b1','shortwave_radiation_01_b1',
# #                 'shortwave_radiation_02_b1', 'shortwave_radiation_03_b1', 'shortwave_radiation_04_b1',
# #                 'shortwave_radiation_05_b1', 'shortwave_radiation_06_b1', 'shortwave_radiation_07_b1',
# #                 'shortwave_radiation_08_b1', 'shortwave_radiation_09_b1', 'shortwave_radiation_10_b1',
# #                 'shortwave_radiation_11_b1', 'shortwave_radiation_12_b1','Temperate_b1','Stratify_column',
# #                 'z_score','BiomesMask_b1.1','Biomes1km_b1.1']
#
# mask_columnz = ['y', 'x', 'stand_age', 'total_AGC_Mg_ha', 'carbon_seqr_rate_Mg_ha_yr', 'data_source', 'Lon_1km',
#                 'Lat_1km', 'Temperate_b1', 'Stratify_column', 'z_score', 'ai_et0_b1',
#                 'BDRLOG_M_1km_ll_b1', 'BDTICM_M_1km_ll_b1', 'BLDFIE_M_sl_b1', 'CM10_1975H_Bio24_V1_2_b1',
#                 'CM10_1975H_Bio27_V1_2_b1', 'CM10_1975H_Bio36_V1_2_b1', 'CM10_1975H_Bio37_V1_2_b1',
#                 'CM10_1975H_Bio38_V1_2_b1', 'CM10_1975H_Bio39_V1_2_b1', 'CM10_1975H_Bio40_V1_2_b1',
#                 'CRFVOL_M_sl_b1', 'et0_yr_b1', 'GMTEDAspect_b1', 'GMTEDElevation_b1', 'GMTEDHillShade_b1',
#                 'GMTEDSlope_b1', 'OCDENS_M_sl_b1', 'OCSTHA_M_sd_b1', 'shortwave_radiation_01_b1',
#                 'shortwave_radiation_02_b1', 'shortwave_radiation_03_b1', 'shortwave_radiation_04_b1',
#                 'shortwave_radiation_05_b1', 'shortwave_radiation_06_b1', 'shortwave_radiation_07_b1',
#                 'shortwave_radiation_08_b1', 'shortwave_radiation_09_b1', 'shortwave_radiation_10_b1',
#                 'shortwave_radiation_11_b1', 'shortwave_radiation_12_b1', 'shortwave_radiaton_1982_2015_b1',
#                 'TerraClim_def_r_b1', 'TerraClim_pdsi_r_b1', 'TerraClim_ro_r_b1', 'TerraClim_soil_r_b1',
#                 'TerraClim_vap_r_b1', 'TerraClim_vs_r_b1', 'wc_bio_30s_12_b1', 'wc_bio_30s_15_b1',
#                 'wc_bio_30s_16_b1', 'wc_bio_30s_18_b1', 'Index', '.geo', 'Biomes',
#                 'system:index', 'data_source', 'stand_age', 'total_AGC_Mg_ha', 'system_ind',
#                 'CM10_1975H_Bio36_V1_2_b1', 'CM10_1975H_Bio37_V1_2_b1', 'CM10_1975H_Bio38_V1_2_b1',
#                 'CM10_1975H_Bio39_V1_2_b1', 'CM10_1975H_Bio40_V1_2_b1', 'shortwave_radiation_01_b1',
#                 'shortwave_radiation_02_b1', 'shortwave_radiation_03_b1', 'shortwave_radiation_04_b1',
#                 'shortwave_radiation_05_b1', 'shortwave_radiation_06_b1', 'shortwave_radiation_07_b1',
#                 'shortwave_radiation_08_b1', 'shortwave_radiation_09_b1', 'shortwave_radiation_10_b1',
#                 'shortwave_radiation_11_b1', 'shortwave_radiation_12_b1', 'Temperate_b1', 'Stratify_column',
#                 'z_score', 'BiomesMask_b1.1', 'Biomes1km_b1.1', 'Lon_1km', 'Lat_1km']
#
# def main():
#     training_h5 = os.path.join(output_directory,'Train.csv')
#     test_h5 = os.path.join(output_directory,'Test.csv')
#
#     sub_directory = os.path.join(output_directory,'ModelResults')
#     if not os.path.exists(sub_directory):
#         os.makedirs(sub_directory)
#     train_out_file = os.path.join(sub_directory, '{}_predicted_train.csv')
#     test_out_file = os.path.join(sub_directory, '{}_predicted_test.csv')
#     groa_out_file = os.path.join(sub_directory, '{}_predicted_groa_test.csv')
#     scores_out_file = os.path.join(sub_directory, '{}_scores.csv')
#     model_out_file = os.path.join(sub_directory, '{}_model.pkl')
#     cv_results_out_file = os.path.join(sub_directory, '{}_cv_results.csv')
#     feature_importance_out_file = os.path.join(sub_directory, '{}_feature_importance.csv')
#
#
#
#     if 'pixels' in training_h5:
#         xy = ['Lat_1km','Lon_1km']
#         ignore_xy = ['x','y']
#     else:
#         xy = ['x','y']
#         ignore_xy = ['Lat_1km','Lon_1km']
#     mask_column = ignore_xy + mask_columnz + xy
#     row_count = sum(1 for row in csv.reader(training_h5))
#     rf_params = {
#         'learn__max_depth': [3,6,10,20],
#         'learn__max_features': ['auto',.2,.3,.4],
#         'learn__min_samples_leaf': [3,5,15,30],
#         'learn__min_samples_split': [3,5,15,30],
#         'learn__n_estimators': [100,200,300]
#     }
#     xgb_params = {
#         'learn__learning_rate': [0.1,0.2,0.3],
#         'learn__gamma':  [1,4,7,10],
#         'learn__max_depth': [3,5,7,10],
#         'learn__colsample_bytree': [0.2,0.3,0.6,0.8],
#         'learn__min_child_weight': [1,7,15,50],
#         'learn__lambda': [0.2,0.5,1]
#     }
#     #options = ['XGB_RF_FS','XGB_Scale','RF_Scale','RF_RF_FS','RF_PCA']
#     options = ['XGB_Scale']
#     for option in options:
#         learning = GridLearn(training_h5, y_column=y_column, xy = xy, mask_column=mask_column,
#                                 cat_feats = cat_feats)
#         if option == 'RF_Scale':
#             learning.setup_rf_model_scale()
#             params = rf_params
#         elif option == 'RF_RF_FS':
#             learning.setup_rf_model_scale_RF_FS()
#             params = rf_params
#         elif option == 'RF_PCA':
#             learning.setup_rf_model_PCA()
#             params = rf_params
#         elif option == 'XGB_RF_FS':
#             learning.setup_xgb_model_RF_FS()
#             params = xgb_params
#         elif option == 'XGB_Scale':
#             learning.setup_xgb_model_scale()
#             params = xgb_params
#         elif option=='XGB_PCA':
#             learning.setup_xgb_model_PCA()
#             params = xgb_params
#
#         print('Starting tune param set for {} at time: {}'.format(option,datetime.datetime.now()))
#         #learning.load_model(model_out_file.format(option))
#         learning.tune_param_set(params, model_out_file.format(option),
#                        cv_results_out_file.format(option), feature_importance_out_file.format(option),k=3)
#
#
#         print('Starting predicting training data at time: {}'.format(datetime.datetime.now()))
#         training_df = pd.read_csv(training_h5)
#         learning.predict_data(training_df, train_out_file.format(option), 'train')
#
#
#         print('Starting predicting test data at time: {}'.format(datetime.datetime.now()))
#         test_df = pd.read_csv(test_h5)
#         learning.predict_data(test_df, test_out_file.format(option), 'test')
#
#         groa_df = test_df[test_df['data_source']=='GROA']
#         learning.predict_data(groa_df, groa_out_file.format(option), 'groa_test')
#         learning.save_scores(scores_out_file.format(option))
#         learning = None
#
#
#
#
# main()

