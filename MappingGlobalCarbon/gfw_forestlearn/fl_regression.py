import subprocess
import gdal
import pandas as pd
import numpy as np
import glob
import rasterio
import os
import datetime
import csv
import random
from math import sqrt
import pickle
import math
import datetime
import warnings
from shutil import copyfile
import csv
import sys
import math
import shutil

from sklearn.model_selection import cross_val_score
from sklearn import metrics
from sklearn import svm
from sklearn.model_selection import GridSearchCV, StratifiedKFold, validation_curve, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.decomposition import PCA
from sklearn.feature_selection import SelectFromModel
from sklearn.svm import LinearSVR
from sklearn.neural_network import MLPClassifier
from sklearn.utils import resample
from sklearn.metrics import mean_squared_error
import xgboost as xgb


     
     
class ForestLearn(object):
    """
    Build machine learning object that can find the best parameters for final run.

    """

    def __init__(self, predictors=[], y_column=None, xy = [], cat_feats = [], one_hot_feats = []):
        """
        Defines predictors, response variable, coordinate column names, binary features, categorical features, and numeric features
        
        Inputs:
        predictors (list): List of predictor variables to be used in model, these will also be referred to as features
        y_column (string): Name of response variable column in dataframes
        xy (list): Name of coordinate columns for x, y (or longitude, latitude) positions of pixels
        cat_feats (list): List of binary features that will stay (0,1)
        one_hot_feats (list): List of categorical features with more than one category, these will be transformed using one hot encoding to binary features.
        
        Numeric features are those that are listed in predictors but not in cat_feats or one_hot_feats.
        
        Predictors, cat_feats, and one_hot_feats are combined to ensure all features are used and removes duplicates, so you can list both cat_feats and 
        one_hot_feats in predictors or enter them seperately. 
        
        For example:
        predictors= ['rainfall','temperature','biome','protected']
        cat_feats = ['protected']
        one_hot_feats = ['biome']
        
        OR
        predictors= ['rainfall','temperature']
        cat_feats = ['protected']
        one_hot_feats = ['biome']
        
        are both accepted.
        """
        # Exit if y_column is not present
        if y_column is None:
            sys.exit('"y_column" must be defined in training process...')
        
        # Merge inputted feature lists and remove duplicates
        predictors = list(set(predictors + cat_feats + one_hot_feats))
        # Select numeric features as predictors that are not in cat_feats or one_hot_feats
        numeric_features = [x for x in predictors if x not in cat_feats+one_hot_feats]
            
        # Save parameters to ForestLearn object
        self.y_column = y_column
        self.xy = xy
        self.predictors = predictors
        self.numeric_features = numeric_features
        self.categorical_features = cat_feats
        self.one_hot_features = one_hot_feats
        self.best_params = {}
        self.rmse = {}
        self.r2 = {}
        self.avg_res = {}
        self.avg_abs_res = {}
        
    def tune_param_set(self, train, params, out_modelfilename, cv_results_filename, k=5, scoring='neg_root_mean_squared_error', n_jobs=4,verbose=1,refit=True): 
        """
        Given a dictionary of lists of parameters to try, runs GridSearchCV to use cross validation to find the best set of parameters based on the cross 
        validation score defined in the scoring variable.
        Saves the best fitting model to ForestLearn object, outputs model to pickle file, and outputs cross-validation results
        See documentation on GridSearchCV: https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.GridSearchCV.html
        
        Inputs:
        train (DataFrame): Dataframe of training data containing predictors and y_column
        params (dictionary): Dictionary with parameters names (str) as keys and lists of parameter settings to try as values, 
                                or a list of such dictionaries, in which case the grids spanned by each dictionary in the list are explored. 
                                This enables searching over any sequence of parameter settings.
        out_modelfilename (string): Output file name to save trained model, using pickle, should have extension .pkl
        cv_results_filename (string): Output file name to save cross-validation results, should be a csv with extension .csv
        k (integer): Integer number for number of folds in cross-validation 
        scoring (string): Scoring name to evaluate model, see https://scikit-learn.org/stable/modules/model_evaluation.html for options
        n_jobs (integer): Integer number of threads (CPUs) to train model, -1 uses all
        verbose (integer): Controls the verbosity: the higher, the more messages.
        refit (boolean): Refit an estimator using the best found parameters on the whole dataset.
        
        Returns: None
        """
        # Define grid_search
        grid_search = GridSearchCV(self.mdl, params, n_jobs=n_jobs, verbose=verbose, cv=k, scoring=scoring, 
                                   refit=refit, return_train_score=True)
        # Fit grid_search
        grid_search.fit(train[self.predictors], train[self.y_column])
        # Save best parameters
        self.best_params = grid_search.best_params_
        # Save best mdoel
        self.mdl = grid_search.best_estimator_
        # Save best model to out_modelfilename
        pickle.dump(self.mdl, open(out_modelfilename, 'wb'))
        # Save cv_results to cv_results_filename
        cv_results_df = pd.DataFrame.from_dict(grid_search.cv_results_)
        cv_results_df.to_csv(cv_results_filename,index=False)
        return self.mdl
                
        
    def fit_model_with_params(self, train, out_modelfilename, in_params=None, in_modelfilename=None): 
        """
        Given a dictionary of parameters or input model filename to load parameters from trained model, trains a model with inputted parameters
         on training data and saves model. This parameters should only be for the machine learning part of the pipeline, named "learn".
         This step should only be run after setting up model pipeline type, i.e. running setup_xgb_model or setup_rf_model where the 
         feature scaling and selecting is done.
        
        Inputs:
        train (DataFrame): Dataframe of training data containing predictors and y_column
        out_modelfilename (string): Output file name to save trained model, using pickle, should have extension .pkl
        in_params (dictionary): Dictionary with parameters names (str) as keys and parameter setting as value to train the model
        in_modelfilename (string): Input file name of trained model to load parameters, should have extension .pkl
        
        Returns: None
        """
        # Exit if in_params or in_modelfilename is not given
        if (in_params is None) and (in_modelfilename is None):
            sys.exit('Either in_params or in_modelfilename must be provided')
        # If in_params is given, load parameters for "learn" machine learning part of the pipeline
        elif in_params:
            self.mdl.named_steps['learn'].set_params(**in_params)
            params = in_params
        # If in_modelfilename is given, load parameters from modelfile for "learn" machine learning part of the pipeline
        elif in_modelfilename:
            in_model = pickle.load(open(in_modelfilename, 'rb'))
            params = in_model.named_steps['learn'].get_params()
            self.mdl.named_steps['learn'].set_params(**params)
        # Fit model on training data in train
        self.mdl.fit(train[self.predictors], train[self.y_column])
        # Save best parameters
        self.best_params = params
        # Save to out_modelfilename
        pickle.dump(self.mdl, open(out_modelfilename, 'wb'))
        return self.mdl
        
    def load_model_from_file(self, in_modelfilename): 
        """
        Loads inputted model and saves to ForestLearn object
        
        Inputs:
        in_modelfilename (string): Input file name of trained model to save, should have extension .pkl
        
        Returns: None
        """
        self.mdl = pickle.load(open(in_modelfilename, 'rb'))
        self.best_params = self.mdl.named_steps['learn'].get_params()
        return self.mdl
        
    def save_feature_importances(self, feature_importance_filename):
    #     """
    #     Saves feature importances from trained model
    #
    #     Inputs:
    #     feature_importance_filename (string): File name to save feature importances to, should have extension .csv
    #     """
    #     # If one_hot_features are listed, grab the new one hot encoded feature names
    #     # Then the list of parameters is numeric features, one hot encoded features, and categorical features
    #
    #     preprocess_step = list(self.mdl.named_steps.keys())[-2]
    #     transformer = self.mdl.named_steps[preprocess_step]
    #     print(transformer)
    #     if isinstance(transformer,ColumnTransformer):
    #         print(transformer.transformers_)
    #     elif isinstance(transformer,SelectFromModel):
    #         print(transformer.get_support())
            #print()
        #print(transformers)
        # names = [x[1].named_steps for x in transformers]
#
#         has_pca = False
#         has_one_hot = False
#
#         for dictionary in names:
#             for value in dictionary.values():
#                 if isinstance(value,PCA):
#                     has_pca = True
#                 if isinstance(value,OneHotEncoder):
#                     has_one_hot=True
#
#         print(self.one_hot_features, has_pca, has_one_hot)
                
        if self.one_hot_features:
            try:
                ohe = (self.mdl.named_steps['preprocess'].named_transformers_['cat'].named_steps['onehot'])
                one_hot_feature_names = ohe.get_feature_names(input_features=self.one_hot_features)
                all_feature_names = np.r_[self.numeric_features, one_hot_feature_names, self.categorical_features]
            except:
                all_feature_names = self.predictors
        # Otherwise the features are in order
        else:
            all_feature_names = self.predictors
        print(len(self.mdl.named_steps['learn'].feature_importances_))
        # Merge feature importances and names, save to file
        # try:
        #     feature_importances = self.mdl.named_steps['learn'].feature_importances_
        #     feature_dictionary = {'Feature Names':all_feature_names,'Importances':feature_importances}
        #     dictionary = pd.DataFrame(feature_dictionary)
        #     dictionary = dictionary.sort_values(by='Importances', axis=0, ascending=False, na_position='last')
        #     dictionary.to_csv(feature_importance_filename,index=0)
        # except Exception as e:
        #     print('No feature importances collected, reporting exception: ', e)
          

    def predict_data(self, df, out_file, name, other_columns=[],dropna=True):
        """
        Uses trained model to predict accuracy over inputted data that has the response variable to asses score, such as training, validation, or test set.
        Saves coordinates, true response variable, predicted response variable, residual, and copies of other_columns (if included) into out_file 
        Calculates the RMSE, R-Squared, average residual, and average absolute residual for scores.
        
        Inputs:
        df (DataFrame): Dataframe of data to predict over containing predictors and y_column
        out_file (string): File name to save data with predictions to
        name (string): Name of dataset to save scores using "save_scores" method, examples could be "training", "testing", or "validation"
        other_columns (list): Other columns that should also be included, this could be a unique ID of datapoints
        dropna (boolean): Whether to remove records with any nan values. If set to False and NaN values are not resolved, this will cause an error.
        
        Returns: None
        """
        # Prepare output dataframe with columns
        if len(other_columns)>0:
            out_df = pd.DataFrame(columns=self.xy+other_columns+['Y_true','Est','Residual'])
        else:
            out_df = pd.DataFrame(columns=self.xy+['Y_true','Est','Residual'])
        out_df.to_csv(out_file,index=False)
        
        # Remove records with NaN values if dropna is tru
        if dropna:
            df = df.dropna()
            
        # Predict and calculate residual
        #print(df[self.predictors])
        y_hat = self.mdl.predict(df[self.predictors])
        residual = df[self.y_column].values - y_hat
        
        # Create series with data
        dfY = pd.Series(df[self.y_column].values, name='Y_true')
        dfY_hat = pd.Series(y_hat, name='Est')
        dfResidual = pd.Series(residual, name='Residual')
        dfCoords = df[self.xy].reset_index(drop=True)
        
        # If other_columns are listed, merge all of this data and output
        if len(other_columns)>0:
            dfOtherVariables = df[other_columns].reset_index(drop=True)
            df0 = pd.concat([dfCoords, dfOtherVariables, dfY, dfY_hat, dfResidual], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+other_columns+['Y_true','Est','Residual'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
        # Otherwise merge all the data and output
        else:
            df0 = pd.concat([dfCoords, dfY, dfY_hat, dfResidual], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+['Y_true','Est','Residual'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
        
        # Calculate scores and save as parameters to ForestLearn to output in "save_scores"
        self.rmse[name] = math.sqrt(metrics.mean_squared_error(df[self.y_column], y_hat))
        self.r2[name] = metrics.r2_score(df[self.y_column], y_hat)
        self.avg_res[name] = np.mean(df[self.y_column] - y_hat)
        self.avg_abs_res[name] = np.mean(abs(df[self.y_column] - y_hat))    
        

    def save_scores(self, out_file):
        """
        Saves scores from predict_data
        
        Inputs:
        out_file (string): File name to save scores to
        
        Returns: None
        """
        # Create dictionary and save
        dict_list = [self.rmse, self.r2, self.avg_res, self.avg_abs_res]
        df = pd.DataFrame(dict_list)
        df.insert(0, 'Scores', ['Root Mean Square Error','R-Squared','Average Residual','Average Absolute Residual'])
        df.to_csv(out_file,index=False)    
      
    def predict_unsupervised_data(self, in_file, out_file, chunksize=500000, dropna=True):
        """
        Uses trained model to predict over data from in_file and saves output to out_file
        
        Inputs:
        in_file (String): File name to load data from
        out_file (string): File name to save data with predictions to
        chunksize (integer): Chunk size to read data as, this is helpfull if the data is larger than memory can read
        dropna (boolean): Whether to remove records with any nan values. If set to False and NaN values are not resolved, this will cause an error.
        
        Returns: None
        """
        # Prepare output dataframe
        out_df = pd.DataFrame(columns=self.xy+['Est'])
        out_df.to_csv(out_file,index=False)
        
        # Read in file using extension
        if '.csv' in in_file:
            chunks = pd.read_csv(in_file, chunksize=chunksize)
        else:
            chunks = pd.read_hdf(in_file, chunksize=chunksize)
        
        # Loop over chunks
        for df in chunks:
            # Remove records with NaN values if dropna is tru
            if dropna:
                df = df.dropna()
            # Predict data
            y_hat = self.mdl.predict(df[self.predictors])
            # Save results
            dfY_hat = pd.Series(y_hat, name='Est').reset_index(drop=True)
            dfCoords = df[self.xy].reset_index(drop=True)
            df0 = pd.concat([dfCoords, dfY_hat, dfProb], axis=1)
            out_df = pd.DataFrame(df0, columns=self.xy+['Est'])
            out_df.to_csv(out_file, mode='a', header=False, index=False)
            
    
    '''
    The following methods are for instatiating model pipelines, which creates steps for numeric feature scaling, one hot encoding, and feature selection.
    Learn more about the sci-kit learn model pipelines here: https://scikit-learn.org/stable/modules/generated/sklearn.pipeline.Pipeline.html
    We have a number of different ones precoded to use
    '''
                    
    def setup_rf_model(self):
        '''
        Sets up a random forest model with no feature selection or scaling
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a random forest model with numeric feature scaling and one-hot-encoding
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a random forest model with numeric feature scaling, one-hot-encoding, and principle component analysis
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a random forest model with numeric feature scaling, one-hot-encoding, and support vector machine feature selection
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a random forest model with numeric feature scaling, one-hot-encoding, and random forest model feature selection
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a XGBoost model 
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a XGBoost model with numeric feature scaling and one-hot-encoding
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a XGBoost model with numeric feature scaling, one-hot-encoding, and principle component analysis
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
        '''
        Sets up a XGBoost model with numeric feature scaling, one-hot-encoding, and random forest feature selection
        
        Returns:
        self.mdl (Sk-learn Pipeline object)
        '''
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
           
             
