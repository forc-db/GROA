import numpy as np
import pandas as pd 
import streamlit as st
import geopandas
import geojson
import zipfile
import tempfile
import folium
import base64
import requests
import json
from osgeo import ogr
from ast import literal_eval
import rasterio
from rasterio.enums import Resampling
from rasterio.vrt import WarpedVRT
#from rio_tiler import *
from rio_tiler.utils import render
from rio_tiler.colormap import cmap
from rio_tiler.utils import linear_rescale
from rio_tiler.reader import read as rio_preview
from rio_tiler import reader
import os
from streamlit_folium import folium_static

from time import sleep
import logging
# from flask import * #Flask, request, abort
# from ai4e_app_insights_wrapper import AI4EAppInsights
# from ai4e_service import APIService

from os import getenv
import sys
import glob
from rasterstats import zonal_stats
import fiona
from geojson import Point, Feature, FeatureCollection, dump
from fiona.crs import from_epsg

#st.set_option('deprecation.showfileUploaderEncoding', False)

target_wkt = 'PROJCS["World_Cylindrical_Equal_Area",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433],AUTHORITY["EPSG","4326"]],PROJECTION["Cylindrical_Equal_Area"],PARAMETER["standard_parallel_1",0],PARAMETER["central_meridian",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]]]'

url = 'https://gfw-files.s3.amazonaws.com/ai4e/{}'#.format(geotiff_name)

COG_SOURCE = 'Sequestration_Rate_Map_Griscom_Restoration_Areas_cog.tif'#'rcog.tif'
rate_tif = 'Sequestration_Rate_Map_Griscom_Restoration_Areas.tif'
variance_tif = 'Sequestration_Rate_Map_Griscom_Restoration_Areas_Variance.tif'
belowground_rate_tif = 'Sequestration_Rate_Map_Griscom_Restoration_Areas_BelowGround_Rate.tif'
belowground_variance_tif = 'Sequestration_Rate_Map_Griscom_Restoration_Areas_BelowGround_Variance.tif'
griscom_tif = 'Griscom.tif'

files_to_download = [rate_tif, variance_tif, belowground_rate_tif, belowground_variance_tif,COG_SOURCE,griscom_tif]


pixel_size_m = 739.34*739.34 #Pixel area in meters
pixel_size_ha = pixel_size_m*0.0001 #Pixel area in hectares




def download_tif_from_aws(geotiff_name, download_url):
    r = requests.get(download_url, stream=True)
    chunk_size = 20000000
    with open(os.path.join(os.getcwd(),geotiff_name), 'wb') as fd:
        for chunk in r.iter_content(chunk_size):
            fd.write(chunk)
            
def caclulate_carbon_stats(user_gdf):
    griscom_stats = zonal_stats(user_gdf, griscom_tif, stats=['sum'])
    griscom_stats = pd.DataFrame(griscom_stats).fillna(0)
    for i,row in griscom_stats.iterrows():
        griscom_stats.at[i,'Griscom_Restoration_Area_Hectares'] = row['sum']*pixel_size_ha
    griscom_stats = griscom_stats[['Griscom_Restoration_Area_Hectares']] 
 
    rate_stats = None
    #try:
    rate_stats = zonal_stats(user_gdf, rate_tif, stats=['mean','sum','max','min','count'])
    rate_stats = pd.DataFrame(rate_stats).fillna(0)
    for i,row in rate_stats.iterrows():
        rate_stats.at[i,'Modeled_AboveGround_Restoration_Area_Hectares'] = row['count']*pixel_size_ha
        rate_stats.at[i,'AboveGround_Carbon_Accumulation'] = row['sum']*pixel_size_ha

    rate_stats = rate_stats.rename(columns={
        "mean": "AboveGround_Sequestration_Rate_Mean", 
        "max": "AboveGround_Sequestration_Rate_Max", 
        "min": "AboveGround_Sequestration_Rate_Min", 
        "count": "Pixel_Count"
    })
    rate_stats = rate_stats[['Modeled_AboveGround_Restoration_Area_Hectares','AboveGround_Sequestration_Rate_Mean',
                            'AboveGround_Carbon_Accumulation',
                            'AboveGround_Sequestration_Rate_Max','AboveGround_Sequestration_Rate_Min']]

    belowground_rate_stats = zonal_stats(user_gdf, belowground_rate_tif, stats=['mean','sum','max','min','count'])
    belowground_rate_stats = pd.DataFrame(belowground_rate_stats).fillna(0)
    for i,row in belowground_rate_stats.iterrows():
        belowground_rate_stats.at[i,'Modeled_BelowGround_Restoration_Area_Hectares'] = row['count']*pixel_size_ha
        belowground_rate_stats.at[i,'BelowGround_Carbon_Accumulation'] = row['sum']*pixel_size_ha

    belowground_rate_stats = belowground_rate_stats.rename(columns={
        "mean": "BelowGround_Sequestration_Rate_Mean", 
        "max": "BelowGround_Sequestration_Rate_Max", 
        "min": "BelowGround_Sequestration_Rate_Min"
    })
    belowground_rate_stats = belowground_rate_stats[['Modeled_BelowGround_Restoration_Area_Hectares','BelowGround_Sequestration_Rate_Mean','BelowGround_Carbon_Accumulation',
                            'BelowGround_Sequestration_Rate_Max','BelowGround_Sequestration_Rate_Min']]
                    
    variance_stats = zonal_stats(user_gdf, variance_tif, stats=['sum','count'])
    variance_stats = pd.DataFrame(variance_stats).fillna(0)
    for i,row in variance_stats.iterrows():
        variance_of_accumulation = row['sum']*pixel_size_ha**2      #variance of accumulation is sum of pixel level variances multiplied by conversion to hectares squared
        if row['count'] !=0:
            variance_of_rate = row['sum']/(row['count']**2)             #variance of rate is sum of pixel level variances, divided by n^2 (n=number of pixels)
        else:
            variance_of_rate = 0

        std_of_accumulation = np.sqrt(variance_of_accumulation)
        std_of_rate = np.sqrt(variance_of_rate)

        variance_stats.at[i,'AboveGround_Sequestration_Rate_Variance'] = variance_of_rate
        variance_stats.at[i,'AboveGround_Sequestration_Rate_LowerBound'] = rate_stats.at[i,'AboveGround_Sequestration_Rate_Mean']-1.96*std_of_rate
        variance_stats.at[i,'AboveGround_Sequestration_Rate_UpperBound'] = rate_stats.at[i,'AboveGround_Sequestration_Rate_Mean']+1.96*std_of_rate

        variance_stats.at[i,'AboveGround_Carbon_Accumulation_Variance'] = variance_of_accumulation
        variance_stats.at[i,'AboveGround_Carbon_Accumulation_LowerBound'] = rate_stats.at[i,'AboveGround_Carbon_Accumulation']-1.96*std_of_accumulation
        variance_stats.at[i,'AboveGround_Carbon_Accumulation_UpperBound'] = rate_stats.at[i,'AboveGround_Carbon_Accumulation']+1.96*std_of_accumulation

    variance_stats = variance_stats[['AboveGround_Sequestration_Rate_Variance','AboveGround_Sequestration_Rate_LowerBound',
                                    'AboveGround_Sequestration_Rate_UpperBound','AboveGround_Carbon_Accumulation_Variance',
                                    'AboveGround_Carbon_Accumulation_LowerBound','AboveGround_Carbon_Accumulation_UpperBound']]
    
    below_variance_stats = zonal_stats(user_gdf, belowground_variance_tif, stats=['sum','count'])
    below_variance_stats = pd.DataFrame(below_variance_stats).fillna(0)
    for i,row in below_variance_stats.iterrows():
        variance_of_accumulation = row['sum']*pixel_size_ha**2      #variance of accumulation is sum of pixel level variances multiplied by conversion to hectares squared
        if row['count'] !=0:
            variance_of_rate = row['sum']/(row['count']**2)             #variance of rate is sum of pixel level variances, divided by n^2 (n=number of pixels)
        else:
            variance_of_rate = 0
        
        std_of_accumulation = np.sqrt(variance_of_accumulation)
        std_of_rate = np.sqrt(variance_of_rate)

        below_variance_stats.at[i,'BelowGround_Sequestration_Rate_Variance'] = variance_of_rate
        below_variance_stats.at[i,'BelowGround_Sequestration_Rate_LowerBound'] = belowground_rate_stats.at[i,'BelowGround_Sequestration_Rate_Mean']-1.96*std_of_rate
        below_variance_stats.at[i,'BelowGround_Sequestration_Rate_UpperBound'] = belowground_rate_stats.at[i,'BelowGround_Sequestration_Rate_Mean']+1.96*std_of_rate

        below_variance_stats.at[i,'BelowGround_Carbon_Accumulation_Variance'] = variance_of_accumulation
        below_variance_stats.at[i,'BelowGround_Carbon_Accumulation_LowerBound'] = belowground_rate_stats.at[i,'BelowGround_Carbon_Accumulation']-1.96*std_of_accumulation
        below_variance_stats.at[i,'BelowGround_Carbon_Accumulation_UpperBound'] = belowground_rate_stats.at[i,'BelowGround_Carbon_Accumulation']+1.96*std_of_accumulation

    below_variance_stats = below_variance_stats[['BelowGround_Sequestration_Rate_Variance','BelowGround_Sequestration_Rate_LowerBound',
                            'BelowGround_Sequestration_Rate_UpperBound','BelowGround_Carbon_Accumulation_Variance',
                            'BelowGround_Carbon_Accumulation_LowerBound','BelowGround_Carbon_Accumulation_UpperBound']]


    #user_gdf = geopandas.read_file(user_user_gdf,encoding='utf-8')
    user_gdf = user_gdf[[x for x in list(user_gdf) if x!='geometry']]
    stats = pd.concat([user_gdf,griscom_stats, rate_stats,variance_stats, belowground_rate_stats, below_variance_stats],axis=1)

    stat_columns = ['Griscom_Restoration_Area_Hectares', 'Modeled_AboveGround_Restoration_Area_Hectares', 
                    'AboveGround_Sequestration_Rate_Mean','AboveGround_Sequestration_Rate_Variance',
                    'AboveGround_Sequestration_Rate_LowerBound','AboveGround_Sequestration_Rate_UpperBound',
                    'AboveGround_Sequestration_Rate_Max','AboveGround_Sequestration_Rate_Min',
                    'AboveGround_Carbon_Accumulation','AboveGround_Carbon_Accumulation_Variance',
                    'AboveGround_Carbon_Accumulation_LowerBound','AboveGround_Carbon_Accumulation_UpperBound',
                    'Modeled_BelowGround_Restoration_Area_Hectares',
                    'BelowGround_Sequestration_Rate_Mean','BelowGround_Sequestration_Rate_Variance',
                    'BelowGround_Sequestration_Rate_LowerBound','BelowGround_Sequestration_Rate_UpperBound',
                    'BelowGround_Sequestration_Rate_Max','BelowGround_Sequestration_Rate_Min',
                    'BelowGround_Carbon_Accumulation','BelowGround_Carbon_Accumulation_Variance',
                    'BelowGround_Carbon_Accumulation_LowerBound','BelowGround_Carbon_Accumulation_UpperBound']

    stats = stats[list(user_gdf)+stat_columns]
    return stats


def download_link(object_to_download, download_filename, download_link_text):
    """
    Generates a link to download the given object_to_download.
    object_to_download (str, pd.DataFrame):  The object to be downloaded.
    download_filename (str): filename and extension of file. e.g. mydata.csv, some_txt_output.txt
    download_link_text (str): Text to display for download link.
    """
    if isinstance(object_to_download,pd.DataFrame):
        object_to_download = object_to_download.to_csv(index=False)
    b64 = base64.b64encode(object_to_download.encode()).decode()
    return f'<a href="data:file/txt;base64,{b64}" download="{download_filename}">{download_link_text}</a>'


def make_folium_map(geo_in, w,s,e,n):
    """
    function to render a folium map
    geo_in: inpit geojson with coordinates in degrees latitude and longitude
    w, s, e, n: bounding coordinates, e.g, resulting from GeoDataFrame.total_bounds
    """
    m = folium.Map(tiles='CartoDB Positron', )
    input_shape_style = {'fillColor': '#00aacc', 'color': '#0000bb','fillOpacity':0.08,'width':3,'opacity':0.5}
    folium.GeoJson(geo_in, name='Uploaded shape preview', style_function=lambda x: input_shape_style).add_to(m)
    folium.raster_layers.ImageOverlay('preview.png',name='Carbon raster',bounds=[[s,w],[n,e]],zindex=99,opacity=0.9,interactive=True).add_to(m)
    m.fit_bounds([[s,w],[n,e]])
    folium.LayerControl().add_to(m)
    return m

#upload rio-compliant colormap, which is fun
with open('rio-colormap.json') as json_data:
    cmap_data = json.load(json_data, object_hook=lambda d: {int(k): [int(i) for i in v] if isinstance(v, list) else v for k, v in d.items()})

#@st.cache
def generate_preview_tile(cog_tif, input_gdf):
    """
    Hodge podge function to pass windowed GDAL virtual format warp args to the
    rio_tiler preview function to create a quick png image to overlay on the
    preview map.

    cog_tif: Filepath
    input_gdf: GeoDataFrame with a valid crs
    -----
    output: writes 'preview.png' file
    """
    with rasterio.open(cog_tif) as src:
        with WarpedVRT(src, crs='EPSG:3857',resampling=Resampling.bilinear) as vrt:
            # Determine the destination tile and its mercator bounds (3857)
            left, bottom, right, top = input_gdf.to_crs('EPSG:3857').total_bounds
            # Determine the window to use in reading from the dataset.
            dst_window = vrt.window(left, bottom, right, top)
            tile,mask = rio_preview(vrt, window=dst_window,width=1024,height=1024)
            out_data = np.where(
            #    mask, linear_rescale(tile, in_range=(np.nanmin(tile), np.nanmax(tile)), out_range=[0, 255]), 0
                mask, linear_rescale(tile, in_range=(0.3, 3), out_range=[0, 255]), 0
            ).astype(np.uint8)
            #cm = cmap.get("viridis")
            cm = cmap_data
            buffer = render(out_data, mask=mask, colormap=cm)
            with open('preview.png', "wb") as f:
                f.write(buffer)

st.title("Web Application for Calculating Potential Carbon Sequestration Due to Natural Forest Regrowth in Your Area(s) of Interest")
st.markdown("This demo allows the user to query our predicted map of carbon sequestration rates in young, naturally regenerating forests. \
            Given an area/areas of interest, the API will return summary statistics of the area(s) including, the area of restoration potential, the average rate of \
            aboveground and belowground carbon sequestration and accumulation, error bounds for those estimates, and the minimum and maximum observed \
            aboveground and belowground rates in the area of interest. \n\nRestoration areas are reported in hectares,\
            sequestration rates are reported in megagrams of carbon per hectare per year (Mg C/ha/year), and carbon accumulation is reported in \
            megagrams of carbon per year (Mg C/year).")
st.subheader('Instructions:')
            
st.markdown('Read through the title for a description of web application above and in the left sidebar. Then scroll to see "Upload a zipped shapefile to initiate analysis" \
                    where you can upload a zip file of a shapefile describing your area(s) of interest. A preview of our map and your area(s) of interest will appear \
                    then click the "Submit Request" check-box to start the results. Once the results are calculated, you will see a preview of the table of results \
                    and a download link to download the results as a CSV, which can be opened in Excel, Pages, or by any text editor.')
st.sidebar.title('About:')             
st.sidebar.markdown("We spatially estimated aboveground and belowground rates of carbon sequestration for young, \
            naturally regenerating forests, where the rates can be applied linearly for the first 30 years of forest regrowth. \
            The map was made by training a Random Forest based machine learning model \
            on forest inventory data using about 80 covariates, with variables covering soil, topography, and climate. \n\nOur map is available at 1-kilometer resolution. \
            \n\nTo estimate belowground carbon sequestration rates, we applied a conversion factor based on \
            root:shoot ratios defined by the [IPCC](https://www.ipcc.ch/report/2019-refinement-to-the-2006-ipcc-guidelines-for-national-greenhouse-gas-inventories/). \
            \n\nWe clipped our prediciton map to restoration areas defined by \
            [Griscom et al 2017](https://www.pnas.org/content/114/44/11645). Only certain biomes from [RESOLVE Ecoregions](https://ecoregions2017.appspot.com/) \
            were including in our areas of model predictions, and the root:shoot ratios were only available for certain biomes. Therefore we also provide in the results the \
            restoration potential area with modeled aboveground rates and modeled belowground ratese.\n\n This map and web app can be \
            used by governments, land use planners, and restoration organizations to assess the potential carbon sequestration potential from naturally \
            regenerating forests in a given region.\n\nThis web app was put together by the [World Resources Institute](https://www.wri.org/) and [Global Forest Watch](globalforestwatch.org).")
st.sidebar.markdown('This application is maintained by Kristine Lister (kristine.lister@wri.org) with many thanks to David Gibbs and Alex Kovac for help in developing.')

        
## UPLOADER PROCESS
uploaded_file = st.file_uploader("Upload a zipped shapefile to initiate analysis", type=['zip']) ### TO DO: ADD geojson file upload and error handling


if uploaded_file:
    st.subheader('Downloading data')
    st.markdown('Starting downloading geotiffs')
    for file_name in files_to_download:
        if os.path.exists(file_name):
            st.markdown('- File '+file_name+' already downloaded')
        else:
            st.markdown('- Downloading file '+file_name)
            download_tif_from_aws(file_name, url.format(file_name))

    st.markdown('Finished downloading geotiffs')
    with tempfile.TemporaryDirectory() as tmpdir:
        z = zipfile.ZipFile(uploaded_file) #z.namelist() printed later...
        z.extractall(tmpdir)
        filenames = [y for y in sorted(z.namelist()) for ending in ['shp'] if y.endswith(ending)]
        shf = tmpdir+'/'+filenames[0] # TO DO: If multiple .shp files in zip, current script will only open up the first in the directory
        driver = ogr.GetDriverByName('ESRI Shapefile')
        shape = driver.Open(shf)
        layer= shape.GetLayer()
        crs = layer.GetSpatialRef()
        gdf = geopandas.GeoDataFrame.from_file(shf, crs=crs.ExportToWkt())
        gdf = gdf.to_crs(target_wkt)
        df_attrs = pd.DataFrame(gdf.drop(columns='geometry'))
        #target_geojson = geojson.loads(gdf.to_json()) #may not be needed? Should reproject to target source
        gdf_4326 = gdf.to_crs("EPSG:4326")
        w,s,e,n= gdf_4326.total_bounds
        geo_4326 = geojson.loads(gdf_4326.to_json())
        
    try:
        generate_preview_tile(COG_SOURCE, gdf_4326)

        ### DISPLAY UPLOADED FILE ON PREVIEW MAP
        st.subheader("Map preview")
        m = make_folium_map(geo_4326,w,s,e,n)

        #m.fit_bounds([[s,w],[n,e]])
        #st.markdown(m._repr_html_(), unsafe_allow_html=True)
        folium_static(m)
    except Exception as e:
        logging.info("Folium preview failed with exception")
        logging.info(e)
        st.markdown('Folium preview failed')
        st.markdown(e)
    some_space = st.markdown('---')
    

    ### BUTTON TO APPROVE AREA
    st.subheader('Check checkbox above to initiate analysis')
    agree = st.checkbox("Submit request")
    # if not agree:
    #
    #     #st.json(z.namelist())

    if agree:
        ### PASS GEOJSON TO API
        
        result = caclulate_carbon_stats(gdf)
        legend_df = pd.read_csv('Result_Column_Names.csv')
        st.header('Results shown below.')
        ### MAKE DOWNLOADABLE CSV
        tmp_download_link = download_link(result, 'carbon_data.csv', 'Click to download csv of results!')
        st.markdown(tmp_download_link, unsafe_allow_html=True)
        tmp_download_legend_link = download_link(legend_df, 'carbon_data_legend.csv', 'Click to download csv of legend for results!')
        st.markdown(tmp_download_legend_link, unsafe_allow_html=True)
        #st.write(result)
        st.table(result)
        st.header('Legend: ')
        st.table(legend_df)
        
        #st.balloons() # uncomment for cheeky balloon animation

