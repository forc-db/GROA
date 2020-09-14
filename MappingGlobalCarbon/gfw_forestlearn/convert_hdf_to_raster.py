#!/usr/bin/python
import getopt
import sys
import pandas as pd
import numpy as np
import os
import subprocess
from osgeo import gdal
import geopandas as gpd
from shapely.geometry import Point
import rasterio
import multiprocessing as mp


'''
Converts a large HDF or CSV file of pixel values to raster

Arguments:
in_csv (String):
column (String):
ref_file (String):
out_file (String):
x (String):
y (String)
'''
in_csv = ''
column = ''
ref_file = ''
out_file = ''
x = ''
y = ''
chunksize = 1000000
min_0 = False

print 'ARGV      :', sys.argv[1:]

options, remainder = getopt.getopt(sys.argv[1:], '', ['in_csv=', 
                                                        'column=',
                                                        'ref_file=',
                                                        'out_file=',
                                                        'x_name=',
                                                        'y_name=',
                                                        'chunksize=',
                                                        'min_0'
                                                         ])
for opt, arg in options:
    if opt in ('--in_csv'):
        in_csv = arg
    elif opt in ('--column'):
        column = arg
    elif opt in ('--ref_file'):
        ref_file = arg
    elif opt in ('--out_file'):
        out_file = arg
    elif opt in ('--x_name'):
        x = arg
    elif opt in ('--y_name'):
        y = arg
    elif opt in ('--chunksize'):
        chunksize = arg
    elif opt in ('--min_0'):
        min_0 = True

xy = [x,y]


src = rasterio.open(ref_file)

data = np.zeros((src.height,src.width))
data[:,:] = np.nan
result = np.ctypeslib.as_ctypes(data)
shared_array = mp.sharedctypes.RawArray(result._type_, result)


def fill_per_window(df):
    for i,row in df.iterrows():
        #print(row)
        tmp = np.ctypeslib.as_array(shared_array)
        index = src.index(row[xy[0]],row[xy[1]])
        if min_0:
            est = np.maximum(row[name],0)
        else:
            est = row[name]
        
        tmp[index] = est

print('Entering pool')
p = mp.Pool()

if '.csv' in in_csv:
    p.imap_unordered(fill_per_window, pd.read_csv(in_csv, chunksize=chunksize)
else:
    p.imap_unordered(fill_per_window, pd.read_hdf(in_csv, chunksize=chunksize)

p.close()
p.join()
result = np.ctypeslib.as_array(shared_array)

profile = src.profile

profile.update(dtype=rasterio.float32,count=1,compress='lzw')

with rasterio.open(out_file, 'w', **profile) as dst:
    dst.write(result.astype(rasterio.float32), 1)
    dst.nodata = np.nan




