import pandas as pd
import numpy as np
import os
import subprocess
from osgeo import gdal
from sklearn.model_selection import train_test_split
import geopandas as gpd
from shapely.geometry import Point
import rasterio
import multiprocessing as mp
from multiprocessing import Pool #  Process pool
from multiprocessing import sharedctypes
import tqdm
import statistics


def get_raster_extent(in0):
    """
    For every input in0 return raster extent, and raster resolution
    
    Inputs:
    in0 (String): Filename of input file to grab raster extent and resolution from
    
    Returns: None
    """
    gt = in0.GetGeoTransform()
    xs = in0.RasterXSize
    ys = in0.RasterYSize
    x1 = gt[0] + 0 * gt[1] + 0 * gt[2]
    y1 = gt[3] + 0 * gt[4] + 0 * gt[5]
    x2 = gt[0] + xs * gt[1] + ys * gt[2]
    y2 = gt[3] + xs * gt[4] + ys * gt[5]
    extent0 = [min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2)]
    res0 = [max(abs(gt[1]), abs(gt[4])), max(abs(gt[2]), abs(gt[5]))]
    return extent0, res0

def raster_clip(mask_file, in_file, out_file, resampling_method='near', out_format='Float32',
                srcnodata='nan', dstnodata='nan'):
    """
    Reprojects raster in "in_file" to the same projection and resolution as "mask_file" using gdalwarp.
        See gdalwarp documentation for more information https://gdal.org/programs/gdalwarp.html
    
    Inputs:
    mask_file (String): Filename of mask file that will be read for projection and resolution
    in_file (String): Filename of raster that will be reprojected to match projection and resolution of mask_file
    out_file (String): Output filename
    resampling_method (String): Resampling method, see gdalwarp documentation for options
    out_format (String): Out format of raster data, see gdalwarp documentation for options
    srcnodata (Number): Output raster no-data value
    dstnodata (Number): Input raster no-data value
                
    Returns: None
    """
    in0 = gdal.Open(mask_file)
    prj0 = in0.GetProjection()
    extent0, res0 = get_raster_extent(in0)
    extent0 = ' '.join(map(str, extent0))
    res0 = ' '.join(map(str, res0))
    size0 = '{} {}'.format(str(in0.RasterXSize), str(in0.RasterYSize))

    if (out_format=='Float32') or (out_format=='Float64'):
        predictor_num = 3
    else:
        predictor_num = 2
    gdal_expression = (
        'gdalwarp -t_srs {} -te {} -ts {} '
        '-srcnodata {} -dstnodata {} -multi -overwrite '
        '-co COMPRESS=LZW -co PREDICTOR={} -co BIGTIFF=YES '
        '-r {} -ot {} "{}" "{}"').format(
        prj0, extent0, size0, srcnodata, dstnodata, predictor_num,
        resampling_method, out_format, in_file, out_file)
    print(gdal_expression)
    subprocess.check_output(gdal_expression, shell=True)
    in0 = None
    return
    
def rasterize_shapefile(shape_file, mask_file, out_file, layer_name, burn_value=1, dstnodata=0, out_format='Byte',at=False):
    """
    Rasterize shapefile in shape_file to same projection and resolution as mask_file using gdal_rasterize
        See gdalwarp documentation for more information https://gdal.org/programs/gdal_rasterize.html
    
    Inputs:
    shape_file (String): Filename of shapefile that will be rasterized
    mask_file (String): Filename of raster that will be read for projection and resolution
    out_file (String): Output filename
    layer_name (String): Name of layer in shapefile to rasterize, often the name of the .shp file
    burn_value (Number): Value to burn to raster
    dstnodata (Number): No-data value to assign to raster
    out_format (String): Out format of raster data, see gdalwarp documentation for options
    at (Binary): If false only pixels with their center in a shape will be burned, otherwise all pixels touching the shape will be burned
    
    Returns: None
    """
    in0 = gdal.Open(mask_file)
    prj0 = in0.GetProjection()
    extent0, res0 = get_raster_extent(in0)
    extent0 = ' '.join(map(str, extent0))
    res0 = ' '.join(map(str, res0))
    size0 = '{} {}'.format(str(in0.RasterXSize), str(in0.RasterYSize))

    if at==True:
        gdal_expression = (
            'gdal_rasterize -burn {} -at -a_srs {} -te {} -ts {} -tr {} '
            '-l {} -a_nodata {} -ot {} -co COMPRESS=LZW -co NUM_THREADS=ALL_CPUS '
            '"{}" "{}"').format(burn_value, prj0, extent0, size0, res0, layer_name, dstnodata, out_format, shape_file, out_file)
    else: 
        gdal_expression = (
            'gdal_rasterize -burn {} -a_srs {} -te {} -ts {} -tr {} '
            '-l {} -a_nodata {} -ot {} -co COMPRESS=LZW -co NUM_THREADS=ALL_CPUS '
            '"{}" "{}"').format(burn_value, prj0, extent0, size0, res0, layer_name, dstnodata, out_format, shape_file, out_file)
    print(gdal_expression)
    subprocess.check_output(gdal_expression, shell=True)
    in0 = None
    return 
    
def modify_vrt_xml(out_file_vrt):
    """
    Modify the virtual raster format file to include multiple bands from each raster
    
    Inputs:
    out_file_vrt (String): Filename of vrt file
    
    Returns:
    field_names (List): Field names from VRT
    """
    vrt_doc = etree.parse(out_file_vrt)
    root = vrt_doc.getroot()
    path = os.path.dirname(out_file_vrt)
    n = 0
    field_names = []
    for element in root.iter('VRTRasterBand'):
        path_relative = element.xpath('.//SourceFilename/@relativeToVRT')
        file_text = element.xpath('.//SourceFilename/text()')
        if path_relative[0] == '0':
            file_name = file_text[0]
        else:
            file_name = os.path.join(path, file_text[0])
        in0 = gdal.Open(file_name)
        if in0.RasterCount == 1:
            n += 1
            element.attrib['band'] = str(n)
            field_names.append('{}_b1'.format(os.path.splitext(os.path.basename(file_text[0]))[0]))
        else:
            for band_num in range(in0.RasterCount):
                band_num += 1
                n += 1
                if band_num == 1:
                    element.attrib['band'] = str(n)
                    field_names.append('{}_b1'.format(os.path.splitext(os.path.basename(file_text[0]))[0]))
                else:
                    new_element = deepcopy(element)
                    new_element.attrib['band'] = str(n)
                    source_band = new_element.xpath('.//SourceBand')
                    source_band[0].text = str(band_num)
                    root.insert(root.index(element) + band_num - 1, new_element)
                    field_names.append('{}_b{}'.format(os.path.splitext(os.path.basename(file_text[0]))[0], band_num))
        in0 = None
    etree.ElementTree(root).write(out_file_vrt, pretty_print=True)
    return field_names

    
def build_stack_vrt(in_file_list, out_file):
    """
    Build raster stack vrt file from in_file_list.
    
    Inputs:
    in_file_list (String): Filename of text file with list of raster files to sample from, separated by new line character
    out_file (String): Filename of output h5 file
    
    Returns: 
    field_names (List): Field names from in_file_list
    """
    gdal_expression_01 = (
        'gdalbuildvrt -separate -overwrite -input_file_list "{}" "{}" --config GDAL_CACHEMAX 2000'
    ).format(in_file_list, out_file)
    subprocess.check_output(gdal_expression_01, shell=True)
    field_names = rt.modify_vrt_xml(out_file)
    print(field_names)

    return field_names

def raster_to_h5(in_file_vrt, out_file_h5, field_names, mask_column, mask_valid_range=1, lines=1000, drop_nan=True):
    """
    Make a layer stack of raster bands to be used in csv output.
    Output is a virtual raster with all bands and csv files with geolocation and valid data.
    All layers should be processed to have the same geolocation and dimensions.
    Mask band should be the 1st band in the in_file_list
    
    Inputs:
    in_file_vrt (String): Filename of the input virtual raster files
    out_file_h5 (String): Filename of output h5 file
    field_names (List): List of column names
    mask_column (String): Column used to mask data
    mask_valid_range (Number): Number denominating valid pixel in mask band (i.e. 1 for valid)
    lines (Integer): Numbers of lines to read at once
    
    Returns: None
    """
    in0 = gdal.Open(in_file_vrt)
    bands = []
    for band_num in range(in0.RasterCount):
        band_num += 1
        band = in0.GetRasterBand(band_num)
        bands.append(band)
    dim0 = (0, 0, in0.RasterXSize, in0.RasterYSize)
    gt = in0.GetGeoTransform()
    
    with pd.HDFStore(out_file_h5, mode='w', complib='blosc:snappy', complevel=9) as store:
        for y in range(dim0[1], dim0[3], lines):
            y2 = min(y + lines, dim0[3])
            lines1 = y2 - y
            cols, rows = np.meshgrid(np.arange(dim0[2]), np.arange(y, y2))
            geo_x = gt[0] + (cols + 0.5) * gt[1] + (rows + 0.5) * gt[2]
            geo_y = gt[3] + (cols + 0.5) * gt[4] + (rows + 0.5) * gt[5]
            data = np.vstack((geo_x.flatten(), geo_y.flatten()))
            for band in bands:
                band_data = band.ReadAsArray(dim0[0], y, dim0[2] - dim0[0], lines1).flatten()
                data = np.vstack((data, band_data))
            if drop_nan is True:
                df1 = pd.DataFrame(data, dtype='float32').transpose().dropna()
            else:
                df1 = pd.DataFrame(data, dtype='float32').transpose()
            df1.columns = ['x', 'y'] + field_names
            df0 = df1.loc[lambda df: df[mask_column] == mask_valid_range, :]
            store.append('df0', df0, index=False, data_columns=df0.columns)
    with pd.HDFStore(out_file_h5) as store:
        store.create_table_index('df0', columns=['x', 'y'], optlevel=6, kind='medium')
    in0 = None
    return
    
def convert_hdf(in_file_list,out_file_name):
    """
    Convert a list of rasters to an HDF file
    
    Inputs:
    in_file_list (String): Filename of text file with list of raster files to sample from, separated by new line character
    out_file (String): Filename of output h5 file
    
    Returns: None
    """
    with open(in_file_list, 'r') as f:
        file_names = f.readlines()
        mask_column = '{}_b1'.format(os.path.basename(os.path.splitext(file_names[0])[0]))
        y_column = '{}_b1'.format(os.path.basename(os.path.splitext(file_names[1])[0]))
    out_vrt = '{}.vrt'.format(out_file_name)
    print(out_vrt)
    field_names = build_stack_vrt(in_file_list, out_vrt)
    out_h5 = '{}.h5'.format(out_file_name)
    raster_to_h5(out_vrt, out_h5, field_names, mask_column, mask_valid_range=0, lines=1000, drop_nan=True)
    
    

def get_reference_coordinates(df, reference_raster_file, in_lat_name='y', in_lon_name = 'x', out_lat_name = 'Lat_1km', out_lon_name = 'Lon_1km'):
    """
    Function to find the center coordinate of pixel a point falls within
    
    Inputs:
    df (DataFrame): Input dataframe with each row corresponding to a point
    reference_raster_file (String): Reference raster used for pixel size
    in_lat_name (String): Point latitude column
    in_lon_name (String): Point longitude column
    out_lat_name (String): New pixel latitude column
    out_lon_name (String): New pixel longitude column
    
    Returns: None
    """
    out_df = df.copy()
    src = rasterio.open(reference_raster_file)
    metadata = src.meta
    for i,row in out_df.iterrows():
        raster_row, raster_column = rasterio.transform.rowcol(metadata.get('transform'), row[in_lon_name], row[in_lat_name])
        ref_coords = rasterio.transform.xy(metadata.get('transform'), raster_row, raster_column, offset='center')
        out_df.at[i,out_lon_name] = ref_coords[0]
        out_df.at[i,out_lat_name] = ref_coords[1]
    return out_df


def stratify_split(df, stratify_column=None, test_size = 0.2):
    """
    Split dataframe randomly, can stratify by defining stratify_column:
    
    Inputs:
    df (DataFrame): Iput dataset to split
    stratify_column (String): Use this column to stratify split the data
    test_size (Number): Test dataset size, can be either an integer to set test set size or fraction to split
    
    Returns:
    training_dataset, test_dataset (Tuple): results of split, first the training dataset and second the test dataset
    """
    if stratify_column:
        return train_test_split(df, test_size=test_size)
    else:
        return train_test_split(df, stratify=df[stratify_column].values, test_size=test_size)


def save_dataframe_as_csv_and_shp(df, out_prefix, x_field, y_field, out_crs='epsg:4326'):
    """
    Save a dataframe of points as a CSV and SHP
    
    Inputs:
    df (DataFrame): Dataframe to save as CSV and shapefile
    out_prefix (String): Prefix for filename to output to, extensions .csv and .shp will be added before being saved
    x_field (String): Longitude column name in df
    y_field (String): Latitude column name in df
    out_crs (String): CRS to use in shapefile
    
    Returns: None
    """
    df.to_csv('{}.csv'.format(out_prefix),index=False)
    if not os.path.exists(out_prefix):
        os.mkdir(out_prefix) 
    out_prefix = '{}/{}'.format(out_prefix,out_prefix)
    geometry = [Point(xy) for xy in zip(df[x_field].values, df[y_field].values)]
    gdf = gpd.GeoDataFrame(df.copy(), crs=out_crs, geometry=geometry)
    gdf.to_file('{}.shp'.format(out_prefix))
    return None
    

def average_plots_with_matching_coords(df, x_field, y_field):
    """
    Average plot values within matching coordinates (use after get_reference_coordinates) 
    
    Inputs:
    df (DataFrame): Dataframe to average
    x_field (String): Longitude column name in df
    y_field (String): Latitude column name in df
    
    Returns: 
    average_plots (DataFrame): DataFrame result of averaging
    """
    average_plots = pd.DataFrame(columns=list(df))
    coordinates = df[[y_field,x_field]].drop_duplicates()
    print('Number of unique coordinates {}'.format(len(coordinates)))
    for i, row in coordinates.iterrows():
        lat = row[y_field]
        lon = row[x_field]
        match_rows = df[(df[y_field]==lat)&(df[x_field]==lon)]
        new_row = match_rows.iloc[0].copy()
        if len(match_rows)>1:
            new_values = match_rows.mean(axis=0)
            new_row.replace(list(new_values),new_values)
        average_plots = average_plots.append(new_row, ignore_index = True)
    return average_plots


def find_matching_plot_coords(unique_df, df, x_field, y_field):
    """
    Find plots that are in the same pixel
    
    Inputs:
    unique_df (DataFrame): 
    df (DataFrame): Dataframe to save as CSV and shapefile
    x_field (String): Longitude column name in df
    y_field (String): Latitude column name in df
        
    Returns: 
    match_df (DataFrame): DataFrame result of unique values
    """
    match_df = pd.DataFrame(columns=list(df))
    for i, row in unique_df.iterrows():
        lat = row[y_field]
        lon = row[x_field]
        match_rows = df[(df[y_field]==lat)&(df[x_field]==lon)]
        match_df = match_df.append(match_rows, ignore_index = True)
    return match_df
    
def remove_outliers_using_z_score(df, target_column, sigma=3):
    """
    Remove outliers from data using z_score
    
    Inputs:
    unique_df (DataFrame): 
    df (DataFrame): Dataframe with values to filter
    target_column (String): Column to use to find outliers
    sigma (Number): Number of standard deviations to use as upper and lower bounds
        
    Returns: 
    out_df (DataFrame): DataFrame result with outliers removed
    """
    target_values = df[target_column].values
    target_values = target_values[~np.isnan(target_values)]
    standard_dev = statistics.stdev(target_values)
    mean = np.mean(target_values)
    lower_bound = mean-1.96*standard_dev
    upper_bound = mean+1.96*standard_dev
    
    out_df = df.copy()
    out_df = out_df[out_df[target_column]>=lower_bound]
    out_df = out_df[out_df[target_column]<=upper_bound]
    return out_df
        

def sample_raster_at_point_location(args):
    """
    Helper function for get_covariates_at_point_locations
    """
    coordinates = args[0]
    raster_file = args[1]
    with rasterio.open(raster_file) as src:
        num_bands = src.count
        raster_name = os.path.splitext(os.path.basename(raster_file))[0]
        band_names = [raster_name+'_b{}'.format(x) for x in np.arange(1,num_bands+1)]
        #columns = ['Coordinates']+band_names
        df = pd.DataFrame(columns=band_names)
        #df['Coordinates'] = coordinates
        try:
            length = sum(1 for _ in src.sample(coordinates))
        except: 
            length = 0
        if length>0:
            values = src.sample(coordinates)
            for i,val in enumerate(values):
                df.at[i,band_names] = val
    return df


def get_covariates_at_point_locations(df, covariate_raster_list, x_field = 'x', y_field = 'y'):
    """
    Sample rasters at point locations
    
    Inputs:
    df (DataFrame): DataFrame of points to sample
    covariate_raster_list (List): List of filenames of rasters to sample
    x_field (String): Longitude column name in df
    y_field (String): Latitude column name in df
        
    Returns: 
    out_df (DataFrame): DataFrame with original df and new values from covariate_raster_list
    """
    coords = df.copy()
    coords = coords[[x_field,y_field]].values
    coordinates = [tuple(x) for x in coords]
    args = [[coordinates, x] for x in covariate_raster_list]
    pool = mp.Pool()
    results = pool.map(sample_raster_at_point_location, args) 
    results = [df]+results
    out_df = pd.concat(results, join="inner",axis=1,sort=False)
    return out_df
        
        
# coords = [[-53.23123,48.95448],[-53.45, 48.94],[-53.66, 48.82], [-53.19, 48.54], [-53.68, 48.58]]
# in_df = pd.DataFrame(columns=['x','y'])
# in_df['x'] = [x[0] for x in coords]
# in_df['y'] = [x[1] for x in coords]
# print(in_df)
# raster_list = ['/Users/kristine/Downloads/landsat_example.tif','/Users/kristine/Downloads/CM10_1975H_Bio20_V1_2.tif']
# out_df = get_covariates_at_point_locations(in_df, raster_list)
# print(out_df)
# out_df.to_csv('/Users/kristine/Downloads/covariate_sample_example.csv',index=False)
# out = sample_raster_at_point_location([coords,raster_file])
# out['x'] = [x[0] for x in out['Coordinates'].values]
# out['y'] = [x[1] for x in out['Coordinates'].values]
# out_csv_name = '/Users/kristine/Downloads/landsat_example.csv'
# out.to_csv(out_csv_name,index=False)
# out_file = '/Users/kristine/Downloads/landsat_example_b1.tif'
# parallel_write_csv_to_raster(out_csv_name, raster_file, 'landsat_example_b1',out_file)
# print(out)    
    
    
#
# if __name__ == '__main__':  # <- prevent RuntimeError for 'spawn'
#     # and 'forkserver' start_methods
#     with multiprocessing.Pool(multiprocessing.cpu_count() - 1) as pool:
#         print(SomeClass().some_method())
#
# def fill_per_window(sa, df, x='x',y='y'):
#     for i,row in df.iterrows():
#         tmp = np.ctypeslib.as_array(sa)
#         index = src.index(row[x],row[y])
#         print(index)
#         tmp[index] = row[target_column]
#
# def convert_csv_to_raster(in_csv, out_raster_name, reference_raster_name, x = 'x', y = 'y', chunksize = 1000000)
#     src = rasterio.open(reference_raster_name)
#     data = np.zeros((src.height,src.width))
#     data[:,:] = np.nan
#     result = np.ctypeslib.as_ctypes(data)
#     shared_array = sharedctypes.RawArray(result._type_, result)
#     length = int(138950955/chunksize)
#     print('Entering pool')
#     p = Pool()
#     for _ in tqdm.tqdm(p.imap_unordered(fill_per_window, pd.read_csv(in_csv, chunksize=chunksize)), total=length):
#         pass
#     p.close()
#     p.join()
#     #res = p.map(fill_per_window, pd.read_csv(in_csv, chunksize=chunksize))
#     result = np.ctypeslib.as_array(shared_array)
#     profile = src.profile
#     profile.update(dtype=rasterio.float32,count=1,compress='lzw')
#     with rasterio.open(out_file, 'w', **profile) as dst:
#         dst.write(result.astype(rasterio.float32), 1)
#         dst.nodata = np.nan
#
    
    
    
    
    
    
    
    
    
    
    


