import pandas as pd
import numpy as np
import os
from sklearn.model_selection import train_test_split
import geopandas as gpd
from geopandas import GeoDataFrame
from shapely.geometry import Point
import rasterio
import multiprocessing as mp
from multiprocessing import Pool #  Process pool
from multiprocessing import sharedctypes
import tqdm

def raster_clip(mask_file, in_file, out_file, resampling_method='near', out_format='Float32',
                srcnodata='nan', dstnodata='nan', max_memory='2000'):
    """
    From learn2map
    for every input in_file, get the same spatial resolution, projection, and
    extent as the input mask_file.

    output is a new raster file: out_file.
    """
    in0 = gdal.Open(mask_file)
    prj0 = in0.GetProjection()
    extent0, res0 = get_raster_extent(in0)
    extent0 = ' '.join(map(str, extent0))
    res0 = ' '.join(map(str, res0))
    size0 = '{} {}'.format(str(in0.RasterXSize), str(in0.RasterYSize))

    in1 = gdal.Open(in_file)
    prj1 = in1.GetProjection()
    extent1, res1 = get_raster_extent(in1)
    extent1 = ' '.join(map(str, extent1))
    res1 = ' '.join(map(str, res1))

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
    in1 = None
    return
    
def rasterize_shapefile(shapefile, mask_file, outfile, layer_name, burn_value=1, nodata_val=0, out_format='Byte',
                        at=True,srcnodata=255, max_memory='2000'):
    """
    rasterize shapefile to same projection as mask_file
    """
    in0 = gdal.Open(mask_file)
    prj0 = in0.GetProjection()
    extent0, res0 = rt.get_raster_extent(in0)
    extent0 = ' '.join(map(str, extent0))
    res0 = ' '.join(map(str, res0))
    size0 = '{} {}'.format(str(in0.RasterXSize), str(in0.RasterYSize))

    if at==True:
        gdal_expression = (
            'gdal_rasterize -burn {} -at -a_srs {} -te {} -ts {} -tr {} '
            '-l {} -a_nodata {} -ot {} -co COMPRESS=LZW -co NUM_THREADS=ALL_CPUS '
            '"{}" "{}"').format(burn_value, prj0, extent0, size0, res0, layer_name, nodata_val, out_format, shapefile, outfile)
    else: 
        gdal_expression = (
            'gdal_rasterize -burn {} -a_srs {} -te {} -ts {} -tr {} '
            '-l {} -a_nodata {} -ot {} -co COMPRESS=LZW -co NUM_THREADS=ALL_CPUS '
            '"{}" "{}"').format(burn_value, prj0, extent0, size0, res0, layer_name, nodata_val, out_format, shapefile, outfile)
    print(gdal_expression)
    subprocess.check_output(gdal_expression, shell=True)
    in0 = None
    in1 = None
    return 
    
def build_stack_vrt(in_file_list, out_file):
    """
    build raster stack vrt file from in_file_list.
    :param in_file_list:
    :param out_file: output vrt file (end with .vrt)
    :return:
    """
    gdal_expression_01 = (
        'gdalbuildvrt -separate -overwrite -input_file_list "{}" "{}" --config GDAL_CACHEMAX 2000'
    ).format(in_file_list, out_file)
    subprocess.check_output(gdal_expression_01, shell=True)
    field_names = modify_vrt_xml(out_file)
    print(field_names)

    return field_names

def raster_to_h5(in_file_vrt, out_file_h5, field_names, mask_column, mask_valid_range=0, lines=100, drop_nan=True):
    """
    Make a layer stack of raster bands to be used in csv output.
    Output is a virtual raster with all bands and csv files with geolocation and valid data.
    All layers should be processed to have the same geolocation and dimensions.
    Mask band should be the 1st band in the in_file_list
    :param in_file_vrt: file name of the input virtual raster files
    :param out_file_h5: file name of output h5 file
    :param field_names: names of all columns
    :param mask_column: column used to mask data
    :param mask_valid_range: define valid data range (e.g.: >0)  in mask band
    :param lines: numbers of lines to read at once
    :return: None
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
            df0 = df1.loc[lambda df: df[mask_column] > mask_valid_range, :]
            store.append('df0', df0, index=False, data_columns=df0.columns)
    with pd.HDFStore(out_file_h5) as store:
        store.create_table_index('df0', columns=['x', 'y'], optlevel=6, kind='medium')
    in0 = None
    return
    
def convert_hdf(in_file_list,out_file_name):
    with open(in_file_list, 'r') as f:
        file_names = f.readlines()
        mask_column = '{}_b1'.format(os.path.basename(os.path.splitext(file_names[0])[0]))
        y_column = '{}_b1'.format(os.path.basename(os.path.splitext(file_names[1])[0]))
    out_vrt = '{}.vrt'.format(out_file_name)
    print(out_vrt)
    field_names = rt.build_stack_vrt(in_file_list, out_vrt)
    out_h5 = '{}.h5'.format(out_file_name)
    raster_to_h5(out_vrt, out_h5, field_names, mask_column, mask_valid_range=0, lines=1000, drop_nan=True)
    
    

def get_reference_coordinates(df, reference_raster_file, in_lat_name='y', in_lon_name = 'x', out_lat_name = 'Lat_1km', out_lon_name = 'Lon_1km'):
    """
    #Function to find the coordinates of a lower resolution raster
    """
    out_df = df.copy()
    src = rasterio.open(reference_raster_file)
    metadata = src.meta
    for i,row in out_df.iterrows():
        raster_row, raster_column = rasterio.transform.rowcol(metadata.get('transform'), row[in_lon_name], row[in_lat_name])
        ref_coords = rasterio.transform.xy(metadata.get('transform'), raster_row, raster_column, offset='center')
        out_df.at[i,out_lon_name] = ref_coords[0]
        out_df.at[i,out_lat_name] = ref_coords[1]


def stratify_split(df, stratify_column, test_size = 0.2):
    """
    #Stratify split dataframe
    """
    return train_test_split(df,stratify=df[stratify_column].values,test_size=test_size)


def save_point_df_and_shp(df, out_prefix, x_field, y_field, out_crs='epsg:4326'):
    """
    #Save plot as a CSV and SHP
    """
    df.to_csv('{}.csv'.format(out_prefix),index=False)
    if not os.path.exists(out_prefix):
        os.mkdir(out_prefix) 
    out_prefix = '{}/{}'.format(out_prefix,out_prefix)
    geometry = [Point(xy) for xy in zip(df[x_field].values, df[y_field].values)]
    crs = {'init': out_crs}
    gdf = GeoDataFrame(df.copy(), crs=crs, geometry=geometry)
    gdf.to_file('{}.shp'.format(out_prefix))
    return None
   

def average_plots_with_matching_coords(df, x_field, y_field):
    """
    #Average plot values within matching coordinates (use after get_reference_coordinates) 
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


def find_matching_plot_coords(unique_df,df, x_field, y_field):
    """
    Find plots that are in the same pixel
    """
    #unique_df = df[[x_field,y_field]].drop_duplicates()
    match_df = pd.DataFrame(columns=list(df))
    for i, row in unique_df.iterrows():
        lat = row[y_field]
        lon = row[x_field]
        match_rows = df[(df[y_field]==lat)&(df[x_field]==lon)]
        match_df = match_df.append(match_rows, ignore_index = True)
    return match_df
        

def sample_raster_at_point_location(args):
    """
    #Sample raster at point location
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


def get_covariates_at_point_locations(df, covariate_raster_list, x_name = 'x', y_name = 'y'):
    """
    #Get covarates at point locations
    """
    coords = df.copy()
    coords = coords[[x_name,y_name]].values
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
# class OtherClass:
#   def run(self, sentence, graph):
#     return False
#
# def single(params):
#     other = OtherClass()
#     sentences, graph = params
#     return [other.run(sentence, graph) for sentence in sentences]
#
# class SomeClass:
#    def __init__(self):
#        self.sentences = [["Some string"]]
#        self.graphs = ["string"]
#    def some_method(self):
#       return list(pool.map(single, zip(self.sentences, self.graphs)))
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
    
    
    
    
    
    
    
    
    
    
    


