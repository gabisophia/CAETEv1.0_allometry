#!-*-coding:utf-8-*-
import os
import sys
import glob
import time
import numpy as np
import pandas as pd
import gdal
import netCDF4 as nc

import write_output as wo
from dtype_dict import dtypes_list

# Import filepaths from homedir
#sys.path.insert(0, '../src/')
import homedir

# UNTAR output files
#os.system("ipython3 untar.py")

# This variable must store the correct path to the results folder
data_dir = homedir.RESULTS_DIR

# This variable contains the correct path to save all pls_attrs.csv together and the final csv for analysis
#attr_dir = homedir.SAVE_CSV_FILES
csv_dir = homedir.SAVE_CSV_FINAL

# Create outputs folder if it dont exists
#if not os.path.exists(attr_dir):
#    os.mkdir(attr_dir)
if not os.path.exists(csv_dir):
    os.mkdir(csv_dir)
# Some variables
root  = os.getcwd()

# ARRAY DIMENSIONS
NX = 720
NY = 360

lat = np.arange(-89.75, 90., 0.5)
lon = np.arange(-179.75, 180., 0.5)
dtypes = dict(dtypes_list)

#function to calculate cwm
def _cwm(area, traits):
    """area :: [np.array  1D] shape(npls,)
    traits [np.array 1D ] shape(npls):: g1 || vcmax || tleaf || twood || troot || aleaf || awood || aroot"""

    # assert area.shape == traits.shape, 'shape mismatch'
    return (area * traits).sum(axis=0,)

mask = np.load("amazon_mask.npy")
mask_forest = np.load("mask_forests.npy")

def read_as_array(nc_fname, var):
    """ only for multilayers files"""
    with nc.Dataset(nc_fname, mode='r') as fcon:
    #fcon = nc.Dataset(nc_fname)
        data_array = fcon.variables[var][:]
    return np.fliplr(data_array)


def make_table_aux(folder):
    
    traits = ['tleaf','tawood','tfroot','aleaf','awood','afroot']
    area_m2 = np.flipud(nc.Dataset("cell_area.nc").variables['cell_area'][:])

    print('\n\nRunning Make_table for folder', end ="-")
    print (folder)
    
    rname = folder.split('_')[0] # to be used in pls_attrs_save

    rname1 = os.sep.join([csv_dir, rname])
    
    # Variable run stores the string ID of the current run(e.g. "r01")
    
    attr_table = pd.read_csv('pft5.csv',dtype=np.float32)  
    os.chdir(data_dir + folder )
    
    ## open files and read variables
    print("Read variables --- %s\n" % (time.ctime()))
    area_ocp = read_as_array("area.nc", "area") / 100.0
    cmass = read_as_array("cmass.nc", "cmass").sum(axis=0,)
    cleaf = read_as_array("cleaf.nc", "cleaf").sum(axis=0,)
    cfroot = read_as_array("cfroot.nc", "cfroot").sum(axis=0,)
    cawood = read_as_array("cawood.nc", "cawood").sum(axis=0,)
    
    npp =  read_as_array('npp.nc', 'annual_cycle_mean_of_npp').mean(axis=0,)
    photo =  read_as_array('photo.nc', 'annual_cycle_mean_of_ph').mean(axis=0,)
    aresp =  read_as_array('aresp.nc', 'annual_cycle_mean_of_ar').mean(axis=0,)
    cue =  read_as_array('cue.nc', 'annual_cycle_mean_of_cue').mean(axis=0,)
    #wue =  read_as_array('wue.nc', 'annual_cycle_mean_of_wue').mean(axis=0,)
    evapm =  read_as_array('evapm.nc', 'annual_cycle_mean_of_et').mean(axis=0,)
    rcm = read_as_array('rcm.nc', 'annual_cycle_mean_of_rcm').mean(axis=0,)
    print("Ended ncdf readings --- %s\n" % (time.ctime()))
        
   
    #rpath = attr_dir + os.sep
    NPLS=rname[3:]
    #attr_table.to_csv(rpath + 'pft_attrs' + '-' + NPLS + '.csv', index=False)
    struct_array = []
    counter = 0
            
    for Y in range(NY):
        for X in range(NX):
            if not mask[Y, X]:
                NPP = npp[Y, X]
                area1 = area_ocp[:,Y,X]
                if NPP <= 1e-5:
                    pass
                else:
                    counter += 1                             # types
                    line =(Y,                    # i2
                           X,                    # i2
                           lat[Y],               # f4
                           lon[X],               # f4
                           mask_forest[Y, X],    # f4
                           area_m2[Y, X],        # f4
                           '%.6f' %  photo[Y, X],          # daqui pra frente tudo f4
                           '%.6f' %  aresp[Y, X],
                           '%.6f' %  npp[Y, X], 
                           '%.6f' %  rcm[Y, X], 
                           '%.6f' %  evapm[Y, X],
                          # '%.6f' %  wue[Y, X],
                           '%.6f' %  cue[Y, X],     
                           '%.6f' %  cmass[Y, X],
                           '%.6f' %  cleaf[Y, X],
                           '%.6f' %  cfroot[Y, X],
                           '%.6f' %  cawood[Y, X],
                           '%.6f' %  _cwm(area1, attr_table[traits[0]]),  # tleaf_cwm
                           '%.6f' %  _cwm(area1, attr_table[traits[1]]),  # twood_cwm
                           '%.6f' %  _cwm(area1, attr_table[traits[2]]),  # troot_cwm
                           '%.6f' %  _cwm(area1, attr_table[traits[3]]),  # aleaf_cwm
                           '%.6f' %  _cwm(area1, attr_table[traits[4]]),  # awood_cwm
                           '%.6f' %  _cwm(area1, attr_table[traits[5]]))  # aroot_cwm

                    
                    sys.stdout.write("\rLines completed: %d" % counter)
                    struct_array.append(line)
    sys.stdout.flush()
    fname_csv = folder + ".csv"
    if os.path.exists(fname_csv):
        pd.DataFrame(np.array(struct_array, dtype=dtypes_list)).to_csv(fname_csv, header=False, index=False, mode='a')
    else:
        pd.DataFrame(np.array(struct_array, dtype=dtypes_list)).to_csv(fname_csv, header=True, index=False, mode='w')
        # clean_variables
    struct_array = None
    area_ocp = None
    area_ocp0 = None
    cmass = None
    cleaf = None
    cfroot = None
    cawood = None
    attr_table = None
    os.chdir(root)
    return None


def make_folder_runs(fl):
    
    #with conc.ThreadPoolExecutor(max_workers=5) as executor:
    make_table_aux(fl)
    

def make_table():
    """ Constructs the final table of caete results"""
    root = os.getcwd()
    # Create the list of lists of output directories
    flds = ["5PFTs_nclim", "5PFTs_lowprec"]
    make_folder_runs(flds[0])
    return None

if __name__ == '__main__':

    # Faz a tabela com dados de cada celula de grid
    make_table()
