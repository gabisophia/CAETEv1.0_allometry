"""MODULE - helper with filepaths
   """

import os
# CHANGE HOMEDIR to make tests in forks
# for example: to run in sombrero make HOMEDIR = os.sep.join(['','home','amazonfaceme','your username'])


py_executable = 'ipython'
HOMEDIR = os.sep.join(['', 'home', 'bianca', 'Área de Trabalho','results_pft_analysis (5PFTs)'])
RESULTS_DIR = os.sep.join([HOMEDIR,''])
TMP_DIR = os.sep.join([HOMEDIR, 'tmp'])
OUTPUT_NC_DIR = os.sep.join([TMP_DIR, 'outputs_nc'])

# Path to save all pls_attrs.csv renamed with run information
SAVE_CSV_FILES = os.sep.join([HOMEDIR, 'csv_outputs'])
# Path to save the final files for each set of runs
SAVE_CSV_FINAL = os.sep.join([HOMEDIR, 'csv_final'])




# EDIT AND SAVE IN your /src folder (in your fork)
# py_executable = 'python'
# HOMEDIR = os.sep.join(['C:', 'Users', 'jpdarela', 'Desktop'])
