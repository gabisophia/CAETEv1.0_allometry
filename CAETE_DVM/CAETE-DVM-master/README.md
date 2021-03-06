# CAETÊ
This is the implementation of the Dynamic version of CAETÊ - including Nitrogen and Phosphorus cycling

The code in this repository is based on an earlier version of CAETÊ that was not dynamic and did not have the N and P cycling implemented.

THis is part of my ongoing PhD research project.

# Development Dependencies
CAETÊ depends on a few packages that must be installed beforehand.
You can installed them using your favorite package manager: `apt`, `brew`, `pip`, `poetry`, etc.

General Dependencies
- make (for building automation)
- gfortran
- gdb (optional for debug)

Python Dependencies
- pyenv
- numpy
- cftime
- joblib

Make sure you have them properly installed before running the code.

# Running and Developing CAETÊ
~This section suposses you have a working python environment and an installed fortran compiler.
If you need help setting up your environment, check the [development environment section](#development-Environment)

CAETÊ uses both Python and Fortran and uses `f2py` module to create an library of code (functions and subroutines) that can be imported and used in python scripts. This means that the Fortran code must be compiled before you can run Python code.

The Makefile inside `/src` folder have useful automation to make it easier.

`clean_plsgen` - remove the cache files containing allocation combinations for PLS creation in plsgen.py. Run this will make the first run of caete in your computer slower but will make sure that you input data is ok!

`make clean` - it clear your python cache, deletes the `./output` folder and deletes all compiled fortran files, including the `.pyf` file.

`make so` - compiles all fortran code and creates the `caete_module.so`, the wraped Fortran for python.

To build and run CAETÊ you can do the following:
```bash
# Use pyenv to select your python executable
CAETE-DVM$ pyenv local <define your python version (>= 3.8.1)>

# Goto the source file
CAETE-DVM$ cd src

# Clean you cache
CAETE-DVM/src$ make clean_plsgen
CAETE-DVM/src$ make clean

# Build caete_module.so
CAETE-DVM/src$ make so

# Run the model, -i will leave you in the python shell that executed model_driver.py
CAETE-DVM/src$ python -i model_driver.py

# At this point the model will ask you if it will run in sombrero
# Just say: n<enter>
```
The results are saved as compressed pickled python dictionaries for each gridcell for each each spinup realized by the `grd.run_caete` method in a numerated file with the extension `.pkz` in the folder `./ouputs/gridcellYX/spinZ.pkz` where Y and X are the geographic coordinates of the grid cell object anx Z is the number of consecutive spinups that the `grd.run_caete` method realizes.

If you want to check your outputs (I reccomend =P):

```bash
# go to the ouputs folder for a specific grid cell
CAETE-DVM$ cd ouputs/gridcellYX # where Y & X are the indexes

# open the python interpreter
CAETE-DVM/outputs/gridcellYX$ python
```
```python
# To open the ouputs and check the file contents
>>> import joblib
>>> # Open the file containing the 5th spinup realized
>>> # edit the functions apply_fun() in model_driver.py script
>>> with open("spin05.pkz", 'rb') as fh:
>>>    dt = joblib.load(fh)
>>> dt.keys() # list the available keys for the ouputs
>>> # PLot some variables
>>> import matplotlib.pyplot as plt
>>> plt.plot(dt['npp'])
>>> plt.show()
>>> plt.plot(dt['area'].T)
>>> plt.show()
>>> plt.plot(dt['u_strat'][0,:,:].T, 'x')
>>> plt.show()
```

# Development Environment
If you need help configuring your development environment, installing python, installing CAETÊ dependencies or setting up a debug enviornment in vscode, check the [CAETÊ starting pack tutorial](https://github.com/fmammoli/CAETE-Tutorials)

## __AUTHORS__:

 - Bianca Rius
 - David Lapola
 - Helena Alves
 - João Paulo Darela Filho
