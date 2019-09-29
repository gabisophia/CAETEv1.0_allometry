import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nt 

variable = input('which variable do you want to see?  ')

raster = nt.Dataset(variable)

print(raster)

variable_name = input('what is the name of the variable? ')

d = raster.variables[variable_name][:]

operation = input('Do you want to see the mean or the total?  ')

if operation == 'mean':
    mean = np.mean(d,axis=0)
elif operation == 'total':
    mean = d

title = input('What is the title you want to put in your map?')

plt.imshow(np.flipud(mean))
plt.colorbar()
plt.title(title)
plt.show()

