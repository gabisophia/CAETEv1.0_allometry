import pandas as pd 
import matplotlib.pyplot as plt

npls=['50','100','200','500','1000','3000']

pls_comb=['a','b','c','d','e']

rich=[40.98466,36.68932,43.84822,58.70292,42.23747,60.67161]

eve=[0.3761549,0.3223939,0.3924115,0.3829173,0.4327324,0.3877795]

div=[0.7892676,0.8383344,0.7709975,0.7960393,0.7559408,0.7636411]

dis=[0.5834462,0.5506618,0.2955311,0.2545723,0.2399428]

fig,ax=plt.subplots(2,2)

ax[0,0].plot(npls,rich,color='teal')
ax[0,0].set(xlabel='PLS number',ylabel='Hypervolume Richness')
ax[0,1].plot(npls,eve,color='darkviolet')
ax[0,1].set(xlabel='PLS number',ylabel='Hypervolume Evenness')
ax[1,0].plot(npls,div,color='gold')
ax[1,0].set(xlabel='PLS number',ylabel='Hypervolume Divergence')
ax[1,1].plot(pls_comb,dis,color='red')
ax[1,1].set(xlabel='PLS combination',ylabel='Hypervolume Dissimilarity')

plt.show()