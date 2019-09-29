import pandas as pd 
import matplotlib.pyplot as plt
 

fig,ax =plt.subplots(2,2) #crates a matrix figure type with 2 lines and 2 columns

npls=['50','100','200','500','1000','3000']

npls_combination=['a','b','c','d','e']

hv=[84.667873, 117.136317, 122.939851, 157.530758, 118.279915, 98.619805]

hv_dist=[1.858193,3.918858,1.900645,1.828473,0.2388126] 

hv_intersection=[46.63591,47.40818,56.73189,57.14257,71.23256]

hv_overlap=[0.4621897,0.3949428,0.4045479,0.4143609,0.6568248]

ax[0,0].scatter(npls,hv,color='grey',alpha=0.7)
ax[0,0].set(xlabel='PLS number',ylabel='Hypervolume size')
ax[0,1].scatter(npls_combination,hv_dist,color='#107ab0',alpha=0.7)
ax[0,1].set(xlabel='PLS number comparison', ylabel="Centroid's hypervolume \n  distance")
ax[1,0].scatter(npls_combination,hv_intersection,color='g',alpha=0.7)
ax[1,0].set(xlabel='PLS number comparison', ylabel='Hypervolume intersection')
ax[1,1].scatter(npls_combination,hv_overlap, color='#ffa62b',alpha=0.7)
ax[1,1].set(xlabel='PLS number comparison', ylabel='Hypervolume overlap')
plt.subplots_adjust(hspace=0.4)
plt.subplots_adjust(wspace=0.4)
plt.show()
