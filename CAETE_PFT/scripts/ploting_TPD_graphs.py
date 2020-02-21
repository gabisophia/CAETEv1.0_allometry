import pandas as pd 
import matplotlib.pyplot as plt
import math
import numpy as np
import matplotlib.axes
 




npls=['50','100','200','500','1000','3000']

aleaf_rich=[0.111,0.103,0.113,0.098,0.106,0.122]

aleaf_eve=[0.446,0.38,0.415,0.412,0.401,0.377]

aleaf_div=[0.811,0.845,0.9,0.896,0.861,0.872]

aroot_rich=[0.049,0.066,0.058,0.052,0.054,0.087]

aroot_eve=[0.451,0.5,0.437,0.497,0.575,0.409]

aroot_div=[0.181,0.857,0.217,0.42,0.726,0.157]

awood_rich=[0.084,0.115,0.099,0.092,0.12,0.119]

awood_eve=[0.591,0.501,0.586,0.577,0.518,0.499]

awood_div=[0.778,0.877,0.78,0.83,0.838,0.863]

tleaf_rich=[2.218,2.688,2.727,2.842,2.938,3.149]

tleaf_eve=[0.662,0.602,0.575,0.567,0.57,0.557]

tleaf_div=[0.724,0.791,0.824,0.837,0.828,0.839]

troot_rich=[1.501,1.845,1.233,1.568,1.558,1.482]

troot_eve=[0.77,0.592,0.761,0.628,0.673,0.685]

troot_div=[0.579,0.743,0.639,0.743,0.607,0.641]

twood_rich=[10.153,7.929,8.043,8.67,11.237,11.066]

twood_eve=[0.494,0.591,0.543,0.546,0.55,0.552]

twood_div=[0.796,0.734,0.692,0.813,0.749,0.729]


fig,ax=plt.subplots(3,2,sharex=True)
#ax1=fig.add_subplot(npls,aleaf_rich,color='limegreen')
#ax[0,0].plot(npls,aroot_rich,color='darkorange')
#ax[0,0].plot(npls,awood_rich,color='saddlebrown')
#ax[0,1].plot(npls,tleaf_rich,color='limegreen')
#ax[0,1].plot(npls,troot_rich,color='darkorange')


ax[0,0].plot(npls,aleaf_rich,color='limegreen',label='leaf')
ax[0,0].plot(npls,awood_rich,color='saddlebrown',label='wood')
ax[0,0].plot(npls,aroot_rich,color='darkorange',label='root')    
ax[0,0].set(ylabel='Richness')
ax[0,0].legend(bbox_to_anchor=(1.05,1),loc='upper left', borderaxespad=0.)

ax[0,1].plot(npls,tleaf_rich,color='limegreen',linestyle='--',label='leaf')
ax[0,1].plot(npls,troot_rich,color='darkorange',linestyle='--',label='wood')
ax[0,1].set(ylabel='Richness')
ax[0,1].legend(bbox_to_anchor=(1.05,1),loc='upper left', borderaxespad=0.)
ax[0,1]=ax[0,1].twinx()
ax[0,1].plot(npls,twood_rich,color='saddlebrown',linestyle='--',label='root')
ax[0,1].set(ylabel='Wood richness')
ax[0,1].legend(bbox_to_anchor=(1.05,0.7),loc='upper left', borderaxespad=0.)

ax[1,0].plot(npls,aleaf_eve,color='limegreen')
ax[1,0].plot(npls,aroot_eve,color='darkorange')
ax[1,0].plot(npls,awood_eve,color='saddlebrown')
ax[1,0].set(ylabel='Evenness')

ax[1,1].plot(npls,tleaf_eve,color='limegreen',linestyle='--')
ax[1,1].plot(npls,troot_eve,color='darkorange',linestyle='--')
ax[1,1].plot(npls,twood_eve,color='saddlebrown',linestyle='--')
ax[1,1].set(ylabel='Evenness')

ax[2,0].plot(npls,aleaf_div,color='limegreen')
ax[2,0].plot(npls,awood_div,color='saddlebrown')
ax[2,0].set(ylabel='Divergence')
ax[2,0]=ax[2,0].twinx()
ax[2,0].plot(npls,aroot_div,color='darkorange')
ax[2,0].set(ylabel='Root divergence')

ax[2,1].plot(npls,tleaf_div,color='limegreen',linestyle='--')
ax[2,1].plot(npls,troot_div,color='darkorange',linestyle='--')
ax[2,1].plot(npls,twood_div,color='saddlebrown',linestyle='--')
ax[2,1].set(ylabel='Divergence')

plt.subplots_adjust(hspace=0.4)
plt.subplots_adjust(wspace=0.4)
fig.text(0.5,0.03,'PLS number', ha='center')
plt.show()
