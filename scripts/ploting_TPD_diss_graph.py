import pandas as pd 
import matplotlib.pyplot as plt

npls_combination=['a','b','c','d','e']

aleaf_dis=[0.3031832,0.2743100,0.2056312,0.1678248,0.1156269]
awood_dis=[0.2864319,0.3349341,0.2189795,0.1449641,0.0974870]
aroot_dis=[0.4915276,0.3517412,0.2381609,0.1710036,0.1401635]

tleaf_dis=[0.1960903,0.1927244,0.1556319,0.1801458,0.1346182]
twood_dis=[0.4975105,0.5923657,0.2362160,0.3653282,0.3993684]
troot_dis=[0.2432855,0.2469612,0.1634436,0.1945502,0.1216699]

fig,(ax1,ax2) =plt.subplots(1,2,sharey=True)
ax1.plot(npls_combination,aleaf_dis,color='limegreen',label='leaf')
ax1.plot(npls_combination,awood_dis,color='saddlebrown',label='wood')
ax1.plot(npls_combination,aroot_dis,color='darkorange',label='root')
ax1.legend()
ax1.set(ylabel='TPD dissimilarity')

ax2.plot(npls_combination,tleaf_dis,color='limegreen',linestyle='--',label='leaf')
ax2.plot(npls_combination,twood_dis,color='saddlebrown',linestyle='--',label='wood')
ax2.plot(npls_combination,troot_dis,color='darkorange',linestyle='--',label='root')
plt.legend()

fig.text(0.5,0.03,'PLS number comparison', ha='center')
plt.show()
