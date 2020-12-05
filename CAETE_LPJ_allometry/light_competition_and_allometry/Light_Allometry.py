# ======================= COPYRIGHT 2020 - LABTERRA/UNICAMP ======================= #

#Logic developed in Fortran90 by Caio Fascina, Bianca Rius e Bárbara Cardeli
#Translated for Python by Luan Correa

######################################################################################

import numpy as np

#ALLOMETRIC EQUATIONS:

def diamf(carbon_stem,dwood):
    diam = ((4+(carbon_stem))/((dwood)*3.14*40))**(1/(2+0.5))
    return diam

def crown_areaf(diam,k_allom1,krp):
    crown_area = k_allom1*(diam**krp)
    return crown_area

# def height(diam, k_allom1, k_allom2):
#     height = k_allom2*(diam**k_allom3)
#     return height

def LAIf(carbon_leaf,spec_leaf,crown_area):
    LAI = (carbon_leaf*spec_leaf)/crown_area
    return LAI

#COMPETITION LIGHT DYNAMIC BY LAYERS:

class layer_array:
    def __init__(self, sum_height, num_height, mean_height, layer_height, sum_LAI, mean_LAI,beers_law,li,lu,la):
        self.sum_height=float(sum_height)
        self.num_height=int(num_height)
        self.mean_height=float(mean_height)
        self.layer_height=float(layer_height)
        self.sum_LAI=float(sum_LAI)
        self.mean_LAI=float(mean_LAI)
        self.beers_law=float(beers_law)
        self.li=float(li)
        self.lu=float(lu)
        self.la=float(la)

# VARIABLES and ARRAYS DECLARATION:

npls=26 #number of plants life strategy (PLS)
watt_rs=210 #shortwave radiation in watts/m2 is equal to joules/m2
k_allom1=100 #allometric constant (Table 3; Sitch et al., 2003)
k_allom2=36. #allometric constant (source: code Allocation_Philip.py)
k_allom3=0.22 #allometric constant (source: code Allocation_Philip.py)
krp=1.6 #allometric constant (Table 3; Sitch et al., 2003)
spec_leaf=15.3656070 #Variant trait to calculate LAI (source: code Allocation_Philip.py)

dwood=np.array([0.74,0.73,0.59,0.99,0.81,0.74,0.42,0.86,0.95,0.69,1.04,0.60,0.36,0.99,0.42,0.92,0.52,0.44,0.60,1.21,0.64,0.86,0.97,0.87,0.79,1.])

carbon_stem=np.array([7.,12.,7.2,8.3,8.8,9.7,7.5,11.5,10.,8.6,7.3,10.3,6.8,9.9,5.3,9.2,15.,12.6,10.7,11.4,7.8,9.9,8.7,7.0,9.0,8.8])

carbon_leaf=np.array([0.15,3.,0.18,0.6,1.5,1.8,0.3,2.,0.8,0.64,0.25,1.,0.2,1.7,0.64,0.6,1.8,0.18,2.,0.8,1.,0.7,1.,0.9,0.87,3.2])

height=np.array([26.1032295,28.2111950,27.4116554,10.5000000,30.2794037,25.5565338,31.3096733,10.2001667,28.6879177,30.0390587,25.8070259,12.0258957,10.9895632,4.65791554,16.1025983,17.0054975,13.0264972,11.0119962,22.0919556,14.0619921,11.1119961,3.0215868,4.8759647,6.5025757,9.6989797,3.5822647])

layer=[]

diam=diamf(carbon_stem,dwood)

crown_area=crown_areaf(diam,k_allom1,krp)

LAI=LAIf(carbon_leaf,spec_leaf,crown_area)

#LAYERS DYNAMICS:

max_height = max(height)
print("max_height",max_height)

num_layer = int((max_height/5))
print("num_layer",num_layer)

last_with_pls=num_layer
layer_size = max_height/num_layer
print("layer_size",layer_size)

layer=[layer_array(0,0,0,0,0,0,0,0,0,0)]
for i in range(1,num_layer):
    layer.append(layer_array(0,0,0,0,0,0,0,0,0,0))


for i in range(1,num_layer+1):
    layer[i-1].layer_height=layer_size*i
    print("layer_height", layer[i-1].layer_height, i)
for i in range(num_layer):
    print(layer[i].layer_height)
for i in range(num_layer):
    for j in range(npls):
        if(i==0):
            if((layer[i].layer_height>=height[j])):
                layer[i].sum_height=layer[i].sum_height+height[j]
                layer[i].num_height=layer[i].num_height+1
                layer[i].sum_LAI=layer[i].sum_LAI+LAI[j]

        else:
            if((layer[i].layer_height>=height[j]) and (layer[i-1].layer_height<height[j])):
                layer[i].sum_height=layer[i].sum_height+height[j]
                layer[i].num_height=layer[i].num_height+1
                layer[i].sum_LAI=layer[i].sum_LAI+LAI[j]

    if(layer[i].num_height!=0):
        layer[i].mean_height=layer[i].sum_height/layer[i].num_height
    if(layer[i].sum_height==0):
        layer[i].mean_height=0
    if(layer[i].num_height!=0):
        layer[i].mean_LAI=layer[i].sum_LAI/layer[i].num_height
        print("mean_LAI",layer[i].mean_LAI)
    if(layer[i].sum_LAI==0):
        layer[i].mean_LAI=0
        print("mean_LAI2",layer[i].mean_LAI)
    print("lyr",i+1,"mean_height", layer[i].mean_height,"lai",layer[i].mean_LAI)
for i in range(num_layer):
    print(layer[i].mean_LAI)
    print(layer[i].mean_height)
    print(i)

#IPAR - incidence photosynthetically active radiation:
# é a quantidade de radiação capturado por várias 
# camadas do dossel conforme o incidente PAR 
# no topo do dossel viaja para baixo através 
# do dossel camadas para o chão.
short_rad=watt_rs
print("short_rad",short_rad)

#APAR - absorved photosynthetically active radiation:
# quantidade de IPAR que é absorvida pelos organismos
# vegetais.
APAR=0.5*short_rad
print("APAR",  APAR)

#LIGHT EXTINCTION - COMPETITION DYNAMIC:

for i in range(num_layer-1,-1,-1):
    layer[i].beers_law=APAR*(1-np.exp(-0.59*layer[i].mean_LAI))
    print("law",layer[i].beers_law)
for i in range(num_layer-1,-1,-1):
    if(i==num_layer-1):
        layer[i].li=APAR
    else:
        if(layer[i].mean_height>0):
            layer[i].li=layer[last_with_pls-1].la
            last_with_pls=i+1
    layer[i].lu=layer[i].li*(1-np.exp(-0.59*layer[i].mean_LAI))
    layer[i].la=layer[i].li-layer[i].lu
    print(i+1, 'inc', layer[i].li, 'used', layer[i].lu,'avai',layer[i].la)
