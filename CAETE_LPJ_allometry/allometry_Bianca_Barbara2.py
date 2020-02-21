import math as m

#1st trying for implementing LPJ allometric equations

#LA: leaf area (m2) (Sitch et al., 2003)

#SA:  sapwood cross sectional area (m2) (Sitch et al., 2003)

#kla_sa: is a constant equal to 8000 (unitless) (Table 3 in Sitch et al., 2003)

#Cleaf: carbon on leaves compartments (kgC/m2)

#Cstem: carbon on stem (kgC/m2) (it is actually Csapwood +Cheartwood, but for now, a general value will be used

#SLA: specific leaf area (m2/kgC) (will be a variant trait)

#CA: crown area (m2) (Sitch et al., 2003)

#k_allom1: allometric constant equal to 100 (Table 3 Sitch et al., 2003)

#krp: allometric constant equal to 1.6 (Table 3 Sitch et al., 2003)

#P50: xylem tension at which 50% of hydraulic conductivity was lost (MPa)

#D: diameter (m) (Smith et al 2001 - SP material)

#k_allom2: allometric constant equal to 40 (Table 3 Sitch et al., 2003)

#k_allom3:  allometric constant equal to 0.5 (Table 3 Sitch et al., 2003)

#dwood: wood density (g/cm-3) (still not known if will be a variant tait or will be derived from P50 (equation in the thesis of Langan,2017-ATENTION: in Langan's equation P50 is kg/m3))

#H: height (m) (Sitch et al., 2003)

#LAI: leaf area index (unitless)(Sitch et al., 2003)

#FPC_pls: foliage projective cover for a PLS (Sitch et al., 2003)

#FPC_pls_gc: overall fractional coverage of a PLS in a grid cell (Sitch et al., 2003)

#APAR= absorbed photosynthetically active radiation (Smith et al. 2001,thesis). This variable will be calculated for each layer in future

#light_ext_factor: light extinction coefficient (Smith et al., 2001, thesis). We will use a general value (0.5)

#nind: number of individuals derived from diameter (selfithing law)

#rad: radiation

SLA=21.7 
Cleaf=0.3 #value for testing purpose
kla_sa=8000 
P50=-1.5 #value for testing purpose
Cstem=12 ##value for testing purpose
k_allom1=100
krp=1.6
k_allom2=40
k_allom3=0.5
light_ext_factor=0.5
rad=100
#dwood=(0.259+(-0.05921*(P50)))*(10**3)*1.55 

height=[] #creates an empty list to receive the height values
layer_number=[] #empty list for the size of each layer

LAI_pls_list = []

layer = 0
wood_density = [0.35, 0.5]
carbon_stem=[5] #,18,15]
carbon_leaf=[0.2]#,0.4,0.8,0.3]
specif_leaf_area=[20.3]#,26,22.5]


for dwood in wood_density:
    for Cstem in carbon_stem:
        for Cleaf in carbon_leaf:
            for SLA in specif_leaf_area:
                
                D=((4+(Cstem))/((dwood)*3.14*40))**(1/(2+0.5)) 

                SA=Cleaf*SLA/kla_sa
                
                H=k_allom2*(D**k_allom3)
                height.append(H)

                CA=k_allom1*(D**krp)

                SA=Cleaf*SLA/kla_sa

                LAI_pls=(Cleaf*SLA)/CA

                LAI_pls_list.append(LAI_pls)

                light_extinction=1-m.exp(-light_ext_factor*LAI_pls)
                
                APAR=0.5*rad*light_extinction
                # print(APAR,light_extinction)
                height.append(H)
	            
max_height = max(height)
                
num_layer = max_height / 5
                
num_layer = round(num_layer - 0.5)
print(num_layer)

layer_size = max_height / num_layer

num_layer_counting = 0

while num_layer_counting < max_height:
    num_layer_counting = num_layer_counting + layer_size
    layer_number.append(num_layer_counting)           

###
camada = []
for x in range(len(height)):
	for y in range(len(layer_number)):
		if height[x] <= layer_number[y]:
			camada.append(y + 1)

received_sun = [x for x in range(len(camada))]
max_sun = 100
for i in range(len(camada), 0, -1):
	if i == len(camada):
		received_sun[i - 1] = max_sun
	else:
		received_sun[i - 1] = received_sun[i] * 0.8
	
print(received_sun)

    # if height[hgt] <= layer_number[hgt]:
    #     camada = 1
    # if hgt <= layer_number[1]:
    #     camada = 2
    




                
                #layer=layer+layer_size
                #while layer <= max_height:
                 #   print(layer)
                
                
                #layer_size=max_height/num_layer
                
               

#print(max(height))
#print(sorted(height))
#print(max_height)
#print(num_layer)
#print(layer_size)
#       print(layer)

#for i in height:
 #   if i==(max(height)):
  #      print('ok')


    # height.append(H)

	#print('sa=', SA)
	#print('ca=', CA)
	#print('lai=', LAIind)
	#print('H=', H)
	#print('SLA=', SLA)
	#print('D=', D)
	#print('dwood',dwood)
	#print('FPCind', FPCind)
	#print('nind', nind)
	#print('FPCgc', FPCgc)
