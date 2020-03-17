# LPJ allometric equations

import math as m

SLA = 21.7 # Specific leaf area (m2/kgC), it will be a variant trait.
Cleaf = 0.3 # Carbon on leaves compartments (kgC/m2). 
kla_sa = 8000 # Constant equal to 8000 (unitless) / Table 3 in Sitch et al., 2003.
P50 = -1.5 # xylem tension at which 50% of hydraulic conductivity was lost (MPa). 
Cstem = 12 # Carbon on stem (kgC/m2) (it is actually Csapwood + Cheartwood, but for now, a general value will be used).

#k_allom1: allometric constant equal to 100 (Table 3 Sitch et al., 2003)
k_allom1 = 100

#krp: allometric constant equal to 1.6 (Table 3 Sitch et al., 2003)
krp = 1.6

#k_allom2: allometric constant equal to 40 (Table 3 Sitch et al., 2003)
k_allom2 = 40

#k_allom3:  allometric constant equal to 0.5 (Table 3 Sitch et al., 2003)
k_allom3 = 0.5

#light_ext_factor: light extinction coefficient (Smith et al., 2001, thesis). We will use a general value (0.5)
light_ext_factor = 0.5

#rad: radiation !!!!(UNIT NEED TO BE FOUND)!!!. The following value is for tersting purpose.
rad = 100

#dwood: wood density (g/cm-3) (still not known if will be a variant tait or will be derived from P50 
#(equation in the thesis of Langan,2017-ATENTION: in Langan's equation P50 is kg/m3))
#dwood=(0.259+(-0.05921*(P50)))*(10**3)*1.55 

#creates an empty list to receive the height values for each PLS
height = []

#creates an empty list to receive the height values for each PLS 
LAI_pls_list = [] #creates an empty list to receive the LAI values for each PLS

##creates an empty list to receive the crown area values for each PLS
Crown_area_list=[]

###creates an empty list to receive the FPC for each PLS
FPC_pls_list=[]

#creates an empty list to receive the ~number of individuals per PLS
Nind_pls_list = []

##creates an empty list to receive the diameter PLS
Diameter_list=[]


#initializing the counter for the total grid cell FPC
FPC_gc_total = 0

#Following list with values for testing the code logic
wood_density = [0.35]
carbon_stem = [1,2,5,10,11] 
carbon_leaf = [0.3]
specif_leaf_area = [22.3]


for dwood in wood_density:
	for Cstem in carbon_stem:
		for Cleaf in carbon_leaf:
			for SLA in specif_leaf_area:

				#D: diameter (m) (Smith et al 2001 - SP material)
				D = ((4+(Cstem))/((dwood)*3.14*40))**(1/(2+0.5))

				#allocates diameter values for each PLS
				Diameter_list.append(D) 

				#LA: leaf area (m2) (Seiler et al., 2014 (eq. 4))
				LA = Cleaf*SLA

				#SA:  sapwood cross sectional area (m2) (Sitch et al., 2003)
				SA = LA/kla_sa
				
				#H: height (m) (Sitch et al., 2003)
				H = k_allom2*(D**k_allom3)

				#allocates heights values for calculating layers and PAR
				height.append(H)

				#CA: crown area (m2) (Sitch et al., 2003)
				CA = k_allom1*(D**krp)

				#allocates crown area values
				Crown_area_list.append(CA)

				#LAI_pls: leaf area index of a PLS (unitless)(Sitch et al., 2003)
				LAI_pls = (Cleaf*SLA)/CA

				#allocates LAI for each PLS values for calculating layers and PAR
				LAI_pls_list.append(LAI_pls)

				#calculates how much of the light will be extincted accordin to the LAI_pls
				light_extinction = 1-m.exp(-light_ext_factor*LAI_pls)
				
				#APAR= absorbed photosynthetically active radiation (Smith et al. 2001,thesis). 
				APAR = 0.5*rad*light_extinction

				#calculates individual foliage projective cover (FPC_pls) (unitless)(Sitch et al., 2003) 
				FPC_pls=1-m.exp(-light_ext_factor*LAI_pls)
				
				#allocates heights values for calculating layers and PAR
				FPC_pls_list.append(FPC_pls)

				#calculates the number of individuals per PLS (Smith, 2001, thesis)
				Nind = D**(-1.6)

				#allocates number of individuals per PLS
				Nind_pls_list.append(Nind)

				#calculates the fractional projective cover in a grid cell (Sitch et al., 2003)
				FPC_gc = D*Nind*FPC_pls


				#Calculating total grid cell FPC
				FPC_gc_total = FPC_gc_total+FPC_gc
print(LAI_pls_list)

#empty list for receive the size of all layers
size_layer_list = [] 

#found the heighest PLS in a grid cell
max_height = max(height)

#calculates the number of layers depending on the max_height. The number of 5 layers was decided in a meeting.               
num_layer = max_height / 5

#turns num_layer as an integer number, rounding for the smaller number                
num_layer = round(num_layer - 0.5)

#calculates the size (m) of each layer
layer_size = max_height / num_layer

#initialize the counter for creating a list with the size of all layers
size_layer_counting = 0

while size_layer_counting < max_height:
	size_layer_counting = size_layer_counting + layer_size
	size_layer_list.append(size_layer_counting)

print(size_layer_list)
print(max_height)

layer_id = []


# Determines the range of values since it will be variable
for height_value in range(len(height)):

	for layer_idx, layer_value in enumerate(range(len(size_layer_list))):
		# determines in which layer each pls is loccated
		if height[height_value] <= size_layer_list[layer_value]:
			layer_id.append(layer_idx)
			break

###########################################
# import pandas as pd

# data = {
#     "height": pd.Series(height),
#     "layer_id": pd.Series(layer_id),
#     "LAI_pls_list": pd.Series(LAI_pls_list)}
# data_frame = pd.DataFrame(data)
# print(data_frame)


# def received_light(current_light, current_lai):
# 	light_applyied = current_light - LAI_pls_list[current_light + 1]
	
# 	return light_applyied * total_sun * pow(1 - e, (0.5 * lai))

########################################
#testing receiving sun logic###
max_sun = 100
sun=[]
y=max(layer_id)
list_var=zip(layer_id,height,LAI_pls_list)
for i in list_var:
	if i[0]==y:
		received_sun=max_sun*(1-m.exp(-0.5*i[2]))
	else:
		received_sun=max_sun-20
	sun.append(received_sun)

print(sun)

#################################



for idx, value in enumerate(sorted(LAI_pls_list, reverse = True)):
	for layer in sorted(layer_id, reverse = True):
		if idx == layer:
			#print(f"Layer: {layer} / LAI: {value}", end = "")
			#print("Aplicar")

			break


# 	if i == len(layer_id):
# 		received_sun[i - 1] = max_sun
# 	else:
# 		received_sun[i - 1] = received_sun[i] * 0.8


# print(received_sun)




   




		   
			   


