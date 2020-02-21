import math as m

#1st trying for implementing LPJ allometric equations

#LA: average individual leaf area (m2)
#SA:  is the sapwood cross sectional area (m2)
#kla_sa: is a constant equal to 8000 (unitless) (Table 3 in Sitch et al., 2003)
#Cleaf: carbon on leaves compartments (kgC/m2)
#SLA: specific leaf area (m2/kgC)
#CA: crown area (m2)
#k_allom1: allometric constant equal to 100 (Table 3 Sitch et al., 2003)
#krp: allometric constant equal to 1.6 (Table 3 Sitch et al., 2003)
#P50: xylem tension at which 50% of hydraulic conductivity was lost (MPa)
#D: diameter (m)
#k_allom2: allometric constant equal to 40 (Table 3 Sitch et al., 2003)
#k_allom3:  allometric constant equal to 0.5 (Table 3 Sitch et al., 2003)
#dwood: wood density (g/cm-3) 
#H: height (m)
#LAI: leaf area index (unitless)
#FPCind: foliage projective cover for an average individual

SLA=21.7 #m2/kgC (calculado a partir da equação de Sitch et al 2003 (Eq. 6))
Cleaf=0.3 #kgC/m2 (valor determinado por nós)
kla_sa=8000 #unitless
P50=-1.5 #MPa (valor determinado por nós)
Cstem=12 #kgC/m2 !!na realidade é Csap +Cheart, porém utilizamos um valor genérico
D=0.2 # m (valor determinado por nós)
k_allom1=100
krp=1.6
k_allom2=40
k_allom3=0.5



#LA=kla_sa*SA #original equation (Sitch et al., 2003)

#SLA=LA/Cleaf #original equation (Seiler et al., 2014)

#LA=Cleaf*SLA #deriving

#SA=LA/kla_sa #derivering

#dwood=(0.259+(-0.05921*(P50)))*(10**3)*1.55 #Equação obtida em Langan, 2017 --> tese (kg/m3)

height=[]

wood_density=[0.35,0.5,0.75,1.25,1.5]
carbon_stem=[5,12]#,18,15]
carbon_leaf=[0.2,1.0]#,0.4,0.8,0.3]
specif_leaf_area=[20.3,25,21.7]#,26,22.5]

for dwood in wood_density:
    for Cstem in carbon_stem:
        for Cleaf in carbon_leaf:
            for SLA in specif_leaf_area:
	            D=((4+(Cstem))/((dwood)*3.14*40))**(1/(2+0.5)) # ATENÇÃO: dwood deve ser colocado nessa equação em g/cm3 --> dwood/1000 transforms from kg/m3 to g/cm3

	            H=k_allom2*(D**k_allom3)

	            CA=k_allom1*(D**krp)

	            SA=Cleaf*SLA/kla_sa

	            LAIind=(Cleaf*SLA)/CA

	            nind=D**(-1/6)

	            FPCind=1-m.exp(-0.5*LAIind)

	            FPCgc=CA*nind*FPCind

	            height.append(H)

print(max(height))
print(sorted(height))

for i in height:
    if i=(max(height)):
        print('ok')


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
