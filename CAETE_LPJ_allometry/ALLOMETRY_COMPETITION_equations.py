import math as m

#LIGHT COMPETITION MODULE - CAETÊ
    #ALLOMETRIC EQUATIONS / GRID-CELL PROPERTIES / ABSORBED PHOTOSYNTHETIC ACTIVE RADIATION
#This equations is from LPJ-Model by Stich et al., 2003. The constants and formulations are be describe in the follow:
                            #Write by Bárbara Cardeli, Bianca Rius, Caio Fascina and David Lapola (02/2020)

########################################################################################################################
            #1. DEFINITIONS

#ALLOMETRIC EQUATIONS - VARIABLES
 #LA: Average individual Leaf Area (m2);
 #SA: Sapwood cross sectional area (m2);
 #SLA: Specific Leaf Area (kgC/m2);
 #LAI: Leaf Area Index (m2/m2);
 #CA: Crown Area (m2);
 #D: Stem diameter (m);
 #H: height (m);
 #P50: Potential of loss 50% conductivity of xylem (mPA);
 #dwood (or WD): wood density (kg/m3);
 #Cleaf: Carbon on leaves compartments (kgC/m2);
 #Cstem: Csapwood + Cheartwood - Carbon on stem compartments (kgC/m2);

#ALLOMETRIC EQUATIONS - CONSTANTS (from Sitch et al., 2003 - Table 3)
 #kla_sa: allometric constant relates to sapwood cross area, equal to 8000 (unitless);
 #k_rp: allometric constant equal to 1.6 (unitless);
 #k_allom1: allometric constant equal to 100 (unitless);
 #k_allom2: allometric constant equal to 40 (unitless);
 #k_allom3: allometric constant equal to 0.5 (unitless);

#GRID-CELL PROPERTIES - VARIABLES
 #FPCind: Average individual Foliage Projective Cover;
 #FPC: Foliage Projective Cover (population - occupation of grid cell) - maybe m2
 #Nind (or P or N only): population density;

#APAR / fAPAR (Absorbed - or fractional - Photosynthetic Active Radiation) - VARIABLES
            #Definiton: APAR, at the canopy level, is computed assuming that 50% of incoming shortwave radiation
            #is of wave lenghts usable for photosynthesis.
 #light_ext_factor: light extinction coefficient - the general values for this is 0.5 for vegetations;
 #rad: Short wave RADIATION;
 #ligh_extinction (or k_l): the light extinction relates to LAI and Beer's law;
        #ATTENTION! Na literatura de Sakschewski (2014) o modelo utilizado é o LPJ-mLFIT, este é um modelo individual
        #que no cálculo da competição por luz e extinção da Luz utiliza de LAYERS, isto é: camadas da vegetação de
        #diferentes alturas. Cada Layer vai possuir um fAPAR de acordo com o LAI individual de cada organismo
        #presente no layer.


########################################################################################################################

            #2. VALUES OF VARIABLES & CONSTANTS

#ALLOMETRIC VARIABLES & CONSTANTS
SLA= 21.7 #m2/kgC - Calculado a partir da equação de Sitch et al., 2003 (Eq. 6)
Cleaf= 0.3 #kgC/m2 - valor genérico
Cstem= 12 #kgC/m2 - valor genérico
D= 0.2 #m - valor genérico
P50= -1.5 #mPA - valor genérico, segundo Langan, 2017 (supplementary) P50 pode variar de -3 (mín.) a -0.2 (máx.)
kla_sa= 8000
k_rp= 1.6
k_allom1= 100
k_allom2= 40
k_allom3= 0.5

#GRID-CELL PROPERTIES
e= 2.7182818285 #número/constante de Euler

             #3. EQUATIONS

#ALLOMETRIC EQUATIONS - ORIGINALS and DERIVATION

LA=Cleaf*SLA #deriving - LA=kla_sa*SA (original from Sitch et al., 2003)
SLA = LA / Cleaf  # (original from Seile et al., 2014)
SA=LA/kla_sa #(deriving from Leaf Area equation - Sitch et al., 2003)
CA=k_allom1*(D**k_rp)
LAIind=(Cleaf*SLA)/CA #average individual LAI - use in FPCind and APAR
H=k_allom2*(D**k_allom3)
dwood=(0.259+(-0.05921*(P50)))*(10**3)*1.55 #From Langan, 2017 (tesis) / unit: kg/m3
D=((4+(Cstem))/((dwood/1000)*3.14*40))**(1/(2+0.5)) #ATTENTION: dwood must be put in this equation in g/cm3 & dwood/1000 transforms from kg/m3 to g/cm3

#GRID-CELL PROPERTIES EQUATIONS (from Sitch et al., 2003; Smith, 2001)
Nind=D**(-1/6)
FPCind=1-e**(-0.5*LAIind)
FPCgc=CA*Nind*FPCind

#APAR / fAPAR EQUATIONS (from Smith, 2001; Sakschewski et al., 2014)
rad=100
light_ext_factor=0.5
light_extinction= 1-e**(-light_ext_factor*LAIind)
APAR=0.5*rad*light_extinction #from Smith, 2001 p. 9

print('SA=', SA)
print('CA=', CA)
print('LAI=', LAIind)
print('H=', H)
print('SLA=', SLA)
print('D=', D)
print('dwood',dwood)
print('FPCind', FPCind)
print('Nind', Nind)
print('FPC', FPC)
print ('APAR', APAR)

#LAYERS - Different heights and LAI relates to APAR and the competition dynamic
            #Write by Bianca Rius and Caio Fascina

height = []  # An empty list to receive the height values for each PLS
LAI_pls_list = []  # An empty list to receive the LAI values for each PLS
LAI_pls_list.append(LAIind)  # allocates LAI for each PLS values for calculating layers and PAR

size_layer_list=[] #empty list for receive the size of all layers
max_height= max(height) #found the heighest PLS in a grid cell
num_layer=max_height/5 #calculates the number of layers depending on the max_height. Will be 5 layers;
num_layer=round(num_layer-0.5) #Turns "num_layer" as an integer number, rouding for the smaller number;
layer_size=max_height/num_layer #calculates the size (m) of each layer;

size_layer_couting=0 #initialize the counter for creating a list with the size of all layers
while size_layer_couting < max_height:
    size_layer_couting = size_layer_couting + layer_size
    size_layer_list.append(size_layer_couting)

#DESCONSIDER FOR NOW:

                #PARA CONTAR OS NÚMEROS DE CAMADAS QUE EXISTIRÃO
#camada =[]
#for x in range (len(height)):
#   for y in range (len(layer_number)):
#       if height [x] <= layer_number[y]:
#               camada.append(y+1)

                #PARA SABER A QUANTIDADE DE SOL RECEBIDA EM CADA CAMADA
#received_sun = [x for x in range(len(camada))]
#max_sun=100
#for i in range(len(camada), 0, -1):
#       if i == len(camada):
#               received_sun[i-1] = max_sun
#       else:
#               received_sun[i-1] = received_sun[i]*0.8

#print(received_sun)


            #MORTALITY
        #Write by Barbara Cardeli

#Tanto o LPJ-Model (Sitch et al., 2003) quanto o aDGVM2 (Langan et al., 2017) possuem suas próprias formulações
        #e relações para se calcular a mortalidade dos individuos (ou individuos médios). Ao que segue:

    #1. LPJ-Model (Sitch et al. 2003)
#Time-Step de mortalidade anual;
#Mortalidade faz a dinâmica de densidade populacional;
#Ao que interessa, a mortalidade ocorre como resultado de:
        #Competição por luz (mortshade / somátoria de FPC (da célula de grade) > 0.95 (ou 95%);
        #Baixo/lento crescimento dos individuos médios (mort_greff) - growth efficiency;
        #Mortalidade dependente da densidade da mandeira (trade-off entre WD e mortalidade)

    #2. aDGVM2 (Langam et al., 2017)
#Time-Step de mortalidade anual;
#Ao que interessa, a mortalidade ocorre como resultado de:
        #Instabilidade mecânica;


    #MORTALITY - VARIABLES
C_year=2 #NECESSARY FOUND THE UNIT - quantidade de Carbono alocado no ano
Cleaf=0.3 #kgC/m2 - valor genérico
SLA=21.7 #kgC/m2 - valor genérico
k_mort1=0.01 #yr-1 - Asymptotic maximum mortality rate (Sitch, 2003)
k_mort2=0.5 #unitless
dwood=0.26 #g/cm3
Mi=10 #Mortality parameter - Table 4. (aDGVM2 - Langan, 2017)
D=0.56 #m
H=17 #H=height[] - An empty list to receive the height values for each PLS

    #MORTALITY - EQUATIONS
greff=C_year/Cleaf*SLA #Sitch, 2003
Hcrit=0.79*((((11.852*dwood+37)/9.81)*dwood)**1/3)*D**2/3 #Langan, 2017
Prob_mort=(Mi*(H/Hcrit-1)/1000) #Probabilidade de mortalidade como resultado da instabilidade mecânica;
mort_WD=-2.66+(0.255/dwood) #Equation derived of King 2006 (retirado de: Sakschewski, 2016); #UTILIZA-SE LOG NO INICIO
#DA OPERAÇÃO
MORT_greff=mort_WD/1+k_mort2*greff

print('growth efficiency', greff)
print('Mortality by growth efficiency', MORT_greff)
print('Height critic', Hcrit)
print('Prob. of mortality by mechanic instability', Prob_mort)
print('Mortalidade to WD', mort_WD)

    #FOLIAGE PROJECTIVE COVER (FPCind) & FRACTIONAL PROJECTIVE COVER (FPCgc)
Nind=D**(-1/6) #self-thinning
FPCind=1-e**(-0.5*LAIind)
FPCgc=CA*Nind*FPCind

#############o FPCgc não pode ser um número inteiro. Criar função para o mesmo.#################

LAI_pls_list = []  #An empty list to receive the LAI values for each PLS
LAI_pls_list.append(LAIind)  #allocates LAI for each PLS values for calculating FPCind
Crown_Area_list.append(CA) #allocates Crown Area (CA) for each PLS values for calculating FPC
Foliage_Projective.append(FPCind) #allocates Foliage Projective Cover for each PLS values for calculating FPC

############Criar um IF para mortalidade com função de soma de todas as variáveis.##############

Fractional_Cover.append(FPCgc) #allocates Fractional Projective Cover for each PLS values for calculating mort_shade
FPCgc_couting=0 #initialize the counter for creating a list with the size of all layers


#       if soma_FPC > 0.95 = mortshade







