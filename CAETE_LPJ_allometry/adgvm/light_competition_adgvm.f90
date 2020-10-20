program light_competition__agdvm2

	integer, parameter::npls=14
	integer::i,j
    real, dimension(npls), allocatable :: height (:) !unit: m
    real, dimension(npls), allocatable :: diam (:) !tree diameter in m
    real, dimension(npls), allocatable :: rad (:) !canopy radius in m
    real, dimension(npls), allocatable :: crown_area (:) !crown area in m
	real, dimension(npls), allocatable :: LAI (:) !leaf area index (m2/m2)
!constant values
	real::b2=2.6 !allometric constants for height equation (the assigned value was a mean between 
!the max and min used by Langan, 2018(thesis, table 3.3))    
	real::b1=0.45 !allometric constants for height equation (the assigned value was a mean between 
!the max and min used by Langan, 2018(thesis, table 3.3))
	real::pi=3.14159 
	real::c1=10.0!allometric constants for canopy radius equation used by Langan, 2018(thesis, table 3.3))
	real::spec_leaf = 21.7 !generic value to calculate leaf area index (LAI); SLA will be a variant trait

! Variables with generic values for testing the logic code
	real, dimension(npls) :: dwood !wood density (g/cm-3) *Fearnside, 1997 - aleatory choices
    real, dimension(npls) :: carbon_stem !KgC/m2 (Cheart + Csap)
    real, dimension(npls) :: carbon_leaf !KgC/m2 

    carbon_stem=(/27.,32.,27.2,25.3,8.8,9.7,12.5,11.5,10.,38.6,27.23,10.3,6.8,9.9/)
    carbon_leaf=(/2.15,3.,1.18,1.6,1.5,1.8,0.3,2.,0.8,.84,0.25,1.,0.2,1.7/)
    dwood=(/0.74,0.73,0.59,0.52,0.41,0.44,0.86,0.42,0.64,0.69,0.92,0.60,0.36,0.99/)

!Allometric equations (Langan 2018, thesis)

	height=exp((log(carbon_stem*2.0)+b1)/b2) !the multiplication by 2 is to convert from carbon content to biomass

	diam=2.0*(sqrt((carbon_stem*2.0)/(pi*(dwood*100.)*height))) !*100 is because in adgvm2 the unit it kgC/cm-3
		                             							!the multiplication by 2 is to convert from carbon content to biomass
	rad=c1*diam															
	
	crown_area=(rad**2)*pi

	LAI=((carbon_leaf*2)*spec_leaf)/crown_area
	
	print*,'H', height, 'D', diam, 'CA',crown_area ,'LAI', LAI
	
end program light_competition__agdvm2