program allometry_2
 use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
 use allometry
 use constants
 use allocation
 implicit none

 real(REAL64):: d
 real(REAL64):: c
 real(REAL64):: h2
 real(REAL64):: delta_leaf
 real(REAL64):: delta_root
 real(REAL64):: delta_sapwood
 
 call leaf_carbon(delta_leaf)
 call root_carbon(delta_leaf,delta_root)
 call sapwood_carbon(delta_leaf,delta_root,delta_sapwood)
 print*,'delta_leaf=', delta_leaf,'delta_root=',delta_root,'delta_sapwood=', delta_sapwood
 
 d = diam(dw,pi,delta_sapwood)
 print*, d, dw,pi

 c = area_crown (k_allom1, krp, d)
 print*, 'AREA DA COPA=', c

 h2 = tree_height (k_allom2, k_allom3, d)
 print*, 'ALTURA =', h2
end program allometry_2