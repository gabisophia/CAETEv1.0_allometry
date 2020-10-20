program allometry_2
 use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
 use allometry
 use constants
 use allocation
 implicit none

 real(REAL64):: d
 real(REAL64):: delta_leaf
 real(REAL64):: delta_root
 real(REAL64):: delta_sapwood
 
 call leaf_carbon(delta_leaf)
 call root_carbon(delta_leaf,delta_root)
 call sapwood_carbon(delta_leaf,delta_root,delta_sapwood)
 print*,'delta_leaf=', delta_leaf,'delta_root=',delta_root,'delta_sapwood=', delta_sapwood
 d = diam(dw,pi,delta_leaf)
 print*, d, dw,pi

end program allometry_2