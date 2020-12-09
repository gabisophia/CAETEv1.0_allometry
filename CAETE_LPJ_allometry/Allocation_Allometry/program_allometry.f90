program allometry_2
 use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
 use allometry
 use constants
 use allocation
 implicit none

 real(REAL64):: diameter
 real(REAL64):: crown_area
 real(REAL64):: h2
 real(REAL64):: leaf
 real(REAL64):: delta_leaf

 call updating_pool_stem(S_updt, H, stem)
 print*, 'CARBON ON STEM =', stem

 call updating_pool_leaf(delta_leaf,L,L_updt)
 print*, 'CARBON ON LEAF =', L_updt

!  call leaf_carbon(delta_leaf)
!  call root_carbon(delta_leaf,delta_root)
!  call sapwood_carbon(delta_leaf,delta_root,delta_sapwood)
!  print*,'delta_leaf=', delta_leaf,'delta_root=',delta_root,'delta_sapwood=', delta_sapwood
 
 diameter = diam(dw,pi,stem)
 print*, 'DIÂMETRO DO CAULE =', diameter

 crown_area = area_crown (k_allom1, krp, diameter)
 print*, 'AREA DA COPA=', crown_area

 h2 = tree_height (k_allom2, k_allom3, diameter)
 print*, 'ALTURA =', h2

 leaf = leaf_area (spec_leaf, L_updt, crown_area)
 print*, 'LAI =', leaf

end program allometry_2