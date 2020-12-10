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
 real(REAL64):: delta_root
 real(REAL64):: delta_sapwood

 

 call leaf_increment(delta_leaf)

 call root_increment(delta_leaf,delta_root)
 
 call sapwood_increment(delta_leaf,delta_root,delta_sapwood)

 call updating_pool_heartwood(H,turnover_sap,H_updt)
 
 call updating_pool_sapwood(delta_sapwood,S,S_updt,turnover_sap,turnover_rate_sap)

 call updating_pool_stem(S_updt, H_updt, stem)
 print*, 'CARBON ON STEM =', stem

 call updating_pool_leaf(delta_leaf,L,L_updt,turnover_leaf,turnover_rate_leaf)
 print*, 'CARBON ON LEAF =', delta_leaf, L, L_updt
 
 diameter = diam(dw,pi,stem)
 print*, 'DIÂMETRO DO CAULE =', diameter

 crown_area = area_crown (k_allom1, krp, diameter)
 print*, 'AREA DA COPA=', crown_area

 h2 = tree_height (k_allom2, k_allom3, diameter)
 print*, 'ALTURA =', h2

 leaf = leaf_area (spec_leaf, L_updt, crown_area)
 print*, 'LAI =', leaf

end program allometry_2