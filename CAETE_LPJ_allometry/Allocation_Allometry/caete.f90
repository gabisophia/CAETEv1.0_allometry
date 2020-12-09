program caete
    use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
    use allocation
    use constants
    implicit none

    real(REAL64) :: delta_leaf
    real(REAL64) :: delta_root
    real(REAL64) :: delta_sapwood

    ! call show_consts()
    
    call leaf_carbon(delta_leaf)
    print*, 'CARBON LEAF INCREMENT =', delta_leaf

    call root_carbon(delta_leaf, delta_root)
    print*, 'CARBON ROOT INCREMENT =', delta_root
    
    call sapwood_carbon(delta_leaf, delta_root, delta_sapwood)
    print*, 'CARBON SAPWOOD INCREMENT =', delta_sapwood

    call updating_pool_leaf(delta_leaf,L,L_updt)
    print*, 'LEAF POOL UPDATED =', L_updt

    call updating_pool_root(delta_root,R,R_updt)
    print*, 'ROOT POOL UPDATED =', R_updt, R, delta_root

    call updating_pool_sapwood(delta_sapwood,S,S_updt)
    print*, 'SAPWOOD POOL UPDATED =', S_updt, S, delta_sapwood

    call updating_pool_stem(S_updt, H, stem)
    print*, 'STEM POOL UPDATED =', stem !H, S_updt

    call updating_turnover_sap(S_updt, H, turnover_sap)
    print*, 'TURNOVER SAPWOOD =', turnover_sap

end program caete