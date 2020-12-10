program caete
    use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
    use allocation
    use constants
    implicit none

    real(REAL64) :: delta_leaf
    real(REAL64) :: delta_root
    real(REAL64) :: delta_sapwood


    ! call show_consts()
    
    call leaf_increment(delta_leaf)
    print*, 'CARBON LEAF INCREMENT =', delta_leaf

    call root_increment(delta_leaf, delta_root)
    print*, 'CARBON ROOT INCREMENT =', delta_root
    
    call sapwood_increment(delta_leaf, delta_root, delta_sapwood)
    print*, 'CARBON SAPWOOD INCREMENT =', delta_sapwood

    call updating_pool_leaf(delta_leaf,L,L_updt,turnover_leaf,turnover_rate_leaf)
    print*, 'LEAF POOL UPDATED =', L_updt

    call updating_pool_root(delta_root,R,R_updt,turnover_root,turnover_rate_root)
    print*, 'ROOT POOL UPDATED =', R_updt, R, delta_root

    call updating_pool_sapwood(delta_sapwood,S,S_updt,turnover_sap,turnover_rate_sap)
    print*, 'SAPWOOD POOL UPDATED =', S_updt, S, delta_sapwood

    call updating_pool_heartwood(H,turnover_sap,H_updt)
    print*, 'HEARTWOOD POOL UPDATED =', H_updt, H, turnover_sap

    call updating_pool_stem(S_updt, H_updt, stem)
    print*, 'STEM POOL UPDATED =', stem !H, S_updt



end program caete