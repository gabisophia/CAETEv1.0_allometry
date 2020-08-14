program allocation
    implicit none

    type :: tree_allocation
        real :: carbon_sapwood = 0.0
        real :: carbon_leaf = 0.0
        real :: carbon_root = 0.0
        real :: carbon_heartwood = 0.0
    end type tree_allocation

    real :: klatosa = 6000.0 !leaf_area:sapwood_area. Value from Philip's code (Allocation.py)
    real :: ltor = 0.77302587552347657 !leaf:root  Value from Philip's code (Allocation.py)
    real :: dw = 200.0 !wood density (variant trait) Value from Philip's code (Allocation.py)
    real :: k_allom3 = 0.22 !allometric constant Value from Philip's code (Allocation.py)
    real :: spec_leaf = 15.365607091853349 !specific leaf area (variant trait) generic value to calculate leaf area index (LAI) value from Philip's code (Allocation.py)
    real :: tol = 0.00000001 !tolerance for iteration in bisection method
    real :: delta_sapwood = 0.0
    real :: delta_leaf = 0.0
    real :: delta_root = 0.0
    real :: delta_heartwood = 0.0
    real :: bminc = 0.0 !total biomass increment (~NPP available for Allocation)
    real :: k_allom2 = 36.0
    
    real, external :: distribution
    real :: result_tau3

    result_tau3 = distribution(k_allom2)
    print *, "result_tau3 = ", result_tau3

end program allocation

function distribution(k_allom2) result(tau3)
    implicit none
    real, intent(in) :: k_allom2
    real :: tau3

    tau3 = k_allom2 + 100
end function distribution