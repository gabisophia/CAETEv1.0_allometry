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
    !real :: delta_sapwood = 0.0
    !real :: delta_leaf = 0.0
    !real :: delta_root = 0.0
    !real :: delta_heartwood = 0.0
    real :: bminc = 0.0 !total biomass increment (~NPP available for Allocation)
    real :: k_allom2 = 36.0
    real :: H = 3 !SOMENTE PARA TESTES
    real :: L = 4 !SOMENTE PARA TESTES
    real :: SS = 2 !SOMENTE PARA TESTES
    real :: R = 4 !SOMENTE PARA TESTES
    real :: SW = 5 !SOMENTE PARA TESTES
    real :: x !SOMENTE PARA TESTES
    real :: a = 0.0
    real :: b = 3.0
    real :: midpoint
    real :: Delta_R
    real :: Delta_S

    type(tree_allocation), allocatable :: tree(:)
    
    real, external :: calc_tau1
    real :: result_tau1
    real, external :: calc_tau2
    real :: result_tau2
    real, external :: calc_tau3
    real :: result_tau3
    real, external :: sapwood
    real :: result_SS
    real, external :: f 
    real :: result_f
    real, external :: bisection_method
    real :: Delta_L
    
    result_tau1 = calc_tau1(k_allom2, k_allom3, dw)
    print*, 'result_tau1 =', result_tau1
    
    result_tau2 = calc_tau2(k_allom3)
    print*, 'result_tau2 =', result_tau2

    result_tau3 = calc_tau3(klatosa, dw, spec_leaf)
    print *, "result_tau3 = ", result_tau3

    result_SS = sapwood(ltor, R, SW, L, bminc)
    print *, 'result_SS =', result_SS

    result_f = f(x, result_tau1, result_tau2, result_tau3, ltor, H, L, SS)
    print *, 'SEARCH X (func. F)=', result_f

! Main Code: Use the bisection method to solve for the leaf mass increment
    Delta_L = bisection_method(a, b, tol, midpoint)
    print *, 'DELTA L =', Delta_L

!Once we have the leaf mass increment we can cant get root mass increment based on the LTOR constant
    Delta_R = (Delta_L + L) / ltor - R
    print *, 'DELTA R=', Delta_R

!Finally using the cmass_increment mass conservation we can calculate sapwood increment
    Delta_S = bminc - Delta_L - Delta_R
    print *, 'DELTA S=', Delta_S

end program allocation

!==========================================================!
function calc_tau1(k_allom2, k_allom3, dw) result(tau1)
    implicit none
    real, intent(in) :: k_allom2
    real, intent(in) :: k_allom3
    real, intent(in) :: dw
    real :: tau1

    tau1 = k_allom2**(2.0/k_allom3) * 4.0 / 3.14159 / dw
end function calc_tau1
!==========================================================!

!==========================================================!
function calc_tau2(k_allom3) result(tau2)
    implicit none
    real, intent(in) :: k_allom3
    real :: tau2 

    tau2 = 1.0 + 2.0 / k_allom3
end function calc_tau2
!==========================================================!

!==========================================================!
function calc_tau3(klatosa, dw, spec_leaf) result(tau3)
    implicit none
    real, intent(in) :: klatosa
    real, intent(in) :: dw
    real, intent(in) :: spec_leaf
    real :: tau3

    tau3 = klatosa/dw/spec_leaf
end function calc_tau3
!==========================================================!

!==========================================================!
function sapwood (ltor, R, SW, L, bminc) result (SS)
    implicit none
    real, intent(in) :: ltor
    real, intent(in) :: R
    real, intent(in) :: SW
    real, intent(in) :: L
    real, intent(in) :: bminc
    real :: SS 

    SS = SW + bminc - L / ltor + R

end function sapwood
!==========================================================!


!===========================================================================!
!Minimization function with delta.leaf as x
!A value of x is searched with f(x) = 0

!FUNÇÃO 'def f(self, x)' DO CÓDIGO DO PHILIP UTILIZANDO VALORES ALEATÓRIOS PARA 'H', 'L' e 'SS' > para testes.

function f (x, result_tau1, result_tau2, result_tau3, ltor, H, L, result_SS) result (searched_x)
    implicit none
    real, intent(in) :: x
    real, intent(in) :: result_tau1
    real, intent(in) :: result_tau2
    real, intent(in) :: result_tau3
    real, intent(in) :: ltor
    real, intent(in) :: H
    real, intent(in) :: L 
    real, intent(in) :: result_SS
    real :: searched_x

    return
    searched_x = result_tau1 * (result_SS - x - x / ltor + H) - ((result_SS - x - x / ltor) /&
     &(L + x) * result_tau3) ** result_tau2
 
end function f
!===========================================================================!

!===========================================================================!
!Numerical root-finding method to find a solution x with f(x) = 0
!there are still some stop-criteria missing, e.g. stop distribution after n=40

!FUNÇÃO DO BISECTION_METHOD >>>> Conferir se está correta com a lógica do Philip em Python.

function bisection_method (a, b, tol, midpoint) result (Delta_L)
    implicit none
    real :: a
    real :: b
    real, intent(in) :: tol
    real :: midpoint
    real :: Delta_L

    if ((a * b) .gt. 0) then 
        return 
            !a * b = -2.0 !VERIFICAR PQ TÁ DANDO ERRADO NO 'a': Unclassifiable statement at (1)
    endif 

    if ((b - a) / 2.0 .gt. tol) then
        midpoint = (a + b) / 2.0
        elseif (midpoint .gt. 0) then
            return 
    endif 

    if ((a * midpoint) .lt. 0) then
        b = midpoint
    else 
        a = midpoint
    endif 
         
end function bisection_method
!===========================================================================!

! function funcao_que_usa_resultado_de_outra(resultado_da_outra) result(novo_resultado)
!     implicit none
!     real, intent(in) :: resultado_da_outra
!     real :: novo_resultado

!     novo_resultado = resultado_da_outra * 2
! end function funcao_que_usa_resultado_de_outra