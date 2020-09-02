module allocation
	implicit none

	!==============================!
	!= Parameters
	!==============================!
	
	real, parameter :: klatosa = 6000.0
	real, parameter :: dw = 200.0
	real, parameter :: ltor = 0.77302587552347657
	real, parameter :: k_allom2 = 36.0
	real, parameter :: k_allom3 = 0.22
	real, parameter :: spec_leaf = 15.365607091853349 
	real, parameter :: bminc = 0.0
	real, parameter :: tol = 10
	
	!==============================!
	
	real :: delta_leafs
	real :: delta_root
	real :: delta_sapwood
	real :: H = 3 !SOMENTE PARA TESTES
	real :: L = 4 !SOMENTE PARA TESTES
	real :: SS = 2 !SOMENTE PARA TESTES
	real :: R = 4 !SOMENTE PARA TESTES
	real :: SW = 5 !SOMENTE PARA TESTES
	
	contains

	!==============================!
	!= Functions
	!==============================!
		
	function calc_tau1(k_allom2, k_allom3, dw) result(tau1)
		implicit none
		real :: k_allom2  
		real :: k_allom3
		real :: dw
		real :: tau1

		tau1 = k_allom2 ** (2.0 / k_allom3) * 4.0 / 3.14159 / dw
	end function calc_tau1

	function calc_tau2(k_allom3) result(tau2)
		implicit none
		real :: k_allom3
		real :: tau2 
	
		tau2 = 1.0 + 2.0 / k_allom3
	end function calc_tau2

	function calc_tau3(klatosa, dw, spec_leaf) result(tau3)
		implicit none
		real :: klatosa
		real :: dw
		real :: spec_leaf
		real :: tau3
	
		tau3 = klatosa / dw / spec_leaf
	end function calc_tau3

	function sapwood (ltor, R, SW, L, bminc) result (SS)
		implicit none
		real :: ltor
		real :: R
		real :: SW
		real :: L
		real :: bminc
		real :: SS 
	
		SS = SW + bminc - L / ltor + R
	end function sapwood

	function f(x) result(searched_x)
		real :: x 
		real :: searched_x
		real :: SS, ltor, H, L, tau3, tau2, tau1
		
		searched_x = 												&
			tau1 * (SS - x - x / ltor + H) -  &
			((SS - x - x / ltor) / 						&
			(L + x) * tau3) ** tau2
	end function f
	
	function bisection_method(a, b) result(delta_leaf)
		implicit none
		real :: a
		real :: b
		real :: delta_leaf
		real :: midpoint
		real :: tol = 0.01
		real :: nstep
		real :: test_var
		real :: test_var2
		real :: test_var3

		if ((f(a)*f(b)).gt. 0.) then
			delta_leaf=-2
			print*, "Condition #01 - delta_leaf: ", delta_leaf
			return
		else
			nstep=0.
			 do while (((b-a)/2).gt.tol)
			 	midpoint=((a+b)/2.)
			 	test_var=f(midpoint)
			 	print*,'midpoint', midpoint, test_var

		 		if ((test_var).eq.0.) then
			  			delta_leaf=midpoint
			  			print*, 'Condition #02 - delta_leaf: ', delta_leaf
			   			return
				! elseif ((f(a)*f(midpoint)).lt.0.) then
			 !  			 b2=midpoint
			 !  			print*, 'running'
			 !  	else
			 !  			 a=midpoint
			 !  			print*, 'running2'	
				 endif
			 end do
			 
			! nstep=nstep+ 1.
		endif
		

		 ! if((f(a) * f(b)) .gt. 0) then
		! 	delta_leaf = -2.0 !end function, no root.
		! 	print*, 'Condition #01 - delta_leaf: ', delta_leaf
		! 	return
		! else nstep = 0
		! ((b - a) / 2.0 .gt. tol) then
		! 	 midpoint = (a + b) / 2.0

		! elseif (f(midpoint) .eq. 0) then
		! 	delta_leaf = 0 !The midpoint is the x-intercept/root.
		! 	print*, 'Condition #02 - delta_leaf: ', delta_leaf
		!  	return
		! elseif ((f(a) * f(midpoint) .lt. 0)) then
		! 	b = midpoint
		! else
		! 	a = midpoint
		! endif
	end function bisection_method

	!==============================!
	!= Subrotines
	!==============================!
	
	! Main Code: Use the bisection method to solve for the leaf mass increment
	subroutine leaf_carbon(delta_leafs)
		real :: delta_leafs

		!delta_leafs = bisection_method(0.0, 3.0)
		 delta_leafs = bisection_method(0.0, 10.0)

		print*, 'CARBON ON LEAF =', delta_leafs
	end subroutine leaf_carbon

	! Once we have the leaf mass increment we can cant get 
	! root mass increment based on the LTOR constant
	subroutine root_carbon(delta_leafs, L, ltor, R, delta_root)
		real :: delta_root
		real :: L
		real :: ltor
		real :: R 
		real :: delta_leafs

		delta_root = (delta_leafs + L) / ltor - R

		print*, 'CARBON ON ROOT =', delta_root
	end subroutine root_carbon

	!Finally using the cmass_increment mass conservation we can calculate sapwood increment
	subroutine sapwood_carbon(delta_leafs, delta_root, bminc, delta_sapwood)
		real :: delta_sapwood
		real :: bminc
		real :: delta_root
		real :: delta_leafs

		delta_sapwood = bminc - delta_leafs - delta_root

		print*, 'CARBON ON SAPWOOD =', delta_sapwood
	end subroutine sapwood_carbon

end module allocation
