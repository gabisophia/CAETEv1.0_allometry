module allometry
	use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
	use constants
	use allocation

	implicit none

	contains

	function diam (dw, pi, stem) result (diameter) !!!!!precisamos rever todas as unidades
		real(REAL64) :: diameter 
		real(REAL64) :: dw 
		real(REAL64) :: pi
		real(REAL64) :: stem !CARBON ON SAPWOOD UPDATED - Allocation.f90 code

		diameter = ((4+(50.961727846323342))/((dw)*3.14*40))**(1/(2+0.5))
		
	end function diam

	function area_crown (k_allom1, krp, diameter)  result (crown_area)
		real(REAL64) :: diameter
		real(REAL64) :: k_allom1
		real(REAL64) :: krp
		real(REAL64) :: crown_area

		crown_area = k_allom1*(diameter**krp)

	end function area_crown

	function tree_height (k_allom2, k_allom3, diameter) result (height)
		real(REAL64) :: diameter
		real(REAL64) :: k_allom2
		real(REAL64) :: k_allom3
		real(REAL64) :: height

		height = k_allom2*(diameter**k_allom3)

	end function tree_height

	function leaf_area (spec_leaf, L_updt, crown_area) result (LAI)
		real(REAL64) :: spec_leaf
		real(REAL64) :: L_updt
		real(REAL64) :: crown_area
		real(REAL64) :: LAI

		LAI = (L_updt*spec_leaf)/crown_area

	end function leaf_area

end module allometry