module allometry
	use ISO_FORTRAN_ENV, only: REAL32, REAL64, REAL128
	use constants
	use allocation
	implicit none
	contains

	function diam (x,y,dl) result (diameter)
		real(REAL64) :: diameter
		real(REAL64) :: x
		real(REAL64) :: y
		real(REAL64) :: dl

		diameter = x+y+dl+1
	end function diam


end module allometry