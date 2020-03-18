program light_competition

integer,parameter::npls=14
real, dimension(npls) :: height
real :: max_height
real :: num_layer
integer::num_layer_round
real :: layer_size
real :: max_layer
real :: heigher_layer
real :: ipar

real :: temp_incidence
real :: temp_availability
real :: temp_used

real,allocatable :: layer_height(:)
real,allocatable :: light_availability(:)
real,allocatable :: light_used (:)
real,allocatable :: light_incidence (:)


do i = 1, npls
	height(i) = 35 -(i*2)+1
	if (i.eq.1) then
		height(i)=3.
	endif
	 print*, 'height', height(i)
enddo

! do i=1, npls
! 	print*, 'height',height(i)
! enddo

max_height = maxval(height)
!print*, 'max_height',max_height

num_layer = max_height/5
!print*, 'num_layer',num_layer

num_layer_round = nint(num_layer)
n=num_layer_round

!print*,'num_layer_round',num_layer_round,n

layer_size = max_height/num_layer_round
!print*, 'layer_size', layer_size

allocate(layer_height(1:n))
allocate(light_availability(1:n))
allocate(light_used(1:n))
allocate(light_incidence(1:n))

do j=1,n
	layer_height(j)=0
enddo

do j=1,n
	layer_height(j)=layer_height(j-1)+layer_size
	print*, 'layer_height',layer_height(j)
enddo

ipar=100.

do i=1,npls
	do j=2,n
		if (j.eq.n) then
			light_incidence(j) = ipar
			light_used(j) = light_incidence(j)*0.2
			light_availability(j) = light_incidence(j)-light_used(j)

		else if ((height(i).lt.layer_height(j)).and.(height(i).gt.(layer_height(j-1)))) then
			light_incidence(j) = light_availability(j+1)
			light_used(j) = light_incidence(j)*0.2
			light_availability(j) = light_incidence(j)-light_used(j)
		endif
		
	enddo
enddo

do i=1,npls
	do j=1,n
		if(j.eq.1) then
			light_incidence(j) = light_availability(2)
			light_used(j) = light_incidence(j)*0.2
			light_availability(j) = light_incidence(j)-light_used(j)
		endif
	enddo
enddo

!!!! ATTENTION: if there is no pls in the previous layer??????

do j=1,n
	print*, j,'avai',light_availability(j),'used',light_used(j),'inc',light_incidence(j)
enddo

end program light_competition