program light_competition

integer,parameter::npls=10
real, dimension(npls) :: height 
integer :: n_heights
real :: max_height
real :: num_layer
integer::num_layer_round
real :: layer_size
real :: layer_height

height = (/12.0,5.0,9.0,25.0,32.0,18.0,3.0,7.0,20.0,30.0/)

max_height = maxval(height)

num_layer = max_height/5

num_layer_round = nint(num_layer)

layer_size = max_height/num_layer_round

layer_height=0

do j=1, num_layer_round
	layer_height=layer_height+layer_size
	print*, layer_height(j), layer_size
enddo


!do i=1,n_heights
!	do j=1, num_layer_round
!		print*,height(i),layer_height(j)
!enddo
!	enddo 


end program light_competition