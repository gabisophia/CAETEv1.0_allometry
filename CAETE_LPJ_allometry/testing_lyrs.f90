program light_competition

integer,parameter::npls=14
real, dimension(npls) :: height 
integer :: n_heights
real :: max_height
real :: num_layer
integer::num_layer_round
real :: layer_size
real :: max_layer

real,allocatable,dimension(:) :: layer_height




do i=1,npls
	height(i)=35-(i*2)+3
	
enddo

do i=1, npls
	print*, 'height',height(i)
enddo

max_height = maxval(height)
print*, 'max_height',max_height

num_layer = max_height/5
print*, 'num_layer',num_layer

num_layer_round = nint(num_layer)
n=num_layer_round
print*,'num_layer_round',num_layer_round,n

layer_size = max_height/num_layer_round
print*, 'layer_size', layer_size

allocate(layer_height(n))

do j=1,n
	layer_height(n)=0
enddo

do j=1, n
	layer_height(n)=layer_height(n)+layer_size
	print*, 'layer_height',layer_height(n), layer_size
enddo

max_layer=maxval(layer_height)
print*, 'max_layer', max_layer

!do j=1,n 
do i=1, npls
	if(height(i).eq.max_layer) then
	  print*, 'heights',height(i)
	else if(height(i).lt.max_layer)then
	 print*, 'lowers',height(i)
	endif

enddo
!enddo
 


!do i=1,n_heights
!	do j=1, num_layer_round
!		print*,height(i),layer_height(j)
!enddo
!	enddo 


end program light_competition