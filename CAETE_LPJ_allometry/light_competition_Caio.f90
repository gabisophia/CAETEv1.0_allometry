program main


	integer, parameter :: npls=14
	real, dimension(npls) :: height
	real :: max_height
	real :: layer_size
	integer :: number_layers


	type :: layer_array
		integer :: id
		real :: mean_height
		real :: height_id
		real :: li !light incidence
		real :: lu !light used
		real :: la !ligh availability
		real :: layer_height=0
	end type layer_array

	type(layer_array), allocatable :: layer(:)

	
	integer :: last_with_pls = 1
	
	
	 do j=1,npls
	 	height(j)=j*3
	 	print*, height(j)
	 	
	 enddo

	 max_height=maxval(height)
	 print*,'max_height', max_height

	 number_layers=nint(max_height/5)
	 print*,'number_layers',number_layers

	 allocate(layer(1:number_layers))

	 layer_size=max_height/number_layers
	 print*,'layer_size', layer_size 

	do j=1, npls
		do i=1, number_layers
			layer(i)%id=i
		
			layer(i)%layer_height=layer_size*i

			if ((height(j).le.layer(i)%layer_height).and.(height(j).gt.layer(i-1)%layer_height)) then 
				layer(i)%height_id=i

				if (layer(i)%height_id.eq.1) then
					layer(i)%li = 100
				else
				layer(i)%li = layer(last_with_pls)%la
				last_with_pls = i
			    endif
			endif

			layer(i)%lu = layer(i)%li * 0.2
			layer(i)%la = layer(i)%li - layer(i)%lu
			print*, layer(i)%li,layer(i)%lu,layer(i)%la
		enddo
	enddo

	




	! This block will be generated automatically.
	! To test a layer without PLS, set the same with 0, like this:
	! layer(n)%height = 0
	! layer(1)%id = 1
	! layer(1)%height = 34.0000000
	! layer(2)%id = 2
	! layer(2)%height = 29.1428585
	! layer(3)%id = 3
	! layer(3)%height = 0
	! layer(4)%id = 4
	! layer(4)%height = 0
	! layer(5)%id = 5
	! layer(5)%height = 14.5714283
	! layer(6)%id = 6
	! layer(6)%height = 9.71428585
	! layer(7)%id = 7
	! layer(7)%height = 4.85714293

	

	! do i = 1, num_layers
	! 	if (i.eq.1) then
	! 		layer(i)%li = 100
	! 	else
	! 		if (layer(i)%height.gt.0) then
	! 			layer(i)%li = layer(last_with_pls)%la
	! 			last_with_pls = i
	! 		else
	! 			continue
	! 		endif
	! 	endif

	! 	layer(i)%lu = layer(i)%li * 0.2
	! 	layer(i)%la = layer(i)%li - layer(i)%lu
	! 	print*, layer(i)
	! enddo

end program main