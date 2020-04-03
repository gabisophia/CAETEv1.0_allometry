program main

    type :: layer_array
        real :: sum_height
        integer :: num_height
        real :: mean_height
        real :: layer_height
        real :: sum_LAI !soma das LAI
        integer :: num_LAI !número de LAI
        real :: mean_LAI !média das LAI
        real :: li
        real :: lu
        real :: la
    end type layer_array

    integer,parameter::npls=14
    real, dimension(npls) :: height
    real, dimension(npls) :: LAI
    real :: max_height
    integer :: num_layer
    real :: layer_size
    real :: APAR !J/m2/s
    real :: watt_rs = 210 !shortwave radiation in watts/m2
    real :: short_rad !shortwave radiation in joules/s
    real :: Beers_law
    integer::i,j

    integer :: last_with_pls

    type(layer_array), allocatable :: layer(:)

    
    LAI=(/1.,1.2,1.4,1.6,1.8,2.0,2.2,2.5,2.7,2.9,3.,3.5,4.,4.2/)

    height=(/2.0,3.0,3.7,10.,10.5,12.,13.,20.,22.,27.,27.5,28.,29.,34./)

    max_height = maxval(height)
   !print*, 'max_height',max_height
    
    num_layer = nint(max_height/5)
    !print*, 'num_layer',num_layer

    allocate(layer(1:num_layer))
    
    last_with_pls=num_layer

    layer_size = max_height/num_layer
    !print*, 'layer_size', layer_size

    layer(i)%layer_height=0

    do i=1,num_layer
        layer(i)%layer_height=layer_size*i
        print*, 'layer_height',layer(i)%layer_height, i
    enddo

    layer(i)%num_height=0
    layer(i)%sum_height=0
    layer(i)%mean_height=0
    layer(i)%num_LAI=0
    layer(i)%sum_LAI=0
    layer(i)%mean_LAI=0
    do i=1, num_layer
        do j=1,npls
            if ((layer(i)%layer_height.ge.height(j)).and.&
                &(layer(i-1)%layer_height.lt.height(j))) then

                layer(i)%sum_height=&
                &layer(i)%sum_height+height(j)

                layer(i)%num_height=&
                &layer(i)%num_height+1

                layer(i)%sum_LAI=&
                &layer(i)%sum_LAI+LAI(j)

                layer(i)%num_LAI=&
                &layer(i)%num_LAI+1

                
            endif
        enddo
        layer(i)%mean_height=layer(i)%sum_height/&
            &layer(i)%num_height
        if(layer(i)%sum_height.eq.0.) then
            layer(i)%mean_height=0.
        endif
        layer(i)%mean_LAI=layer(i)%sum_LAI/&
            &layer(i)%num_LAI
        if(layer(i)%sum_LAI.eq.0.) then
            layer(i)%mean_LAI=0.
        endif
        print*,i, layer(i)%mean_height, layer(i)%mean_LAI
    enddo

    layer(i)%li = 0

    layer(i)%la = 0

    layer(i)%lu = 0

!Light Extinction

    !short_rad = watt_rs*1000

    APAR = 0.5*short_rad

    Beers_law = 100*(1-exp(-0.5*layer(i)%mean_LAI))

    print*, 'law', Beers_law

    do i=num_layer,1,-1
        if(i.eq.num_layer)then
            layer(i)%li=100
        else
            if(layer(i)%mean_height.gt.0.)then

                layer(i)%li = layer(last_with_pls)%la
                last_with_pls=i

            else
                continue
            endif
        endif

        layer(i)%lu = layer(i)%li * 0.2
        layer(i)%la = layer(i)%li - layer(i)%lu

        print*,layer(i)%li,layer(i)%lu,layer(i)%la

    enddo

    

end program main