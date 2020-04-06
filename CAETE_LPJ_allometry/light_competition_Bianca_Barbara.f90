program light_competition

    type :: layer_array
        real :: sum_height
        integer :: num_height !!corresponds to the number of pls
        real :: mean_height
        real :: layer_height
        real :: sum_LAI !LAI sum in a layer
        real :: mean_LAI !mean LAI in a layer
        real :: beers_law !layer's light extinction
        real :: li !layer's light incidence
        real :: lu !layer's light used (relates to light extinction - Beers Law)
        real :: la !light availability
    end type layer_array

    integer,parameter::npls=14
    real, dimension(npls) :: height
    real, dimension(npls) :: LAI
    
    real :: max_height
    integer :: num_layer
    real :: layer_size
    real :: incidence_rad !Incidence radiation (relates do APAR) in J/m2/s
    real :: watt_rs = 210 !shortwave radiation in watts/m2
    real :: short_rad !shortwave radiation in joules/s
    
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
        !print*, 'layer_height',layer(i)%layer_height, i
    enddo

    layer(i)%num_height=0
    layer(i)%sum_height=0
    layer(i)%mean_height=0
    layer(i)%sum_LAI=0
   

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
                                      
            endif
        enddo

        layer(i)%mean_height=layer(i)%sum_height/&
            &layer(i)%num_height

        if(layer(i)%sum_height.eq.0.) then
            layer(i)%mean_height=0.
        endif

        layer(i)%mean_LAI=layer(i)%sum_LAI/&
            &layer(i)%num_height
             !print*,'mean_LAI',layer(i)%mean_LAI

        if(layer(i)%sum_LAI.eq.0.) then
            layer(i)%mean_LAI=0.
             !print*, 'mean_LAI2', layer(i)%mean_LAI
        endif
        
        print*,'lyr',i,'mean_height',&
            &layer(i)%mean_height,'lai',&
            &layer(i)%mean_LAI
    enddo

    layer(i)%li = 0

    layer(i)%la = 0

    layer(i)%lu = 0

!Light Extinction

    short_rad = watt_rs*1000.
    !print*,'short_rad',short_rad

    incidence_rad = 0.5*short_rad
    !print*,'APAR', incidence_rad

!=================== TEST ====================
    do i=num_layer,1,-1
        layer(i)%beers_law = incidence_rad*&
            &(1-exp(-0.5*layer(i)%mean_LAI))
         print*,'law',layer(i)%beers_law
    enddo
!=============================================


    do i=num_layer,1,-1
        if(i.eq.num_layer)then
            layer(i)%li = incidence_rad
        else
            if(layer(i)%mean_height.gt.0.)then

                layer(i)%li = layer(last_with_pls)%la
                last_with_pls=i

            else
                continue
            endif
        endif

        layer(i)%lu = layer(i)%li * (1-exp(-0.5*layer(i)%mean_LAI))
        
        layer(i)%la = layer(i)%li - layer(i)%lu

        print*,i, 'inc', layer(i)%li, 'used', layer(i)%lu,& 
            &'avai', layer(i)%la

    enddo

    

end program light_competition