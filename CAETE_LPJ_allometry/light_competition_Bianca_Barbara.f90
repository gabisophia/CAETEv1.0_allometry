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

    integer, parameter::npls=14
    real, dimension(npls), allocatable :: height (:)
    real, dimension(npls), allocatable :: LAI (:) !Leaf Area Index (m2/m2)
    real, dimension(npls), allocatable :: diam (:) !Tree diameter in m. (Smith et al., 2001 - Supplementary)
    real, dimension(npls), allocatable :: crown_area (:) !Tree crown area (m2) (Sitch et al., 2003)
    real, allocatable :: FPCind (:) !Foliage projective cover for each PLS (Sitch et al., 2003)
    real, allocatable :: FPCgrid (:) !Fractional projective cover in grid cell (Sitch et al., 2003)
    real, allocatable :: nind (:) !number of individuals per PLS (Smith, 2001, thesis)
    
    real :: max_height
    integer :: num_layer
    real :: layer_size
    real :: incidence_rad !Incidence radiation (relates do APAR) in J/m2/s
    real :: watt_rs = 210 !shortwave radiation in watts/m2
    real :: short_rad !shortwave radiation in joules/s
    real :: k_allom1 = 100. !allometric constant (Table 3; Sitch et al., 2003)
    real :: k_allom2 = 40. !allometric constant (Table 3; Sitch et al., 2003)
    real :: k_allom3 = 0.5 !allometric constant (Table 3; Sitch et al., 2003)
    real :: krp = 1.6 !allometric constant (Table 3; Sitch et al., 2003)
    real :: kla_sa = 8000 !constant relates to leaf properties (Table 3; Sitch et al., 2003)
    real :: spec_leaf = 21.7 !generic value to calculate leaf area index (LAI)
    
    integer::i,j

    integer :: last_with_pls

    type(layer_array), allocatable :: layer(:)

! Variables with generic values for testing the logic code

    real, dimension(npls) :: dwood !wood density (g/cm-3) *Fearnside, 1997 - aleatory choices
    real, dimension(npls) :: carbon_stem !KgC/m2 (Cheart + Csap)
    real, dimension(npls) :: carbon_leaf !KgC/m2 

    dwood=(/0.74,0.73,0.59,0.52,0.41,0.44,0.86,0.42,0.64,0.69,0.92,0.60,0.36,0.99/)
    carbon_stem=(/7.,12.,7.2,8.3,8.8,9.7,7.5,11.5,10.,8.6,7.3,10.3,6.8,9.9/)
    carbon_leaf=(/0.15,3.,0.18,0.6,1.5,1.8,0.3,2.,0.8,0.64,0.25,1.,0.2,1.7/)

! Allometric Equations

    diam = ((4+(carbon_stem))/((dwood)*3.14*40))**(1/(2+0.5))
    print*, 'diam', diam

    height = k_allom2*(diam**k_allom3)
    print*, 'height', height
    
    crown_area = k_allom1*(diam**krp)
    print*, 'crown', crown_area

    LAI = (carbon_leaf*spec_leaf)/crown_area
    print*, 'LAI', LAI


! Grid-Cell Properties

    allocate (nind(1:npls))
    allocate (FPCind(1:npls))
    allocate (FPCgrid(1:npls))

    do j=1,npls
        nind(j) = diam(j)**(-1.6)
        print*, 'Nind', nind(j)

        FPCind(j) = (1-exp(-0.5*LAI(j)))
        print*, 'FPC', FPCind(j)

        FPCgrid(j) = diam(j)*nind(j)*FPCind(j)
        print*, 'FPC-GRID', FPCgrid(j)
    enddo

! Layer's dynamics

    max_height = maxval(height)
    print*, 'max_height',max_height
    
    num_layer = nint(max_height/5)
    print*, 'num_layer',num_layer

    allocate(layer(1:num_layer))
    
    last_with_pls=num_layer

    layer_size = max_height/num_layer
    print*, 'layer_size', layer_size

    layer(i)%layer_height=0

    do i=1,num_layer
        layer(i)%layer_height=layer_size*i
        print*, 'layer_height',layer(i)%layer_height, i
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