program light_competition
    implicit none

    real, allocatable, dimension(:) :: height
    integer, allocatable, dimension(:) :: pls_h_id
    integer, allocatable, dimension(:) :: ordered_index

    real :: ipar
    real :: max_height
    integer:: i, j, n, npls, id_1
    real :: layer_size



    real,allocatable :: layer_height(:)
    real,allocatable :: light_availability(:)
    real,allocatable :: light_used (:)
    real,allocatable :: light_incidence (:)
    integer, allocatable :: id(:)

    allocate(height(7))

    height=(/4.85714293, 29.1428585, 14.5714283, 34.00000, 19.4285717, 9.71428585, 24.2857151/)

!    height=(/10.0, 10.0, 10.0, 10.0, 19.4285717, 0.0, 24.2857151/)

    npls = size(height)
    allocate(pls_h_id(npls))
    allocate(ordered_index(npls))

    ! FIND THE MAXIMUM height
    max_height = maxval(height)

    ! Dynamic layering - FIND the number of layers
    n = nint(max_height/5)

    ! Calculate the size of each layer in m
    layer_size = max_height/n

    ! Allocate some variables
    allocate(layer_height(n))
    allocate(id(n))

    ! FILL layer height with zeros
    layer_height = 0.0

    ! Fill layer_height array with the maximum height of each layer
    ! IDs = from 1 (lower) to n (higher)
    do j=1,n
        layer_height(j) = layer_size * j
        id(j) = j
    enddo
    ! print *, 'layer_height', layer_height
    ! print *, 'layer ID', id


    ! OUR PROBLEM: find the height category of each PLS
    ! FIND the ID if each PLS
    do i=1,npls
        pls_h_id(i) = classify_pls(height(i), layer_height)
    enddo

    print *, 'pls_layer ID', pls_h_id
    print *, 'layer_height', layer_height
    print *, 'layer_id', id

    ipar= 100. ! Wm⁻²
    ! Cada camada usa 20% da radiação total
    ! Split the teoretical value of ipar for each occupied layer

    ! find the ordered indexes of PLSs
    call ordered_indxs(height, ordered_index)
    print *, 'ordered indexes', ordered_index


    ! Reproduz o script do caio ( temos ERROS aqui)
    allocate(light_availability(npls))
    allocate(light_used(npls))
    allocate(light_incidence(npls))

    id_1 = -99999
    do i = 1, npls
        ! id_1 = pls_h_id(ordered_index(i))
        ! Calcula luz utilizada
        light_used(ordered_index(i)) = ipar * 0.2
        ! Calcula luz para proxima camada
        light_availability(ordered_index(i)) = ipar -  light_used(ordered_index(i))
        ! Luz incidente total na layer L
        light_incidence(ordered_index(i)) = ipar
        ! Atualiza IPAR pro proximo loop
        !if(i .gt. 1 .and. pls_h_id(ordered_index(i)) == id_1) then
        !    ipar = light_incidence(ordered_index(i))
        !else
        ipar = light_availability(ordered_index(i))
        !endif

        print *, height(ordered_index(i)), light_incidence(ordered_index(i)),&
               & light_used(ordered_index(i)),light_availability(ordered_index(i))
    enddo



    contains

    function classify_pls(pls_height, height_classes) result(pls_id)
        implicit none

        real(kind=4), intent(in) :: pls_height
        real(kind=4), dimension(:), intent(in) ::  height_classes
        integer(kind=4) :: pls_id

        integer(kind=4) ::pid, n_classes
        real(kind=4),allocatable, dimension(:) :: points ! will store the number of breaks

        n_classes = size(height_classes) ! IDs: number of IDs
        ! allocate treshold points array
        allocate(points(n_classes-1))    !
        ! Armazena o treshold para todas as classes, exceto a altura máxima
        points = height_classes(1:n_classes-1)

        pid = n_classes
10      continue
        if(pid .eq. 0) goto 20
        if(pls_height .gt. maxval(points)) then
            pls_id = pid
            goto 20
        else
            points(maxloc(points)) = -3.0
            pid = pid - 1
            goto 10
        endif

20      continue
    return
    end function classify_pls


    subroutine ordered_indxs(pls_height, ordered_index)

        real(kind=4), dimension(:), intent(in) :: pls_height
        integer(kind=4), dimension(:), intent(out) :: ordered_index

        real(kind=4), allocatable, dimension(:) :: pls_id
        integer(kind=4), dimension(1) :: inx
        integer(kind=4) :: i, n

        n = size(pls_height)
        allocate(pls_id(n))
        pls_id = pls_height

        do i = 1, n
            inx = maxloc(pls_id)
            ordered_index(i) = inx(1)
            pls_id(maxloc(pls_id)) = -1.0
        enddo

    end subroutine ordered_indxs

end program light_competition
