   ! Copyright 2017- LabTerra

!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.)

!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.

!     You should have received a copy of the GNU General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.

module alloc

    use types
    use carbon_costs, only: fixed_n, passive_uptake,&
                          & active_costn, active_costp,&
                          & prep_out_n, prep_out_p,&
                          & retran_nutri_cost, select_active_strategy
    use global_par, only: ntraits, sapwood
    use photo, only: f_four, spec_leaf_area, realized_npp
    use allometry_par
    use IEEE_ARITHMETIC

    implicit none
    private

    public :: allocation

    contains

!=====================================================================
!c     subroutine allocation calculates the daily carbon content of each
!c     compartment plus plant uptake and allocation of nutrients
!c
!c     code written by Bianca Rius & David Lapola (27.Ago.2015)
!c     Modified 02-08-2017 jp - nutrient cycles implementation
!c=====================================================================

   subroutine allocation(dt,npp,npp_costs,ts,wsoil,te,nmin, plab,on,&
      & sop,op,scl1,sca1,scf1,scs1,sch1,storage,storage_out_alloc,scl2,sca2,scf2,&
      & sch2,scs2,leaf_litter,cwd,root_litter,nitrogen_uptake, phosphorus_uptake,&
      & litter_nutrient_content, limiting_nutrient, c_costs_of_uptake,&
      & uptk_strategy)


      ! PARAMETERS
      integer(i_2), parameter :: nitrog = 1
      integer(i_2), parameter :: phosph = 2
      integer(i_2), parameter :: colimi = 3

      integer(i_2), parameter :: leaf = 1
      integer(i_2), parameter :: wood = 2
      integer(i_2), parameter :: root = 3
      integer(i_2), parameter :: sapwood = 4
      ! The Nutrient uptake possible strategies
      ! Soluble inorg_n_pool = (1, 2, 3, 4)
      ! Organic N pool = (5, 6)
      ! Soluble inorg_p_pool = (1, 2, 3, 4)
      ! Organic P pool = (5, 6, 7)
      ! Insoluble inorg p pool = (8)

      ! ------UPTAKE STRATEGIES CODE

      ! integer(i_4), parameter ::  nma   = 1 ,& ! (Nut. = N/P) Active uptake by root AM colonized on Solution N/P pool (costs of)
      !                           & nme   = 2 ,& ! (Nut. = N/P) Active uptake by root EM colonized on Solution N/P pool
      !                           & am    = 3 ,& ! (Nut. = N/P) Active uptake by root AM hyphae on Solution N/P pool
      !                           & em    = 4 ,& ! (Nut. = N/P) Active uptake by root EM hyphae on Solution N/P pool
      !                           & ramAP = 5 ,& ! (Nut. = P)   PA - Active P uptake by root AM colonized via PA-phosphatase activity on Organic P pool
      !                           & AM0   = 5 ,& ! (Nut. = N)   NA - Active N uptake by root AM hyphae via NA nitrogenase activity on Ornagic N pool
      !                           & remAP = 6 ,& ! (Nut. = P)   PA - Active P uptake by root EM colonized via PA-phosphatase activity on Organic P pool
      !                           & EM0   = 6 ,& ! (Nut. = N)   NA - Active N uptake by EM hyphae via NA nitrogenase activity on Ornagic N pool
      !                           & AMAP  = 7 ,& ! (Nut. = P)   PA - Active P uptake by AM hyphae production of Phosphatase to clive opganic P
      !                           & EM0x  = 8    ! (Nut. = P)   Active P uptake via Exudation activity (e.g. oxalates) on strong sorbed inorganic P (or primary)


      ! variables I/O
      real(r_8),dimension(ntraits),intent(in) :: dt  ! PLS attributes
      real(r_4),intent(in) :: npp  ! npp (KgC/m2/yr) gpp (µmol m-2 s)
      real(r_8),intent(in) :: npp_costs ! Carbon costs of Nutrient active uptake and retranslocation
      real(r_4),intent(in) :: ts   ! soil temp °C
      real(r_4),intent(in) :: wsoil! soil water depth (mm)
      real(r_8),intent(in) :: te   ! plant transpiration (mm/s)
      real(r_8),dimension(3), intent(in) :: scl1 ! previous day carbon content on leaf compartment (KgC/m2)
      real(r_8),intent(in) :: sca1 ! previous day carbon content on aboveground woody biomass compartment(KgC/m2)
      real(r_8),intent(in) :: scf1 ! previous day carbon content on fine roots compartment (KgC/m2)
      real(r_8),intent(in) :: scs1 ! previous day carbon content on sapwood compartment (KgC/m2)
      real(r_8),intent(in) :: sch1 ! previous day carbon content on heartwood compartment (KgC/m2)
      real(r_4),intent(in) :: nmin ! N in mineral N pool(g m-2) SOLUTION
      real(r_4),intent(in) :: plab ! P in labile pool (g m-2)   SOLUTION
      real(r_8),intent(in) :: on,sop,op ! Organic N, Sorbed P, Organic P
      real(r_8),dimension(3),intent(in) :: storage ! Three element array- storage pool([C,N,P]) g m-2
      ! O
      real(r_8),dimension(3),intent(out) :: storage_out_alloc
      real(r_8),dimension(3), intent(out) :: scl2 ! final carbon content on leaf compartment (KgC/m2)
      real(r_8),intent(out) :: sca2 ! final carbon content on aboveground woody biomass compartment (KgC/m2)
      real(r_8),intent(out) :: scf2 ! final carbon content on fine roots compartment (KgC/m2)
      real(r_8),intent(out) :: scs2 ! final carbon content on sapwwod compartment (KgC/m2)
      real(r_8),intent(out) :: sch2 ! final carbon content on sapwwod compartment (KgC/m2)
      real(r_8),intent(out) :: cwd  ! coarse wood debris (to litter)(C) g m-2
      real(r_8),intent(out) :: root_litter ! to litter g(C) m-2
      real(r_8),intent(out) :: leaf_litter ! to litter g(C) m-2
      real(r_8), dimension(2),intent(out) :: nitrogen_uptake ! N plant uptake g(N) m-2  INDEX //avail_n = 1, on = 2//
      real(r_8), dimension(3),intent(out) :: phosphorus_uptake ! P plant uptake g(P) m-2  INDEX //avail_p = 1, sop = 2, op=3//
      real(r_8),dimension(6),intent(out) :: litter_nutrient_content ! [(lln),(rln),(cwdn),(llp),(rlp),(cwdp)] g m-2
      integer(i_2), dimension(3), intent(out) :: limiting_nutrient
      real(r_8), intent(out) :: c_costs_of_uptake
      integer(i_4), dimension(2), intent(out) :: uptk_strategy


      ! Auxiliary variables
      real(r_8) :: nuptk ! N plant uptake g(N) m-2
      real(r_8) :: puptk ! P plant uptake g(P) m-2
      real(r_8) :: scf2_tmp ! Store veg carbon pool in a 64bit fp
      real(r_8) :: sca2_tmp
      real(r_8), dimension(3) :: scl2_tmp
      real(r_8) :: leaf_av_n
      real(r_8) :: wood_av_n
      real(r_8) :: root_av_n
      real(r_8) :: leaf_av_p
      real(r_8) :: wood_av_p
      real(r_8) :: root_av_p

      real(r_8) :: npp_pot ! potential npp g m-2 day-1
      real(r_8), dimension(4) :: daily_growth ! amount of carbon allocated to leaf/wood/root g m-2 day-1
      real(r_8), dimension(3, 2) :: real_npp
      real(r_8), dimension(3) :: age_limits
      logical(l_1), dimension(3, 2) :: is_limited
      logical(l_1) :: lim_aux, kappa, test34, test35

      real(r_8) :: npp_sapwood , npp_root , npp_leaf  ! Partitioned npp (g(C) m-2 day-1)
      real(r_8) :: npp_aux

      ! Auxiliary variables to calculate Plant Nutrient Uptake
      real(r_8) :: aux1
      real(r_8) :: nscl  ! g(N) m-2
      real(r_8) :: nsca  ! g(N) m-2
      real(r_8) :: nscf  ! g(N) m-2
      real(r_8) :: pscl  ! g(P) m-2
      real(r_8) :: psca  ! g(P) m-2
      real(r_8) :: pscf  ! g(P) m-2

      ! traits
      real(r_8) :: resorpt_frac
      real(r_8) :: aleaf     ! allocatation to plant compartments
      real(r_8) :: awood
      real(r_8) :: aroot
      real(r_8) :: tleaf  ! Residence time(yr)
      real(r_8) :: twood
      real(r_8) :: troot
      real(r_8) :: leaf_n2c  ! N:C ratios
      real(r_8) :: wood_n2c
      real(r_8) :: root_n2c
      real(r_8) :: leaf_p2c  ! P:C ratios
      real(r_8) :: wood_p2c
      real(r_8) :: root_p2c
      real(r_8) :: pdia
      real(r_8) :: amp

      real(r_8) :: avail_n
      real(r_8) :: avail_p
      real(r_8) :: internal_n
      real(r_8) :: internal_n_leaf
      real(r_8) :: internal_n_wood
      real(r_8) :: internal_n_root
      real(r_8) :: internal_p
      real(r_8) :: internal_p_leaf
      real(r_8) :: internal_p_wood
      real(r_8) :: internal_p_root
      real(r_8), dimension(3) :: n_uptake, p_uptake, rn_uptake, rp_uptake
      real(r_8), dimension(3) :: n_to_storage, p_to_storage
      real(r_8) :: to_storage_234, from_sto2npp
      real(r_8) :: mult_factor_n, mult_factor_p
      real(r_8) :: n_leaf, p_leaf, new_leaf_n2c, new_leaf_p2c, leaf_litter_o
      real(r_8) :: fluxc_1, fluxc_2  !fluxo de C dentre a coorte 1-2 e 2-3. deixa aqui mesmo?
      real(r_8) :: root_litter_o
      real(r_8) :: cwd_o
      real(r_8) :: heart_o

      ! CC auxiliary
      real(r_8) :: npp_to_fixer, n_fixed
      real(r_8), dimension(2) :: to_pay, to_sto, plant_uptake
      real(r_8), dimension(6) :: ccn ! Carbon Costs of  N uptake by strategy :
      real(r_8), dimension(8) :: ccp ! CC of P uptake by strategy
      real(r_8) :: active_nupt_cost, active_pupt_cost
      integer(i_4) :: naquis_strat, paquis_strat
      real(r_8) :: p_cost_resorpt, n_cost_resorpt
      real(r_8) :: negative_one
      real(r_8) :: aux_on, aux_sop, aux_op

      !ALLOMETRY VARIABLES
      
      real(r_8) :: teste_one
      real(r_8) :: tau1
      real(r_8) :: tau2
      real(r_8) :: tau3
      real(r_8) :: SS
      real(r_8) :: H
      real(r_8) :: searched_x
      real(r_8) :: x
      real(r_8) :: delta_leaf
      real(r_8) :: delta_root
      real(r_8) :: delta_sapwood
      real(r_8) :: carbon_sapwood !variável de teste para calculo do sapwood
      real(r_8) :: carbon_heartwood !variável de teste para calculo do sapwood
      real(r_8) :: heart 
      real(r_8) :: scs1_previous_day !previous day day carbon content on sapwood (in order to updt using allometric restrictions)
      real(r_8) :: sch1_previous_day !previous day day carbon content on heart (in order to updt using allometric restrictions)
      real(r_8) :: sca1_previous_day !previous day day carbon content on heart (in order to updt using allometric restrictions)


      ! real(r_8) :: test_a = 1
      ! real(r_8) :: test_b = 2


      ! initialize ALL outputs
      storage_out_alloc            = (/0.0D0, 0.0D0, 0.0D0/)
      litter_nutrient_content = (/0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/)
      scl2(:)                = 0.0D0
      scf2                   = 0.0D0
      sca2                   = 0.0D0
      scs2                   = 0.0D0
      sch2                   = 0.0D0
      cwd                    = 0.0D0
      root_litter            = 0.0D0
      leaf_litter            = 0.0D0
      c_costs_of_uptake      = 0.0D0
      nitrogen_uptake(:)     = 0.0D0
      phosphorus_uptake(:)   = 0.0D0
      limiting_nutrient(:)   = 0
      uptk_strategy(:)       = 0
      tau1                   = 0.0D0
      tau2                   = 0.0D0
      tau3                   = 0.0D0
      SS                     = 0.0D0
      H                      = 0.0D0
      searched_x             = 0.0D0
      delta_leaf             = 0.0D0
      delta_root             = 0.0D0
      delta_sapwood          = 0.0D0
      heart                  = 0.0D0

      ! initialize uptake/carbon costs related variables
      nuptk                  = 0.0D0
      puptk                  = 0.0D0
      avail_n                = 0.0D0
      avail_p                = 0.0D0
      internal_n             = 0.0D0
      internal_n_leaf        = 0.0D0
      internal_n_wood        = 0.0D0
      internal_n_root        = 0.0D0
      internal_p             = 0.0D0
      internal_p_leaf        = 0.0D0
      internal_p_wood        = 0.0D0
      internal_p_root        = 0.0D0
      n_uptake(:)            = 0.0D0
      p_uptake(:)            = 0.0D0
      rn_uptake(:)           = 0.0D0
      rp_uptake(:)           = 0.0D0
      scf2_tmp               = 0.0D0
      sca2_tmp               = 0.0D0
      scl2_tmp(:)            = 0.0D0
      leaf_av_n              = 0.0D0
      wood_av_n              = 0.0D0
      root_av_n              = 0.0D0
      leaf_av_p              = 0.0D0
      wood_av_p              = 0.0D0
      root_av_p              = 0.0D0

      npp_pot                = 0.0D0
      daily_growth(:)        = 0.0D0
      real_npp               = 0.0D0

      ! 0 for passive uptake
      naquis_strat           = 0
      paquis_strat           = 0

      pdia                   = 0.0D0
      amp                    = 0.0D0
      npp_to_fixer           = 0.0D0
      n_fixed                = 0.0D0
      active_nupt_cost       = 0.0D0
      active_pupt_cost       = 0.0D0
      p_cost_resorpt         = 0.0D0
      n_cost_resorpt         = 0.0D0
      negative_one           = 0.0D0
      aux_on                 = 0.0D0
      aux_sop                = 0.0D0
      aux_op                 = 0.0D0
      to_pay(:)              = 0.0D0
      to_sto(:)              = 0.0D0
      plant_uptake(:)        = 0.0D0
      ccn(:)                 = 0.0D0
      ccp(:)                 = 0.0D0




      ! Catch the functional/demographic traits of pls
      !  head = ['g1', 'resopfrac', 'tleaf', 'twood', 'troot', 'aleaf', 'awood', 'aroot', 'c4',
      !  'leaf_n2c', 'awood_n2c', 'froot_n2c', 'leaf_p2c', 'awood_p2c', 'froot_p2c', pdia, amp]
      resorpt_frac = dt(2)
      tleaf = dt(3) ! RESIDENCE TIME (years)
      twood = dt(4)
      troot = dt(5)
      aleaf = dt(6) ! ALLOCATION  (proportion  %/100)
      awood = dt(7)
      aroot = dt(8)

      leaf_n2c = dt(10) ! Nutrient:Carbon Ratios gg⁻¹
      wood_n2c = dt(11)
      root_n2c = dt(12)
      leaf_p2c = dt(13)
      wood_p2c = dt(14)
      root_p2c = dt(15)
      pdia = dt(16)
      amp = dt(17)




     

      ! If there is not nutrients or NPP then no allocation process
      ! only deallocation label 294
      if(nmin .le. 0.0 .and. storage(2) .le. 0.0D0) then
         daily_growth(wood) = 0.0D0
         daily_growth(root) = 0.0D0
         daily_growth(leaf) = 0.0D0
         daily_growth(sapwood) = 0.0D0         
         nuptk = 0.0D0
         puptk = 0.0D0
         storage_out_alloc(1) = max(0.0D0, real(npp,kind=r_8) * (1000.0D0 / 365.242D0))
         storage_out_alloc(2) = 0.0D0
         storage_out_alloc(3) = max(storage(3), 0.0D0)
         goto 294
      endif

      if(plab .le. 0.0D0 .and. storage(3) .le. 0.0D0) then
         daily_growth(wood) = 0.0D0
         daily_growth(root) = 0.0D0
         daily_growth(leaf) = 0.0D0
         daily_growth(sapwood) = 0.0D0
         nuptk = 0.0D0
         puptk = 0.0D0
         storage_out_alloc(1) = max(0.0D0, real(npp,kind=r_8) * (1000.0D0 / 365.242D0))
         storage_out_alloc(2) = max(storage(2), 0.0D0)
         storage_out_alloc(3) = 0.0D0
         goto 294
      endif

      if(npp .le. 0.0 .and. storage(1) .le. 0.0D0) then
         daily_growth(wood) = 0.0D0
         daily_growth(root) = 0.0D0
         daily_growth(leaf) = 0.0D0
         daily_growth(sapwood) = 0.0D0
         nuptk = 0.0D0
         puptk = 0.0D0
         storage_out_alloc(1) = storage(1)
         storage_out_alloc(2) = storage(2)
         storage_out_alloc(3) = storage(3)
         goto 294
      endif

      !# if you reach this point ---> There is C and nutrients to allocate!

      ! INTERNAL VARIABLES
      scf2_tmp = 0.0D0
      sca2_tmp = 0.0D0
      scl2_tmp(:) = 0.0D0
      npp_pot  = 0.0D0
      avail_n = 0.0D0
      avail_p = 0.0D0
      scs1_previous_day = 0.0D0
      sch1_previous_day = 0.0D0
      sca1_previous_day = 0.0D0
      ! You have: kg m-2 year-1
      ! You want: g m-2 day-1
      npp_pot = (real(npp,kind=r_8) * (1000.0D0 / 365.242D0)) ! Transform Kg m-2 Year-1 to g m-2 day
      daily_growth = 0.0D0
      npp_to_fixer = npp_pot * pdia
      npp_pot = npp_pot - npp_to_fixer - npp_costs

      ! START STORAGE_OUT_alloc
      storage_out_alloc(1) = 0.0D0
      ! If there is not enough npp to pay uptake the you have to pay tomorrow
      negative_one = 0.0D0

      ! SUM UP STORAGE AND NPP to create POTNPP
      if(storage(1) .gt. 0.0D0) then
         from_sto2npp = 0.15D0 * storage(1)
         npp_pot = npp_pot + from_sto2npp
         storage_out_alloc(1) = storage(1) - from_sto2npp
      endif

      if(npp_pot .le. 0.0D0 .and. storage_out_alloc(1) .le. 0.0D0) then
         daily_growth(wood) = 0.0D0
         daily_growth(root) = 0.0D0
         daily_growth(leaf) = 0.0D0
         nuptk = 0.0D0
         puptk = 0.0D0
         storage_out_alloc(1) = storage(1)
         storage_out_alloc(2) = storage(2)
         storage_out_alloc(3) = storage(3)
         if(npp_pot .lt. 0.0D0) negative_one = abs(npp_pot)
         goto 294
      else
         if (npp_pot .gt. 0.0D0) goto 29
         npp_pot = npp_pot + storage_out_alloc(1)
         storage_out_alloc(1) = 0.0D0
         if(npp_pot .lt. 0.0D0)then
             negative_one = abs(npp_pot) ! amount of nutrient uptk that must be paid
             goto 294
         endif
      endif
29 continue

      ! Potential NPP for each compartment
      ! Partitioning NPP for CVEG pools

      ! POTENTIAL NPP FOR EACH POOL (WITH NO NUTRIENT LIMITATION)

      ! Use the bisection method (function below) to solve the leaf mass increment
      npp_leaf = bisection_method(0.0, 3.0) !the new allocation logic, considering allometry

      ! Once we have the leaf mass increment we can cant get 
      ! root mass increment based on the LTOR constant
      npp_root = (npp_leaf + sum(scl1)) / ltor - scf1

      if (awood .gt. 0.0D0) then  !new logic.
         npp_sapwood = npp_pot - npp_leaf - npp_root   ! g(C)m⁻² !new logic.
      else
         npp_sapwood = 0.0 !new logic.
      endif

      ! ==================== OLD LOGIC ======================= !
      ! npp_leaf =  aleaf * npp_pot    ! g(C)m⁻² !old logic.
      ! npp_root =  aroot * npp_pot    ! g(C)m⁻² !old logic.

      ! if (awood .gt. 0.0D0) then  !old logic.
      !    npp_wood =  awood * npp_pot    ! g(C)m⁻² !old logic.
      ! else
      !    npp_wood = 0.0 !old logic.
      ! endif
      ! ====================================================== !

      ! POTENTIAL NUTRIENT UPTAKE
      nscl = npp_leaf * leaf_n2c    ! NITROGEN TO ALLOCATE LEAF NPP g(N)m⁻²
      pscl = npp_leaf * leaf_p2c    ! g(P)m⁻²
      nscf = npp_root * root_n2c    ! NITROGEN TO ALLOCATE ROOT NPP g(N)m⁻²
      pscf = npp_root * root_p2c    ! g(P)m⁻²'

      if (awood .gt. 0.0D0) then
         nsca = npp_sapwood * wood_n2c    ! [...] g(N)m⁻²'
         psca = npp_sapwood * wood_p2c    ! g(P)m⁻²
      else
         nsca = 0.0D0
         psca = 0.0D0
      endif

      ! Partitioning Nutrients for cveg pools (weight by allocation coeffs)
      ! FIND AVAILABLE NUTRIENTS:
      ! Only a very small amount of total nutrients are available in fact
      mult_factor_n  = 0.075D0
      mult_factor_p  = 0.0080D0
      avail_n = (mult_factor_n * nmin) !g m⁻²
      avail_p = (mult_factor_p * plab) !g m⁻²

      ! Auxiliary to calculate Carbon costs of Nutrient uptake/uptake of nutrients
      if (on .lt. 0.0D0) then
         aux_on = 0.0D0
      else
         aux_on = on * mult_factor_n
      endif
      if (op .lt. 0.0D0) then
         aux_op = 0.0D0
      else
         aux_op = op * mult_factor_p
      endif
      if (sop .lt. 0.0D0) then
         aux_sop = 0.0D0
      else
         aux_sop = sop * mult_factor_p * 0.3
      endif

      ! NITROGEN FIXATION goes direct to plant use
      n_fixed = fixed_n(npp_to_fixer, ts)

      ! SUM UP THE STORED NUTRIENTS AND DIVIDE IT AMONG PLANT POOLS
      ! GET THE VALUES OF STORAGE NUTRIENT POOLS for subsequent subtraction of uptk
      internal_n = storage(2) + n_fixed
      internal_n_leaf = internal_n * aleaf
      internal_n_wood = internal_n * awood
      internal_n_root = internal_n * aroot
      internal_p = storage(3)
      internal_p_leaf = internal_p * aleaf
      internal_p_wood = internal_p * awood
      internal_p_root = internal_p * aroot

      ! Calculate the available nutirents for uptake
      leaf_av_n = (avail_n * aleaf) + internal_n_leaf ! g(N)m⁻²
      leaf_av_p = (avail_p * aleaf) + internal_p_leaf ! g(P)m⁻²

      root_av_n = (avail_n * aroot) + internal_n_root
      root_av_p = (avail_p * aroot) + internal_p_root

      if (awood .gt. 0.0D0) then
         wood_av_p = (avail_p * awood) + internal_p_wood
         wood_av_n = (avail_n * awood) + internal_n_wood
      else
         wood_av_p = 0.0D0
         wood_av_n = 0.0D0
      endif

      ! START NURTIENT STORAGE FOR OUTPUT  storage of NUTRIENTS
      storage_out_alloc(2) = 0.0D0 ! N
      storage_out_alloc(3) = 0.0D0 ! P

      !----------END OF POTENTIAL VALUES CALCULATION (in MASS)

      ! FIND REALIZED NPP
      ! (Nutrient) LIMITED NPP TO(CVEGpool):

      ! DEFINITION OF SOME IMPORTANT ARRAYS

      ! is_limited will store the truth value of limitation
      ! DIM(3, 2)
      !    !       N          !        P      !
      !____!__________________!_______________!
      !LEAF!       V/F        !         V/F   !
      !WOOD!       V/F        !         V/F   !
      !ROOT!       V/F        !         V/F   !

      ! real_npp will store the npp for each pool
      ! DIM(3, 2)
      !    !       N          !        P      !
      !____!__________________!_______________!
      !LEAF!     NPP          !       NPP     !
      !WOOD!     NPP          !       NPP     !
      !ROOT!     NPP          !       NPP     !

      real_npp(:,:) = 0.0D0 !NPP CONSIDERING ALL LIMITATIONS
      is_limited(:,:) = .false.

      ! FIND REALIZED NPP
      !----------------------NITROGEN-------------------------
      ! LEAF -------------------

      ! AUX variables
      lim_aux = .false.
      npp_aux = 0.0D0
      CALL realized_npp(npp_leaf, nscl, leaf_av_n, npp_aux, lim_aux)

      ! Limitation
      is_limited(leaf, nitrog) = lim_aux
      ! NPP
      real_npp(leaf, nitrog) = npp_aux


      ! WOOD -------------------
      ! Clean aux
      lim_aux = .false.
      npp_aux = 0.0D0
      if (awood .gt. 0.0D0) then
         CALL realized_npp(npp_sapwood, nsca, wood_av_n, npp_aux, lim_aux)
      endif

      ! Limitation
      is_limited(wood, nitrog) = lim_aux
      ! NPP
      real_npp(wood, nitrog) = npp_aux

      ! ROOT -------------------
      ! Clean aux
      lim_aux = .false.
      npp_aux = 0.0D0
      CALL realized_npp(npp_root, nscf, root_av_n, npp_aux, lim_aux)

      ! Limitation
      is_limited(root, nitrog) = lim_aux
      ! NPP
      real_npp(root, nitrog) = npp_aux

      lim_aux = .false.
      !--------------------------------END NITROGEN-------------------------------

      !---------------------------------PHOSPHORUS--------------------------------
      ! LEAF ----------------------

      ! AUX variables
      lim_aux = .false.
      npp_aux = 0.0D0
      CALL realized_npp(npp_leaf, pscl, leaf_av_p, npp_aux, lim_aux)

      ! Limitation
      is_limited(leaf, phosph) = lim_aux
      ! NPP
      real_npp(leaf, phosph) = npp_aux

      ! WOOD
      ! Clean aux
      lim_aux = .false.
      npp_aux = 0.0D0
      if (awood .gt. 0.0D0) then
         CALL realized_npp(npp_sapwood, psca, wood_av_p, npp_aux, lim_aux)
      endif

      ! Limitation
      is_limited(wood, phosph) = lim_aux
      ! NPP
      real_npp(wood, phosph) = npp_aux

      ! ROOT
      ! Clean aux
      lim_aux = .false.
      npp_aux = 0.0D0
      CALL realized_npp(npp_root, pscf, root_av_p, npp_aux, lim_aux)

      ! Limitation
      is_limited(root, phosph) = lim_aux
      ! NPP
      real_npp(root, phosph) = npp_aux

      lim_aux = .false.
      npp_aux = 0.0D0

      ! (DONE) FIND REALIZED NPP (DONE)

      if (.not. any(is_limited)) then
         daily_growth(leaf) = npp_leaf
         daily_growth(root) = npp_root
         if(awood .gt. 0.0D0) then
            daily_growth(wood) = npp_sapwood
         else
            daily_growth(wood) = 0.0D0
         endif

         limiting_nutrient(leaf) = 0
         limiting_nutrient(wood) = 0
         limiting_nutrient(root) = 0

         rn_uptake(leaf) = daily_growth(leaf) * leaf_n2c
         rn_uptake(root) = daily_growth(root) * root_n2c
         rn_uptake(wood) = daily_growth(wood) * wood_n2c
         rp_uptake(leaf) = daily_growth(leaf) * leaf_p2c
         rp_uptake(root) = daily_growth(root) * root_p2c
         rp_uptake(wood) = daily_growth(wood) * wood_p2c
      else
         ! THERE IS LIMITATION
         !! LEAF
         ! FIND THE REALIZED NPP GIVEN THE POSSIBLE COMBINATIONS OF LIMITATION
         if (.not. any(is_limited(leaf, :))) then
            ! NO LIMITATION IN THE LEAF POOL
            limiting_nutrient(leaf) = 0
            daily_growth(leaf) = npp_leaf
            to_storage_234 = 0.0D0
            rn_uptake(leaf) = daily_growth(leaf) * leaf_n2c
            rp_uptake(leaf) = daily_growth(leaf) * leaf_p2c
         else
         ! IDENTIFY LIMITING
            if(is_limited(leaf, nitrog) .and. is_limited(leaf, phosph)) then
               kappa = abs(real_npp(leaf, nitrog) - real_npp(leaf, phosph)) .lt. 1D-6

               if (kappa) then
                  limiting_nutrient(leaf) = colimi + colimi
               else if(real_npp(leaf, nitrog) .gt. real_npp(leaf, phosph)) then
                  limiting_nutrient(leaf) = colimi + phosph
               else
                  limiting_nutrient(leaf) = colimi + nitrog
               endif
            else
               if(is_limited(leaf, nitrog)) then
                   limiting_nutrient(leaf) = nitrog
               endif

               if(is_limited(leaf, phosph)) then
                   limiting_nutrient(leaf) = phosph
               endif
            endif
            daily_growth(leaf) = min(real_npp(leaf,nitrog), real_npp(leaf,phosph))
            to_storage_234 = npp_leaf - daily_growth(leaf)
            storage_out_alloc(1) = add_pool(storage_out_alloc(1), to_storage_234)
            to_storage_234 = 0.0D0
            rn_uptake(leaf) = ((daily_growth(leaf) * leaf_av_n) / npp_leaf)
            rp_uptake(leaf) = ((daily_growth(leaf) * leaf_av_p) / npp_leaf)
         endif
         ! ROOT
         if (.not. any(is_limited(root, :))) then
            ! NO LIMITATION IN THE root POOL
            limiting_nutrient(root) = 0
            daily_growth(root) = npp_root
            to_storage_234 = 0.0D0
            rn_uptake(root) = daily_growth(root) * root_n2c
            rp_uptake(root) = daily_growth(root) * root_p2c
         else
         ! IDENTIFY LIMITING
            if(is_limited(root, nitrog) .and. is_limited(root, phosph)) then
               kappa = abs(real_npp(root, nitrog) - real_npp(root, phosph)) .lt. 1D-6

               if(kappa) then
                  limiting_nutrient(root) = colimi + colimi
               else if(real_npp(root, nitrog) .gt. real_npp(root, phosph)) then
                  limiting_nutrient(root) = colimi + phosph
               else
                  limiting_nutrient(root) = colimi + nitrog
               endif
            else
               if(is_limited(root, nitrog)) then
                   limiting_nutrient(root) = nitrog
               endif

               if(is_limited(root, phosph)) then
                   limiting_nutrient(root) = phosph
               endif
            endif
            daily_growth(root) = min(real_npp(root,nitrog), real_npp(root,phosph))
            to_storage_234 = npp_root - daily_growth(root)
            storage_out_alloc(1) = add_pool(storage_out_alloc(1), to_storage_234)
            to_storage_234 = 0.0D0
            rn_uptake(root) = ((daily_growth(root) * root_av_n) / npp_root)
            rp_uptake(root) = ((daily_growth(root) * root_av_p) / npp_root)
         endif

         ! WOOD
         if(awood .gt. 0.0D0) then
            if (.not. any(is_limited(wood, :))) then
               ! NO LIMITATION IN THE wood POOL
               limiting_nutrient(wood) = 0
               daily_growth(wood) = npp_sapwood
               to_storage_234 = 0.0D0
               rn_uptake(wood) = daily_growth(wood) * wood_n2c
               rp_uptake(wood) = daily_growth(wood) * wood_p2c
            else
            ! IDENTIFY LIMITING
               if(is_limited(wood, nitrog) .and. is_limited(wood, phosph)) then
                  kappa = abs(real_npp(wood, nitrog) - real_npp(wood, phosph)) .lt. 1D-6

                  if(kappa) then
                     limiting_nutrient(wood) = colimi + colimi
                  else if(real_npp(wood, nitrog) .gt. real_npp(wood, phosph)) then
                     limiting_nutrient(wood) = colimi + phosph
                  else
                     limiting_nutrient(wood) = colimi + nitrog
                  endif
               else
                  if(is_limited(wood, nitrog)) then
                      limiting_nutrient(wood) = nitrog
                  endif
                  if(is_limited(wood, phosph)) then
                      limiting_nutrient(wood) = phosph
                  endif
               endif
               daily_growth(wood) = min(real_npp(wood,nitrog), real_npp(wood,phosph))
               to_storage_234 = npp_sapwood - daily_growth(wood)
               storage_out_alloc(1) = add_pool(storage_out_alloc(1), to_storage_234)
               to_storage_234 = 0.0D0
               rn_uptake(wood) = ((daily_growth(wood) * wood_av_n) / npp_sapwood)
               rp_uptake(wood) = ((daily_growth(wood) * wood_av_p) / npp_sapwood)
            endif
         else
            daily_growth(wood) = 0.0D0
         endif
      endif

      !CALCULATE UPTAKE

      n_to_storage(:) = 0.0D0
      p_to_storage(:) = 0.0D0
      n_uptake(:) = 0.0D0
      p_uptake(:) = 0.0D0


      if(rn_uptake(leaf) .gt. internal_n_leaf) then
         n_uptake(leaf) = rn_uptake(leaf) - internal_n_leaf
         n_to_storage(leaf) = 0.0D0
      else
         n_uptake(leaf) = 0.0D0
         n_to_storage(leaf) = max(0.0D0, (internal_n_leaf - rn_uptake(leaf)))
      endif

      if(rp_uptake(leaf) .gt. internal_p_leaf) then
         p_uptake(leaf) = rp_uptake(leaf) - internal_p_leaf
         p_to_storage(leaf) =  0.0D0
      else
         p_uptake(leaf) = 0.0D0
         p_to_storage(leaf) = max(0.0D0, (internal_p_leaf - rp_uptake(leaf)))
      endif

      if(awood .gt. 0.0D0) then
         if(rn_uptake(wood) .gt. internal_n_wood) then
            n_uptake(wood) = rn_uptake(wood) - internal_n_wood
            n_to_storage(wood) = 0.0D0
         else
            n_uptake(wood) = 0.0D0
            n_to_storage(wood) = max(0.0D0, (internal_n_wood - rn_uptake(wood)))
         endif

         if(rp_uptake(wood) .gt. internal_p_wood) then
            p_uptake(wood) = rp_uptake(wood) - internal_p_wood
            p_to_storage(wood) =  0.0D0
         else
            p_uptake(wood) = 0.0D0
            p_to_storage(wood) = max(0.0D0, (internal_p_wood - rp_uptake(wood)))
         endif
      endif

      if(rn_uptake(root) .gt. internal_n_root) then
         n_uptake(root) = rn_uptake(root) - internal_n_root
         n_to_storage(root) = 0.0D0
      else
         n_uptake(root) = 0.0D0
         n_to_storage(root) = max(0.0D0, (internal_n_root - rn_uptake(root)))
      endif

      if(rp_uptake(root) .gt. internal_p_root) then
         p_uptake(root) = rp_uptake(root) - internal_p_root
         p_to_storage(root) =  0.0D0
      else
         p_uptake(root) = 0.0D0
         p_to_storage(root) = max(0.0D0, (internal_p_root - rp_uptake(root)))
      endif

      storage_out_alloc(2) = add_pool(storage_out_alloc(2), sum(n_to_storage))
      storage_out_alloc(3) = add_pool(storage_out_alloc(3), sum(p_to_storage))
      nuptk = sum(n_uptake)
      puptk = sum(p_uptake)

      ! ! CALCULATE CARBON COSTS OF NUTRIENT UPTAKE (gC g(N/P)-1)
      ! 1 - Check Passve uptake

      call passive_uptake(wsoil, avail_n, avail_p, nuptk, puptk, te, &
                        & to_pay, to_sto, plant_uptake)
      ! N
      ccn(:) = 0.0D0
      active_nupt_cost = 0.0D0
      naquis_strat = 0   ! Passive uptake
      nitrogen_uptake(:) = 0.0D0
      if (to_pay(1) .gt. 0.0D0) then
         call active_costn(amp, avail_n - plant_uptake(1), aux_on, scf1 * 1D3, ccn)
         call select_active_strategy(ccn, active_nupt_cost, naquis_strat)
         call prep_out_n(naquis_strat, nuptk, to_pay(1), nitrogen_uptake)
      else
         nitrogen_uptake(1) = nuptk
         nitrogen_uptake(2) = 0.0D0
      endif
      test34 = nitrogen_uptake(2) .gt. on
      uptk_strategy(1) = naquis_strat
      storage_out_alloc(2) = add_pool(storage_out_alloc(2), to_sto(1))

      !  P
      ccp(:) = 0.0D0
      active_pupt_cost = 0.0D0
      paquis_strat = 0
      phosphorus_uptake(:) = 0.0D0
      if(to_pay(2) .gt. 0.0D0) then
         call active_costp(amp, avail_p - plant_uptake(2), aux_sop, aux_op, scf1 * 1D3, ccp)
         call select_active_strategy(ccp, active_pupt_cost, paquis_strat)
         call prep_out_p(paquis_strat, puptk, to_pay(2), phosphorus_uptake)
      else
         phosphorus_uptake(1) = puptk
      endif
      uptk_strategy(2) = paquis_strat
      storage_out_alloc(3) = add_pool(storage_out_alloc(3), to_sto(2))
      test34 = phosphorus_uptake(3) .gt. op
      test35 = phosphorus_uptake(2) .gt. sop

      ! TODO calculate enzimatic n costs of enzimatic activity?
      ! ???????????????????????????????????????????????????????

      ! CARBON AND NUTRIENTS TURNOVER
294   continue ! Material going to soil + updating veg pools

      ! LEAF LITTER FLUX and flux C between cohorts
      leaf_litter = scl1(3) / tleaf  !/ tleaf ! kg(C) m-2 year-1
      fluxc_2 = scl1(2) / age_limits(2)
      fluxc_1 = scl1(1) / age_limits(1)

      ! ROOT LITTER
      root_litter = scf1 / troot  !/ tfroot! kg(C) m-2 year-1

!!!!!! UPDATE C content of each compartment in g m-2
      !CONFERIR UNIDADES DO CARBONO e ver se SCL é 1 ou 2 no meio da equação
      scl2(1) = ((1D3 * scl1(1)) + daily_growth(leaf)) - fluxc_1 * 2.73791075D0
      scl2(2) = ((1D3 * scl1(2)) + scl1(1)) - (fluxc_2 * 2.73791075D0)
      scl2(3) = ((1D3 * scl1(3)) + scl1(2)) - (leaf_litter * 2.73791075D0)
      
      scf2 = ((1D3 * scf1) + daily_growth(root)) - (root_litter * 2.73791075D0)

      !inside update,now the carbon contents are calculated considering the allometrics restriction
      carbon_sapwood = sapwood2()
      if(carbon_sapwood.le.0.0D0) then
         carbon_sapwood = 0.0D0
      endif
! !      print*,'carbon_sapwood', carbon_sapwood,'scs1', scs1, 'sca1', sca1

      carbon_heartwood = heartwood2()
      if(carbon_heartwood.le.0.0D0) then
         carbon_heartwood = 0.0D0
      endif
!      print*, 'sca1', sca1, 'sch1', sch1, 'carbon heartwood', carbon_heartwood


      ! ## if it's a woody strategy:
      if(awood .gt. 0.0D0) then
         scs1_previous_day = carbon_sapwood
         heart = (scs1_previous_day*1D3) * turnover_rate_sapwood ![total of sapwood that is converted in heartwood - year-1
         scs2 = (scs1_previous_day*1D3) + daily_growth(wood) - (heart * 2.73791075D0) !quantidade de C final no sap. (internal variable)

         sch1_previous_day = carbon_heartwood
         sch2 = (sch1_previous_day*1D3) + (heart * 2.73791075D0) ! !quantidade de C final no heart. (internal variable)
        
         sca1_previous_day = ((scs1_previous_day*1D3) + (sch1_previous_day*1D3))  !quantidade de C final no caule
         cwd =  sca1_previous_day/ twood !/ tawood! Kg(C) m-2 [total de C do caule todo q vai pro litter] - cálculo de C final no caule
         sca2 = (scs2 + sch2)- (cwd * 2.73791075D0)  !quantidade de C final no caule
        

      else
         cwd = 0.0D0
         heart = 0.0D0
         scs2 = 0.0D0
         sch2 = 0.0D0
         sca2 = 0.0D0
      endif

      ! COnvert kg m-2 year-1 in  g m-2 day-1
      leaf_litter = leaf_litter * 2.73791075D0
      root_litter = root_litter * 2.73791075D0
      heart = heart * 2.73791075D0
      cwd = cwd * 2.73791075D0

      ! END CARBON TURNOVER
      leaf_litter_o =  leaf_litter
      root_litter_o = root_litter
      cwd_o = cwd


      ! Nutrient resorption

      aux1 = 0.0D0
      ! Nutrient N in litter
      if(ieee_is_nan(leaf_litter)) print *, "HAHAAA"
      n_leaf = leaf_litter * leaf_n2c
      ! resorbed_nutrient (N)
      aux1 = n_leaf * resorpt_frac   ! g(N) m-2

      n_cost_resorpt = retran_nutri_cost(n_leaf, aux1, 1)
      ! NEW LITTER N2C
      new_leaf_n2c = 0.0D0
      new_leaf_n2c = (n_leaf - aux1) / leaf_litter

      ! N resorbed goes to storage pool
      storage_out_alloc(2) = add_pool(storage_out_alloc(2), aux1)     ! g(N) m-2

      ! CALCULATE THE NUTRIENT N IN LITTER FLUX
      litter_nutrient_content(1) = leaf_litter_o * new_leaf_n2c            ! g(N)m-2
      litter_nutrient_content(2) = root_litter_o * root_n2c
      if(awood .gt. 0.0D0) then
         litter_nutrient_content(3) = cwd_o * wood_n2c
      else
         litter_nutrient_content(3) = 0.0D0
      endif

      aux1 = 0.0D0

      p_leaf = leaf_litter * leaf_p2c

      ! Resorbed P
      aux1 = p_leaf * resorpt_frac
      p_cost_resorpt = retran_nutri_cost(p_leaf, aux1, 2)

      ! NEW LITTER P2C
      new_leaf_p2c = 0.0D0
      new_leaf_p2c = (p_leaf - aux1) / leaf_litter

      storage_out_alloc(3) = add_pool(storage_out_alloc(3) , aux1)

      ! CALCULATE THE NUTRIENT N IN LITTER FLUX
      litter_nutrient_content(4) = leaf_litter_o * new_leaf_p2c            ! g(P)m-2
      litter_nutrient_content(5) = root_litter_o * root_p2c
      if(awood .gt. 0.0D0) then
         litter_nutrient_content(6) = cwd_o * wood_p2c
      else
         litter_nutrient_content(6) = 0.0D0
      endif
      ! END RETRANSLOCATION CALCULATIONS

      !======================================================================================================
      
      !FINALIZE:

      scl2(:) = scl2(:) * 1.0D-3 !TRANSFOR FROM G/M2 TO KG/M2
      scf2 = scf2 * 1.0D-3 !TRANSFOR FROM G/M2 TO KG/M2
    
      if(awood .gt. 0.0D0) then
         sca2 = sca2 * 1.0D-3 !TRANSFOR FROM G/M2 TO KG/M2 
         scs2 = scs2 * 1.0D-3 !TRANSFOR FROM G/M2 TO KG/M2
         sch2 = sch2 * 1.0D-3 !TRANSFOR FROM G/M2 TO KG/M2
!         print*,'scs2', scs2,'sch2',sch2,'sca2',sca2,'leaf',scl2,'root',scf2
      else
         sca2 = 0.0D0 !TRANSFOR FROM G/M2 TO KG/M2
         scs2 = 0.0D0 !TRANSFOR FROM G/M2 TO KG/M2
         sch2 = 0.0D0 !TRANSFOR FROM G/M2 TO KG/M2
      endif

      !print*, 'LEAF =', scl2, 'ROOT =', scf2, 'WOOD =', sca2

      !======================================================================================================

      c_costs_of_uptake = active_nupt_cost + active_pupt_cost &
      &                   + n_cost_resorpt + p_cost_resorpt + negative_one
      ! END OF CALCULATIONS

      ! teste_one = diameter(dw, k_allom2, k_allom3, pi, sca2)
      ! print*, 'RESULTADO TESTE/DELTA_LEAF =', teste_one


   contains

      function add_pool(a1, a2) result(new_amount)

         real(r_8), intent(in) :: a1, a2
         real(r_8) :: new_amount

         if(a2 .ge. 0.0D0) then
            new_amount = a1 + a2
         else
            new_amount = a1
         endif
      end function add_pool

      !AUXILIARY FUNCTIONS TO ALLOCATION  - Bianca & Bárbara, 2021

      function bisection_method(a, b) result(midpoint)

         real(r_4) :: a, b
         real(r_8) :: aux_a, aux_b
         real(r_8) :: midpoint
         
         aux_a = a
         aux_b = b
 
         if((f(aux_a) * f(aux_b)) .gt. 0) then
             midpoint = -2.0
             return
         endif
         
         do while((aux_b - aux_a) / 2.0 .gt. tol)
             midpoint = (aux_a + aux_b) / 2
             
             if(f(midpoint) .eq. 0.0) then
                 exit            
             elseif(f(aux_a) * f(midpoint) .lt. 0) then
                 aux_b = midpoint
             else
                 aux_a = midpoint
             endif
         end do
      end function bisection_method

      function f(x) result(searched_x)

         real(r_8) :: x
         real(r_8) :: searched_x

         !SCA1 = ABOVEGROUND WOOD TISSUES PORÉM É UTILIZADO >SOMENTE< HEARTWOOD
         
         searched_x = & 
             calc_tau1() * &
             (sapwood2() - x - x / ltor + sch1) - &
             ( &
                 (sapwood2() - x - x / ltor) / &
                 (sum(scl1) + x) * calc_tau3() &
             ) ** calc_tau2()
      end function f

      function calc_tau1() result(tau1)

         real(r_8) :: tau1
         
         tau1 = k_allom2 ** ((2.0 / k_allom3) * 4.0 / 3.14159 / dw)
      end function calc_tau1

      function calc_tau2() result(tau2)

         real(r_8) :: tau2 
         
         tau2 = 1.0 + 2.0 / k_allom3
      end function calc_tau2
      
      function calc_tau3() result(tau3)

         real(r_8) :: tau3
         
         tau3 = klatosa / dw / spec_leaf
      end function calc_tau3

      function sapwood2() result (SS) !The variable sapwood already exist then sapwood2 will be changed in the future
   
         real(r_8) :: SS
        
         SS = scs1 + npp_pot - sum(scl1) / ltor + scf1
      end function sapwood2

       function heartwood2() result (H)

          real(r_8) :: H

          H = sch1 + (turnover_rate_sapwood*scs1)

      end function heartwood2

   end subroutine allocation

end module alloc