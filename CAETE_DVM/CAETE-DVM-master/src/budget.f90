! Copyright 2017- LabTerra

!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.

!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.

!     You should have received a copy of the GNU General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.

! contacts :: David Montenegro Lapola <lapoladm ( at ) gmail.com>
!             João Paulo Darela Filho <darelafilho ( at ) gmail.com>


module budget
   implicit none
   private

   public :: daily_budget

contains

   subroutine daily_budget(dt, w1, g1, s1, ts, temp, prec, p0, ipar, rh&
        &, mineral_n, labile_p, on, sop, op, catm, sto_budg_in, cl1_in, ca1_in, cf1_in, cs1_in, ch1_in&
        &, dleaf_in, dwood_in, droot_in, uptk_costs_in, w2, g2, s2, smavg, ruavg, evavg, epavg, phavg, aravg&
        &, nppavg, laiavg, rcavg, f5avg, rmavg, rgavg, cleafavg_pft, cawoodavg_pft&
        &, cfrootavg_pft, storage_out_bdgt_1, ocpavg, wueavg, cueavg, c_defavg&
        &, vcmax_1, specific_la_1, nupt_1, pupt_1, litter_l_1, cwd_1, litter_fr_1, npp2pay_1, lit_nut_content_1&
        &, delta_cveg_1, limitation_status_1, uptk_strat_1, wp, cp)


      use types
      use global_par
      use alloc
      use productivity
      use omp_lib

      use photo, only: pft_area_frac, sto_resp
      use water, only: evpot2, penman, available_energy, runoff

      !     ----------------------------INPUTS-------------------------------
      real(r_8),dimension(ntraits,npls),intent(in) :: dt
      real(r_4),dimension(npls),intent(in) :: w1   !Initial (previous month last day) soil moisture storage (mm)
      real(r_4),dimension(npls),intent(in) :: g1   !Initial soil ice storage (mm)
      real(r_4),dimension(npls),intent(in) :: s1   !Initial overland snow storage (mm)
      real(r_4),intent(in) :: ts                   ! Soil temperature (oC)
      real(r_4),intent(in) :: temp                 ! Surface air temperature (oC)
      real(r_4),intent(in) :: prec                 ! Precipitation (mm/day)
      real(r_4),intent(in) :: p0                   ! Surface pressure (mb)
      real(r_4),intent(in) :: ipar                 ! Incident photosynthetic active radiation mol Photons m-2 s-1
      real(r_4),intent(in) :: rh                   ! Relative humidity
      real(r_4),intent(in) :: mineral_n            ! Solution N NOx/NaOH gm-2
      real(r_4),intent(in) :: labile_p             ! solution P O4P  gm-2
      real(r_8),intent(in) :: on, sop, op          ! Organic N, isoluble inorganic P, Organic P g m-2
      real(r_8),intent(in) :: catm                 ! ATM CO2 concentration ppm


      real(r_8),dimension(3,npls),intent(in)  :: sto_budg_in ! Rapid Storage Pool (C,N,P)  g m-2
      real(r_8),dimension(3,npls),intent(in) :: cl1_in  ! initial BIOMASS cleaf compartment kgm-2
      real(r_8),dimension(npls),intent(in) :: cf1_in  !                 froot
      real(r_8),dimension(npls),intent(in) :: ca1_in  !                 cawood
      real(r_8),dimension(npls),intent(in) :: cs1_in
      real(r_8),dimension(npls),intent(in) :: ch1_in
      real(r_8),dimension(npls),intent(in) :: dleaf_in  ! CHANGE IN cVEG (DAILY BASIS) TO GROWTH RESP
      real(r_8),dimension(npls),intent(in) :: droot_in  ! k gm-2
      real(r_8),dimension(npls),intent(in) :: dwood_in  ! k gm-2
      real(r_8),dimension(npls),intent(in) :: uptk_costs_in ! g m-2


      !     ----------------------------OUTPUTS------------------------------
      real(r_4),intent(out) :: epavg          !Maximum evapotranspiration (mm/day)
      real(r_8),intent(out) :: smavg          !Snowmelt Daily average (mm/day)
      real(r_8),intent(out) :: ruavg          !Runoff Daily average (mm/day)
      real(r_8),intent(out) :: evavg          !Actual evapotranspiration Daily average (mm/day)
      real(r_8),intent(out) :: phavg          !Daily photosynthesis (Kg m-2 y-1)
      real(r_8),intent(out) :: aravg          !Daily autotrophic respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: nppavg         !Daily NPP (average between PFTs)(Kg m-2 y-1)
      real(r_8),intent(out) :: laiavg         !Daily leaf19010101', '19551231 area Index m2m-2
      real(r_8),intent(out) :: rcavg          !Daily canopy resistence s/m
      real(r_8),intent(out) :: f5avg          !Daily canopy resistence s/m
      real(r_8),intent(out) :: rmavg          !maintenance/growth respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: rgavg          !maintenance/growth respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: wueavg         ! Water use efficiency
      real(r_8),intent(out) :: cueavg         ! [0-1]
      real(r_8),intent(out) :: vcmax_1          ! µmol m-2 s-1
      real(r_8),intent(out) :: specific_la_1    ! m2 g(C)-1
      real(r_8),intent(out) :: c_defavg       ! kg(C) m-2 Carbon deficit due to negative NPP - i.e. ph < ar
      real(r_8),intent(out) :: litter_l_1       ! g m-2
      real(r_8),intent(out) :: cwd_1            ! g m-2
      real(r_8),intent(out) :: litter_fr_1      ! g m-2
      real(r_8),dimension(2),intent(out) :: nupt_1         ! g m-2 (1) from Soluble (2) from organic
      real(r_8),dimension(3),intent(out) :: pupt_1         ! g m-2
      real(r_8),dimension(6),intent(out) :: lit_nut_content_1 ! g(Nutrient)m-2 ! Lit_nut_content variables         [(lln),(rln),(cwdn),(llp),(rl),(cwdp)]

      ! FULL OUTPUT
      real(r_4),dimension(npls),intent(out) :: w2             !Final (last day) soil moisture storage (mm)
      real(r_4),dimension(npls),intent(out) :: g2             !Final soil ice storage (mm)
      real(r_4),dimension(npls),intent(out) :: s2             !Final overland snow storage (mm)
      real(r_8),dimension(3,npls),intent(out) :: cleafavg_pft   !Carbon in plant tissues (kg m-2)
      real(r_8),dimension(npls),intent(out) :: cawoodavg_pft  !
      real(r_8),dimension(npls),intent(out) :: cfrootavg_pft  !
      real(r_8),dimension(npls),intent(out) :: ocpavg         ! [0-1] Gridcell occupation
      real(r_8),dimension(3,npls),intent(out) :: delta_cveg_1
      real(r_8),dimension(3,npls),intent(out) :: storage_out_bdgt_1
      integer(i_2),dimension(3,npls),intent(out) :: limitation_status_1
      integer(i_4),dimension(2,npls),intent(out) :: uptk_strat_1
      real(r_8),dimension(npls),intent(out) ::  npp2pay_1
      real(r_8),dimension(3),intent(out) :: wp, cp

      !     -----------------------Internal Variables------------------------
      integer(i_4) :: p, counter, nlen, ri, i, j
      real(r_8),dimension(ntraits) :: dt1 ! Store one PLS attributes array (1D)
      real(r_8) :: carbon_in_storage
      real(r_8) :: testcdef
      real(r_8) :: sr, mr_sto, growth_stoc
      real(r_8),dimension(npls) :: ocp_mm      ! TODO include cabon of dead plssss in the cicle?
      logical(l_1),dimension(npls) :: ocp_wood
      integer(i_4),dimension(npls) :: run

      real(r_4),parameter :: tsnow = -1.0
      real(r_4),parameter :: tice  = -2.5

      real(r_8),dimension(3,npls) :: cl1_pft
      real(r_8),dimension(npls) :: cf1_pft, ca1_pft, cs1_pft, ch1_pft
      real(r_4) :: soil_temp
      real(r_4) :: psnow                !Snowfall (mm/day)
      real(r_4) :: prain                !Rainfall (mm/day)
      real(r_4) :: emax

      real(r_8),dimension(:),allocatable :: ocp_coeffs
      real(r_4),dimension(:),allocatable :: rimelt !Runoff due to soil ice melting
      real(r_4),dimension(:),allocatable :: smelt  !Snowmelt (mm/day)
      real(r_4),dimension(:),allocatable :: w      !Daily soil moisture storage (mm)
      real(r_4),dimension(:),allocatable :: g      !Daily soil ice storage (mm)
      real(r_4),dimension(:),allocatable :: s      !Daily overland snow storage (mm)
      real(r_4),dimension(:),allocatable :: ds
      real(r_4),dimension(:),allocatable :: dw
      real(r_4),dimension(:),allocatable :: roff   !Total runoff
      real(r_4),dimension(:),allocatable :: evap   !Actual evapotranspiration (mm/day)
      !c     Carbon Cycle
      real(r_4),dimension(:),allocatable :: ph     !Canopy gross photosynthesis (kgC/m2/yr)
      real(r_4),dimension(:),allocatable :: ar     !Autotrophic respiration (kgC/m2/yr)
      real(r_4),dimension(:),allocatable :: nppa   !Net primary productivity / auxiliar
      real(r_8),dimension(:),allocatable :: laia   !Leaf area index (m2 leaf/m2 area)
      real(r_4),dimension(:),allocatable :: rc2    !Canopy resistence (s/m)
      real(r_4),dimension(:,:),allocatable :: f1   !
      real(r_8),dimension(:),allocatable :: f5     !Photosynthesis (mol/m2/s)
      real(r_4),dimension(:),allocatable :: vpd    !Vapor Pressure deficit
      real(r_4),dimension(:),allocatable :: rm     !maintenance & growth a.resp
      real(r_4),dimension(:),allocatable :: rg
      real(r_4),dimension(:),allocatable :: wue
      real(r_4),dimension(:),allocatable :: cue
      real(r_4),dimension(:),allocatable :: c_def
      real(r_8),dimension(:,:),allocatable :: cl1_int
      real(r_8),dimension(:),allocatable :: cf1_int
      real(r_8),dimension(:),allocatable :: ca1_int
      real(r_8),dimension(:),allocatable :: tra
      real(r_8),dimension(:,:),allocatable :: cl2
      real(r_8),dimension(:),allocatable :: cf2
      real(r_8),dimension(:),allocatable :: ca2    ! carbon pos-allocation
      real(r_8),dimension(:),allocatable :: cs2    ! carbon pos-allocation
      real(r_8),dimension(:),allocatable :: ch2    ! carbon pos-allocation
      real(r_8),dimension(:,:),allocatable :: day_storage      ! D0=3 g m-2
      real(r_8),dimension(:),allocatable   :: vcmax            ! µmol m-2 s-1
      real(r_8),dimension(:),allocatable   :: specific_la      ! m2 g(C)-1
      real(r_8),dimension(:,:),allocatable :: nupt             !d0 =2      ! g m-2 (1) from Soluble (2) from organic
      real(r_8),dimension(:,:),allocatable :: pupt             !d0 =3      ! g m-2
      real(r_8),dimension(:),allocatable   :: litter_l         ! g m-2
      real(r_8),dimension(:),allocatable   :: cwd              ! g m-2
      real(r_8),dimension(:),allocatable   :: litter_fr        ! g m-2
      real(r_8),dimension(:),allocatable   :: npp2pay          ! G M-2
      real(r_8),dimension(:,:),allocatable :: lit_nut_content  ! d0=6 g(Nutrient)m-2 ! Lit_nut_content variables         [(lln),(rln),(cwdn),(llp),(rl),(cwdp)]
      real(r_8),dimension(:,:),allocatable :: delta_cveg       ! d0 = 3
      real(r_8),dimension(:,:),allocatable :: storage_out_bdgt ! d0 = 3

      integer(i_2),dimension(:,:),allocatable   :: limitation_status ! D0=3
      integer(i_4), dimension(:, :),allocatable :: uptk_strat        ! D0=2
      INTEGER(i_4), dimension(:), allocatable :: lp ! index of living PLSs

      real(r_8), dimension(npls) :: awood_aux, dleaf, dwood, droot, uptk_costs
      real(r_8), dimension(3,npls) :: sto_budg

      !     START
      !     --------------
      !     Grid cell area fraction 0-1
      !     ============================

      ! create copies of some input variables (arrays) - ( they passed by reference by standard)
      do i = 1,npls
         awood_aux(i) = dt(7,i)
         ca1_pft(i) = ca1_in(i)
         cf1_pft(i) = cf1_in(i)
         cs1_pft(i) = cs1_in(i)
         ch1_pft(i) = ch1_in(i)
 !        print*, 'sap2=', cs1_pft(i), 'heart 2=', ch1_pft(i), 'wood2=', ca1_pft(i)
         dleaf(i) = dleaf_in(i)
         dwood(i) = dwood_in(i)
         droot(i) = droot_in(i)
         uptk_costs(i) = uptk_costs_in(i)
         do j = 1,3
            sto_budg(j,i) = sto_budg_in(j,i)
            cl1_pft(j,i) = cl1_in(j,i)
         enddo

      enddo

      call pft_area_frac(sum(cl1_pft,dim=1), cf1_pft, ca1_pft, awood_aux,&
      &                  ocpavg, ocp_wood, run, ocp_mm)

      nlen = sum(run)    ! New length for the arrays in the main loop
      allocate(w(nlen))
      allocate(g(nlen))
      allocate(s(nlen))
      allocate(lp(nlen))
      allocate(ocp_coeffs(nlen))

      ! Get only living PLSs
      counter = 1
      do p = 1,npls
         if(run(p).eq. 1) then
              w(counter) = w1(p)     ! hidrological pools state vars
              g(counter) = g1(p)
              s(counter) = s1(p)
              lp(counter) = p
              ocp_coeffs(counter) = ocpavg(p)
              counter = counter + 1
         endif
      enddo

      soil_temp = ts

      allocate(rimelt(nlen))
      allocate(smelt(nlen))
      allocate(ds(nlen))
      allocate(dw(nlen))
      allocate(roff(nlen))
      allocate(evap(nlen))
      allocate(nppa(nlen))
      allocate(ph(nlen))
      allocate(ar(nlen))
      allocate(laia(nlen))
      allocate(f5(nlen))
      allocate(f1(3,nlen))
      allocate(vpd(nlen))
      allocate(rc2(nlen))
      allocate(rm(nlen))
      allocate(rg(nlen))
      allocate(wue(nlen))
      allocate(cue(nlen))
      allocate(c_def(nlen))
      allocate(vcmax(nlen))
      allocate(specific_la(nlen))
      allocate(storage_out_bdgt(3, nlen))
      allocate(tra(nlen))
      allocate(nupt(2, nlen))
      allocate(pupt(3, nlen))
      allocate(litter_l(nlen))
      allocate(cwd(nlen))
      allocate(litter_fr(nlen))
      allocate(lit_nut_content(6, nlen))
      allocate(delta_cveg(3, nlen))
      allocate(npp2pay(nlen))
      allocate(limitation_status(3,nlen))
      allocate(uptk_strat(2,nlen))
      allocate(cl1_int(3,nlen))
      allocate(cf1_int(nlen))
      allocate(ca1_int(nlen))
      allocate(cl2(3,nlen))
      allocate(cf2(nlen))
      allocate(ca2(nlen))
      allocate(ch2(nlen))
      allocate(cs2(nlen))
      allocate(day_storage(3,nlen))

      !     Maximum evapotranspiration   (emax)
      !     =================================
      emax = evpot2(p0,temp,rh,available_energy(temp))
      soil_temp = ts

      !     Productivity & Growth (ph, ALLOCATION, aresp, vpd, rc2 & etc.) for each PLS
      !     =====================
      call OMP_SET_NUM_THREADS(2)

      !$OMP PARALLEL DO &
      !$OMP SCHEDULE(AUTO) &
      !$OMP DEFAULT(SHARED) &
      !$OMP PRIVATE(p, ri, carbon_in_storage, testcdef, sr, dt1, mr_sto, growth_stoc)
      do p = 1,nlen

         carbon_in_storage = 0.0D0
         testcdef = 0.0D0
         sr = 0.0D0
         ri = lp(p)
         dt1 = dt(:,ri) ! Pick up the pls functional attributes list

         call prod(dt1, ocp_wood(ri),catm, temp, soil_temp, p0, w(p), ipar, rh, emax&
               &, cl1_pft(:,ri), ca1_pft(ri), cf1_pft(ri), dleaf(ri), dwood(ri), droot(ri)&
               &, ph(p), ar(p), nppa(p), laia(p), f5(p), vpd(p), rm(p), rg(p), rc2(p)&
               &, wue(p), c_def(p), vcmax(p), specific_la(p), tra(p))


      ! Check if the carbon deficit can be conpensated by stored carbon
         carbon_in_storage = sto_budg(1, ri)
         storage_out_bdgt(1, p) = carbon_in_storage
         if (c_def(p) .gt. 0.0) then
            testcdef = c_def(p) - carbon_in_storage
            if(testcdef .lt. 0.0) then
               storage_out_bdgt(1, p) = carbon_in_storage - c_def(p)
               c_def(p) = 0.0D0
            else
               storage_out_bdgt(1, p) = 0.0D0
               c_def(p) = real(testcdef, kind=r_4)       ! testcdef is zero or positive
            endif
         endif
         carbon_in_storage = 0.0D0
         testcdef = 0.0D0

         ! calculate maintanance respirarion of stored C
         mr_sto = sto_resp(temp, storage_out_bdgt(:,p))
         if (isnan(mr_sto)) mr_sto = 0.0D0
         storage_out_bdgt(1,p) = max(0.0D0, (storage_out_bdgt(1,p) - mr_sto))

         !     Carbon/Nitrogen/Phosphorus allocation/deallocation
         !     =====================================================
         call allocation (dt1,nppa(p),uptk_costs(ri), soil_temp, w(p), tra(p)&
            &,  mineral_n,labile_p, on, sop, op, cl1_pft(:,ri),ca1_pft(ri)&
            &, cf1_pft(ri),cs1_pft(ri),ch1_pft(ri),storage_out_bdgt(:,p),day_storage(:,p),cl2(:,p),ca2(p)&
            &, cf2(p),ch2(p),cs2(p),litter_l(p),cwd(p), litter_fr(p),nupt(:,p),pupt(:,p)&
            &, lit_nut_content(:,p), limitation_status(:,p), npp2pay(p), uptk_strat(:, p))

         ! Estimate growth of storage C pool
         ! print*, uptk_strat(:,p)
         growth_stoc = max( 0.0D0, (day_storage(1,p) - storage_out_bdgt(1,p)))
         if (isnan(growth_stoc)) growth_stoc = 0.0D0

         storage_out_bdgt(:,p) = day_storage(:,p)

         ! Calculate storage GROWTH respiration
         sr = 0.45D0 * growth_stoc ! g m-2
         if(sr .gt. 1.0D2) sr = 0.0D0
         ar(p) = ar(p) + real(((sr + mr_sto) * 0.365242), kind=r_4) ! Convert g m-2 day-1 in kg m-2 year-1
         storage_out_bdgt(1, p) = storage_out_bdgt(1, p) - sr

         growth_stoc = 0.0D0
         mr_sto = 0.0D0
         sr = 0.0D0

         ! CUE & Delta C
         if(ph(p) .eq. 0.0 .or. nppa(p) .eq. 0.0) then
            cue(p) = 0.0
         else
            cue(p) = nppa(p)/ph(p)
         endif

         delta_cveg(1,p) = sum(cl2(:,p)) - sum(cl1_pft(:,ri))  !kg m-2
         if(dt1(4) .le. 0) then
            delta_cveg(2,p) = 0.0D0
         else
            delta_cveg(2,p) = ca2(p) - ca1_pft(ri)
         endif
         delta_cveg(3,p) = cf2(p) - cf1_pft(ri)

         ! Mass Balance

         if(c_def(p) .gt. 0.0) then
            if(dt1(7) .gt. 0.0) then
               cl1_int(:,p) = cl2(:,p) - ((c_def(p) * 1e-3) * 0.333333333)
               ca1_int(p) = ca2(p) - ((c_def(p) * 1e-3) * 0.333333333)
               cf1_int(p) = cf2(p) - ((c_def(p) * 1e-3) * 0.333333333)
            else
               cl1_int(:,p) = cl2(:,p) - ((c_def(p) * 1e-3) * 0.5)
               ca1_int(p) = 0.0
               cf1_int(p) = cf2(p) - ((c_def(p) * 1e-3) * 0.5)
            endif
         else
            if(dt1(7) .gt. 0.0) then
               cl1_int(:,p) = cl2(:,p)
               ca1_int(p) = ca2(p)
               cf1_int(p) = cf2(p)
            else
               cl1_int(:,p) = cl2(:,p)
               ca1_int(p) = 0.0
               cf1_int(p) = cf2(p)
            endif
         endif
         if(cl1_int(1,p) .lt. 0.0D0) cl1_int(1,p) = 0.0D0
         if(cl1_int(2,p) .lt. 0.0D0) cl1_int(2,p) = 0.0D0
         if(cl1_int(3,p) .lt. 0.0D0) cl1_int(3,p) = 0.0D0
         if(ca1_int(p) .lt. 0.0D0) ca1_int(p) = 0.0D0
         if(cf1_int(p) .lt. 0.0D0) cf1_int(p) = 0.0D0

         ! WATER BALANCE - GABRIEL
         !     Precipitation
         !     =============
         psnow = 0.0
         prain = 0.0
         if (temp.lt.tsnow) then
            psnow = prec
         else
            prain = prec
         endif
         !     Snow budget
         !     ===========
         smelt(p) = 2.63 + 2.55*temp + 0.0912*temp*prain !Snowmelt (mm/day)
         smelt(p) = amax1(smelt(p),0.)
         smelt(p) = amin1(smelt(p),s(p)+psnow)
         ds(p) = psnow - smelt(p)
         s(p) = s(p) + ds(p)

         !     Water budget
         !     ============
         if (soil_temp .le. tice) then !Frozen soil
            g(p) = g(p) + w(p) !Soil moisture freezes
            w(p) = 0.0
            roff(p) = smelt(p) + prain !mm/day
            evap(p) = 0.0

         else                !Non-frozen soil
            w(p) = w(p) + g(p)
            g(p) = 0.0
            rimelt(p) = 0.0
            if (w(p).gt.wmax) then
               rimelt(p) = w(p) - wmax !Runoff due to soil ice melting
               w(p) = wmax
            endif

            roff(p) = runoff(w(p)/wmax)       !Soil moisture runoff (roff, mm/day)

            evap(p) = penman(p0,temp,rh,available_energy(temp),rc2(p)) !Actual evapotranspiration (evap, mm/day)
            dw(p) = prain + smelt(p) - evap(p) - roff(p)
            w(p) = w(p) + dw(p)
            if (w(p).gt.wmax) then
               roff(p) = roff(p) + (w(p) - wmax)
               w(p) = wmax
            endif
            if (w(p).lt.0.) w(p) = 0.
            roff(p) = roff(p) + rimelt(p) !Total runoff
         endif


      enddo ! end pls_loop (p)
      !$OMP END PARALLEL DO
      epavg = emax !mm/day

      ! FILL OUTPUT DATA
      smavg   = 0.0D0
      ruavg = 0.0D0
      evavg = 0.0D0
      rcavg = 0.0D0
      f5avg = 0.0D0
      laiavg = 0.0D0
      phavg = 0.0D0
      aravg = 0.0D0
      nppavg = 0.0D0
      rmavg = 0.0D0
      rgavg = 0.0D0
      wueavg = 0.0D0
      cueavg = 0.0D0
      litter_l_1 = 0.0D0
      cwd_1 = 0.0D0
      litter_fr_1 = 0.0D0
      c_defavg = 0.0D0
      vcmax_1 = 0.0D0
      specific_la_1 = 0.0D0
      lit_nut_content_1(:) = 0.0D0
      nupt_1(:) = 0.0D0
      pupt_1(:) = 0.0D0
      w2(:) = 0.0
      g2(:) = 0.0
      s2(:) = 0.0
      wp(:) = 0.0D0
      cleafavg_pft(:,:) = 0.0D0
      cawoodavg_pft(:) = 0.0D0
      cfrootavg_pft(:) = 0.0D0
      delta_cveg_1(:,:) = 0.0D0
      storage_out_bdgt_1(:,:) = 0.0D0
      limitation_status_1(:,:) = 0
      uptk_strat_1(:,:) = 0
      npp2pay_1(:) = 0.0

      ! CALCULATE CWM FOR ECOSYSTEM PROCESSES

      ! FILTER NaN in ocupation (abundance) coefficients
      do p = 1, nlen
         if(isnan(ocp_coeffs(p))) ocp_coeffs(p) = 0.0D0
      enddo


      smavg = sum(real(smelt, kind=r_8) * ocp_coeffs)
      ruavg = sum(real(roff, kind=r_8) * ocp_coeffs)
      evavg = sum(real(evap, kind=r_8) * ocp_coeffs)
      phavg = sum(real(ph, kind=r_8) * ocp_coeffs)
      aravg = sum(real(ar, kind=r_8) * ocp_coeffs)
      nppavg = sum(real(nppa, kind=r_8) * ocp_coeffs)
      laiavg = sum(laia * ocp_coeffs)
      rcavg = sum(real(rc2, kind=r_8) * ocp_coeffs)
      f5avg = sum(f5 * ocp_coeffs)
      rmavg = sum(real(rm, kind=r_8) * ocp_coeffs)
      rgavg = sum(real(rg, kind=r_8) * ocp_coeffs)
      wueavg = sum(real(wue, kind=r_8) * ocp_coeffs)
      cueavg = sum(real(cue, kind=r_8) * ocp_coeffs)
      c_defavg = sum(real(c_def, kind=r_8) * ocp_coeffs) / 2.73791
      vcmax_1 = sum(vcmax * ocp_coeffs)
      specific_la_1 = sum(specific_la * ocp_coeffs)
      litter_l_1 = sum(litter_l * ocp_coeffs)
      cwd_1 = sum(cwd * ocp_coeffs)
      litter_fr_1 = sum(litter_fr * ocp_coeffs)
      wp(1) = sum(w * ocp_coeffs)
      wp(2) = sum(g * ocp_coeffs)
      wp(3) = sum(s * ocp_coeffs)
      cp(1) = sum(sum(cl1_int) * ocp_coeffs)
      cp(2) = sum(ca1_int * ocp_coeffs)
      cp(3) = sum(cf1_int * ocp_coeffs)

      ! print*, ''
      ! print*, 'ca', ca1_int
      ! print*, ''

      ! FILTER BAD VALUES
      do p = 1,2
         do i = 1, nlen
            if (isnan(nupt(p, i))) nupt(p, i) = 0.0D0
            if (nupt(p, i) .gt. 0.01D2) nupt(p, i) = 0.0D0
            if (nupt(p, i) .lt. 0.0D0) nupt(p, i) = 0.0D0
         enddo
      enddo

      do p = 1,3
         do i = 1, nlen
            if(isnan(pupt(p, i))) pupt(p, i) = 0.0D0
            if (pupt(p, i) .gt. 0.01D2) pupt(p, i) = 0.0D0
            if (pupt(p, i) .lt. 0.0D0) pupt(p, i) = 0.0D0
         enddo
      enddo

      do p = 1,2
         nupt_1(p) = sum(nupt(p,:) * ocp_coeffs)
      enddo
      do p = 1,3
         pupt_1(p) = sum(pupt(p,:) * ocp_coeffs)
      enddo

      do p = 1,6
         do i = 1, nlen
            if(isnan(lit_nut_content(p, i))) lit_nut_content(p, i) = 0.0D0
            if (lit_nut_content(p, i) .gt. 0.01D2) lit_nut_content(p, i) = 0.0D0
            if (lit_nut_content(p, i) .lt. 0.0D0) lit_nut_content(p, i) = 0.0D0
         enddo
      enddo

      do p = 1, 6
         lit_nut_content_1(p) = sum(lit_nut_content(p, :) * ocp_coeffs)
      enddo

      do p = 1, nlen
         ri = lp(p)
         ! write output data
         ! FULL ARRAYS
         w2(ri) = w(p)
         g2(ri) = g(p)
         s2(ri) = s(p)
         cleafavg_pft(:,ri)  = cl1_int(:,p)
         cawoodavg_pft(ri) = ca1_int(p)
         cfrootavg_pft(ri) = cf1_int(p)
         delta_cveg_1(:,ri) = delta_cveg(:,p)
         storage_out_bdgt_1(:,ri) = storage_out_bdgt(:,p)
         limitation_status_1(:,ri) = limitation_status(:,p)
         uptk_strat_1(:,ri) = uptk_strat(:,p)
         npp2pay_1(ri) = npp2pay(p)

      enddo
      ! print*, 'ca_out---------->', cawoodavg_pft
      deallocate(w)
      deallocate(g)
      deallocate(s)
      deallocate(lp)
      deallocate(rimelt)
      deallocate(smelt)
      deallocate(ds)
      deallocate(dw)
      deallocate(roff)
      deallocate(evap)
      deallocate(nppa)
      deallocate(ph)
      deallocate(ar)
      deallocate(laia)
      deallocate(f5)
      deallocate(f1)
      deallocate(vpd)
      deallocate(rc2)
      deallocate(rm)
      deallocate(rg)
      deallocate(wue)
      deallocate(cue)
      deallocate(c_def)
      deallocate(vcmax)
      deallocate(specific_la)
      deallocate(storage_out_bdgt)
      deallocate(tra)
      deallocate(nupt)
      deallocate(pupt)
      deallocate(litter_l)
      deallocate(cwd)
      deallocate(litter_fr)
      deallocate(lit_nut_content)
      deallocate(delta_cveg)
      deallocate(npp2pay)
      deallocate(limitation_status)
      deallocate(uptk_strat)
      deallocate(cl1_int)
      deallocate(cf1_int)
      deallocate(ca1_int)
      deallocate(cl2)
      deallocate(cf2)
      deallocate(ca2)
      deallocate(ch2)
      deallocate(cs2)
      deallocate(day_storage)

   end subroutine daily_budget

end module budget
