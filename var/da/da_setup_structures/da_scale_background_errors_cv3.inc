subroutine da_scale_background_errors_cv3 ( grid, be, it )
!
   type (domain), intent(in)     :: grid
   TYPE (be_type), INTENT(INOUT) :: be     ! Back. errors structure
   INTEGER,        INTENT(IN)    :: it     ! outer-loop index
!
   REAL, DIMENSION( ids: ide,  jds: jde,  kds: kde, 1:4) :: hwll
   REAL, DIMENSION( ids: ide,  jds: jde)                 :: hwllp, global_fac
   REAL, DIMENSION( kts: kte,  kts: kte)                 :: vv
   integer                     :: n, i, j, k, ic, jc, ii, ij, ijk, &
                                  iis, iie, jjs, jje, kks, kke
   real, dimension(1)          :: xsum
   real, dimension(ids: ide,  jds: jde)                   :: global_2d
   real, dimension(ids: ide,  jds: jde,  kds: kde)        :: global_3d
   character(len=6)            :: vname
   character(len=9)            :: chr
   character(len=8)            :: i_char

   INTEGER :: nta, ndeg, ku, kz
   real    :: samp,s2u,tin,as(5),as0(5),slim
   character(len=256) :: mesg

   integer                     :: ier, be_rf_unit, be_print_unit
!
! 1, to get global_fac from grid:

#ifdef DM_PARALLEL
   ij = ( ide- ids+1)*( jde- jds+1)
!  Collect xb component of fac into global buffer.
   call da_patch_to_global( grid, grid%xb%map_factor, global_fac )
   call wrf_dm_bcast_real( global_fac, ij )
#else
   do i = ids, ide
      do j = jds, jde
         global_fac(i,j) = grid%xb%map_factor(i,j)
      enddo
   enddo
#endif

! 2, Read in the variance and horizontal scales
!
! 2.1 assign the units for reading and printing:

! Rewind the unit:
    be_rf_unit    = unit_end + 3
    be_print_unit = unit_end + 5
    rewind (be_rf_unit)

    if (rootproc .and. print_detail_be) &
    write(be_print_unit,'(//10x,a,i2,a)') '====== SCALE CV3 BE for IT =', it, ' ======' 
!
! 2.2 Read the dimensions
!
    read(be_rf_unit) iis, iie, jjs, jje, kks, kke, ku, samp
    if (rootproc .and. print_detail_be) &
    write (be_print_unit,'(/a,7i5,f15.5)') &
              "Read in: ids, ide, jds, jde, kds, kde, ku, samp:", &
                        iis, iie, jjs, jje, kks, kke, ku, samp
!
! 2.3 Read in the variances before normalization and write to "be_print_unit":

!    write (*,'(3x,a)')  'READ IN VARIANCE BEFORE NORMALIZATION:'
! Print out the sum(x*X) for verification:
!
   if (rootproc .and. print_detail_be) &
   write (be_print_unit,'(3x,a)')  'VARIANCE:'
   do ii = 1, 4
     read (be_rf_unit) chr, vname, i, global_3d
     be%corz(its:ite,jts:jte,kts:kte,i) = global_3d(its:ite,jts:jte,kts:kte)
     xsum(1) = sum (be%corz(its:ite,jts:jte,kts:kte,i)*be%corz(its:ite,jts:jte,kts:kte,i))
     call da_proc_sum_real(xsum)
     if (rootproc .and. print_detail_be) &
     write (be_print_unit,'(5x,i3,1x,a,2x,"sum^2=",e20.12)') ii, vname, xsum(1)
!     write (*,'(3x,a9,2x,a6,2x,i3)') chr, vname, i
   enddo

!  Pscf Variance before the normalization.
   read (be_rf_unit) chr, vname, ii, global_2d
   be%corp(its:ite,jts:jte) = global_2d(its:ite,jts:jte)
   xsum(1) = sum (be%corp(its:ite,jts:jte)*be%corp(its:ite,jts:jte))
   call da_proc_sum_real(xsum)
   if (rootproc .and. print_detail_be) &
   write (be_print_unit,'(9x,a,2x,"sum^2=",e20.12)') 'PSFC_u', xsum(1)
!   write (*,'(3x,a9,2x,a6,2x,i3)') chr, vname, ii
!
! 2.4 Read in the vertical scales to "be_print_unit":
!
!   write (*,'(3x,a)')  'READ IN VERTICAL SCALE BEFORE TUNING:'
!
   if (rootproc .and. print_detail_be) &
   write (be_print_unit,'(3x,a)')  'VERTICAL SCALE:'
   do ii = 1, 4
     read (be_rf_unit) chr, vname, n, global_3d
     do i = its, ite
     do j = jts, jte
     do k = kts, kte
        be%vz(k,i,j,n) = global_3d(i,j,k)
     enddo
     enddo
     enddo
     xsum(1) = sum (be%vz(kts:kte,its:ite,jts:jte,n)*be%vz(kts:kte,its:ite,jts:jte,n))
     call da_proc_sum_real(xsum)
     if (rootproc .and. print_detail_be) &
     write (be_print_unit,'(5x,i3,1x,a,2x,"sum^2=",e20.12)') ii, vname, xsum(1)
!     write (*,'(3x,a9,2x,a6,2x,i3)') chr, vname, n
   enddo
!
! 2.5 Read in the Horizotal scales, and write out vertical scales to "be_print_unit":

   if (rootproc .and. print_detail_be) &
   write (be_print_unit,'(3x,a)')  'READ IN THE HORIZONTAL SCALES:'
   read (be_rf_unit) hwll
   if (rootproc .and. print_detail_be) then
   write (be_print_unit,'(3x,a)')  'HORIZONTAL SCALES:'
   xsum(1) = sum (hwll*hwll)
   write (be_print_unit,'(9x,a,2x,"sum^2=",e20.12)') 'HORIZONTAL PSI, CHI_u, T_u, RH_u SCALES:', xsum(1)
   endif

   read (be_rf_unit) hwllp
   if (rootproc .and. print_detail_be) then
   xsum(1) = sum (hwllp*hwllp)
   write (be_print_unit,'(9x,a,2x,"sum^2=",e20.12)') 'HORIZONTAL PS_u SCALES:', xsum(1)
!   write (*,'(9x,a,2x,"sum^2=",e20.12)') 'HORIZONTAL PS_u SCALES:', xsum(1)
   endif

! 2.6 Write out the regression coefficients to "be_print_unit":

   if (print_detail_be) then

   if (rootproc) &
   write (be_print_unit,'(3x,a)')  'REGRESSION COEFF. T, CHI, PSFC:'
   do ii = kts, kte
     xsum(1) = sum (be%agvz(:,:,:,ii)*be%agvz(:,:,:,ii))
     call da_proc_sum_real(xsum)
     if (rootproc) &
     write (be_print_unit,'(5x,i3,1x,a,2x,"sum^2=",e20.12)') ii, 'TMP-PSI', xsum(1)
   enddo

!  Rg. Coeff. has already stored in "be" data structure, not need to be save.
   xsum(1) = sum (be%bvz*be%bvz)
   call da_proc_sum_real(xsum)
   if (rootproc) &
   write (be_print_unit,'(9x,a,2x,"sum^2=",e20.12)') 'CHI-PSI', xsum(1)

!  Rg. Coeff. has already stored in "be" data structure, not need to be save.
   xsum(1) = sum (be%wgvz*be%wgvz)
   call da_proc_sum_real(xsum)
   if (rootproc) &
   write (be_print_unit,'(9x,a,2x,"sum^2=",e20.12)') 'PSF-PSI', xsum(1)
!
   endif

! 3.0 Re-scale the scale-lengths
!
! 3.1 horizontal scales

   as0(1)=as1(2)
   as0(2)=as2(2)
   as0(3)=as3(2)
   as0(4)=as4(2)

   as(1)=as1(2+(it-1)*3)
   as(2)=as2(2+(it-1)*3)
   as(3)=as3(2+(it-1)*3)
   as(4)=as4(2+(it-1)*3)

   do n=1, 4
      hwll(:,:,:,n) = hwll(:,:,:,n) * as(n) / as0(n)
   enddo

   hwllp = hwllp * as5(2+(it-1)*3) / as5(2)

! 3.2 vertical scales

   as0(1)=as1(3)
   as0(2)=as2(3)
   as0(3)=as3(3)
   as0(4)=as4(3)

   as(1)=as1(3+(it-1)*3)
   as(2)=as2(3+(it-1)*3)
   as(3)=as3(3+(it-1)*3)
   as(4)=as4(3+(it-1)*3)

   do n=1, 4
      be%vz(kts:kte,its:ite,jts:jte,n) = be%vz(kts:kte,its:ite,jts:jte,n) * as(n) / as0(n)
   enddo

! 4, Normalize the variances
 
   kz =  kde - kds + 1
!
! 4.1 Get the square-root of the variance tuning factor:

   n = 1+(it-1)*3
   if ( as1(n) <= 1.0E-7 .or. as2(n) <= 1.0E-7 .or. as3(n) <= 1.0E-7 .or. as4(n) <= 1.0E-7 .or. as5(n) <= 1.0E-7 ) then
     Write (i_char, '(i8)') it
     call da_error(__FILE__,__LINE__, &
                    (/"Missing or invaliad AS1 ~ AS5 value for IT = "//i_char/))
   endif

   as(1)=sqrt(as1(1+(it-1)*3))
   as(2)=sqrt(as2(1+(it-1)*3))
   as(3)=sqrt(as3(1+(it-1)*3))
   as(4)=sqrt(as4(1+(it-1)*3))
   as(5)=sqrt(as5(1+(it-1)*3))

! 4.2 Scale the horizontal scale in unit of grid-point:

   s2u= 1./grid%xb%ds
   hwll=hwll*s2u
   hwllp=hwllp*s2u
 
! 4.3 Normalize the covariance for psi, chi, t, and rh:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (ij, n, j, i, vv, k)
   do ij = 1, grid%num_tiles
   do n=1,4

      do j=grid%j_start(ij), grid%j_end(ij)
         do i=its,ite
     ! Initialize the matrix vv:
            vv=0.
            do k=kts,kte
               vv(k,k)=1.
            enddo

     ! Apply pseudo-Guassian "right-left" smoother in vertical with 
     ! the vertical scale length be%vz: 
            call da_rfz0(vv,kz,kz,be%ndeg,&
                         be%vz(kts:kte,i,j,n),be%be,be%table,be%nta,be%swidth)

     ! Normalize the covariance for psi, chi, t, and rh:
            do k=kts,kte
               be % corz(i,j,k,n)=be % corz(i,j,k,n)*as(n) &
                                  *samp/hwll(i,j,k,n)/vv(k,k)/global_fac(i,j)
            enddo

         enddo
      enddo
   enddo
   enddo
   !$OMP END PARALLEL DO

! 4.4 Normalize the covariance for Psfc:

!    xsum(1) = sum (be%corp(its:ite,jts:jte)*be%corp(its:ite,jts:jte))
!    call da_proc_sum_real(xsum)
!    if (rootproc) &
!    write (*,'("BEFORE: ",a,2x,"sum^2=",e20.12)') 'PSFC_u', xsum(1)

!   if (rootproc) then
!   xsum(1) = sum (hwllp*hwllp)
!   write (*,'(9x,a,2x,"sum^2=",e20.12)') 'HORIZONTAL PS_u SCALES:', xsum(1)
!   endif
!   write (*,'("as(5)=",f15.5,2x,"samp=",f15.5)') as(5), samp

    be % corp(its:ite,jts:jte)=be % corp(its:ite,jts:jte)*as(5) &
         *samp/hwllp(its:ite,jts:jte)/global_fac(its:ite,jts:jte)

!    xsum(1) = sum (be%corp(its:ite,jts:jte)*be%corp(its:ite,jts:jte))
!    call da_proc_sum_real(xsum)
!    if (rootproc) &
!    write (*,'(" AFTER: ",a,2x,"sum^2=",e20.12)') 'PSFC_u', xsum(1)

    write(mesg,*) 'Normalized covariance for Psfc: sum(be%corp*be%corp)=', &
                  sum(be%corp*be%corp)
    call wrf_debug ( 1 , mesg )
!
! 5, Assign the inverse of scale length fields for recursive filter:

   ic = 1 + ( ide- ids)/2
   jc = 1 + ( jde- jds)/2

!   if (rootproc) &
!   write (*,'("i=",i4,2x,"jc=",i4)') ic, jc
!
! 5.1 allocate the arrays for be scales: y-direction and x-direction:

    ALLOCATE ( be % sljpy ( grid%xp%ipsy: grid%xp%ipey, grid%xp%jpsy: grid%xp%jpey) )
    ALLOCATE ( be % sljy ( grid%xp%ipsy: grid%xp%ipey, grid%xp%jpsy: grid%xp%jpey, grid%xp%kpsy: grid%xp%kpey,1:4) )

    ALLOCATE ( be % slipx ( grid%xp%ipsx: grid%xp%ipex, grid%xp%jpsx: grid%xp%jpex) )
    ALLOCATE ( be % slix ( grid%xp%ipsx: grid%xp%ipex, grid%xp%jpsx: grid%xp%jpex, grid%xp%kpsx: grid%xp%kpex,1:4) )

! 5.2 Y-direction:

! 5.2.1 3-D fields: psi, chi, t, and rh:

    do n=1,4
       do k= grid%xp%kpsy, grid%xp%kpey
          do j= grid%xp%jpsy, grid%xp%jpey
             do i= grid%xp%ipsy, grid%xp%ipey
                be%sljy(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
             enddo
          enddo
       enddo
    enddo
!    write (*,'("a, be%sljy:sum^2=",e20.12)') sum(be%sljy*be%sljy)

    ! Above level ku,the sljy fields are set to a constant 
    ! for psi and chi, i.e. homogenous:
    do n=1,2
       do k=max(ku, grid%xp%kpsy), grid%xp%kpey
          slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
          do j= grid%xp%jpsy, grid%xp%jpey
             do i= grid%xp%ipsy, grid%xp%ipey
                be%sljy(i,j,k,n)=slim
             enddo
          enddo
       enddo
    enddo
!    write (*,'("b, be%sljy:sum^2=",e20.12)') sum(be%sljy*be%sljy)

! 5.2.2 2-D field: Psfc:
    do j= grid%xp%jpsy, grid%xp%jpey
       do i= grid%xp%ipsy, grid%xp%ipey
          be%sljpy(i,j)=1./global_fac(i,j)/hwllp(i,j)
       enddo
    enddo
!    write (*,'("   be%sljpy:sum^2=",e20.12)') sum(be%sljpy*be%sljpy)

! 5.3 X-direction:

! 5.3.1 3-D fields: psi, chi, t, and rh:

    do n=1,4
      do k= grid%xp%kpsx, grid%xp%kpex
         do j= grid%xp%jpsx, grid%xp%jpex
            do i= grid%xp%ipsx, grid%xp%ipex
               be%slix(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
            enddo
         enddo
      enddo
    enddo
!    write (*,'("a, be%slix:sum^2=",e20.12)') sum(be%slix*be%slix)

   ! Above level ku,the sljy fields are set to a constant 
   ! for psi and chi, i.e. homogenous:
    do n=1,2
      do k=max(ku, grid%xp%kpsx), grid%xp%kpex
         slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
         do j= grid%xp%jpsx, grid%xp%jpex
            do i= grid%xp%ipsx, grid%xp%ipex
               be%slix(i,j,k,n)=slim
            enddo
         enddo
      enddo
    enddo
!    write (*,'("b, be%slix:sum^2=",e20.12)') sum(be%slix*be%slix)

! 5.3.2 2-D field: Psfc:
    do j= grid%xp%jpsx, grid%xp%jpex
      do i= grid%xp%ipsx, grid%xp%ipex
         be%slipx(i,j)=1./global_fac(i,j)/hwllp(i,j)
      enddo
    enddo
!    write (*,'("   be%slipx:sum^2=",e20.12)') sum(be%slipx*be%slipx)

end subroutine da_scale_background_errors_cv3
