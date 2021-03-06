subroutine da_rttov_init(iv,ob,nsensor,nchan)

   !---------------------------------------------------------------------------
   !  Purpose: interface to the initialization subroutine of RTTOV.
   !
   !  METHOD:  1. read RTTOV coefs files; 2. allocate radiance structure
   !
   !  HISTORY: 07/28/2005 - Creation                               Zhiquan Liu
   !           03/22/2006   add error tuning factor read in        Zhiquan Liu
   !           10/24/2007   limit this routine to RTTOV init       Tom Auligne
   !---------------------------------------------------------------------------

   implicit none 

   type (iv_type), intent (inout) :: iv
   type (y_type) , intent (inout) :: ob
   integer ,       intent (in)    :: nsensor
   integer ,       intent (in)    :: nchan(nsensor)

   !  local arguments
   !------------------- 
   integer   :: i, j, n

   !  input parameters of RTTOV_SETUP
   !----------------------------------
   integer :: err_unit        ! Logical error unit (<0 for default)
   integer :: verbosity_level ! (<0 for default)
   integer, allocatable  :: sensor(:,:) ! instrument id
   integer, allocatable  :: coefs_channels (:,:)
   character(len=256)    :: fmt_string
   character(len=256)    :: file_path, file_suffix
   character(len=256)    :: coef_prefix, scaer_prefix, sccld_prefix
   character(len=256)    :: coef_filename, scaer_filename, sccld_filename

   !  output parameters of RTTOV_SETUP
   !-------------------------------------
   integer :: errorstatus  ! return code

   ! local variables
   !----------------
   integer               :: mxchn
 
   if (trace_use) call da_trace_entry("da_rttov_init")

   !--------------------------------------------------------------
   !  1.0 setup RTTOV instrument triplets from namelist parameter
   !--------------------------------------------------------------

   mxchn           =  MAXVAL(nchan)
   err_unit        =  stderr
   verbosity_level =  rtminit_print

   allocate (coefs(nsensor))
   allocate (opts(nsensor))
   allocate (sensor(3,nsensor))
   allocate (coefs_channels(mxchn,nsensor))

   sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
   sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
   sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)

   coefs_channels(:,:) = 0
   do n = 1, nsensor
      coefs_channels(1:nchan(n),n) = iv%instid(n)%ichan(1:nchan(n))
   end do

   if (print_detail_rad) then
      write(unit=message(1),fmt='(A,I5)') 'err_unit             = ', err_unit
      write(unit=message(2),fmt='(A,I5)') 'verbosity_level      = ', verbosity_level
      write(unit=message(3),fmt='(A,I5)') 'nsensor              = ', nsensor
      write(unit=message(4),fmt='(A,10I5)') 'sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
      write(unit=message(5),fmt='(A,10I5)') 'sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
      write(unit=message(6),fmt='(A,10I5)') 'sensor (3,1:nsensor) = ', sensor (3,1:nsensor)
      call da_message(message(1:6))
   end if

   !-----------------------------------------------------------
   ! 2.0 read and initialize coefficients
   !     rttov_read_coefs and rttov_init_coefs are called for each instrument
   !-----------------------------------------------------------
 
   file_path    = 'rttov_coeffs/'
   file_suffix  = '.dat'
   coef_prefix  = 'rtcoef_'
   scaer_prefix = 'scaercoef_'  ! scattering coefficients - aerosols
   sccld_prefix = 'sccldcoef_'  ! scattering coefficients - clouds

   write(unit=message(1),fmt='(a)') 'Read in the RTTOV coef files for the following sensors'
   call da_message(message(1:1))

   do n = 1, nsensor

      opts(n)%switchrad  = .true.   ! brightness temperature radiance%bt is the input perturbation
      opts(n)%addrefrac  = .false.  ! refraction in path calc
      opts(n)%addinterp  = .false.  ! interpolation of input profile
      opts(n)%addsolar   = .false.  ! reflected solar
      opts(n)%addclouds  = .false.  ! cloud effect
      opts(n)%addaerosl  = .false.  ! aerosol effect
      opts(n)%ozone_data = .false.
      opts(n)%co2_data   = .false.
      opts(n)%n2o_data   = .false.
      opts(n)%ch4_data   = .false.
      opts(n)%co_data    = .false.
      opts(n)%clw_data   = .false.

      ! construct the full path name to the coef file
      coef_filename = trim(file_path)//trim(coef_prefix)//trim(iv%instid(n)%rttovid_string_coef)//trim(file_suffix)
      scaer_filename = trim(file_path)//trim(scaer_prefix)//trim(iv%instid(n)%rttovid_string_coef)//trim(file_suffix)
      sccld_filename = trim(file_path)//trim(sccld_prefix)//trim(iv%instid(n)%rttovid_string_coef)//trim(file_suffix)

      write(unit=fmt_string, fmt='(a,i3,a)') '(2a,2x,a,', nchan(n), 'i5)'
      write(unit=message(2),fmt=trim(fmt_string)) "   ", &
      !write(unit=message(2),fmt='(2a,2x,a,(30i5))') "   ", &
         trim(iv%instid(n)%rttovid_string), 'nchan = ', coefs_channels(1:nchan(n),n)
      call da_message(message(2:2))

      call rttov_read_coefs( &
        & errorstatus,       &   ! out
        & coefs(n),          &   ! out
        & opts(n),           &   ! in
        & channels     = coefs_channels(1:nchan(n),n),  &   ! list of channels to extract coefs
        & file_coef    = trim(coef_filename),  &
        & file_scaer   = trim(scaer_filename), &
        & file_sccld   = trim(sccld_filename) )
      if ( errorstatus /= errorstatus_success ) then
         call da_error(__FILE__,__LINE__,(/"rttov_read_coefs fatal error"/))
      end if

      call rttov_init_coefs (errorstatus, opts(n), coefs(n))
      if ( errorstatus /= errorstatus_success ) then
         call da_error(__FILE__,__LINE__,(/"rttov_init_coefs fatal error"/))
      end if

      iv%instid(n)%nlevels = coefs(n)%coef%nlevels

   end do

   deallocate (sensor)
   deallocate (coefs_channels)

   if (trace_use) call da_trace_exit("da_rttov_init")
 
end subroutine da_rttov_init
