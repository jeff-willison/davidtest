  subroutine da_solve ( grid , config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),               intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   type (xbx_type)              :: xbx         ! For header & non-grid arrays.
   type (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   real, allocatable            :: qhat(:,:)   ! Control variable structure.
   real*8, allocatable          :: eignvec(:,:)
   real*8, allocatable          :: eignval(:)   
!   real, allocatable            :: full_eignvec(:)   
   type (y_type)                :: ob          ! Observation structure.
   type (iv_type)               :: iv          ! Obs. increment structure.
   type (y_type)                :: re          ! Residual (o-a) structure.
   type (y_type)                :: y           ! y = H(x_inc) structure.
   integer                      :: it          ! External loop counter.
   integer                      :: neign       
   type (j_type)                :: j           ! Cost function.
   type (y_type)                :: jo_grad_y   ! Grad_y(jo)

   integer                      :: cv_size, i, ichan, k, n
   real                         :: j_grad_norm_target ! Target j norm.

   character (len=3)            :: ci
   character (len=2)            :: outerloop
   character (len=256)          :: timestr
   integer   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   integer   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   character :: s
   real*8    :: time_min, time_max 
   integer   :: jl_start, jl_end
   character(len=256) :: timestr1
   type(x_type) :: shuffle
   
   if (trace_use) call da_trace_entry("da_solve")

#ifdef DM_PARALLEL
   call mpi_barrier(comm,ierr)
#endif

   if ( config_flags%use_baseparam_fr_nml ) then
      call nl_get_base_pres  ( 1 , base_pres )
      call nl_get_base_temp  ( 1 , base_temp )
      call nl_get_base_lapse ( 1 , base_lapse )
      call nl_get_iso_temp   ( 1 , iso_temp )
      grid%p00   = base_pres
      grid%t00   = base_temp
      grid%tlp   = base_lapse
      grid%tiso  = iso_temp
   else
      base_pres  = grid%p00
      base_temp  = grid%t00
      base_lapse = grid%tlp
      iso_temp   = grid%tiso
      if ( base_temp < 100.0 .or. base_pres < 10000.0 ) then
         write(unit=message(1),fmt='(A)') &
         'did not find base state parameters in fg. Add use_baseparam_fr_nml = .t. in &dynamics and rerun '
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   ! Calculate the num_fgat_time based on time_window_min, time_window_max
    if ( var4d ) then
       read(unit=time_window_min,fmt='(i4,5(a1,i2))') min_yyyy,s,min_mm,s,min_dd,s,min_hh,s,min_mn,s,min_ss
       read(unit=time_window_max,fmt='(i4,5(a1,i2))') max_yyyy,s,max_mm,s,max_dd,s,max_hh,s,max_mn,s,max_ss
       call da_get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,time_min)
       call da_get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,time_max)
       if ( var4d_bin < time_step ) call nl_set_var4d_bin (1, time_step)
       time_max = (time_max - time_min) * 60   ! unit is : seconds
       num_fgat_time = NINT(time_max/var4d_bin)
       if ( NINT(time_max/var4d_bin)*var4d_bin .ne. NINT(time_max) .or. &
          var4d_bin/time_step*time_step .ne. var4d_bin ) then
          write(unit=message(1),fmt='(A,A,2I7)') &
          'Please change var4d_bin in namelist and rerun==>', 'run_seconds, var4d_bin:',NINT(time_max),var4d_bin
          call da_error(__FILE__,__LINE__,message(1:1))
       endif
       num_fgat_time = num_fgat_time + 1
       write(unit=message(1),fmt='(a,i10)') 'num_fgat_time is: ', num_fgat_time
       call da_message(message(1:1))
       if ( use_rainobs ) then
          allocate (fgat_rain_flags(1:num_fgat_time))
          fgat_rain_flags = .false.
          if ( INT(var4d_bin_rain/var4d_bin)*var4d_bin .ne. INT(var4d_bin_rain) ) then
             write(unit=message(1),fmt='(A,A,2I7)') &
             'Please change var4d_bin_rain in namelist and rerun==>', 'var4d_bin_rain, var4d_bin:',var4d_bin_rain,var4d_bin
             call da_error(__FILE__,__LINE__,message(1:1))
          endif
          do n = 1, num_fgat_time, INT(var4d_bin_rain/var4d_bin)
             fgat_rain_flags(n) = .true.
          end do
       end if
    endif

   !---------------------------------------------------------------------------
   ! [1.0] Initial checks
   !---------------------------------------------------------------------------

   if (cv_options_hum /= cv_options_hum_specific_humidity .and. &
       cv_options_hum /= cv_options_hum_relative_humidity) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if (vert_corr == vert_corr_2) then
      if (vertical_ip < vertical_ip_0 .or. vertical_ip > vertical_ip_delta_p) then
         write (unit=message(1),fmt='(A,I3)') &
           'Invalid vertical_ip = ', vertical_ip
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   if( use_rf )then
      if (0.5 * real(rf_passes) /= real(rf_passes / 2)) then
         write(unit=stdout,fmt='(A,I4,A)') &
            'rf_passes = ', rf_passes, ' .Should be even.'
         rf_passes = int(real(rf_passes / 2))
         write(unit=stdout,fmt='(A,I4)') 'Resetting rf_passes = ', rf_passes
      end if
   else
      write(stdout,'("da_solve: using wavelet transform")')
   endif

   if (anal_type_randomcv) then
      ntmax = 0
      write(unit=stdout,fmt='(a)') &
         ' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   call da_solve_init(grid &
#include "actual_new_args.inc"
)

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (grid%xb):
   !---------------------------------------------------------------------------

   call da_setup_firstguess(xbx, grid, config_flags)

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------
   call da_setup_obs_structures (grid, ob, iv, j)
   if (use_rad) then
      allocate (j % jo % rad(1:iv%num_inst))
      do i=1,iv%num_inst
         allocate (j % jo % rad(i) % jo_ichan(iv%instid(i)%nchan))
         allocate (j % jo % rad(i) % num_ichan(iv%instid(i)%nchan))
      end do
   end if

   !---------------------------------------------------------------------------
   ! [4.1] Observer (ANAL_TYPE="VERIFY")
   !---------------------------------------------------------------------------

   if (anal_type_verify) then
      check_max_iv = .false.
      ntmax=0
      it = 1
      num_qcstat_conv=0

#if defined(RTTOV) || defined(CRTM)
      if (use_varbc.or.freeze_varbc) call da_varbc_init(iv, be)
#endif

      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)
      call da_allocate_y (iv, re)
 
      ! write out O-B statistics
      call da_write_diagnostics(it, grid,num_qcstat_conv, ob, iv, re, y, j)

      ! write out Gradient of Jo for adjoint sensitivity
      if (adj_sens) then
         cv_size = 1
         allocate (xhat(cv_size))
         call da_allocate_y (iv, y)
         call da_allocate_y (iv, jo_grad_y)

         call da_calculate_residual(iv, y, re)
         call da_calculate_grady(iv, re, jo_grad_y)
         call da_zero_x(grid%xa)

         call da_transform_xtoy_adj(cv_size, xhat, grid, iv, jo_grad_y, grid%xa)
         call da_transform_xtoxa_adj(grid)
         call da_transfer_wrftltoxa_adj(grid, config_flags, 'fcst', timestr)

         call da_deallocate_y (y)
         call da_deallocate_y (jo_grad_y)
      end if

      call da_deallocate_y(re)
      call da_deallocate_observations (iv)
      return   
   end if
   
   !---------------------------------------------------------------------------
   ! [5.0] Set up control variable:
   !---------------------------------------------------------------------------
   be % cv % size_jb = 0
   be % cv % size_je = 0
   be % cv % size_jp = 0
   be % cv % size_js = 0
   be % cv % size_jl = 0
   
   !---------------------------------------------------------------------------
   ! [5.1] Set up background errors (be):
   !---------------------------------------------------------------------------
   if (use_background_errors .and. multi_inc /= 1) then
      call da_setup_background_errors (grid, be)
   else
      be % ne = ensdim_alpha
      be % v1 % mz = 0
      be % v2 % mz = 0
      be % v3 % mz = 0
      be % v4 % mz = 0
      be % v5 % mz = 0
   end if

   !---------------------------------------------------------------------------
   ! [5.2] Set up observation bias correction (VarBC):
   !---------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc.or.freeze_varbc) call da_varbc_init(iv, be)
#endif

   !---------------------------------------------------------------------------
   ! [5.3] Set up satellite control variable:
   !---------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (ANY(use_satcv)) call da_setup_satcv(iv, be)
#endif
   
   !---------------------------------------------------------------------------
   ! [5.4] Total control variable:
   !---------------------------------------------------------------------------   
   be % cv % size = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_js + be%cv%size_jl
   cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

      grid % ep % ne = be % ne
      if (use_background_errors .and. be % ne > 0) then
         call da_setup_flow_predictors ( ide, jde, kde, be % ne, grid%ep, &
                                         its, ite, jts, jte, kts, kte )
      end if

   !---------------------------------------------------------------------------
   ! [7.0] Setup control variable (cv):
   !---------------------------------------------------------------------------

      allocate (cvt(1:cv_size))
      allocate (xhat(1:cv_size))
      if (use_lanczos) then
         allocate (qhat(1:cv_size, 0:ntmax))
         allocate (eignvec(ntmax, ntmax))
	 allocate (eignval(ntmax))
!	 allocate (full_eignvec(cv_size))
      end if	 
      
      call da_initialize_cv (cv_size, cvt)
      
      call da_zero_vp_type (grid%vv)
      call da_zero_vp_type (grid%vp)
      if ( var4d ) then
         call da_zero_vp_type (grid%vv6)
         call da_zero_vp_type (grid%vp6)
      end if

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0
   do it = 1, max_ext_its

! Re-scale the variances and the scale-length for outer-loop > 1:
      if (it > 1 .and. cv_options == 5) then
         print '(/10X,"===> Re-set BE SCALINGS for outer-loop=",i2)', it
         call  da_scale_background_errors ( be, it )
      else if (it > 1 .and. cv_options == 3) then
         print '(/10X,"===> Re-set CV3 BE SCALINGS for outer-loop=",i2)', it
         call  da_scale_background_errors_cv3 ( grid, be, it )
      endif

      call da_initialize_cv (cv_size, xhat)

      ! [8.1] Calculate nonlinear model trajectory 

!     if (var4d .and. multi_inc /= 2 ) then
      if (var4d) then
#ifdef VAR4D
         if (it > 1) then
            call kj_swap (grid%u_2, model_grid%u_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%v_2, model_grid%v_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%w_2, model_grid%w_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%t_2, model_grid%t_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%ph_2, model_grid%ph_2, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            call kj_swap (grid%p, model_grid%p, &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            model_grid%mu_2 = grid%mu_2
            model_grid%t2 = grid%t2
            model_grid%th2 = grid%th2
            model_grid%q2 = grid%q2
            model_grid%u10 = grid%u10
            model_grid%v10 = grid%v10
            model_grid%tsk = grid%tsk
            model_grid%psfc = grid%psfc

            do i = PARAM_FIRST_SCALAR, num_moist
               call kj_swap (grid%moist(:,:,:,i), model_grid%moist(:,:,:,i), &
                             grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
            enddo
            
            ! Update boundary condition of model

            model_grid%u_bxs = grid%u_bxs
            model_grid%u_bxe = grid%u_bxe
            model_grid%u_bys = grid%u_bys
            model_grid%u_bye = grid%u_bye
            model_grid%v_bxs = grid%v_bxs
            model_grid%v_bxe = grid%v_bxe
            model_grid%v_bys = grid%v_bys
            model_grid%v_bye = grid%v_bye
            model_grid%w_bxs = grid%w_bxs
            model_grid%w_bxe = grid%w_bxe
            model_grid%w_bys = grid%w_bys
            model_grid%w_bye = grid%w_bye
            model_grid%t_bxs = grid%t_bxs
            model_grid%t_bxe = grid%t_bxe
            model_grid%t_bys = grid%t_bys
            model_grid%t_bye = grid%t_bye
            model_grid%mu_bxs = grid%mu_bxs
            model_grid%mu_bxe = grid%mu_bxe
            model_grid%mu_bys = grid%mu_bys
            model_grid%mu_bye = grid%mu_bye
            model_grid%ph_bxs = grid%ph_bxs
            model_grid%ph_bxe = grid%ph_bxe
            model_grid%ph_bys = grid%ph_bys
            model_grid%ph_bye = grid%ph_bye
            model_grid%moist_bxs = grid%moist_bxs
            model_grid%moist_bxe = grid%moist_bxe
            model_grid%moist_bys = grid%moist_bys
            model_grid%moist_bye = grid%moist_bye

            model_grid%u_btxs = grid%u_btxs
            model_grid%u_btxe = grid%u_btxe
            model_grid%u_btys = grid%u_btys
            model_grid%u_btye = grid%u_btye
            model_grid%v_btxs = grid%v_btxs
            model_grid%v_btxe = grid%v_btxe
            model_grid%v_btys = grid%v_btys
            model_grid%v_btye = grid%v_btye
            model_grid%w_btxs = grid%w_btxs
            model_grid%w_btxe = grid%w_btxe
            model_grid%w_btys = grid%w_btys
            model_grid%w_btye = grid%w_btye
            model_grid%t_btxs = grid%t_btxs
            model_grid%t_btxe = grid%t_btxe
            model_grid%t_btys = grid%t_btys
            model_grid%t_btye = grid%t_btye
            model_grid%mu_btxs = grid%mu_btxs
            model_grid%mu_btxe = grid%mu_btxe
            model_grid%mu_btys = grid%mu_btys
            model_grid%mu_btye = grid%mu_btye
            model_grid%ph_btxs = grid%ph_btxs
            model_grid%ph_btxe = grid%ph_btxe
            model_grid%ph_btys = grid%ph_btys
            model_grid%ph_btye = grid%ph_btye
            model_grid%moist_btxs = grid%moist_btxs
            model_grid%moist_btxe = grid%moist_btxe
            model_grid%moist_btys = grid%moist_btys
            model_grid%moist_btye = grid%moist_btye

            ! Turn off model boundary reading as we already provide a new one.
            call da_model_lbc_off
         endif

         call da_nl_model(it)
!      elseif (var4d .and. multi_inc == 2 ) then
#else
         write(unit=message(1),fmt='(A)')'Please re-compile the code with 4dvar option'
         call da_error(__FILE__,__LINE__,message(1:1))
#endif
      end if

      ! [8.2] Calculate innovation vector (O-B):

      num_qcstat_conv=0
      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)
      if ( multi_inc == 1 ) return      

      if (test_transforms .or. test_gradient) then

         if (test_gradient) then
            call da_allocate_y (iv, re)
            call da_allocate_y (iv, y)
            call da_check_gradient (grid, config_flags, cv_size, xhat, cvt, 1.0e-10, 8, &
                         xbx, be, iv, y, re, j)
            call da_deallocate_y (re)
            call da_deallocate_y (y)
         endif

         if (test_transforms) then
            call da_check (grid, config_flags, cv_size, xbx, be, grid%ep, iv, &
                        grid%vv, grid%vp, y)
         endif

         ! No point continuing, as data corrupted
         call da_wrfvar_finalize
#ifdef VAR4D
         if (var4d) call da_finalize_model
#endif

         call wrf_message("*** WRF-Var check completed successfully ***")
         if (trace_use) call da_trace_exit("da_wrfvar_main")
         if (trace_use) call da_trace_report

         call wrfu_finalize
         call wrf_shutdown
         stop
      end if

      ! [8.4] Minimize cost function:

      call da_allocate_y (iv, re)
      call da_allocate_y (iv, y)

      if (use_lanczos) then
         if (read_lanczos) then
            call da_lanczos_io('r',cv_size,ntmax,neign,eignvec,eignval,qhat)

            call da_kmat_mul(grid,config_flags,it,cv_size,xbx, &
                             be,iv,xhat,qhat,cvt,re,y,j,eignvec,eignval,neign)
          ! Output Cost Function
            call da_calculate_j(it, 1, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                             be%cv%size_jl, xbx, be, iv, xhat, cvt, re, y, j, xhat, grid, config_flags)
         else
            call da_minimise_lz(grid, config_flags, it, cv_size, xbx,& 
                             be, iv, j_grad_norm_target, xhat, qhat, cvt, re, y, j, eignvec, eignval, neign)  

         end if

         if (write_lanczos) call da_lanczos_io('w',cv_size,ntmax,neign,eignvec,eignval,qhat)

         if (adj_sens) call da_sensitivity(grid,config_flags,cv_size,xbx, &
                                           be,iv,xhat,qhat,cvt,y,eignvec,eignval,neign)

	 
      else
         call da_minimise_cg( grid, config_flags, it, be % cv % size, & 
            xbx, be, iv, j_grad_norm_target, xhat, cvt, re, y, j)
      end if
 
      ! Update outer-loop control variable
      cvt = cvt + xhat

      !------------------------------------------------------------------------

      ! reset cv to random noise
      if (anal_type_randomcv) then
         call da_set_randomcv (cv_size, xhat)
      end if

      ! [8.5] Update latest analysis solution:
  
      if (.not. var4d) then
         call da_transform_vtox (grid,cv_size,xbx,be,grid%ep,xhat,grid%vv,grid%vp)
      else
         call da_transform_vtox (grid,be%cv%size_jb,xbx,be,grid%ep,xhat(1:be%cv%size_jb),grid%vv,grid%vp)
      endif
      call da_transform_xtoxa (grid)

      ! [8.6] Only when use_radarobs = .false. and calc_w_increment =.true.,
      !       the w_increment need to be diagnosed:

      if (calc_w_increment .and. .not. use_radarobs .and. .not. var4d) then
         call da_uvprho_to_w_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
      end if

      ! [8.7] Write out diagnostics

      call da_write_diagnostics (it, grid, num_qcstat_conv, ob, iv, re, y, j)

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
         ! if (it == 1) then
          if (write_mod_filtered_obs) then    !cys_change
            call da_write_modified_filtered_obs (grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          else
            call da_write_filtered_obs (it, grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          end if     
         ! end if     
      end if

      ! [8.7.1] Write Ascii radar OMB and OMA file

      if (use_radarobs) then
         write(unit=stdout,fmt='(A)')  'Writing radar OMB and OMA ascii file'
         write(unit=stdout,fmt=*)  " "
         call da_write_oa_radar_ascii (ob,iv,re)
      end if

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.8] Write Ascii radiance OMB and OMA file

#if defined(CRTM) || defined(RTTOV)
      if (write_oa_rad_ascii) then
         write(unit=stdout,fmt='(A)')  'Writing radiance OMB and OMA ascii file'
         write(unit=stdout,fmt=*)  " "
         call da_write_oa_rad_ascii (it,ob,iv,re)
      end if
#endif

      ! [8.9] Update VarBC parameters and write output file
#if defined(CRTM) || defined(RTTOV)      
      if ( (use_varbc).or.(freeze_varbc) ) &
                call da_varbc_update(it, cv_size, xhat, iv)
#endif


      !------------------------------------------------------------------------
      ! [8.10] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis (it, xbx, grid, config_flags)

     if ( it < max_ext_its .and. print_detail_outerloop ) then
        write(outerloop,'(i2.2)') it
        call da_med_initialdata_output (grid , config_flags, 'wrfvar_output_'//outerloop)
#ifdef VAR4D
        !if (var4d) call da_med_initialdata_output_lbc (grid , config_flags, 'wrfvar_bdyout_'//outerloop)
#endif
     end if

     call da_deallocate_y (re)
     call da_deallocate_y (y)

   end do

   ! output wrfvar analysis

   if ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_output (head_grid, config_flags)
#ifdef VAR4D
      !if (var4d) call da_med_initialdata_output_lbc (head_grid , config_flags)
      if ( var4d_lbc ) then
         call domain_clock_get (grid, stop_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
         call da_med_initialdata_input (grid, config_flags, 'fg02')
         call da_setup_firstguess(xbx, grid, config_flags)
         shuffle = grid%xa
         jl_start    = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + 1
         jl_end      = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_jl
         grid%xa  = grid%x6a
         call da_transform_vtox(grid, be%cv%size_jl, xbx, be, grid%ep, &
              xhat(jl_start:jl_end), grid%vv6, grid%vp6)
         grid%xa  = shuffle
         call da_transfer_xatoanalysis (it, xbx, grid, config_flags)
         call da_med_initialdata_output (grid, config_flags,'ana02')
         call domain_clock_get (grid, start_timestr=timestr1)
         call domain_clock_set( grid, current_timestr=timestr1 )
      endif
#endif
      call med_shutdown_io (grid, config_flags)
   end if

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate (cvt)
   deallocate (xhat)
   if (use_lanczos) then
      deallocate (qhat)
      deallocate (eignvec)
      deallocate (eignval)
!      deallocate (full_eignvec)
   end if

#if defined(RTTOV) || defined(CRTM)
   if (use_rad) then
      do i =1, iv%num_inst
         deallocate (j % jo % rad(i) % jo_ichan)
         deallocate (j % jo % rad(i) % num_ichan)
         deallocate (satinfo(i) % ichan)
         deallocate (satinfo(i) % iuse)
         deallocate (satinfo(i) % error)
         deallocate (satinfo(i) % polar)

         deallocate (satinfo(i) % scanbias)
         deallocate (satinfo(i) % scanbias_b)
         deallocate (satinfo(i) % bcoef)
         deallocate (satinfo(i) % bcoef0)
         deallocate (satinfo(i) % error_std)

         deallocate (ob%instid(i) % ichan)
         deallocate (iv%instid(i)%ichan)

         if (iv%instid(i)%num_rad > 0) then
 
            deallocate (iv%instid(i)%info%date_char)
            deallocate (iv%instid(i)%info%name)
            deallocate (iv%instid(i)%info%platform)
            deallocate (iv%instid(i)%info%id)
            deallocate (iv%instid(i)%info%levels)     
            deallocate (iv%instid(i)%info%lat)      
            deallocate (iv%instid(i)%info%lon)      
            deallocate (iv%instid(i)%info%elv)   

            deallocate (iv%instid(i)%info%pstar)
            deallocate (iv%instid(i)%info%i)
            deallocate (iv%instid(i)%info%j)
            deallocate (iv%instid(i)%info%k)
            deallocate (iv%instid(i)%info%zk)
            deallocate (iv%instid(i)%info%dx)
            deallocate (iv%instid(i)%info%dy)
            deallocate (iv%instid(i)%info%dz)
            deallocate (iv%instid(i)%info%dxm)
            deallocate (iv%instid(i)%info%dym)
            deallocate (iv%instid(i)%info%dzm)
            deallocate (iv%instid(i)%info%proc_domain)

            deallocate (iv%instid(i)%t)
            deallocate (iv%instid(i)%mr)
            deallocate (iv%instid(i)%tm)
            deallocate (iv%instid(i)%qm)
            deallocate (iv%instid(i)%qrn)
            deallocate (iv%instid(i)%qcw)
            if ( crtm_cloud ) then
               deallocate (iv%instid(i)%qci)
               deallocate (iv%instid(i)%qsn)
               deallocate (iv%instid(i)%qgr)
               deallocate (iv%instid(i)%qhl)
               deallocate (iv%instid(i)%rcw)
               deallocate (iv%instid(i)%rci)
               deallocate (iv%instid(i)%rrn)
               deallocate (iv%instid(i)%rsn)
               deallocate (iv%instid(i)%rgr)
               deallocate (iv%instid(i)%rhl)
            end if
            deallocate (iv%instid(i)%pm)
            deallocate (iv%instid(i)%pf)
            deallocate (iv%instid(i)%u10)
            deallocate (iv%instid(i)%v10)
            deallocate (iv%instid(i)%t2m)
            deallocate (iv%instid(i)%q2m)
            deallocate (iv%instid(i)%mr2m)
            deallocate (iv%instid(i)%psfc)
            deallocate (iv%instid(i)%ts)
            deallocate (iv%instid(i)%smois)
            deallocate (iv%instid(i)%tslb)
            deallocate (iv%instid(i)%snowh)
            deallocate (iv%instid(i)%isflg)
            deallocate (iv%instid(i)%soiltyp)
            deallocate (iv%instid(i)%landsea_mask)
            if (rtm_option == rtm_option_rttov) then
               deallocate (iv%instid(i)%surftype)
               deallocate (iv%instid(i)%snow_frac)
            end if
            deallocate (iv%instid(i)%elevation)
            deallocate (iv%instid(i)%vegfra)
            deallocate (iv%instid(i)%vegtyp)
            deallocate (iv%instid(i)%clwp)
            deallocate (iv%instid(i)%ps)
            deallocate (iv%instid(i)%tb_xb)
            deallocate (iv%instid(i)%tb_qc)
            deallocate (iv%instid(i)%tb_inv)
            deallocate (iv%instid(i)%tb_error)
            deallocate (iv%instid(i)%tb_sens)
            deallocate (iv%instid(i)%tb_imp)
            deallocate (iv%instid(i)%rad_xb)
            deallocate (iv%instid(i)%rad_obs)
            deallocate (iv%instid(i)%rad_ovc)
            deallocate (iv%instid(i)%emiss)
            deallocate (iv%instid(i)%scanpos)
            deallocate (iv%instid(i)%scanline)
            deallocate (iv%instid(i)%ifgat)
            deallocate (iv%instid(i)%cloud_flag)
            deallocate (iv%instid(i)%rain_flag)
            deallocate (iv%instid(i)%satzen)
            deallocate (iv%instid(i)%satazi)
            deallocate (iv%instid(i)%solzen)
            deallocate (iv%instid(i)%solazi)
            deallocate (iv%instid(i)%gamma_jacobian)

           if (ANY(use_satcv)) then
	      if (use_satcv(2)) then
                 do n = 1,iv%instid(i)%num_rad
                    deallocate (iv%instid(i)%cv_index(n)%cc)
                    deallocate (iv%instid(i)%cv_index(n)%vtox)
                 end do
	      end if
              deallocate (iv%instid(i)%cv_index)  
           end if
        
           if ( use_rttov_kmatrix .or. use_crtm_kmatrix ) then
              deallocate(iv%instid(i)%ts_jacobian)
              deallocate(iv%instid(i)%ps_jacobian)
              deallocate(iv%instid(i)%emiss_jacobian)
              deallocate(iv%instid(i)%windspeed_jacobian)
              deallocate(iv%instid(i)%t_jacobian)
              deallocate(iv%instid(i)%q_jacobian)
           end if
            if (rtm_option == rtm_option_crtm) then
               deallocate(iv%instid(i)%crtm_climat)
               deallocate(iv%instid(i)%water_coverage)
               deallocate(iv%instid(i)%land_coverage)
               deallocate(iv%instid(i)%ice_coverage)
               deallocate(iv%instid(i)%snow_coverage)
               if (use_crtm_kmatrix) then
                 if ( crtm_cloud ) then
                     deallocate(iv%instid(i)%water_jacobian)
                     deallocate(iv%instid(i)%ice_jacobian)
                     deallocate(iv%instid(i)%rain_jacobian)
                     deallocate(iv%instid(i)%snow_jacobian)
                     deallocate(iv%instid(i)%graupel_jacobian)
                     deallocate(iv%instid(i)%hail_jacobian)
                     deallocate(iv%instid(i)%water_r_jacobian)
                     deallocate(iv%instid(i)%ice_r_jacobian)
                     deallocate(iv%instid(i)%rain_r_jacobian)
                     deallocate(iv%instid(i)%snow_r_jacobian)
                     deallocate(iv%instid(i)%graupel_r_jacobian)
                     deallocate(iv%instid(i)%hail_r_jacobian)
                 end if
              end if
            end if

         end if

	  if ( (use_varbc).or.(freeze_varbc) ) then
	     if (iv%instid(i)%varbc_info%npredmax > 0) then
                deallocate (iv%instid(i)%varbc_info%pred)
                deallocate (iv%instid(i)%varbc_info%pred_mean)
                deallocate (iv%instid(i)%varbc_info%pred_std)
                deallocate (iv%instid(i)%varbc_info%nbgerr)
	     end if
	     do ichan = 1, iv%instid(i)%nchan
	        if (iv%instid(i)%varbc(ichan)%npred <= 0) cycle	 
	        deallocate (iv%instid(i)%varbc(ichan)%pred_use)
	        deallocate (iv%instid(i)%varbc(ichan)%ipred)
	        deallocate (iv%instid(i)%varbc(ichan)%index)
	        deallocate (iv%instid(i)%varbc(ichan)%param)
    	        deallocate (iv%instid(i)%varbc(ichan)%bgerr)
	        deallocate (iv%instid(i)%varbc(ichan)%vtox)
	     end do
   	     deallocate (iv%instid(i)%varbc)
	  end if
#ifdef RTTOV
         if (rtm_option == rtm_option_rttov) then
            call rttov_dealloc_coefs (ierr,coefs(i))
            if ( ierr /= 0 ) then
               call da_error(__FILE__,__LINE__,(/'failure in rttov_dealloc_coefs'/))
            end if
         end if
#endif
      end do
      deallocate (iv%instid)
      deallocate (j % jo % rad)     
      deallocate (satinfo)
      deallocate (time_slots)
#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         deallocate (coefs)
         deallocate (opts)
      end if
#endif
   end if
#endif

   if (var4d .and. use_rainobs) deallocate(fgat_rain_flags)
   call da_deallocate_observations (iv)
   call da_deallocate_y (ob)
   if (use_background_errors) call da_deallocate_background_errors (be)

   if (xbx%pad_num > 0) then
      deallocate (xbx%pad_loc)
      deallocate (xbx%pad_pos)
   end if

   deallocate (xbx % fft_factors_x)
   deallocate (xbx % fft_factors_y)
   deallocate (xbx % fft_coeffs)
   deallocate (xbx % trig_functs_x)
   deallocate (xbx % trig_functs_y)

   if (global) then
      deallocate (xbx%coslat)
      deallocate (xbx%sinlat)
      deallocate (xbx%coslon)
      deallocate (xbx%sinlon)
      deallocate (xbx%int_wgts)
      deallocate (xbx%alp)
      deallocate (xbx%wsave)
      if (jts == jds) then
         deallocate (cos_xls)
         deallocate (sin_xls)
      end if
                                                                                
      if (jte == jde) then
         deallocate (cos_xle)
         deallocate (sin_xle)
      end if
   end if

#ifdef DM_PARALLEL
   call mpi_barrier (comm,ierr)
#endif

   if (trace_use) call da_trace_exit ("da_solve")


contains

#include "da_solve_init.inc"

end subroutine da_solve

