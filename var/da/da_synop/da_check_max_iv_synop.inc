subroutine da_check_max_iv_synop(iv,ob, it, num_qcstat_conv)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.


   logical :: failed
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_synop")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(synop)%n1,iv%info(synop)%n2

      failed=.false.
      if( iv%synop(n)%u%qc >= obs_qc_pointer ) then 
      call da_max_error_qc (it, iv%info(synop), n, iv%synop(n)%u, max_error_uv, failed)
      if( iv%info(synop)%proc_domain(1,n) ) then
      num_qcstat_conv(1,synop,1,1)= num_qcstat_conv(1,synop,1,1) + 1
      if(failed) then
      num_qcstat_conv(2,synop,1,1)= num_qcstat_conv(2,synop,1,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'synop',ob_vars(1),iv%info(synop)%lat(1,n),iv%info(synop)%lon(1,n),0.01*ob%synop(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%synop(n)%v%qc >= obs_qc_pointer )  then
      call da_max_error_qc (it, iv%info(synop), n, iv%synop(n)%v, max_error_uv, failed)             
      if( iv%info(synop)%proc_domain(1,n) ) then
      num_qcstat_conv(1,synop,2,1)= num_qcstat_conv(1,synop,2,1) + 1
      if(failed) then
      num_qcstat_conv(2,synop,2,1)= num_qcstat_conv(2,synop,2,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'synop',ob_vars(2),iv%info(synop)%lat(1,n),iv%info(synop)%lon(1,n),0.01*ob%synop(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%synop(n)%t%qc >= obs_qc_pointer )  then
      call da_max_error_qc (it, iv%info(synop), n, iv%synop(n)%t, max_error_t , failed)
      if( iv%info(synop)%proc_domain(1,n) ) then
      num_qcstat_conv(1,synop,3,1)= num_qcstat_conv(1,synop,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,synop,3,1)= num_qcstat_conv(2,synop,3,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'synop',ob_vars(3),iv%info(synop)%lat(1,n),iv%info(synop)%lon(1,n),0.01*ob%synop(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%synop(n)%p%qc >= obs_qc_pointer )  then
      call da_max_error_qc (it, iv%info(synop), n, iv%synop(n)%p, max_error_p , failed)         
      if( iv%info(synop)%proc_domain(1,n) ) then
      num_qcstat_conv(1,synop,5,1)= num_qcstat_conv(1,synop,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,synop,5,1)= num_qcstat_conv(2,synop,5,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'synop',ob_vars(5),iv%info(synop)%lat(1,n),iv%info(synop)%lon(1,n),0.01*ob%synop(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%synop(n)%q%qc >= obs_qc_pointer ) then
       if( iv%synop(n)%t%qc == fails_error_max .or. iv%synop(n)%p%qc == fails_error_max) then
       failed=.true.
       iv%synop(n)%q%qc  = fails_error_max
       iv%synop(n)%q%inv = 0.0
       else
       call da_max_error_qc (it, iv%info(synop), n, iv%synop(n)%q, max_error_q , failed)
       endif
      if( iv%info(synop)%proc_domain(1,n) ) then
      num_qcstat_conv(1,synop,4,1)= num_qcstat_conv(1,synop,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,synop,4,1)= num_qcstat_conv(2,synop,4,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'synop',ob_vars(4),iv%info(synop)%lat(1,n),iv%info(synop)%lon(1,n),0.01*ob%synop(n)%p
      end if
      end if
      end if 
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_synop")

end subroutine da_check_max_iv_synop


