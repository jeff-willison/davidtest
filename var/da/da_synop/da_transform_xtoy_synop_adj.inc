subroutine da_transform_xtoy_synop_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_psfc(:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_synop_adj")

   if (sfc_assi_options == sfc_assi_options_1) then
      allocate (model_u(1,iv%info(synop)%n1:iv%info(synop)%n2))
      allocate (model_v(1,iv%info(synop)%n1:iv%info(synop)%n2))
      allocate (model_t(1,iv%info(synop)%n1:iv%info(synop)%n2))
      allocate (model_q(1,iv%info(synop)%n1:iv%info(synop)%n2))
      allocate (model_psfc(iv%info(synop)%n1:iv%info(synop)%n2))
      ! [1.2] Interpolate horizontally:
      do n=iv%info(synop)%n1,iv%info(synop)%n2
         model_u(1,n)  = jo_grad_y%synop(n)%u
         model_v(1,n)  = jo_grad_y%synop(n)%v
         model_t(1,n)  = jo_grad_y%synop(n)%t
         model_q(1,n)  = jo_grad_y%synop(n)%q
         model_psfc(n) = jo_grad_y%synop(n)%p
      end do
#ifdef A2C
      call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(synop),model_u,'u')
      call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(synop),model_v,'v')
#else
      call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(synop),model_u)
      call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(synop),model_v)
#endif
      call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(synop),model_t)
      call da_interp_lin_3d_adj (jo_grad_x%q, iv%info(synop),model_q)

      call da_interp_lin_2d_adj (jo_grad_x%psfc, iv%info(synop), 1, model_psfc)
      deallocate (model_u)
      deallocate (model_v)
      deallocate (model_t)
      deallocate (model_q)
      deallocate (model_psfc)
   else if (sfc_assi_options == sfc_assi_options_2) then
      call da_transform_xtopsfc_adj(grid,iv, synop, iv%synop(:), jo_grad_y%synop(:),jo_grad_x)
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_synop_adj")

end subroutine da_transform_xtoy_synop_adj


