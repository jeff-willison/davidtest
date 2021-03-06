subroutine da_add_flow_dependence_xa (grid, ne,  ep, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (grid%xa).
   !-----------------------------------------------------------------------
                                      
   implicit none

   type (domain), intent(inout)         :: grid
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.
   
   integer                              :: i, j, k, n  ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.

   do k = kts, kte
      do j = jts, jte
         do i = its, ite

            do n = 1, ne
               alpha = vp % alpha(i,j,k,n)

               ! u:
               grid%xa % u(i,j,k) = grid%xa % u(i,j,k) + alpha * ep % v1(i,j,k,n) ! v1 = u

               ! v:
               grid%xa % v(i,j,k) = grid%xa % v(i,j,k) + alpha * ep % v2(i,j,k,n) ! v2 = v

               ! t:
               grid%xa % t(i,j,k) = grid%xa % t(i,j,k) + alpha * ep % v3(i,j,k,n) ! v3 = t

               ! q:
               grid%xa % q(i,j,k) = grid%xa % q(i,j,k) + alpha * ep % v4(i,j,k,n) ! v4 = q

            end do
         end do
      end do
   end do

   ! ps:
   do n = 1, ne
      grid%xa % psfc(its:ite,jts:jte) = grid%xa % psfc(its:ite,jts:jte) + &
                                        vp % alpha(its:ite,jts:jte,1,n) * &
                                        ep % v5(its:ite,jts:jte,1,n) ! v5 = ps
   end do

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa")

end subroutine da_add_flow_dependence_xa


