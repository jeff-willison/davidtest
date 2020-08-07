        module complex_number_module

        implicit none

        integer, parameter :: loc_real_precision = 8
        integer, parameter :: zero = 0.0_loc_real_precision
        real,    parameter :: min_val = 1.0e-30_loc_real_precision

        type complex_number
          real(kind=loc_real_precision) :: real_part, imag_part
        end type complex_number

        interface c_sub
          module procedure c_sub_cc,    &      ! z1 - z2
                           c_sub_cr,    &      ! z1 - num, where num is a real number
                           c_sub_rc            ! num - z1, where num is a real number
        end interface

        contains

! --------------------------------------------------------------------------
        type (complex_number) function c_sub_cc (z1, z2)

        type (complex_number), intent(in) :: z1, z2

        c_sub_cc%real_part = z1%real_part - z2%real_part
        c_sub_cc%imag_part = z1%imag_part - z2%imag_part

        end function c_sub_cc

! --------------------------------------------------------------------------
        type (complex_number) function c_sub_cr (z1, num1)

        type (complex_number),     intent(in) :: z1
        real(kind=loc_real_precision), intent(in) :: num1

        c_sub_cr%real_part = z1%real_part - num1
        c_sub_cr%imag_part = z1%imag_part

        end function c_sub_cr

! --------------------------------------------------------------------------
        type (complex_number) function c_sub_rc (num2, z2)

        type (complex_number),     intent(in) :: z2
        real(kind=loc_real_precision), intent(in) :: num2

        c_sub_rc%real_part = num2 - z2%real_part
        c_sub_rc%imag_part = - z2%imag_part

        end function c_sub_rc

        end module complex_number_module
