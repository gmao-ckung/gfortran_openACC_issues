module testMod

    use openacc

    implicit none

    public set_array

    double precision, dimension(:), allocatable :: test_array
    integer :: array_size = 1000*1000*900

    interface set_array
        module procedure set_array_without_param
        module procedure set_array_with_param
    end interface

!$acc declare create(array_size, test_array)

    contains

    subroutine allocate_array()
        allocate(test_array(array_size))
#ifdef GNU_OACC
        call acc_copyin(test_array)
#endif
    end subroutine


    subroutine set_array_without_param
    !subroutine set_array
!$acc routine gang
        integer :: ii
!$acc loop gang vector collapse(1)
        do ii = 1, array_size
            test_array(ii) = 1.0
        enddo

    end subroutine

    subroutine set_array_with_param(test_array)
        !subroutine set_array
    !$acc routine gang
            integer :: ii
            double precision :: test_array(array_size)
    !$acc loop gang vector collapse(1)
            do ii = 1, array_size
                test_array(ii) = 1.0
            enddo

        end subroutine

    subroutine print_array()

        ! integer :: ii

!$acc update host(test_array)
        ! do ii = 1, array_size
        print*, 'array_size = ', array_size
        print*, 'sum(test_array) = ', sum(test_array)
        ! enddo   
    end subroutine

end module