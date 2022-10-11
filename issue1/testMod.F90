module testMod

    use openacc

    implicit none

    public zero_array, array_add_one, allocate_array

    double precision, dimension(:), allocatable :: test_array
    integer :: array_size = 1000*1000*900

    interface zero_array
        module procedure zero_array_without_param
        module procedure zero_array_with_param
    end interface
    interface array_add_one
        module procedure array_add_one_without_param
        module procedure array_add_one_with_param
    end interface

    interface allocate_array
        module procedure allocate_with_param
        module procedure allocate_without_param
    end interface

!$acc declare create(array_size, test_array)

    contains

    subroutine allocate_with_param(test_array)
        double precision, allocatable :: test_array(:)
        allocate(test_array(array_size))
    end subroutine

    subroutine allocate_without_param()
        allocate(test_array(array_size))
#ifdef GNU_OACC
        call acc_create(test_array)
#endif
    end subroutine

    subroutine zero_array_without_param
        !$acc routine gang
        integer :: ii
        !$acc loop gang !collapse(1)
        do ii = 1, array_size
            test_array(ii) = 0.0
        enddo
    end subroutine

    subroutine array_add_one_without_param
        !$acc routine gang
        integer :: ii
        !$acc loop gang !collapse(1)
        do ii = 1, array_size
            test_array(ii) = test_array(ii) + 1.0
        enddo

    end subroutine

    subroutine zero_array_with_param(test_array)
        !$acc routine gang
        integer :: ii
        double precision :: test_array(:)
        !$acc loop gang !collapse(1)
        do ii = 1, array_size
            test_array(ii) = 0.0
        enddo
    end subroutine

    subroutine array_add_one_with_param(test_array)
        !subroutine set_array
    !$acc routine gang
            integer :: ii
            double precision :: test_array(:)
    !$acc loop gang !collapse(1)
            do ii = 1, array_size
                test_array(ii) = test_array(ii) + 1.0
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