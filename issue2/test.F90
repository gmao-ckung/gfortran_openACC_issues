module testMod

    use openacc

    implicit none

    private

    public zero_array, allocate_array, check_allocation, lvl_one_add

    double precision, dimension(:), allocatable :: test_array

!$acc declare create(test_array)

    contains

    subroutine allocate_array(in_size)

        integer :: in_size

        allocate(test_array(in_size))
#ifdef GNU_OACC
        call acc_create(test_array)
#endif

    end subroutine

    subroutine zero_array()
        integer :: ii, in_size

        in_size = size(test_array,1)
        !$acc parallel loop gang
        do ii = 1,in_size
            test_array(ii) = 0.0
        enddo
        !$acc end parallel loop
    end subroutine

    subroutine lvl_one_add()

        integer ii, in_size

        double precision, dimension(:), allocatable :: sub_array

        in_size = size(test_array,1)
        print*, 'in_size = ', in_size
        allocate(sub_array(in_size))

!$acc enter data create(sub_array)

        print*,"here 1"

        !$acc parallel loop gang
        do ii = 1, in_size
            test_array(ii) = 1.0
            sub_array(ii) = 1.0
        enddo
        !$acc end parallel loop
        print*,"here 2"
        call lvl_two_add()
        print*,"here 3"
!$acc exit data copyout(sub_array)

        print*,'sum(sub_array) = ', sum(sub_array)

        contains

        subroutine lvl_two_add()
            integer ii, in_size2
            in_size2 = size(sub_array,1)

            print*, 'in_size = ', in_size2

            !$acc parallel loop gang
            do ii = 1, in_size2
                sub_array(ii) = sub_array(ii) + 1.0
            enddo
            !$acc end parallel loop
        end subroutine
    end subroutine

    subroutine check_allocation

        ! print*,'Is test_array allocated on device? : ', acc_is_present(test_array)
        !$acc update host(test_array)
        print*,'sum(test_array) = ', sum(test_array)

    end subroutine

end module
program issue1
    use testMod
    
    call allocate_array(400000000)
    call check_allocation()
    print*,'Entering parallel region'
    call zero_array()
    call lvl_one_add()
    call check_allocation()


end program issue1
