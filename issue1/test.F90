module testMod

    use openacc

    implicit none

    private

    public zero_array, allocate_array, check_allocation

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

    subroutine zero_array(in_size)
!$acc routine gang
        integer :: ii, in_size

        !$acc loop gang
        do ii = 1,in_size
            test_array(ii) = 0.0
        enddo
    end subroutine

    subroutine check_allocation

        print*,'Is test_array allocated on device? : ', acc_is_present(test_array)

    end subroutine

end module
program issue1
    use testMod
    
    call allocate_array(10)
    call check_allocation()
    print*,'Entering parallel region'
!$acc parallel
    call zero_array(10)
!$acc end parallel

end program issue1
