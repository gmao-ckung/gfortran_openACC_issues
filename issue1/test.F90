module testMod

    use openacc

    implicit none

    private

    public zero_array, allocate_array, print_test_array, deallocate_array, array_calc

    double precision, dimension(:), allocatable :: test_array

    contains

    subroutine allocate_array(in_size)

        integer :: in_size

        allocate(test_array(in_size))
!$omp target enter data map(alloc:test_array)
    end subroutine

    subroutine zero_array(in_size)
        integer :: ii, in_size
!$omp target teams distribute parallel do
        do ii = 1,in_size
            test_array(ii) = 0.0
        enddo
!$omp end target teams distribute parallel do
    end subroutine

    subroutine array_calc(in_size)
        integer :: ii, in_size

!$omp target teams distribute parallel do
        do ii = 1, in_size
            test_array(ii) = 1.0
        enddo
    end subroutine

    subroutine print_test_array
        print*,'test_array = ', sum(test_array)
    end subroutine

    subroutine deallocate_array
!$omp target exit data map(from:test_array)
    end subroutine

end module

program issue1
    use testMod
    
    integer :: sizeA = 900*1000*1000

    call allocate_array(sizeA)
    
    print*,'Entering parallel region'

    call zero_array(sizeA)

    call array_calc(sizeA)

    call deallocate_array()

    call print_test_array()

end program issue1
