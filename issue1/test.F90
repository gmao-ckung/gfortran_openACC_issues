program test_array_adjust
    use testMod
    implicit none

    call allocate_array()
!$acc parallel
    call set_array(test_array)
    ! call set_array()
!$acc end parallel
    call print_array()

end program test_array_adjust

