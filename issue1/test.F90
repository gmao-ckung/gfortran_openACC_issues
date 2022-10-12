program issue1
    use testMod
    
    implicit none

    call allocate_array()
!$acc parallel

    call zero_array(test_array)
    !call zero_array()

    call array_add_one()

!$acc end parallel
    call print_array()

end program issue1
