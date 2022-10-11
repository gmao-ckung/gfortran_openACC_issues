program issue1
    use testMod
    
    implicit none

    call allocate_array()
!$acc parallel

    !*****************************************************************
    ! For gfortran to see a module array in a module subroutine running
    ! within an OpenACC parallel region, the module array (seems like it)
    ! needs to be passed first into a subroutine that has that module array
    ! as a parameter.  Subsequent module subroutines that run within an OpenACC
    ! parallel region do not need to pass the array as a parameter.
    ! Thus, to execute properly with gfortran, the call to 'zero_array' will
    ! need 'test_array' as a parameter.  Otherwise, it will return the following:
    ! libgomp: cuStreamSynchronize error: an illegal memory access was encountered
    !
    ! nvfortran is able to execute all the module subroutines in the OpenACC parallel
    ! region without having 'test_array' as a parameter.
    !*****************************************************************

    call zero_array(test_array)
    ! call zero_array()

    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()
    call array_add_one()

!$acc end parallel
    call print_array()

end program issue1