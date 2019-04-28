! ============================================================================
! Name        : get_commdn_line.f90
! Author      : Johnny Liang
! Version     : 0.1
! Description : Test program on command line functions
! ============================================================================

PROGRAM get_command_line
    IMPLICIT NONE
    INTEGER  :: i
    CHARACTER(LEN=128) :: command
    CHARACTER(LEN=80) :: arg

    CALL get_command_argument(0,command)

    WRITE (*,'(A,A)') 'Program name is :',TRIM(command)

    DO i = 1,command_argument_count()
        CALL get_command_argument(i,arg)
        WRITE (*,'(A,I2,A,A)') 'Argument ', i, ' is ',TRIM(arg)
    END DO
END PROGRAM get_command_line