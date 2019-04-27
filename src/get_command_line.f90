PROGRAM get_command_line
    implicit none
integer :: i
character(len=128) :: command
character(len=80) :: arg

call get_command_argument(0,command)

write (*,'(A,A)') 'Program name is :',trim(command)

do i = 1,command_argument_count()
    call get_command_argument(i,arg)
    write (*,'(A,I2,A,A)') 'Argument ', i, ' is ',trim(arg)
end do

end Program get_command_line