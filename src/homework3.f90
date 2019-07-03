program homework3
    implicit none
    integer, parameter :: recl_unit = 1
    integer, parameter :: buffer_size = 256*256
    integer, parameter :: matrix_length = 256
    integer, parameter :: newbuffer_size = 86*86
    integer, parameter :: newbuffer = 86
    character :: cbuffer(buffer_size)
    integer   :: ibuffer(buffer_size)
    integer   :: jbuffer(buffer,buffer)
    character :: cnewbuffer(newbuffer_size)
    integer   :: inewbuffer(newbuffer_size)
    integer   :: jnewbuffer(newbuffer,newbuffer)
    integer   :: error, i, j, code

    open(10, file="lena.raw", form="unformatted", access="direct",&
    recl=256*256/recl_unit, status="old", iostat=error)
    if(error/=0)then
        write(*,*)"open lena.raw fail."
        stop
    end if

    read(10, rec=1)cbuffer
    close(10)

    do i=1, buffer_size
        code = ichar(cbuffer(i))
        if(code>=0)then
            ibuffer(i)=code
        else
            ibuffer(i)=256+code
        end if
    end do

    do i=1, buffer
        do j=1, buffer
            jbuffer(i,j)=ibuffer(buffer*(-1+i)+j)
        end do
    end do

    do i=1, newbuffer
        do j=1, newbuffer
            jnewbuffer(i,j)=jbuffer(-2+3*i, -2+3*j)
        end do
    end do

    do i=1, newbuffer
        do j=1, newbuffer
            cnewbuffer(newbuffer*(-1+i)+j)=char(jnewbuffer(i,j))
        end do
    end do

    open(20, file="newlena.raw", form="unformatted", access="direct",&
    recl=86*86/recl_unit, status="replace")
    write(20,rec=1)cnewbuffer
    close(20)
    stop

end program homework3