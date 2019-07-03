program homework4
    implicit none
    integer, parameter :: buffer_size = 256*256
    integer, parameter :: buffer_a = 256
    integer, parameter :: newbuffer_size = 86*86
    integer, parameter :: newbuffer_a = 86
    character :: cbuffer(buffer_size)
    integer   :: ibuffer(buffer_size)
    integer   :: jbuffer(buffer_a,buffer_a)
    character :: cnewbuffer(newbuffer_size)
    integer   :: inewbuffer(newbuffer_size)
    integer   :: jnewbuffer(newbuffer_a,newbuffer_a)
    integer   :: error, i, j, code

    open(10, file="lena.raw", form="unformatted", access="direct",&
    recl=256*256, status="old", iostat=error)
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

    do i=1, buffer_a
        do j=1, buffer_a
            jbuffer(i,j)=ibuffer(buffer_a*(-1+i)+j)
        end do
    end do

    do i=1, newbuffer_a
        do j=1, newbuffer_a
            jnewbuffer(i,j)=jbuffer(-2+3*i, -2+3*j)
        end do
    end do

    do i=1, newbuffer_a
        do j=1, newbuffer_a
            cnewbuffer(newbuffer_a*(-1+i)+j)=char(jnewbuffer(i,j))
        end do
    end do

    open(20, file="newlena1.raw", form="unformatted", access="direct",&
    recl=86*86, status="replace")
    write(20,rec=1)cnewbuffer
    close(20)
    if(error/=0)then
        write(*,*)"open newlena.raw fail."
        stop
    end if

end program homework4