program homework2
    implicit none
    integer, parameter :: row_size = 256    
    integer, parameter :: buffer_size = 256*256
    integer, parameter :: newbuffer_size = 86*86
    integer, parameter :: new_row_size = 86    

    character :: cbuffer(buffer_size)
    integer   :: ibuffer(buffer_size)
    character :: newbuffer(newbuffer_size)
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
 
 	do i = 1, new_row_size
 		do j = 1, new_row_size
 			newbuffer((i-1)*new_row_size+j) = char(ibuffer((i*3-2-1)*row_size+j*3-2))
		end do
	end do

    open(20, file="newlena.raw", form="unformatted", access="direct",&
    recl=86*86, status="replace", iostat=error)
    if(error/=0)then
    	write(*,*)"open newlena.raw fail."
    	stop
    end if
    write(20,rec=1)newbuffer
    close(20)

end program homework2