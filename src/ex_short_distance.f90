! ============================================================================
! Name        : ex_short_distance.f90
! Author      : Summer Liang
! Version     : 0.1
! Description : Fortran程序设计课程的五一作业题：求解最短路径
! Solution   :  使用Dijkstra算法求解
! ============================================================================

program ex_short_distance
    implicit none
    type :: path
        real :: length
        integer :: pre
    end type path

    real, dimension(10,10) :: adj(10,10)
    type(path),dimension(10) :: dist
    real :: min
    integer :: k,i,u
    
    adj(1,1)=0
    dist(8)=path(0.5,1)
    min = 0

    k = 0
    i = 0
    u = 0

end program 