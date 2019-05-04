program shortest_path
    implicit none

    real,parameter :: MAX = 9999.0
    integer,parameter :: START_POINT = 7
    integer,parameter :: END_POINT = 10

    type :: path
      real :: length
      integer :: pre
    end type path

    type(path) :: dist(10)  !声明一个path类型的变量

    character(1) :: point_name(10)=(/'A','B','C','D','E','F','G','H','I','J'/)
    character(256) :: temp_str
    integer :: ptr =0 !打印路径的指针

    real :: adj(10,10)= transpose(reshape((/ 0.0,2.0,MAX,2.0,MAX,MAX,MAX,MAX,MAX,MAX,&
    &2.0,0.0,2.0,2.828,2.0,2.828,MAX,MAX,MAX,MAX,&
    &MAX,2.0,0.0,MAX,MAX,MAX,MAX,MAX,MAX,1.0,&
    &2.0,2.828,MAX,0.0,2.0,MAX,2.0,2.828,MAX,MAX,&
    &MAX,2.0,MAX,2.0,0.0,2.0,MAX,2.0,MAX,MAX,&
    &MAX,MAX,MAX,MAX,2.0,0.0,MAX,MAX,MAX,1.0,&
    &MAX,MAX,MAX,2.0,MAX,MAX,0.0,2.0,MAX,MAX,&
    &MAX,MAX,MAX,2.828,2.0,2.828,2.0,0.0,2.0,MAX,&
    &MAX,MAX,MAX,MAX,MAX,2.0,MAX,2.0,0.0,MAX,&
    &MAX,MAX,1.0,MAX,MAX,1.0,MAX,MAX,MAX,0.0 /),shape(adj)))
    !声明一个10x10的数组，描述从图中各点到其余点的路径长度
    real :: min     !声明最小距离值
    integer :: k=START_POINT  !声明起始点序号
    integer :: i    !声明循环中的编号
    integer :: u    !声明该行数据所在的数据组别判断依据

    u = 0
    !1.把结点初始划分为两个组
    loop_1: do i=1,10
          dist(i)%length = adj(k,i)
          if ( dist(i)%length/=MAX ) then
            dist(i)%pre=k
          else
            dist(i)%pre=0
          end if
        end do loop_1 
        adj(k,k)=1
        
    !2.把第二组结点逐个加入第一组中
    loop_2: do while(.true.)
        !2.1 从第二组中找距离最小的结点
        min = MAX
        u=0
        loop_3:do i=1,10
            if( adj(i,i)==0 .and. dist(i)%length<min) then
              u=i 
              min=dist(i)%length
            end if
          end do loop_3
        !2.2将找到的结点加入到第一组中
        if( u==0 ) then 
            exit
        else
            adj(u,u)=1
        end if
        !2.3修改第二组结点的距离值
        loop_4:do i=1,10
            if ( adj(i,i)==0 .and. dist(i)%length > dist(u)%length + adj(u,i)) then
               dist(i)%length=dist(u)%length + adj(u,i)
               dist(i)%pre=u
            end if
        end do loop_4
    end do loop_2

    write(*,'(A24,F10.4)') 'The shortest length is:',dist(END_POINT)%length
    ptr=END_POINT
    temp_str =point_name(ptr)
    i = 1
    do while((dist(ptr)%pre/=START_POINT) .and. i<10 )
      ptr = dist(ptr)%pre
      temp_str = point_name(ptr) // '->' // trim(temp_str)
      i = i+1
    end do
      temp_str = point_name(START_POINT) // '->' // trim(temp_str)
      write(*,*) 'The shortest path is:',temp_str

end program shortest_path