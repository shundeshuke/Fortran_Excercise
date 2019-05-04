
PROGRAM shortest_path_jl
    IMPLICIT NONE
! Dijkstra算法使用的自定义链表数据类型和变量
    REAL, PARAMETER :: MAX = 99999.0
    INTEGER, PARAMETER :: POINT_AMT = 10 
    TYPE :: path
        REAL :: length
        INTEGER :: pre
    END TYPE path
    TYPE(path), DIMENSION(POINT_AMT) :: dist
    CHARACTER(len=1), DIMENSION(POINT_AMT) :: point_name_array =['A','B','C','D','E','F','G','H','I','J']
    INTEGER :: start_point_num = 7,end_point_num = 10
    REAL, DIMENSION(POINT_AMT,POINT_AMT) :: adj 
! 中间变量
   REAL :: short_length
   INTEGER :: i,u,temp_p
   CHARACTER(LEN=160) :: result_path_str

! 初始化有向图的值
   adj = transpose(reshape((/ 0.0,2.0,MAX,2.0,MAX,MAX,MAX,MAX,MAX,MAX,&
   &2.0,0.0,2.0,2.828,2.0,2.828,MAX,MAX,MAX,MAX,&
   &MAX,2.0,0.0,MAX,MAX,MAX,MAX,MAX,MAX,1.0,&
   &2.0,2.828,MAX,0.0,2.0,MAX,2.0,2.828,MAX,MAX,&
   &MAX,2.0,MAX,2.0,0.0,2.0,MAX,2.0,MAX,MAX,&
   &MAX,MAX,MAX,MAX,2.0,0.0,MAX,MAX,MAX,1.0,&
   &MAX,MAX,MAX,2.0,MAX,MAX,0.0,2.0,MAX,MAX,&
   &MAX,MAX,MAX,2.828,2.0,2.828,2.0,0.0,2.0,MAX,&
   &MAX,MAX,MAX,MAX,MAX,2.0,MAX,2.0,0.0,MAX,&
   &MAX,MAX,1.0,MAX,MAX,1.0,MAX,MAX,MAX,0.0 /),shape(adj)))
 
   !
   !transpose(reshape((/&
   !&0.0,2.0,MAX,9.0,2.828,MAX,MAX,MAX,MAX,MAX,&
   !&2.0,0.0,2.0,MAX,2.0,MAX,MAX,MAX,MAX,MAX,&
   !&MAX,2.0,0.0,MAX,2.828,MAX,MAX,MAX,MAX,1.0,&
   !&2.0,MAX,MAX,0.0,2.0,MAX,2.0,MAX,MAX,MAX,&
   !&2.828,2.0,2.828,2.0,0.0,2.0,2.828,2.0,2.828,MAX,&
   !&MAX,MAX,MAX,MAX,2.0,0.0,MAX,MAX,2.0,1.0,&
   !&MAX,MAX,MAX,2.0,2.828,MAX,0.0,2.0,MAX,MAX,&
   !&MAX,MAX,MAX,MAX,2.0,MAX,2.0,0.0,2.0,MAX,&
   !&MAX,MAX,MAX,MAX,2.828,2.0,MAX,2.0,0.0,MAX,&
   !&MAX,MAX,1.0,MAX,MAX,1.0,MAX,MAX,MAX,0.0/),&
   !&shape(adj)))

! 步骤二：实现Dijkstra算法，计算出从start_point_name能达到的各个点的最短路径
   ! 2.1 初始化最短路径链表，把起点节点标示为已加入最短路径集合
   DO i = 1,POINT_AMT
      dist(i)%length = adj(start_point_num,i)
      IF (dist(i)%length/=MAX) THEN
         dist(i)%pre = start_point_num
      END IF
      adj(start_point_num,start_point_num) = 1
   END DO
   ! 2.2 根据Dijkstra算法进行最短路径的计算和标示节点加入最短路径集合
   dij_main_loop: DO WHILE(.TRUE.) 
         short_length = MAX
         u = 0
         find_qualify_point_loop: DO i = 1,POINT_AMT
         IF ((adj(i,i)==0) .AND. dist(i)%length < short_length) THEN 
            u = i
            short_length = dist(i)%length 
         END IF
      END DO find_qualify_point_loop
      IF (u==0) THEN
         EXIT
      END IF
      adj(u,u)=1
      upd_path_dis_loop: DO i = 1,POINT_AMT
         IF (adj(i,i)==0 .AND. (dist(i)%length >  dist(u)%length + adj(u,i))) THEN 
            dist(i)%length = dist(u)%length + adj(u,i)
            dist(i)%pre = u
         END IF
      END DO upd_path_dis_loop
   END DO dij_main_loop

! 步骤三：通过字符串的拼接，输出最短路径信息
   IF (dist(end_point_num)%pre /= 0) THEN
      result_path_str = point_name_array(end_point_num)
      temp_p = dist(end_point_num)%pre
      get_path_loop: DO WHILE((temp_p/=start_point_num) .AND. (temp_p/=0))
         result_path_str = point_name_array(temp_p) // '-->' // TRIM(result_path_str)
         temp_p = dist(temp_p)%pre
      END DO get_path_loop
      result_path_str = point_name_array(start_point_num) // '-->' // TRIM(result_path_str)
      WRITE(*,'(A22,A)') 'The shortest path is: ',result_path_str
      WRITE(*,'(A,F12.4)') 'The shortest path lenght is ',dist(end_point_num)%length
   END IF

END PROGRAM shortest_path_jl