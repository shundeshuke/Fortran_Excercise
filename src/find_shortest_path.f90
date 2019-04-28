! ============================================================================
! Name        : find_shortest_path.f90
! Author      : Johnny Liang
! Version     : 0.5
! Description : Fortran程序设计作业题：求解无向图的最短路径
! Solution   :  把无向图转换为有向图，并使用Dijkstra算法求解
! ============================================================================

PROGRAM find_shortest_path
    IMPLICIT NONE
! 本程序的常量定义
    ! 本程序能处理的节点名称长度
    INTEGER, PARAMETER :: POINT_NAME_LENGTH = 1
    CHARACTER, PARAMETER :: POINT_NAME_LENGTH_C = '1'
   ! 本程序能处理的路径长度最大值
    INTEGER, PARAMETER :: FILE_HANDLE = 9

! Dijkstra算法使用的自定义链表数据类型和变量
    TYPE :: path
        REAL :: length
        INTEGER :: pre
    END TYPE path
    REAL, ALLOCATABLE, DIMENSION(:,:) :: graph_matrix
    TYPE(path), ALLOCATABLE, DIMENSION(:) :: short_paths
    INTEGER :: start_point_num
    INTEGER :: end_point_num
    REAL :: min_length

! 存放无向图连线信息的自定义数据类型和变量
    TYPE :: line
      CHARACTER(len=POINT_NAME_LENGTH) :: start_name
      CHARACTER(len=POINT_NAME_LENGTH) :: end_name
      real :: length
    END TYPE line 
   TYPE(line), ALLOCATABLE, DIMENSION(:) :: lines_array
   CHARACTER(len=POINT_NAME_LENGTH), ALLOCATABLE, DIMENSION(:) :: point_name_array,reverse_name_array
   INTEGER :: point_amount = 0 
   INTEGER :: line_amount = 0
   REAL :: max_length = 99999.0
   CHARACTER(len=POINT_NAME_LENGTH) :: start_point_name = ''
   CHARACTER(len=POINT_NAME_LENGTH) :: end_point_name = ''
   
! 文件和内存分配处理的变量
   CHARACTER(len=80) :: undirected_graph_info_filename = ''
   CHARACTER(len=80) :: error_message
   INTEGER :: file_status,allocate_status

! 临时变量
   INTEGER :: i,j,u
   INTEGER :: temp_s,temp_e,temp_p
   CHARACTER(LEN=80) :: arg
   CHARACTER(LEN=160) :: result_path_str

! 步骤一：从文件中获取无向图的总节点数、线段数，节点名称，线段信息，开始节点名称，结束节点名称等信息
   ! 1.1 获取数据文件名称
   IF (command_argument_count()>=1) THEN
      CALL get_command_argument(1,arg)
      undirected_graph_info_filename = TRIM(arg)
   ELSE
      WRITE (*,*) 'Enter the file name with undirected graph information: '
      READ (*,'(A)')  undirected_graph_info_filename
   END IF
   ! 1.2 打开数据文件
   OPEN (UNIT=FILE_HANDLE, FILE=undirected_graph_info_filename, STATUS='OLD', IOSTAT=file_status, IOMSG=error_message)
   IF (file_status/=0) THEN     
      WRITE(*,'(A,A)') 'File open failed: ',error_message
      STOP
   END IF
   ! 1.3 获取节点数量和线段数量，需要注意文本文件中的数组位置，1-3的值为point_amount, 4-6的值为line_amount
   READ(FILE_HANDLE,'(I3,I3,F12.4)',IOSTAT=file_status) point_amount, line_amount, max_length
   IF (file_status/=0) THEN     
      STOP 'Read file to get point_amount&line_amount&max_length error, program stop!' 
   END IF   
   IF (point_amount * line_amount == 0) THEN
      STOP 'Point number or line_amount is zero, program stop!'
   END IF

   ! 1.4 根据节点数和线段树，分配数组使用的存储空间
   ALLOCATE(point_name_array(point_amount),STAT=allocate_status)
   ALLOCATE(reverse_name_array(point_amount),STAT=allocate_status)
   ALLOCATE(lines_array(line_amount),STAT=allocate_status)
   ALLOCATE(short_paths(point_amount),STAT=allocate_status)
   ALLOCATE(graph_matrix(point_amount,point_amount),STAT=allocate_status)
   IF (allocate_status /=0) THEN
      STOP 'Memory allocation error, program stop!' 
   END IF
   ! 1.5 获取节点名称数据
   DO i = 1,point_amount
         READ(FILE_HANDLE,'(A'//POINT_NAME_LENGTH_C//')',IOSTAT=file_status) point_name_array(i)
         IF (file_status/=0) THEN     
            STOP 'Read file to get point name information error, program stop!' 
         END IF   
   END DO
   ! 1.6 获取线段数据
   DO i = 1,line_amount
      READ(FILE_HANDLE,'(A'//POINT_NAME_LENGTH_C//',A'//POINT_NAME_LENGTH_C//',F12.4)',IOSTAT=file_status) &
            &lines_array(i)%start_name,lines_array(i)%end_name,lines_array(i)%length
      IF (file_status/=0) THEN     
         STOP 'Read file to get line information error, program stop!' 
      END IF   
   END DO
   ! 1.7 获取起点名称和终点名称
   READ(FILE_HANDLE,'(A'//POINT_NAME_LENGTH_C//',A'//POINT_NAME_LENGTH_C//')',IOSTAT=file_status) &
         &start_point_name, end_point_name
   IF (file_status/=0) THEN     
      STOP 'Read file to get start/end point name error, program stop!' 
   END IF   
   ! 1.8 关闭文件
   CLOSE(FILE_HANDLE)

! 步骤二：转换无向图信息数据为适合Dijkstra算法的数据结构数据，对于有线段连接的两个节点，将设置为两条距离一样的有向边
   ! 2.1 若在点名称数组中找不到起点或终点，程序终止
   start_point_num = get_point_position(point_name_array,point_amount,start_point_name,POINT_NAME_LENGTH)
   end_point_num = get_point_position(point_name_array,point_amount,end_point_name,POINT_NAME_LENGTH)
   IF ((start_point_num * end_point_num) == 0) THEN
      STOP 'Point name is not found, program stop!'
   END IF
   ! 2.2 初始化有向图矩阵
   x_loop: DO i = 1, point_amount
      y_loop: DO j = 1, point_amount
         IF (i==j) THEN
            graph_matrix(i,j) = 0
         ELSE 
            graph_matrix(i,j) = max_length
         END IF
      END DO y_loop
   END DO x_loop
   ! 2.3 根据无向图的信息更新有向图矩阵
   DO i = 1, line_amount
      temp_s = get_point_position(point_name_array,point_amount,lines_array(i)%start_name,POINT_NAME_LENGTH)
      temp_e = get_point_position(point_name_array,point_amount,lines_array(i)%end_name,POINT_NAME_LENGTH)
      IF (temp_s*temp_e /= 0) THEN
         graph_matrix(temp_s,temp_e) = lines_array(i)%length
         graph_matrix(temp_e,temp_s) = lines_array(i)%length
      ENDIF
   END DO
    
! 步骤三：实现Dijkstra算法，计算出从start_point_name能达到的各个点的最短路径
   ! 3.1 初始化最短路径链表，把起点节点标示为已加入最短路径集合
   DO i = 1,point_amount
      short_paths(i)%length = graph_matrix(start_point_num,i)
      IF (short_paths(i)%length/=max_length) THEN
         short_paths(i)%pre = start_point_num
      END IF
      graph_matrix(start_point_num,start_point_num) = 1
   END DO

   ! 3.2 根据Dijkstra算法进行最短路径的计算和标示节点加入最短路径集合
   dij_main_loop: DO WHILE(.TRUE.) 
         min_length = max_length
         u = 0
         find_qualify_point_loop: DO i = 1,point_amount
         IF ((graph_matrix(i,i)==0) .AND. short_paths(i)%length < min_length) THEN 
            u = i
            min_length = short_paths(i)%length 
         END IF
      END DO find_qualify_point_loop
      IF (u==0) THEN
         EXIT
      END IF
      graph_matrix(u,u)=1
      upd_path_dis_loop: DO i = 1,point_amount
         IF (graph_matrix(i,i)==0 .AND. (short_paths(i)%length >  short_paths(u)%length + graph_matrix(u,i))) THEN 
            short_paths(i)%length = short_paths(u)%length + graph_matrix(u,i)
            short_paths(i)%pre = u
         END IF
      END DO upd_path_dis_loop
   END DO dij_main_loop
 
   !   DO i = 1,point_amount
   !      WRITE(*,'(I3,F8.4,I3)') i,short_paths(i)%length,short_paths(i)%pre
   !   END DO
   
! 步骤四：通过字符串的拼接，输出最短路径信息
   IF (short_paths(end_point_num)%pre /= 0) THEN
      WRITE(*,'(A,F12.4)') 'The shortest path lenght is ',short_paths(end_point_num)%length
      result_path_str = end_point_name
      temp_p = short_paths(end_point_num)%pre
      get_path_loop: DO WHILE((temp_p/=start_point_num) .AND. (temp_p/=0))
         result_path_str = point_name_array(temp_p) // '-->' // TRIM(result_path_str)
         temp_p = short_paths(temp_p)%pre
      END DO get_path_loop
      result_path_str = start_point_name // '-->' // TRIM(result_path_str)
      WRITE(*,'(A)') result_path_str
   END IF

! 步骤五：释放已经分配的内存空间
   DEALLOCATE(point_name_array,STAT=allocate_status)
   DEALLOCATE(reverse_name_array,STAT=allocate_status)
   DEALLOCATE(lines_array,STAT=allocate_status)
   DEALLOCATE(short_paths,STAT=allocate_status)
   DEALLOCATE(graph_matrix,STAT=allocate_status)

! 步骤六：结束程序
   STOP '本程序正常结束，谢谢您的使用！'

! 内部过程函数，若今后有需要可以改为外部过程函数
CONTAINS
   ! 根据节点名称获取节点在Dijkstra算法的二维数组中的位置   
   INTEGER PURE FUNCTION get_point_position(point_name_array_f,point_array_size_f,point_name_f,point_name_length_s)
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: point_array_size_f
      INTEGER,INTENT(IN) :: point_name_length_s
      CHARACTER(len=point_name_length_s), DIMENSION(point_array_size_f),INTENT(IN) :: point_name_array_f
      CHARACTER(len=point_name_length_s), INTENT(IN) :: point_name_f
      INTEGER :: i

      get_point_position = 0
      DO i = 1, point_array_size_f
         IF (point_name_array_f(i)==TRIM(point_name_f)) THEN
            get_point_position=i
            EXIT
         END IF  
      END DO
   END FUNCTION get_point_position

END PROGRAM find_shortest_path