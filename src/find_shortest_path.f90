! ============================================================================
! Name        : find_shortest_path.f90
! Author      : Johnny Liang
! Version     : 0.1
! Description : SYSU理论力学专业Fortran程序设计课程的2019年五一作业题：求解无向图的最短路径
! Solution   :  把无向图转换为有向图，并使用Dijkstra算法求解
! ============================================================================

PROGRAM find_shortest_path
    IMPLICIT NONE
 ! 本程序能处理的最大节点数
    INTEGER, PARAMETER :: MAX_POINT_SIZE = 36
 ! 本程序能处理的节点名称长度
    INTEGER, PARAMETER :: POINT_NAME_LENGTH = 1
! 本程序能处理的路径长度最大值
    REAL, PARAMETER :: MAX_LENGTH = 99999.0
 ! Dijkstra算法使用的自定义链表数据类型
    TYPE :: path
        REAL :: length
        INTEGER :: pre
    END TYPE path
 ! Dijkstra算法使用的变量
    REAL, DIMENSION(MAX_POINT_SIZE,MAX_POINT_SIZE) :: graph_matrix
    TYPE(path), DIMENSION(MAX_POINT_SIZE) :: short_paths
    INTEGER :: start_point_num
    INTEGER :: end_point_num
    REAL :: min_length

! 存放无向图连线信息的自定义数据类型
    TYPE :: line
      CHARACTER(len=POINT_NAME_LENGTH) :: start_name
      CHARACTER(len=POINT_NAME_LENGTH) :: end_name
      real :: length
    END TYPE line 
! 存放无向图文本信息的变量
   CHARACTER(len=POINT_NAME_LENGTH), DIMENSION(MAX_POINT_SIZE) :: point_name_array,reverse_name_array
   INTEGER :: point_amount = 0 
   INTEGER :: line_amount = 0
   CHARACTER(len=POINT_NAME_LENGTH) :: start_point_name = ''
   CHARACTER(len=POINT_NAME_LENGTH) :: end_point_name = ''
   TYPE(line), DIMENSION(MAX_POINT_SIZE) :: lines_array
    
! 文件处理的变量
   CHARACTER(len=50) :: undirected_graph_info_filename = ''
   CHARACTER(len=80) :: error_message
   INTEGER :: file_status
! 临时变量
   INTEGER :: k,i,j,u
   INTEGER :: temp_s,temp_e,temp_p,short_path_pos
 
  ! 从文件中获取无向图的总节点数、线段数，节点名称，线段信息，开始节点名称，结束节点名称等信息
   WRITE (*,*) 'Enter the file name with undirected graph information: '
   READ (*,'(A50)')  undirected_graph_info_filename

   OPEN (UNIT=9, FILE=undirected_graph_info_filename, STATUS='OLD', IOSTAT=status, IOMSG=error_message)
   IF (file_status/=0) THEN     
      WRITE(*,'(A,A)') 'File open failed: ',error_message
      STOP
   END IF

   READ(9,'(I3,I3)',IOSTAT=file_status) point_amount, line_amount
   IF (file_status/=0) THEN     
      STOP 'Read file error.' 
   END IF   
   IF (point_amount * line_amount == 0) THEN
      STOP 'Point number or line_amount is zero, program stop!'
   END IF
   IF (point_amount>MAX_POINT_SIZE) THEN
      STOP 'Point number is above max size!'
   END IF

   DO i = 1,point_amount
         READ(9,'(A)',IOSTAT=status) point_name_array(i)
         IF (file_status/=0) THEN     
            STOP 'Read file error.' 
         END IF   
   END DO

   DO i = 1,line_amount
         READ(9,'(A,A,F10.4)',IOSTAT=status) lines_array(i)%start_name,lines_array(i)%end_name,lines_array(i)%length
         IF (file_status/=0) THEN     
            STOP 'Read file error.' 
         END IF   
   END DO

   READ(9,'(A,A)',IOSTAT=status) start_point_name, end_point_name
   IF (file_status/=0) THEN     
      STOP 'Read file error.' 
   END IF   

 ! 转换无向图信息数据为适合Dijkstra算法的数据结构数据，对于有线段连接的两个节点，将设置为两条距离一样的有向边
 ! 若在点名称数组中找不到起点或终点，程序终止
   start_point_num = get_point_position(point_name_array,point_amount,start_point_name,POINT_NAME_LENGTH)
   end_point_num = get_point_position(point_name_array,point_amount,end_point_name,POINT_NAME_LENGTH)
   IF ((start_point_num * end_point_num) == 0) THEN
      STOP 'Point name is not found, program stop!'
   END IF

 ! 初始化有向图矩阵
   DO i = 1, point_amount
      DO j = 1, point_amount
         IF (i==j) THEN
            graph_matrix(i,j) = 0
         ELSE 
            graph_matrix(i,j) = MAX_LENGTH
         END IF
      END DO
   END DO
! 根据无向图的信息更新有向图矩阵
   DO i = 1, line_amount
      temp_s = get_point_position(point_name_array,point_amount,lines_array(i)%start_name,POINT_NAME_LENGTH)
      temp_e = get_point_position(point_name_array,point_amount,lines_array(i)%end_name,POINT_NAME_LENGTH)
      IF (temp_s*temp_e /= 0) THEN
         graph_matrix(temp_s,temp_e) = lines_array(i)%length
         graph_matrix(temp_e,temp_s) = lines_array(i)%length
      ENDIF
   END DO

    
 ! 实现Dijkstra算法，计算出从start_point_name能达到的各个点的最短路径
   DO i = 1,point_amount
      short_paths(i)%length = graph_matrix(start_point_num,i)
      IF (short_paths(i)%length/=MAX_LENGTH) THEN
         short_paths(i)%pre = start_point_num
      END IF
      graph_matrix(start_point_num,start_point_num) = 1
   END DO

   DO WHILE(.TRUE.) 
      min_length = MAX_LENGTH
      u = 0
      DO i = 1,point_amount
         IF (graph_matrix(i,i)==0 .AND short_paths(i)%length < min_length) THEN 
            u = i
            min_length = short_paths(i)%length 
         END IF
      END DO
      IF (u==0) THEN
         EXIT
      END IF
      graph_matrix(u,u)=1
      DO i = 1,point_amount
         IF (graph_matrix(i,i)==0 .AND. (short_paths(i)%length >  short_paths(u)%length + graph_matrix(u,i))) THEN 
            short_paths(i)%length = short_paths(u)%length + graph_matrix(u,i)
            short_paths(i)%pre = u
         END IF
      END DO
   END DO
 
 ! 通过中间数组进行倒序处理之后，输出最短路径信息
   IF (short_paths(end_point_num)%pre /= 0) THEN
      WRITE(*,'(A,F10.4)') 'The shortest path lenght is ',short_paths(end_point_num)%length
      short_path_pos = 1
      temp_p = end_point_num
      DO WHILE((temp_p/=start_point_num) .AND. (temp_p==0))
         reverse_name_array(short_path_pos) = point_name_array(temp_p)
         temp_p = short_paths(temp_p)%pre
         short_path_pos = short_path_pos + 1
      END DO
      reverse_name_array(short_path_pos) = point_name_array(start_point_num)

      DO i = short_path_pos,2
         WRITE(*,'(A,A)') reverse_name_array(i),'-->'
      END DO
      WRITE(*,'(A)') reverse_name_array(1)  !终点名称
   END IF

CONTAINS

INTEGER PURE FUNCTION get_point_position(point_name_array_f,point_array_size_f,point_name_f,point_name_length_s)
   IMPLICIT NONE
   INTEGER,INTENT(IN) :: point_array_size_f
   INTEGER,INTENT(IN) :: point_name_length_s
   CHARACTER(len=point_name_length_s), DIMENSION(point_array_size_f),INTENT(IN) :: point_name_array_f
   CHARACTER(len=POINT_NAME_LENGTH), INTENT(IN) :: point_name_f
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