! ============================================================================
! Name        : condense_dot_matrix.f90
! Author      : Johnny Liang
! Version     : 0.1
! Description : Fortran程序设计作业题：使用九宫格方式压缩256x256的字节文件
! Solution   :  扩展图片为258*258的字节数组后，按取九宫格中的中间格方式压缩为86*86的字节文件
! ============================================================================

PROGRAM condense_dot_matrix
   USE, INTRINSIC :: iso_c_binding
   IMPLICIT NONE
! 本程序的常量定义
    ! 能处理的正方形图片的大小
    INTEGER, PARAMETER :: CHAR_SIZE = 1
    INTEGER, PARAMETER :: CONTENSE_GAP = 3
    INTEGER, PARAMETER :: MATRIX_SIZE = 256
    INTEGER, PARAMETER :: EXTEND_SIZE = 2
    INTEGER, PARAMETER :: COMPRESSED_SIZE = (MATRIX_SIZE + EXTEND_SIZE) / CONTENSE_GAP
    ! 处理文件的句柄
    INTEGER, PARAMETER :: INPUT_FILE_HANDLE = 9
    INTEGER, PARAMETER :: OUTPUT_FILE_HANDLE = 10

! 数据处理数组变量
    INTEGER(Kind=C_INT8_T), DIMENSION(MATRIX_SIZE+EXTEND_SIZE,MATRIX_SIZE+EXTEND_SIZE) :: extend_picture_matrix
    INTEGER(Kind=C_INT8_T), DIMENSION(COMPRESSED_SIZE,COMPRESSED_SIZE)  :: compress_matrix
   
! 文件处理的变量
   CHARACTER(len=80) :: picture_filename = '', result_filename = '', error_message = ''
   INTEGER :: file_status = 0

! 临时变量
   INTEGER :: i = 0, j = 0, start_pos = 0, end_pos = 0
   CHARACTER(LEN=80) :: arg = ''

! 步骤一： 获取数据文件和保持结果文件的名称
   IF (command_argument_count()>=2) THEN
      CALL get_command_argument(1,arg)
      picture_filename = TRIM(arg)
      CALL get_command_argument(2,arg)
      result_filename = TRIM(arg)
   ELSE
      WRITE (*,*) 'Run command with format: condense_dot_matrix <input_file_name> <output_file_name>'
      STOP
   END IF

! 步骤二：以stream方式打开图片文件, 读取256*256个字节数据到258*258字节数组中
   ! 2.1 打开文件
   OPEN (UNIT=INPUT_FILE_HANDLE, FILE=picture_filename, STATUS='OLD', ACCESS='STREAM', &
          & FORM='UNFORMATTED', IOSTAT=file_status, IOMSG=error_message)
   IF (file_status/=0) THEN     
      WRITE(*,'(A,A)') 'File open failed: ',error_message
      STOP
   END IF
   ! 2.2 从文件中获取256*256个字节数据到258*258字节数组中
   start_pos = 1+(EXTEND_SIZE/2)
   end_pos = MATRIX_SIZE + (EXTEND_SIZE/2)
   extend_picture_matrix = 0
   x_loop: DO i = start_pos, end_pos
      y_loop: DO j = start_pos, end_pos
         READ(INPUT_FILE_HANDLE, IOSTAT=file_status) extend_picture_matrix(i,j)
         IF (file_status/=0) THEN     
            STOP 'Read file to get point name information error, program stop!' 
         END IF   
      END DO y_loop
   END DO x_loop
   ! 2.3 关闭文件
   CLOSE(INPUT_FILE_HANDLE)

! 步骤三：把258x258字节数据按九宫格取中间格的方式压缩到86x86字节数组中
   compress_matrix = 0
   col_loop: DO i = 1,COMPRESSED_SIZE
      row_loop: DO j = 1,COMPRESSED_SIZE
         compress_matrix(i,j) = extend_picture_matrix(i*CONTENSE_GAP-1,j*CONTENSE_GAP-1)
      END DO row_loop
   END DO col_loop

! 步骤四：把86x86字节数据保存到stream文件
   ! 4.1 打开文件
   OPEN (UNIT=OUTPUT_FILE_HANDLE, FILE=result_filename, &
         & STATUS='REPLACE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=file_status, IOMSG=error_message)
   IF (file_status/=0) THEN     
      WRITE(*,'(A,A)') 'File open failed: ',error_message
      STOP
   END IF
   ! 4.2 写文件
   i_loop: DO i = 1,COMPRESSED_SIZE
      j_loop: DO j = 1,COMPRESSED_SIZE
         WRITE(OUTPUT_FILE_HANDLE,IOSTAT=file_status) compress_matrix(i,j)
         IF (file_status/=0) THEN     
            STOP 'Read file to get point name information error, program stop!' 
         END IF   
      END DO j_loop
   END DO i_loop
   ! 4.3 关闭文件
   CLOSE(OUTPUT_FILE_HANDLE)

! 步骤五：结束程序
   WRITE(*,*) '本程序正常结束，谢谢您的使用！'

END PROGRAM condense_dot_matrix