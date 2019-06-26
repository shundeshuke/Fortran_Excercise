! ============================================================================
! Name        : condense_dot_matrix_2.f90
! Author      : Johnny Liang
! Version     : 0.1
! Description : Fortran程序设计作业题：使用九宫格方式压缩256x256的字节文件
! Solution   :  扩展图片为258*258的字节数组后，按取九宫格中的中间格方式压缩为86*86的字节文件
! ============================================================================

PROGRAM condense_dot_matrix_2
   USE, INTRINSIC :: iso_c_binding
   IMPLICIT NONE
! 本程序的常量定义
    ! 能处理的正方形图片的大小
    INTEGER, PARAMETER :: CONTENSE_GAP = 3
    INTEGER, PARAMETER :: MATRIX_SIZE = 256
    INTEGER, PARAMETER :: EXTEND_SIZE = 2
    INTEGER, PARAMETER :: COMPRESSED_SIZE = (MATRIX_SIZE + EXTEND_SIZE) / CONTENSE_GAP
    ! 处理文件的句柄
    INTEGER, PARAMETER :: INPUT_FILE_HANDLE = 9
    INTEGER, PARAMETER :: OUTPUT_FILE_HANDLE = 10
   ! 数据处理数组变量
    INTEGER(Kind=C_INT8_T), DIMENSION(MATRIX_SIZE,MATRIX_SIZE) :: picture_matrix
    INTEGER(Kind=C_INT8_T), DIMENSION(COMPRESSED_SIZE,COMPRESSED_SIZE)  :: compress_matrix
   ! 中间变量
   CHARACTER(LEN=80) :: picture_filename = '', result_filename = '', error_message = '',  arg = ''
   INTEGER :: file_status = 0, i = 0, j = 0

! 步骤一： 获取数据文件和保持结果文件的名称
   IF (command_argument_count()>=2) THEN
      CALL get_command_argument(1,arg)
      picture_filename = TRIM(arg)
      CALL get_command_argument(2,arg)
      result_filename = TRIM(arg)
   ELSE
      WRITE (*,*) 'Run command with format: condense_dot_matrix_2 <input_file_name> <output_file_name>'
      STOP
   END IF

! 步骤二：以stream方式打开图片文件, 读取256*256个字节数据到字节数组中
   ! 2.1 打开文件
   OPEN (UNIT=INPUT_FILE_HANDLE, FILE=picture_filename, STATUS='OLD', ACCESS='STREAM', &
          & FORM='UNFORMATTED', IOSTAT=file_status, IOMSG=error_message)
   READ(INPUT_FILE_HANDLE, IOSTAT=file_status) picture_matrix 
   CLOSE(INPUT_FILE_HANDLE)

! 步骤三：把258x258字节数据按九宫格取中间格的方式压缩到86x86字节数组中
   col_loop: DO i = 1,COMPRESSED_SIZE
      row_loop: DO j = 1,COMPRESSED_SIZE
         compress_matrix(i,j) = picture_matrix(i*CONTENSE_GAP-EXTEND_SIZE,j*CONTENSE_GAP-EXTEND_SIZE)
      END DO row_loop
   END DO col_loop

! 步骤四：把86x86字节数据保存到stream文件
   OPEN (UNIT=OUTPUT_FILE_HANDLE, FILE=result_filename, &
         & STATUS='REPLACE', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=file_status, IOMSG=error_message)
   WRITE(OUTPUT_FILE_HANDLE,IOSTAT=file_status) compress_matrix
   CLOSE(OUTPUT_FILE_HANDLE)

! 步骤五：结束程序
   WRITE(*,*) '本程序正常结束，谢谢您的使用！'

END PROGRAM condense_dot_matrix_2