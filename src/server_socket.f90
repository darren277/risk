MODULE ServerSocket
  USE ISO_C_BINDING
  USE SocketHelpers
  IMPLICIT NONE

  TYPE :: sockaddr_in
    SEQUENCE
    INTEGER(C_SHORT)  :: sin_family
    INTEGER(C_SHORT) :: sin_port
    INTEGER(C_INT)    :: sin_addr
    CHARACTER(LEN=8)  :: sin_zero  ! padding to match the C struct
  END TYPE sockaddr_in

CONTAINS

  SUBROUTINE createServerSocket(server_fd, port, status)
    INTEGER, INTENT(OUT) :: server_fd
    INTEGER, INTENT(IN)  :: port
    INTEGER, INTENT(OUT) :: status

    TYPE(sockaddr_in), TARGET :: addr
    INTEGER(C_INT)    :: ret
    INTEGER(C_INT)    :: yes

    ! 1) Create the socket
    server_fd = c_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    IF (server_fd < 0) THEN
      WRITE(*,*) "Error creating socket"
      status = -1
      RETURN
    END IF

    ! 2) Prepare the sockaddr_in struct
    addr%sin_family = AF_INET
    ! Convert port to network byte order
    addr%sin_port = transfer(hton16(INT(port, C_SHORT)), 0_C_SHORT)
    ! For 0.0.0.0, sin_addr = 0
    addr%sin_addr   = 0_C_INT
    addr%sin_zero   = CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)

    ! 3) Bind
    ret = c_bind(server_fd, C_LOC(addr), INT(SIZEOF(addr), C_SIZE_T))
    IF (ret /= 0) THEN
      WRITE(*,*) "Error binding socket to port ", port
      ret = c_close(server_fd)
      status = -2
      RETURN
    END IF

    ! 4) Listen
    ret = c_listen(server_fd, 10_C_INT)
    IF (ret /= 0) THEN
      WRITE(*,*) "Error listening on socket."
      ret = c_close(server_fd)
      status = -3
      RETURN
    END IF

    ! If we reached here, everything is OK
    status = 0
  END SUBROUTINE createServerSocket

  !-----------------------------------------------
  ! 16-bit host to network byte order (simplified)
  !-----------------------------------------------
  INTEGER(C_SHORT) FUNCTION hton16(hostval)
    INTEGER(C_SHORT), INTENT(IN) :: hostval
    hton16 = IOR(ISHFT(hostval, -8), IAND(hostval, INT(Z'00FF', C_SHORT)) * INT(256, C_SHORT))
  END FUNCTION hton16

END MODULE ServerSocket
