MODULE SocketHelpers
  USE ISO_C_BINDING
  IMPLICIT NONE

  ! Constants for AF_INET, SOCK_STREAM, etc.
  INTEGER, PARAMETER :: AF_INET   = 2
  INTEGER, PARAMETER :: SOCK_STREAM = 1
  INTEGER, PARAMETER :: IPPROTO_TCP = 6

  ! Flags for send/recv
  INTEGER, PARAMETER :: MSG_NOSIGNAL = 0  ! Not always defined on all OS's

  INTERFACE
    FUNCTION c_socket(domain, socket_type, protocol) BIND(C, NAME="socket")
      IMPORT :: C_INT
      INTEGER(C_INT) :: c_socket
      INTEGER(C_INT), VALUE :: domain, socket_type, protocol
    END FUNCTION c_socket

    FUNCTION c_bind(sockfd, addr, addrlen) BIND(C, NAME="bind")
      IMPORT :: C_INT, C_SIZE_T, C_PTR
      INTEGER(C_INT) :: c_bind
      INTEGER(C_INT), VALUE :: sockfd
      TYPE(C_PTR), VALUE :: addr
      INTEGER(C_SIZE_T), VALUE :: addrlen
    END FUNCTION c_bind

    FUNCTION c_listen(sockfd, backlog) BIND(C, NAME="listen")
      IMPORT :: C_INT
      INTEGER(C_INT) :: c_listen
      INTEGER(C_INT), VALUE :: sockfd, backlog
    END FUNCTION c_listen

    FUNCTION c_accept(sockfd, addr, addrlen) BIND(C, NAME="accept")
      IMPORT :: C_INT, C_PTR, C_SIZE_T
      INTEGER(C_INT) :: c_accept
      INTEGER(C_INT), VALUE :: sockfd
      TYPE(C_PTR), VALUE :: addr
      TYPE(C_PTR), VALUE :: addrlen
    END FUNCTION c_accept

    FUNCTION c_read(fd, buf, count) BIND(C, NAME="read")
      IMPORT :: C_INT, C_PTR, C_SIZE_T
      INTEGER(C_INT) :: c_read
      INTEGER(C_INT), VALUE :: fd
      TYPE(C_PTR), VALUE :: buf
      INTEGER(C_SIZE_T), VALUE :: count
    END FUNCTION c_read

    FUNCTION c_write(fd, buf, count) BIND(C, NAME="write")
      IMPORT :: C_INT, C_PTR, C_SIZE_T
      INTEGER(C_INT) :: c_write
      INTEGER(C_INT), VALUE :: fd
      TYPE(C_PTR), VALUE :: buf
      INTEGER(C_SIZE_T), VALUE :: count
    END FUNCTION c_write

    FUNCTION c_close(fd) BIND(C, NAME="close")
      IMPORT :: C_INT
      INTEGER(C_INT) :: c_close
      INTEGER(C_INT), VALUE :: fd
    END FUNCTION c_close
  END INTERFACE

CONTAINS

  ! Helper functions to handle struct sockaddr_in in Fortran
  ! Typically, you'd define a derived type matching the C struct,
  ! plus a function to set the port, IP, etc.
  
END MODULE SocketHelpers
