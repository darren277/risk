MODULE HTTPServer
  USE ISO_C_BINDING
  USE SocketHelpers
  USE ServerSocket
  USE Router          ! <- your module that handles `routeRequest(method, path, body, response)`
  IMPLICIT NONE

CONTAINS

  SUBROUTINE startServer(port)
    INTEGER, INTENT(IN) :: port
    INTEGER :: server_fd, client_fd, status
    LOGICAL :: running
    CHARACTER(LEN=1024), TARGET :: requestBuffer
    CHARACTER(LEN=1024), TARGET :: response
    INTEGER(C_INT)      :: bytesRead, bytesWritten
    TYPE(sockaddr_in), TARGET :: clientAddr
    INTEGER(C_INT), TARGET :: clientLen
    INTEGER(C_INT) :: ret

    ! 1) Create and bind server socket
    CALL createServerSocket(server_fd, port, status)
    IF (status /= 0) THEN
      WRITE(*,*) "Failed to start server."
      RETURN
    END IF

    running = .TRUE.
    WRITE(*, '(A,I0)') "Server listening on port ", port

    ! 2) Main accept loop
    DO WHILE (running)
      clientLen = SIZEOF(clientAddr)
      client_fd = c_accept(server_fd, C_LOC(clientAddr), C_LOC(clientLen))
      IF (client_fd < 0) THEN
        WRITE(*,*) "Error accepting connection."
        CYCLE
      END IF

      ! Clear buffer
      requestBuffer = ''
      
      ! 3) Read from client (very simplistic - real servers read until the full HTTP request is consumed)
      bytesRead = c_read(client_fd, C_LOC(requestBuffer), INT(LEN(requestBuffer), C_SIZE_T))
      IF (bytesRead <= 0) THEN
        WRITE(*,*) "Error or zero bytes read from client."
        ret = c_close(client_fd)
        CYCLE
      END IF

      ! Null-terminate the string for safety
      IF (bytesRead < LEN(requestBuffer)) requestBuffer(bytesRead+1:) = ''

      ! 4) Minimal parse of HTTP request (method, path, body)
      !    This is *highly* simplified. Real HTTP parsing is more complex.
      CALL parseAndRoute(requestBuffer, response)

      ! 5) Write response
      bytesWritten = c_write(client_fd, C_LOC(response), INT(LEN_TRIM(response), C_SIZE_T))

      ! 6) Close client connection
      ret = c_close(client_fd)
    END DO

    ! 7) If we exit the loop, close the server socket
    ret = c_close(server_fd)

  END SUBROUTINE startServer

  !-----------------------------------------------------------------------
  ! parseAndRoute: super-simplified helper to extract method, path, body,
  ! and call the router. In reality, you'd parse HTTP headers and more.
  !-----------------------------------------------------------------------
  SUBROUTINE parseAndRoute(requestBuffer, response)
    USE Router
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN)  :: requestBuffer
    CHARACTER(*), INTENT(OUT) :: response
    CHARACTER(LEN=8)  :: method
    CHARACTER(LEN=256) :: path
    CHARACTER(LEN=512) :: body
    INTEGER :: iMethodEnd, iPathEnd

    ! Dummy parse for demonstration
    ! For instance, a typical request line: "GET /loans HTTP/1.1"
    method = 'GET     '
    path   = '/       '
    body   = ''

    ! Call the router logic (from your Router module), e.g.:
    CALL routeRequest(TRIM(method), TRIM(path), TRIM(body), response)
  END SUBROUTINE parseAndRoute

END MODULE HTTPServer
