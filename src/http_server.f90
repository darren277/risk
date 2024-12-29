MODULE HTTPServer
  USE ISO_C_BINDING
  USE SocketHelpers
  USE ServerSocket
  USE Router          ! <- Your module that handles `routeRequest(method, path, body, response)`
  IMPLICIT NONE

CONTAINS

  SUBROUTINE safeReplace(input, pattern, replacement, output)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: input, pattern, replacement
    CHARACTER(*), INTENT(OUT) :: output
    INTEGER :: i, j, pLen, rLen
    
    pLen = LEN_TRIM(pattern)
    rLen = LEN_TRIM(replacement)
    j = 1
    i = 1
    
    DO WHILE (i <= LEN_TRIM(input))
        IF (i + pLen - 1 <= LEN_TRIM(input)) THEN
            IF (input(i:i+pLen-1) == pattern) THEN
                IF (j + rLen - 1 <= LEN(output)) THEN
                    output(j:j+rLen-1) = replacement
                    j = j + rLen
                    i = i + pLen
                    CYCLE
                END IF
            END IF
        END IF
        
        IF (j <= LEN(output)) THEN
            output(j:j) = input(i:i)
            j = j + 1
        END IF
        i = i + 1
    END DO
    
    IF (j <= LEN(output)) output(j:) = ' '
  END SUBROUTINE safeReplace

  SUBROUTINE safeParseRequestLine(line, method, path, version)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: line
    CHARACTER(*), INTENT(OUT) :: method, path, version
    INTEGER :: pos1, pos2
    
    method = 'UNKNOWN'
    path = '/'
    version = ''
    
    pos1 = INDEX(line, ' ')
    IF (pos1 <= 1 .OR. pos1 >= LEN_TRIM(line)) RETURN
    
    pos2 = INDEX(line(pos1+1:), ' ')
    IF (pos2 <= 0) RETURN
    pos2 = pos2 + pos1
    
    IF (pos1 <= LEN(method)) method = line(1:pos1-1)
    IF (pos2-pos1-1 <= LEN(path)) path = line(pos1+1:pos2-1)
    IF (LEN_TRIM(line) - pos2 <= LEN(version)) version = line(pos2+1:)
  END SUBROUTINE safeParseRequestLine

  SUBROUTINE safeExtractHeaderValue(headers, name, value)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: headers, name
    CHARACTER(*), INTENT(OUT) :: value
    INTEGER :: start, end, valueStart
    CHARACTER(LEN=:), ALLOCATABLE :: headerLine
    
    value = ''
    headerLine = name // ':'
    
    start = INDEX(headers, headerLine)
    IF (start <= 0) RETURN
    
    end = INDEX(headers(start:), NEW_LINE('A'))
    IF (end <= 0) RETURN
    end = start + end - 1
    
    valueStart = start + LEN_TRIM(headerLine)
    IF (valueStart >= end) RETURN
    
    value = ADJUSTL(headers(valueStart:end-1))
  END SUBROUTINE safeExtractHeaderValue

  SUBROUTINE extractHeaderValue(headers, key, value)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN)  :: headers
    CHARACTER(*), INTENT(IN)  :: key
    CHARACTER(*), INTENT(OUT) :: value
    INTEGER :: start, keyLength, end

    keyLength = LEN_TRIM(key)

    ! Locate the key in the headers
    start = INDEX(headers, key // ":")
    IF (start > 0) THEN
      start = start + keyLength + 1  ! Move past "key:"
      end = INDEX(headers(start:), NEW_LINE('A'))
      IF (end == 0) end = LEN_TRIM(headers) + 1 - start
      value = TRIM(headers(start:start + end - 2))
    ELSE
      value = ""
    END IF
  END SUBROUTINE extractHeaderValue

  SUBROUTINE replace(input, target, replacement, output)
    CHARACTER(*), INTENT(IN)  :: input       ! The input string
    CHARACTER(*), INTENT(IN)  :: target      ! The substring to replace
    CHARACTER(*), INTENT(IN)  :: replacement ! The replacement substring
    !CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: output
    CHARACTER(*), INTENT(OUT) :: output      ! The output string (fixed-length)

    INTEGER :: targetLen, replacementLen, inputLen, idx, startPos
    !CHARACTER(LEN=:), ALLOCATABLE :: temp
    CHARACTER(LEN=LEN(input)) :: temp

    inputLen = LEN_TRIM(input)
    targetLen = LEN_TRIM(target)
    replacementLen = LEN_TRIM(replacement)

    ! Start with the input string
    temp = input
    startPos = 1

    DO
      idx = INDEX(temp(startPos:), target)
      IF (idx == 0) EXIT

      idx = idx + startPos - 1  ! Adjust to the full string index
      temp = temp(1:idx-1) // replacement // temp(idx+targetLen:)
      startPos = idx + replacementLen
    END DO

    output = TRIM(temp)
  END SUBROUTINE replace

  SUBROUTINE parseRequestLine(requestLine, method, path, httpVersion)
    CHARACTER(*), INTENT(IN) :: requestLine
    CHARACTER(*), INTENT(OUT) :: method, path, httpVersion

    INTEGER :: start, end, ios

    ! Initialize
    method = 'UNKNOWN'
    path = '/'
    httpVersion = ''

    ! Parse method
    start = 1
    end = INDEX(requestLine, ' ')
    IF (end > 0) THEN
      method = TRIM(ADJUSTL(requestLine(start:end-1)))
      start = end + 1
    ELSE
      RETURN
    END IF

    ! Parse path
    end = INDEX(requestLine(start:), ' ')
    IF (end > 0) THEN
      path = TRIM(ADJUSTL(requestLine(start:start+end-2)))
      start = start + end - 1
    ELSE
      path = TRIM(ADJUSTL(requestLine(start:)))
      RETURN
    END IF

    ! Parse HTTP version
    httpVersion = TRIM(ADJUSTL(requestLine(start:)))
  END SUBROUTINE parseRequestLine

  !-----------------------------------------------------------------------
  ! sendHttpResponse:
  ! Constructs a minimal HTTP/1.1 response with JSON content.
  !-----------------------------------------------------------------------
  SUBROUTINE sendHttpResponse(client_fd, status, content)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(C_INT), INTENT(IN)       :: client_fd
    CHARACTER(LEN=*), INTENT(IN)     :: status    ! e.g. "200 OK"
    CHARACTER(LEN=*), INTENT(IN)     :: content   ! JSON or other body

    CHARACTER(LEN=1024), TARGET :: response
    CHARACTER(LEN=64)   :: contentLengthStr
    INTEGER(C_INT)      :: contentLength, bytesWritten

    ! 1) Calculate the content length (body size).
    contentLength = LEN_TRIM(content)
    WRITE(contentLengthStr, '(I0)') contentLength  ! Convert length to string

    ! 2) Construct a minimal HTTP response:
    !    - Status line: "HTTP/1.1 200 OK"
    !    - One header:  "Content-Type: application/json"
    !    - Another:     "Content-Length: <size>"
    !    - Blank line
    !    - Body
    !
    ! Notice we use CR-LF or just newline (depending on OS).
    ! Here, for simplicity, we use newline (NEW_LINE('A')).

    response =  "HTTP/1.1 " // TRIM(status)                       // NEW_LINE('A') // &
                "Content-Type: application/json"                  // NEW_LINE('A') // &
                "Content-Length: " // TRIM(contentLengthStr)       // NEW_LINE('A') // &
                NEW_LINE('A') // &
                TRIM(content)

    ! 3) Send the response to the client socket.
    bytesWritten = c_write(client_fd, C_LOC(response), INT(LEN_TRIM(response), C_SIZE_T))
    IF (bytesWritten < 0) THEN
       WRITE(*, *) "Error sending response to client."
    END IF
  END SUBROUTINE sendHttpResponse

  !-----------------------------------------------------------------------
  ! startServer:
  ! Creates/binds a socket, listens for connections, reads requests, and
  ! sends responses using sendHttpResponse.
  !-----------------------------------------------------------------------
  SUBROUTINE startServer(port)
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: port
    INTEGER(C_INT)      :: server_fd, client_fd
    INTEGER(C_INT)      :: status, ret
    LOGICAL             :: running
    CHARACTER(LEN=1024), TARGET :: requestBuffer
    CHARACTER(LEN=1024), TARGET :: response
    INTEGER(C_INT)      :: bytesRead
    TYPE(sockaddr_in),  TARGET :: clientAddr
    INTEGER(C_INT),     TARGET :: clientLen

    ! 1) Create and bind the server socket
    CALL createServerSocket(server_fd, port, status)
    IF (status /= 0) THEN
      WRITE(*, *) "Failed to start server."
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

      ! 3) Read from client
      bytesRead = c_read(client_fd, C_LOC(requestBuffer), INT(LEN(requestBuffer), C_SIZE_T))
      IF (bytesRead <= 0) THEN
        WRITE(*,*) "Error or zero bytes read from client."
        ret = c_close(client_fd)
        CYCLE
      END IF

      ! Null-terminate the string (if there's room)
      IF (bytesRead < LEN(requestBuffer)) requestBuffer(bytesRead+1:) = ''

      ! 4) Minimal parse (method, path, body) -> get response body
      CALL parseAndRoute(requestBuffer, response)

      ! 5) Send a proper HTTP response (200 OK for now)
      CALL sendHttpResponse(client_fd, "200 OK", TRIM(response))

      ! 6) Close client connection
      ret = c_close(client_fd)
    END DO

    ! 7) If we exit the loop, close the server socket
    ret = c_close(server_fd)

  END SUBROUTINE startServer

  SUBROUTINE parseAndRoute(requestBuffer, response)
    USE Router
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)  :: requestBuffer
    CHARACTER(*), INTENT(OUT) :: response
    CHARACTER(LEN=1024) :: normalizedBuffer

    CHARACTER(LEN=8)  :: method
    CHARACTER(LEN=256) :: path
    CHARACTER(LEN=16)  :: httpVersion  ! e.g. "HTTP/1.1"
    CHARACTER(LEN=:), ALLOCATABLE :: rawBody
    CHARACTER(LEN=512) :: body
    CHARACTER(LEN=1024) :: requestLine
    INTEGER :: iLineEnd, iBlankLine, ios

    ! Initialize variables
    method = 'UNKNOWN'
    path = '/'
    httpVersion = ''
    body = ''
    ALLOCATE(character(LEN=0) :: rawBody)

    !----------------------------------------------------------------------------
    ! Normalize line endings to handle CRLF (`\r\n`) and LF (`\n`) consistently
    !----------------------------------------------------------------------------
    CALL replace(requestBuffer, CHAR(13)//NEW_LINE('A'), NEW_LINE('A'), normalizedBuffer)

    ! Log normalized request buffer
    WRITE(*, '(A)') "[DEBUG] Raw Request Buffer (After Normalization):"
    WRITE(*, '(A)') TRIM(normalizedBuffer)

    !----------------------------------------------------------------------------
    ! 1) Find the first newline to isolate the request line, e.g. "POST /loans HTTP/1.1"
    !----------------------------------------------------------------------------
    iLineEnd = INDEX(normalizedBuffer, NEW_LINE('A'))
    IF (iLineEnd <= 0) THEN
      ! No newline at all => invalid or incomplete request
      WRITE(*, '(A)') "[DEBUG] Invalid or incomplete request received:"
      WRITE(*, '(A)') TRIM(normalizedBuffer)
      RETURN
    ELSE
      ! Extract the request line (up to but excluding the newline)
      requestLine = TRIM(normalizedBuffer(1:iLineEnd-1))

      ! Parse method, path, and HTTP version
      CALL parseRequestLine(requestLine, method, path, httpVersion)
    END IF

    ! Log the parsed request line
    WRITE(*, '(A)') "[DEBUG] Request Line Parsed:"
    WRITE(*, '(A)') "  Method: " // TRIM(method)
    WRITE(*, '(A)') "  Path: " // TRIM(path)
    WRITE(*, '(A)') "  HTTP Version: " // TRIM(httpVersion)

    !----------------------------------------------------------------------------
    ! 2) Find the blank line that signals the end of headers
    !----------------------------------------------------------------------------
    iBlankLine = INDEX(normalizedBuffer(iLineEnd+1:), NEW_LINE('A')//NEW_LINE('A'))
    IF (iBlankLine > 0) THEN
      iBlankLine = iBlankLine + iLineEnd  ! Adjust position relative to the full string

      ! Check if body exists and is within bounds
      IF (ALLOCATED(rawBody)) DEALLOCATE(rawBody)
      IF (iBlankLine+2 <= LEN(normalizedBuffer)) THEN
        ALLOCATE(character(LEN=(LEN(normalizedBuffer)-(iBlankLine+1))) :: rawBody)
        rawBody = normalizedBuffer(iBlankLine+2:)
      ELSE
        ALLOCATE(character(LEN=0) :: rawBody)
        rawBody = ''
      END IF
    ELSE
      ! No blank line found; log this and handle as an invalid request
      WRITE(*, '(A)') "[DEBUG] No blank line found after headers. Request may be malformed."
      rawBody = ''  ! Set body to empty
      RETURN
    END IF

    !----------------------------------------------------------------------------
    ! Log request body for debugging
    !----------------------------------------------------------------------------
    IF (LEN_TRIM(rawBody) > 0) THEN
      WRITE(*, '(A)') "[DEBUG] Request Body:"
      WRITE(*, '(A)') TRIM(rawBody)
    ELSE
      WRITE(*, '(A)') "[DEBUG] No body found in request."
    END IF

    !----------------------------------------------------------------------------
    ! 3) Dispatch to your Router
    !----------------------------------------------------------------------------
    CALL routeRequest(TRIM(method), TRIM(path), TRIM(rawBody), response)

    ! Clean up
    IF (ALLOCATED(rawBody)) DEALLOCATE(rawBody)
  END SUBROUTINE parseAndRoute

END MODULE HTTPServer
