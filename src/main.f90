PROGRAM Main
  USE DB
  USE HTTPServer
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: stderr => error_unit
  IMPLICIT NONE

  INTEGER :: port

  ! 1) Connect to DB
  CALL dbInitConnection()

  ! 2) Start server
  port = 8085
  WRITE(stderr,*) "Starting Fortran HTTP server on port ", port
  CALL startServer(port)

  ! 3) On shutdown (signal or otherwise), close DB
  CALL dbCloseConnection()
END PROGRAM Main
