MODULE Router
  USE Handlers
  IMPLICIT NONE
CONTAINS

  SUBROUTINE routeRequest(method, path, body, response)
    CHARACTER(*), INTENT(IN) :: method, path, body
    CHARACTER(*), INTENT(OUT) :: response

    SELECT CASE (method)
    CASE ("POST")
      IF (path == "/loans") THEN
        CALL handleCreateLoan(body, response)
      ELSE
        ! 404 Not Found
        response = '{"error": "Not Found"}'
      END IF
    CASE ("GET")
      IF (path == "/loans") THEN
        CALL handleListLoans(response)
      ELSEIF (path(1:7) == "/loans/" ) THEN
        ! Extract ID from path
        CALL handleGetLoan(path, response)
      ELSE
        response = '{"error": "Not Found"}'
      END IF
    CASE ("PUT")
      IF (path(1:7) == "/loans/") THEN
        CALL handleUpdateLoan(path, body, response)
      ELSE
        response = '{"error": "Not Found"}'
      END IF
    CASE ("DELETE")
      IF (path(1:7) == "/loans/") THEN
        CALL handleDeleteLoan(path, response)
      ELSE
        response = '{"error": "Not Found"}'
      END IF
    CASE DEFAULT
      response = '{"error": "Method Not Allowed"}'
    END SELECT
  END SUBROUTINE routeRequest

END MODULE Router
