MODULE DB
  USE ISO_C_BINDING
  USE LoanTypes
  IMPLICIT NONE

  ! PostgreSQL result statuses
  INTEGER, PARAMETER :: PGRES_COMMAND_OK = 1
  INTEGER, PARAMETER :: PGRES_TUPLES_OK = 2

  ! PostgreSQL connection statuses
  INTEGER, PARAMETER :: CONNECTION_OK = 0

  INTERFACE
    TYPE(C_PTR) FUNCTION PQconnectdb(conninfo) BIND(C, NAME="PQconnectdb")
      IMPORT :: C_CHAR, C_PTR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: conninfo(*)
    END FUNCTION PQconnectdb

    ! PGresult *PQexec(PGconn *conn, const char *query);
    TYPE(C_PTR) FUNCTION PQexec(conn, query) BIND(C, NAME="PQexec")
      IMPORT :: C_PTR, C_CHAR
      TYPE(C_PTR), INTENT(IN) :: conn
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: query(*)
    END FUNCTION PQexec

    ! char *PQgetvalue(const PGresult *res, int row_number, int column_number);
    FUNCTION PQgetvalue(res, row_number, column_number) RESULT(c_str_ptr) BIND(C, NAME="PQgetvalue")
      USE ISO_C_BINDING
      IMPORT :: C_PTR, C_INT, C_CHAR
      TYPE(C_PTR) :: c_str_ptr             ! Return a C pointer (TYPE(C_PTR))
      TYPE(C_PTR), INTENT(IN) :: res
      INTEGER(C_INT), VALUE :: row_number, column_number
    END FUNCTION PQgetvalue

    ! ... plus many more libpq function interfaces (PQexec, PQfinish, PQresultStatus, etc.)
    TYPE(C_PTR) FUNCTION PQexecParams(conn, query, nParams, paramTypes, paramValues, &
                paramLengths, paramFormats, resultFormat) BIND(C, NAME="PQexecParams")
      IMPORT :: C_CHAR, C_INT, C_PTR
      TYPE(C_PTR), INTENT(IN) :: conn
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: query(*)
      INTEGER(C_INT), VALUE :: nParams
      TYPE(C_PTR), INTENT(IN) :: paramTypes(*)     ! Parameter types (array)
      TYPE(C_PTR), INTENT(IN) :: paramValues(*)    ! Parameter values (array)
      TYPE(C_PTR), INTENT(IN) :: paramLengths(*)   ! Parameter lengths (array)
      TYPE(C_PTR), INTENT(IN) :: paramFormats(*)   ! Parameter formats (array)
      INTEGER(C_INT), VALUE :: resultFormat
    END FUNCTION PQexecParams

    INTEGER(C_INT) FUNCTION PQresultStatus(result) BIND(C, NAME="PQresultStatus")
      IMPORT :: C_PTR, C_INT
      TYPE(C_PTR), INTENT(IN) :: result
    END FUNCTION PQresultStatus

    INTEGER(C_INT) FUNCTION PQntuples(result) BIND(C, NAME="PQntuples")
      IMPORT :: C_PTR, C_INT
      TYPE(C_PTR), INTENT(IN) :: result
    END FUNCTION PQntuples

    FUNCTION PQerrorMessage(conn) BIND(C, NAME="PQerrorMessage")
      IMPORT :: C_PTR
      TYPE(C_PTR) :: PQerrorMessage       ! Return type is a C pointer
      TYPE(C_PTR), INTENT(IN) :: conn     ! Connection pointer
    END FUNCTION PQerrorMessage
  END INTERFACE

  TYPE(C_PTR) :: connPtr = C_NULL_PTR   ! We'll store a handle to our DB connection

CONTAINS

  SUBROUTINE dbInitConnection()
    ! This would call PQconnectdb with a connection string
    ! e.g. "host=localhost dbname=mydb user=myuser password=mypass"
    CHARACTER(KIND=C_CHAR, LEN=256) :: conninfo
    CHARACTER(LEN=256) :: errmsg

    conninfo = "host=localhost dbname=risk user=myusername password=mypassword" // C_NULL_CHAR
    connPtr = PQconnectdb(conninfo)

    IF (connPtr == C_NULL_PTR) THEN
      WRITE(*, *) "Failed to allocate memory for connection pointer."
      STOP
    END IF

    ! Check connection status
    IF (PQstatus(connPtr) /= CONNECTION_OK) THEN
      errmsg = PQerrorMessage(connPtr)
      WRITE(*, *) "Failed to connect to the database: ", TRIM(errmsg)
      CALL PQfinish(connPtr)
      STOP
    END IF

    WRITE(*, *) "Database connection initialized successfully."
  END SUBROUTINE dbInitConnection

  SUBROUTINE dbCloseConnection()
    ! This would call PQfinish(connPtr), etc.
    IF (connPtr /= C_NULL_PTR) THEN
      CALL PQfinish(connPtr)
      connPtr = C_NULL_PTR
      WRITE(*, *) "Database connection closed."
    ELSE
      WRITE(*, *) "No active database connection to close."
    END IF
  END SUBROUTINE dbCloseConnection

  !---------------------------
  ! Insert a new Loan record
  !---------------------------
  SUBROUTINE dbInsertLoan(loan, outId)
    USE ISO_C_BINDING
    TYPE(LoanRecord), INTENT(IN) :: loan
    INTEGER, INTENT(OUT) :: outId
    CHARACTER(LEN=1024) :: sql
    TYPE(C_PTR) :: resPtr
    CHARACTER(LEN=256) :: errmsg

    ! Construct SQL query
    sql = "INSERT INTO loans (name, past_debts, payment_history, current_income, time_at_current_job, " // &
      "credit_utilization, number_of_open_accounts, risk_factor) " // &
      "VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id;"

    ! Execute query
    resPtr = PQexecParams(connPtr, sql, 8, C_NULL_PTR, &
                          [C_LOC(loan%name), &
                          C_LOC(loan%pastDebts), &
                          C_LOC(loan%paymentHistory), &
                          C_LOC(loan%currentIncome), &
                          C_LOC(loan%timeAtCurrentJob), &
                          C_LOC(loan%creditUtilization), &
                          C_LOC(loan%numberOfOpenAccounts), &
                          C_LOC(loan%riskFactor)], &
                          ![C_INT, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_INT, C_NUMERIC], &
                          [C_INT, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_INT, C_DOUBLE], &
                          C_NULL_PTR, C_NULL_PTR)

    ! Check result status
    IF (PQresultStatus(resPtr) /= PGRES_TUPLES_OK) THEN
      errmsg = PQerrorMessage(connPtr)
      WRITE(*, *) "Failed to insert loan: ", TRIM(errmsg)
      outId = -1
      RETURN
    END IF

    ! Retrieve the ID of the inserted row
    outId = PQgetvalue(resPtr, 0, 0)

    ! Clean up
    CALL PQclear(resPtr)
  END SUBROUTINE dbInsertLoan

  !---------------------------
  ! Select all loans
  !---------------------------
  SUBROUTINE dbSelectAllLoans(loans, count)
    TYPE(LoanRecord), ALLOCATABLE, INTENT(OUT) :: loans(:)
    INTEGER, INTENT(OUT) :: count
    CHARACTER(LEN=256) :: sql
    TYPE(C_PTR) :: resPtr
    INTEGER :: i

    ! sql = "SELECT id, name, past_debts, payment_history, current_income, time_at_current_job, credit_utilization, number_of_open_accounts, risk_factor FROM loans;"
    sql = "SELECT * FROM loans;"
    resPtr = PQexec(connPtr, sql)

    IF (PQresultStatus(resPtr) /= PGRES_TUPLES_OK) THEN
      WRITE(*, *) "Failed to retrieve loans: ", PQerrorMessage(connPtr)
      count = 0
      RETURN
    END IF

    count = PQntuples(resPtr)
    ALLOCATE(loans(count))

    DO i = 1, count
      loans(i)%id = PQgetvalue(resPtr, i-1, 0)
      loans(i)%name = TRIM(PQgetvalue(resPtr, i-1, 1))
      loans(i)%pastDebts = REAL(PQgetvalue(resPtr, i-1, 2))
      loans(i)%paymentHistory = REAL(PQgetvalue(resPtr, i-1, 3))
      loans(i)%currentIncome = REAL(PQgetvalue(resPtr, i-1, 4))
      loans(i)%timeAtCurrentJob = REAL(PQgetvalue(resPtr, i-1, 5))
      loans(i)%creditUtilization = REAL(PQgetvalue(resPtr, i-1, 6))
      loans(i)%numberOfOpenAccounts = INT(PQgetvalue(resPtr, i-1, 7))
      loans(i)%riskFactor = REAL(PQgetvalue(resPtr, i-1, 8))
    END DO

    CALL PQclear(resPtr)
  END SUBROUTINE dbSelectAllLoans

  !---------------------------
  ! Select a loan by ID
  !---------------------------
  SUBROUTINE dbSelectLoanById(id, loan, found)
    USE ISO_C_BINDING
    !!!USE DB  ! Assuming PQexec, PQgetvalue, and other libpq functions are declared here
    IMPLICIT NONE

    INTEGER(C_INT), INTENT(IN), TARGET :: id
    TYPE(LoanRecord), INTENT(OUT) :: loan
    LOGICAL, INTENT(OUT) :: found
    CHARACTER(LEN=256) :: sql
    TYPE(C_PTR) :: resPtr
    CHARACTER(KIND=C_CHAR), POINTER :: c_result
    CHARACTER(LEN=256) :: tempString

    ! Construct SQL query
    sql = "SELECT id, name, past_debts, payment_history, current_income, time_at_current_job, " // &
          "credit_utilization, number_of_open_accounts, risk_factor FROM loans WHERE id = $1;" // &
          C_NULL_CHAR

    ! Execute query
    resPtr = PQexecParams(connPtr, sql, 1, C_NULL_PTR, &
                          [C_LOC(id)], &
                          [C_INT], &
                          C_NULL_PTR, C_NULL_PTR)

    ! Check result status
    IF (PQresultStatus(resPtr) /= PGRES_TUPLES_OK .OR. PQntuples(resPtr) == 0) THEN
      found = .FALSE.
      CALL PQclear(resPtr)
      RETURN
    END IF

    ! Populate the LoanRecord structure
    CALL safeGetValue(resPtr, 0, 0, loan%id)                   ! id
    CALL safeGetValue(resPtr, 0, 1, loan%name)                 ! name
    CALL safeGetValue(resPtr, 0, 2, loan%pastDebts)            ! pastDebts
    CALL safeGetValue(resPtr, 0, 3, loan%paymentHistory)       ! paymentHistory
    CALL safeGetValue(resPtr, 0, 4, loan%currentIncome)        ! currentIncome
    CALL safeGetValue(resPtr, 0, 5, loan%timeAtCurrentJob)     ! timeAtCurrentJob
    CALL safeGetValue(resPtr, 0, 6, loan%creditUtilization)    ! creditUtilization
    CALL safeGetValue(resPtr, 0, 7, loan%numberOfOpenAccounts) ! numberOfOpenAccounts
    CALL safeGetValue(resPtr, 0, 8, loan%riskFactor)           ! riskFactor

    found = .TRUE.
    CALL PQclear(resPtr)
  END SUBROUTINE dbSelectLoanById

  !---------------------------
  ! Update a loan by ID
  !---------------------------
  SUBROUTINE dbUpdateLoanById(loan, success)
    USE ISO_C_BINDING
    TYPE(LoanRecord), TARGET :: loan
    LOGICAL, INTENT(OUT) :: success
    CHARACTER(LEN=1024) :: sql
    TYPE(C_PTR) :: resPtr
    CHARACTER(LEN=256) :: errmsg

    ! Construct SQL query
    sql = "UPDATE loans " // &
          "SET name = $1, " // &
          "    past_debts = $2, " // &
          "    payment_history = $3, " // &
          "    current_income = $4, " // &
          "    time_at_current_job = $5, " // &
          "    credit_utilization = $6, " // &
          "    number_of_open_accounts = $7, " // &
          "    risk_factor = $8 " // &
          "WHERE id = $9;"

    ! Execute query
    resPtr = PQexecParams(connPtr, sql, 9, C_NULL_PTR, &
                          [C_LOC(loan%name), &
                          C_LOC(loan%pastDebts), &
                          C_LOC(loan%paymentHistory), &
                          C_LOC(loan%currentIncome), &
                          C_LOC(loan%timeAtCurrentJob), &
                          C_LOC(loan%creditUtilization), &
                          C_LOC(loan%numberOfOpenAccounts), &
                          C_LOC(loan%riskFactor), &
                          C_LOC(loan%id)], &
                          ! [C_CHAR, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_NUMERIC, C_INT, C_NUMERIC, C_INT], &
                          [C_INT, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_DOUBLE, C_INT, C_DOUBLE, C_INT], &
                          C_NULL_PTR, C_NULL_PTR)

    ! Check result status
    IF (PQresultStatus(resPtr) /= PGRES_COMMAND_OK) THEN
      errmsg = PQerrorMessage(connPtr)
      WRITE(*, *) "Failed to update loan with ID ", loan%id, ": ", TRIM(errmsg)
      success = .FALSE.
      CALL PQclear(resPtr)
      RETURN
    END IF

    success = .TRUE.
    CALL PQclear(resPtr)
  END SUBROUTINE dbUpdateLoanById

  !---------------------------
  ! Delete a loan by ID
  !---------------------------
  SUBROUTINE dbDeleteLoanById(id, success)
    USE ISO_C_BINDING
    INTEGER(C_INT), INTENT(IN), TARGET :: id
    LOGICAL, INTENT(OUT) :: success
    CHARACTER(LEN=256) :: sql
    TYPE(C_PTR) :: resPtr
    CHARACTER(LEN=256) :: errmsg
    INTEGER(C_INT), TARGET :: lengths(1)
    TYPE(C_PTR) :: paramLengths(1)

    ! Construct SQL query
    sql = "DELETE FROM loans WHERE id = $1;"

    ! Populate paramLengths with pointers to lengths
    paramLengths(1) = C_LOC(lengths(1))

    ! Execute query
    resPtr = PQexecParams(connPtr, sql, 1, C_NULL_PTR, &
                          [C_LOC(id)], &
                          paramLengths, &
                          C_NULL_PTR, C_NULL_PTR)

    ! Check result status
    IF (PQresultStatus(resPtr) /= PGRES_COMMAND_OK) THEN
      errmsg = PQerrorMessage(connPtr)
      WRITE(*, *) "Failed to delete loan with ID ", id, ": ", TRIM(errmsg)
      success = .FALSE.
      CALL PQclear(resPtr)
      RETURN
    END IF

    success = .TRUE.
    CALL PQclear(resPtr)
  END SUBROUTINE dbDeleteLoanById

END MODULE DB
