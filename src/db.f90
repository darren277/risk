MODULE DB
  USE ISO_C_BINDING
  USE LoanTypes
  IMPLICIT NONE

  ! ------------------------------------------------------------------
  !  PostgreSQL result statuses
  ! ------------------------------------------------------------------
  INTEGER, PARAMETER :: PGRES_COMMAND_OK = 1
  INTEGER, PARAMETER :: PGRES_TUPLES_OK  = 2

  ! PostgreSQL connection statuses
  INTEGER, PARAMETER :: CONNECTION_OK    = 0

  ! We'll store the DB connection handle here
  TYPE(C_PTR) :: connPtr = C_NULL_PTR

  ! ------------------------------------------------------------------
  !  libpq INTERFACES
  ! ------------------------------------------------------------------
  INTERFACE
    TYPE(C_PTR) FUNCTION PQconnectdb(conninfo) BIND(C, NAME="PQconnectdb")
      IMPORT :: C_CHAR, C_PTR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: conninfo(*)
    END FUNCTION PQconnectdb

    INTEGER(C_INT) FUNCTION PQstatus(conn) BIND(C, NAME="PQstatus")
      IMPORT :: C_PTR, C_INT
      TYPE(C_PTR), INTENT(IN) :: conn
    END FUNCTION PQstatus

    SUBROUTINE PQfinish(conn) BIND(C, NAME="PQfinish")
      IMPORT :: C_PTR
      TYPE(C_PTR), INTENT(IN) :: conn
    END SUBROUTINE PQfinish

    FUNCTION PQerrorMessage(conn) BIND(C, NAME="PQerrorMessage")
      IMPORT :: C_PTR
      TYPE(C_PTR) :: PQerrorMessage
      TYPE(C_PTR), INTENT(IN) :: conn
    END FUNCTION PQerrorMessage

    TYPE(C_PTR) FUNCTION PQexec(conn, query) BIND(C, NAME="PQexec")
      IMPORT :: C_PTR, C_CHAR
      TYPE(C_PTR), INTENT(IN) :: conn
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: query(*)
    END FUNCTION PQexec

    TYPE(C_PTR) FUNCTION PQexecParams(conn, query, nParams, paramTypes, paramValues, &
                                      paramLengths, paramFormats, resultFormat) &
                                      BIND(C, NAME="PQexecParams")
      IMPORT :: C_CHAR, C_INT, C_PTR
      TYPE(C_PTR), INTENT(IN) :: conn
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: query(*)
      INTEGER(C_INT), VALUE :: nParams
      TYPE(C_PTR), INTENT(IN) :: paramTypes(*)     ! or C_NULL_PTR
      TYPE(C_PTR), INTENT(IN) :: paramValues(*)    ! array of param data pointers
      TYPE(C_PTR), INTENT(IN) :: paramLengths(*)   ! or C_NULL_PTR
      TYPE(C_PTR), INTENT(IN) :: paramFormats(*)   ! or C_NULL_PTR
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

    SUBROUTINE PQclear(result) BIND(C, NAME="PQclear")
      IMPORT :: C_PTR
      TYPE(C_PTR), INTENT(IN) :: result
    END SUBROUTINE PQclear

    FUNCTION PQgetvalue(res, row_number, column_number) RESULT(rPtr) BIND(C, NAME="PQgetvalue")
      IMPORT :: C_PTR, C_INT, C_CHAR
      TYPE(C_PTR) :: rPtr
      TYPE(C_PTR), INTENT(IN) :: res
      INTEGER(C_INT), VALUE   :: row_number, column_number
    END FUNCTION PQgetvalue
  END INTERFACE

CONTAINS

  ! ------------------------------------------------------------------
  ! 1) Helper: check if a C pointer is null
  ! ------------------------------------------------------------------
  LOGICAL FUNCTION isNullPtr(ptr) RESULT(isNull)
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN) :: ptr
    IF (c_associated(ptr)) THEN
       isNull = .FALSE.
    ELSE
       isNull = .TRUE.
    END IF
  END FUNCTION isNullPtr

  ! ------------------------------------------------------------------
  ! 2) Helper: copy a C-string into a Fortran CHARACTER
  ! ------------------------------------------------------------------
  SUBROUTINE cPtrToFortranString(srcPtr, fstr)
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN)         :: srcPtr
    CHARACTER(*),     INTENT(OUT)   :: fstr

    CHARACTER(KIND=C_CHAR), POINTER :: cbuf(:)
    INTEGER                        :: i, maxlen

    maxlen = LEN(fstr)

    ! If null pointer, blank out result
    IF (.NOT. c_associated(srcPtr)) THEN
       fstr = ' '
       RETURN
    END IF

    ! Create pointer to (at most) maxlen chars
    CALL C_F_POINTER(srcPtr, cbuf, [maxlen])

    DO i = 1, maxlen
       IF (cbuf(i) == C_NULL_CHAR) EXIT
       ! For many compilers, ACHAR and IACHAR might be safer:
       !   fstr(i:i) = ACHAR(IACHAR(cbuf(i)))
       ! If your compiler accepts CHAR() with the same KIND, do:
       fstr(i:i) = CHAR(ICHAR(cbuf(i)), KIND=KIND('A'))
    END DO

    IF (i <= maxlen) fstr(i:) = ' '
  END SUBROUTINE cPtrToFortranString

  ! ------------------------------------------------------------------
  ! 3) Helper: convert C-string -> INTEGER
  ! ------------------------------------------------------------------
  INTEGER FUNCTION cPtrToInt(srcPtr) RESULT(ivalue)
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN) :: srcPtr
    CHARACTER(LEN=256)      :: buffer
    INTEGER                 :: ios

    CALL cPtrToFortranString(srcPtr, buffer)
    READ(buffer, *, IOSTAT=ios) ivalue
    IF (ios /= 0) THEN
       ivalue = 0
    END IF
  END FUNCTION cPtrToInt

  ! ------------------------------------------------------------------
  ! 4) Helper: convert C-string -> REAL(C_DOUBLE)
  ! ------------------------------------------------------------------
  REAL(C_DOUBLE) FUNCTION cPtrToDouble(srcPtr) RESULT(rvalue)
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN) :: srcPtr
    CHARACTER(LEN=256)      :: buffer
    INTEGER                 :: ios

    CALL cPtrToFortranString(srcPtr, buffer)
    READ(buffer, *, IOSTAT=ios) rvalue
    IF (ios /= 0) THEN
       rvalue = 0.0D0
    END IF
  END FUNCTION cPtrToDouble

  ! ------------------------------------------------------------------
  ! 5) Helper: get libpq error message into Fortran string
  ! ------------------------------------------------------------------
  SUBROUTINE getErrorMessage(conn, errmsg)
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN) :: conn
    CHARACTER(LEN=*), INTENT(OUT) :: errmsg
    TYPE(C_PTR) :: msgPtr

    msgPtr = PQerrorMessage(conn)
    CALL cPtrToFortranString(msgPtr, errmsg)
  END SUBROUTINE getErrorMessage

  ! ------------------------------------------------------------------
  ! Initialize DB connection
  ! ------------------------------------------------------------------
  SUBROUTINE dbInitConnection()
    IMPLICIT NONE
    CHARACTER(KIND=C_CHAR, LEN=256) :: conninfo
    CHARACTER(LEN=256)             :: errmsg

    conninfo = 'host=localhost dbname=risk user=myuser password=mypass' // C_NULL_CHAR

    connPtr = PQconnectdb(conninfo)

    IF (isNullPtr(connPtr)) THEN
      WRITE(*,*) 'Failed to allocate memory for connection pointer.'
      STOP
    END IF

    IF (PQstatus(connPtr) /= CONNECTION_OK) THEN
      CALL getErrorMessage(connPtr, errmsg)
      WRITE(*,*) 'Failed to connect to DB: ', TRIM(errmsg)
      CALL PQfinish(connPtr)
      connPtr = C_NULL_PTR
      STOP
    END IF

    WRITE(*,*) 'Database connection initialized successfully.'
  END SUBROUTINE dbInitConnection

  ! ------------------------------------------------------------------
  ! Close DB connection
  ! ------------------------------------------------------------------
  SUBROUTINE dbCloseConnection()
    IMPLICIT NONE

    IF (.NOT. isNullPtr(connPtr)) THEN
      CALL PQfinish(connPtr)
      connPtr = C_NULL_PTR
      WRITE(*,*) 'Database connection closed.'
    ELSE
      WRITE(*,*) 'No active database connection to close.'
    END IF
  END SUBROUTINE dbCloseConnection

  ! ------------------------------------------------------------------
  ! Insert a Loan record (example)
  ! ------------------------------------------------------------------
  SUBROUTINE dbInsertLoan(loan, outId)
    IMPLICIT NONE
    TYPE(LoanRecord), INTENT(IN) :: loan
    INTEGER,         INTENT(OUT) :: outId

    CHARACTER(KIND=C_CHAR, LEN=1024) :: sql
    CHARACTER(LEN=256)              :: errmsg
    TYPE(C_PTR)                     :: resPtr

    ! Declare local buffers for text parameters, param array
    CHARACTER(KIND=C_CHAR, LEN=50), TARGET :: s_name, s_pastDebts, s_paymentHistory
    CHARACTER(KIND=C_CHAR, LEN=50), TARGET :: s_currentIncome, s_timeAtCurrentJob
    CHARACTER(KIND=C_CHAR, LEN=50), TARGET :: s_creditUtil, s_openAccounts, s_riskFactor
    TYPE(C_PTR), DIMENSION(8)      :: paramValues

    ! Construct the SQL
    sql = 'INSERT INTO loans (name, past_debts, payment_history, current_income, time_at_current_job, ' // &
          'credit_utilization, number_of_open_accounts, risk_factor) ' // &
          'VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id;' // C_NULL_CHAR

    ! Convert each field to a C-string representation
    s_name  = loan%name
    WRITE(s_pastDebts,      '(g0)') loan%pastDebts
    WRITE(s_paymentHistory, '(g0)') loan%paymentHistory
    WRITE(s_currentIncome,  '(g0)') loan%currentIncome
    WRITE(s_timeAtCurrentJob,'(g0)')loan%timeAtCurrentJob
    WRITE(s_creditUtil,     '(g0)') loan%creditUtilization
    WRITE(s_openAccounts,   '(i0)') loan%numberOfOpenAccounts
    WRITE(s_riskFactor,     '(g0)') loan%riskFactor

    ! Point paramValues to each local buffer
    paramValues(1) = C_LOC(s_name)
    paramValues(2) = C_LOC(s_pastDebts)
    paramValues(3) = C_LOC(s_paymentHistory)
    paramValues(4) = C_LOC(s_currentIncome)
    paramValues(5) = C_LOC(s_timeAtCurrentJob)
    paramValues(6) = C_LOC(s_creditUtil)
    paramValues(7) = C_LOC(s_openAccounts)
    paramValues(8) = C_LOC(s_riskFactor)

    ! Execute query
    resPtr = PQexecParams( connPtr, sql, 8,   &
                           C_NULL_PTR,        &
                           paramValues,       &
                           C_NULL_PTR,        &
                           C_NULL_PTR,        &
                           0 )    ! 0 => text result

    IF (PQresultStatus(resPtr) /= PGRES_TUPLES_OK) THEN
      CALL getErrorMessage(connPtr, errmsg)
      WRITE(*,*) 'Failed to insert loan: ', TRIM(errmsg)
      outId = -1
      CALL PQclear(resPtr)
      RETURN
    END IF

    ! Return the newly generated ID
    outId = cPtrToInt( PQgetvalue(resPtr, 0, 0) )

    CALL PQclear(resPtr)
  END SUBROUTINE dbInsertLoan

  ! ------------------------------------------------------------------
  ! etc.: dbSelectAllLoans, dbSelectLoanById, dbUpdateLoanById,
  !       dbDeleteLoanById, etc., following the same pattern
  ! ------------------------------------------------------------------

END MODULE DB
