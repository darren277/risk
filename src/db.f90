MODULE DB
  USE ISO_C_BINDING
  USE LoanTypes
  IMPLICIT NONE

  INTERFACE
    TYPE(C_PTR) FUNCTION PQconnectdb(conninfo) BIND(C, NAME="PQconnectdb")
      IMPORT :: C_CHAR, C_PTR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: conninfo(*)
    END FUNCTION PQconnectdb

    ! ... plus many more libpq function interfaces (PQexec, PQfinish, PQresultStatus, etc.)
  END INTERFACE

  TYPE(C_PTR) :: connPtr = C_NULL_PTR   ! We'll store a handle to our DB connection

CONTAINS

  SUBROUTINE dbInitConnection()
    ! This would call PQconnectdb with a connection string
    ! e.g. "host=localhost dbname=mydb user=myuser password=mypass"
  END SUBROUTINE dbInitConnection

  SUBROUTINE dbCloseConnection()
    ! This would call PQfinish(connPtr), etc.
  END SUBROUTINE dbCloseConnection

  !---------------------------
  ! Insert a new Loan record
  !---------------------------
  SUBROUTINE dbInsertLoan(loan, outId)
    TYPE(LoanRecord), INTENT(IN)  :: loan
    INTEGER,          INTENT(OUT) :: outId
    ! 1) Construct SQL with placeholders
    ! 2) PQexecParams to insert the record
    ! 3) Retrieve the auto-generated ID
  END SUBROUTINE dbInsertLoan

  !---------------------------
  ! Select all loans
  !---------------------------
  SUBROUTINE dbSelectAllLoans(loans, count)
    TYPE(LoanRecord), ALLOCATABLE, INTENT(OUT) :: loans(:)
    INTEGER,                         INTENT(OUT) :: count
    ! 1) Execute "SELECT ..."  
    ! 2) Allocate `loans(count)`  
    ! 3) Fill each LoanRecord  
  END SUBROUTINE dbSelectAllLoans

  !---------------------------
  ! Select a loan by ID
  !---------------------------
  SUBROUTINE dbSelectLoanById(id, loan, found)
    INTEGER,       INTENT(IN)  :: id
    TYPE(LoanRecord), INTENT(OUT) :: loan
    LOGICAL,       INTENT(OUT) :: found
    ! 1) Execute "SELECT ... WHERE id = $1"
    ! 2) If found, fill the loan structure
  END SUBROUTINE dbSelectLoanById

  !---------------------------
  ! Update a loan by ID
  !---------------------------
  SUBROUTINE dbUpdateLoanById(loan, success)
    TYPE(LoanRecord), INTENT(IN) :: loan
    LOGICAL,          INTENT(OUT) :: success
    ! 1) Execute "UPDATE ... WHERE id = $1"
    ! 2) Return success/failure
  END SUBROUTINE dbUpdateLoanById

  !---------------------------
  ! Delete a loan by ID
  !---------------------------
  SUBROUTINE dbDeleteLoanById(id, success)
    INTEGER, INTENT(IN)  :: id
    LOGICAL, INTENT(OUT) :: success
    ! 1) Execute "DELETE FROM ... WHERE id = $1"
  END SUBROUTINE dbDeleteLoanById

END MODULE DB
