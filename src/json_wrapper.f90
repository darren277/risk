MODULE JSONWrapper
  USE LoanTypes
  IMPLICIT NONE

CONTAINS
  FUNCTION parseLoanJSON(jsonString, loan) RESULT(ok)
    CHARACTER(*), INTENT(IN)  :: jsonString
    TYPE(LoanRecord), INTENT(INOUT) :: loan
    LOGICAL :: ok

    ! Use JSON-Fortran calls to parse the string into an in-memory JSON object.
    ! Then read fields if they exist.
    ok = .TRUE.  ! or .FALSE. on error
  END FUNCTION parseLoanJSON

  FUNCTION toString_int(n) RESULT(str)
    INTEGER, INTENT(IN) :: n
    CHARACTER(LEN=20) :: str
    WRITE(str, '(I0)') n
    str = ADJUSTL(str)
  END FUNCTION

  FUNCTION toString_real(n) RESULT(str)
    REAL(8), INTENT(IN) :: n
    CHARACTER(LEN=20) :: str
    WRITE(str, '(F0.2)') n
    str = ADJUSTL(str)
  END FUNCTION

  FUNCTION serializeLoanJSON(loan) RESULT(jsonString)
    TYPE(LoanRecord), INTENT(IN) :: loan
    CHARACTER(LEN=1000) :: jsonString
    CHARACTER(1), PARAMETER :: DQ = CHAR(34)
    
    jsonString = '{'//DQ//'id'//DQ//': '//TRIM(toString_int(loan%id))//','//&
                 DQ//'name'//DQ//': '//DQ//TRIM(loan%name)//DQ//','//&
                 DQ//'riskFactor'//DQ//': '//TRIM(toString_real(loan%riskFactor))//'}'
  END FUNCTION serializeLoanJSON

  FUNCTION serializeLoansArrayJSON(loans) RESULT(jsonString)
    TYPE(LoanRecord), INTENT(IN) :: loans(:)
    CHARACTER(LEN=10000) :: jsonString
    ! Build a JSON array of LoanRecords
    ! e.g. [ {loan1}, {loan2}, ... ]
  END FUNCTION serializeLoansArrayJSON

END MODULE JSONWrapper
