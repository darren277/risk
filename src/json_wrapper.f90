MODULE JSONWrapper
  USE LoanTypes
  IMPLICIT NONE

CONTAINS
  FUNCTION parseLoanJSON(jsonString, loan) RESULT(ok)
    USE ISO_C_BINDING
    CHARACTER(*), INTENT(IN) :: jsonString
    TYPE(LoanRecord), INTENT(INOUT) :: loan
    LOGICAL :: ok
    
    INTEGER :: pos, endPos
    CHARACTER(1), PARAMETER :: DQ = CHAR(34)
    !CHARACTER(LEN=50) :: valueStr
    INTEGER :: ios

    loan%id = 0  ! Default for new loan
    ok = .FALSE.

    WRITE(*, '(A)') "[DEBUG] Starting JSON parse"

    WRITE(*, '(A)') "[DEBUG] JSON string length: " // TRIM(toString_int(LEN_TRIM(jsonString)))

    ! Parse name field
    ! Get name bounds
    ! Extract name between quotes
    ! Extract name value 
    pos = INDEX(jsonString, '"name": "')
    loan%name = jsonString(pos+9:pos+16)  ! "John Doe" is 8 chars
    loan%name = TRIM(loan%name) // C_NULL_CHAR
    WRITE(*, *) "Name:", loan%name

    ! Parse past_debts
    pos = INDEX(jsonString, '"past_debts": ')

    WRITE(*, '(A,I0)') "[DEBUG] past_debts position: ", pos

    IF (pos <= 0) THEN
        WRITE(*, '(A)') "[DEBUG] Failed to find past_debts field"
        RETURN
    END IF
    
    pos = pos + 13
    READ(jsonString(pos:), *, IOSTAT=ios) loan%pastDebts
    IF (ios /= 0) RETURN

    WRITE(*, '(A)') "[DEBUG] Found past_debts: " // TRIM(toString_real(loan%pastDebts))

    ! Parse payment_history
    pos = INDEX(jsonString, '"payment_history": ')

    WRITE(*, '(A,I0)') "[DEBUG] payment_history position: ", pos

    IF (pos <= 0) THEN
        WRITE(*, '(A)') "[DEBUG] Failed to find payment_history field"
        RETURN
    END IF

    pos = pos + 18
    READ(jsonString(pos:), *, IOSTAT=ios) loan%paymentHistory
    IF (ios /= 0) RETURN

    WRITE(*, '(A)') "[DEBUG] Found payment_history: " // TRIM(toString_real(loan%paymentHistory))

    ! Add remaining fields...
    pos = INDEX(jsonString, '"current_income": ')

    WRITE(*, '(A,I0)') "[DEBUG] current_income position: ", pos

    IF (pos <= 0) THEN
        WRITE(*, '(A)') "[DEBUG] Failed to find current_income field"
        RETURN
    END IF

    pos = pos + 17
    READ(jsonString(pos:), *, IOSTAT=ios) loan%currentIncome
    IF (ios /= 0) RETURN

    WRITE(*, '(A)') "[DEBUG] Found current_income: " // TRIM(toString_real(loan%currentIncome))

    pos = INDEX(jsonString, '"time_at_current_job": ')

    WRITE(*, '(A,I0)') "[DEBUG] time_at_current_job position: ", pos

    IF (pos <= 0) THEN
        WRITE(*, '(A)') "[DEBUG] Failed to find time_at_current_job field"
        RETURN
    END IF

    pos = pos + 22
    READ(jsonString(pos:), *, IOSTAT=ios) loan%timeAtCurrentJob
    IF (ios /= 0) RETURN

    WRITE(*, '(A)') "[DEBUG] Found time_at_current_job: " // TRIM(toString_real(loan%timeAtCurrentJob))

    pos = INDEX(jsonString, '"credit_utilization": ')

    WRITE(*, '(A,I0)') "[DEBUG] credit_utilization position: ", pos

    IF (pos <= 0) THEN
        WRITE(*, '(A)') "[DEBUG] Failed to find credit_utilization field"
        RETURN
    END IF

    pos = pos + 21
    READ(jsonString(pos:), *, IOSTAT=ios) loan%creditUtilization
    IF (ios /= 0) RETURN

    WRITE(*, '(A)') "[DEBUG] Found credit_utilization: " // TRIM(toString_real(loan%creditUtilization))

    ! Parse number_of_open_accounts
    pos = INDEX(jsonString, '"number_of_open_accounts": ') + 27
    IF (pos <= 27) RETURN  ! Not found

    endPos = INDEX(jsonString(pos:), ',')
    IF (endPos <= 0) THEN
        endPos = INDEX(jsonString(pos:), '}')
        IF (endPos <= 0) RETURN
    END IF
    endPos = pos + endPos - 1

    WRITE(*, '(A)') "[DEBUG] Accounts value: '" // jsonString(pos:endPos-1) // "'"
    READ(jsonString(pos:endPos-1), *, IOSTAT=ios) loan%numberOfOpenAccounts
    IF (ios /= 0) RETURN

    !WRITE(*, '(A)') "[DEBUG] Found number_of_open_accounts: " // TRIM(toString_int(loan%numberOfOpenAccounts))

    ! Make risk_factor optional - parse only if present
    pos = INDEX(jsonString, '"risk_factor": ')  ! Optional field
    WRITE(*, '(A)') "[DEBUG] Risk factor position: " // TRIM(toString_int(pos))
    IF (pos > 0) THEN
        pos = pos + 14  ! Skip field name + ": "
        endPos = INDEX(jsonString(pos:), '}')
        IF (endPos <= 0) RETURN
        endPos = pos + endPos - 1
        WRITE(*, '(A)') "[DEBUG] Risk value: '" // jsonString(pos:endPos-1) // "'"
        READ(jsonString(pos:endPos-1), *, IOSTAT=ios) loan%riskFactor
        IF (ios /= 0) RETURN
    END IF

    !WRITE(*, '(A)') "[DEBUG] Found risk_factor: " // TRIM(toString_real(loan%riskFactor))

    ok = .TRUE.
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
