MODULE Handlers
  USE JSONWrapper   ! hypothetical JSON parse/serialize module
  USE RiskCalc
  USE DB
  USE LoanTypes
  IMPLICIT NONE

CONTAINS

  FUNCTION parseIdFromPath(path) RESULT(id)
    CHARACTER(LEN=*), INTENT(IN) :: path
    INTEGER :: id, pos, status
    CHARACTER(LEN=20) :: id_str

    pos = INDEX(path, '/loans/') + 6
    id_str = path(pos:)
    READ(id_str, *, IOSTAT=status) id
    IF (status /= 0) id = -1
  END FUNCTION

  ! POST /loans (Create Loan)
  SUBROUTINE handleCreateLoan(body, response)
    CHARACTER(*), INTENT(IN)  :: body
    CHARACTER(*), INTENT(OUT) :: response

    TYPE(LoanRecord) :: loan
    INTEGER          :: newId
    LOGICAL          :: parseSuccess

    WRITE(*, '(A)') "[DEBUG] Starting loan creation"
    WRITE(*, '(A)') "[DEBUG] Received body: "//TRIM(body)

    ! 1) Parse JSON body into a LoanRecord
    parseSuccess = parseLoanJSON(body, loan)
    WRITE(*, '(A,L1)') "[DEBUG] JSON parse success: ", parseSuccess

    IF (.NOT. parseSuccess) THEN
      response = '{"error": "Invalid JSON"}'
      RETURN
    END IF

    !WRITE(*, '(A,F10.2)') "[DEBUG] Parsed loan holders name: ", loan%name
    WRITE(*, '(A)') "[DEBUG] Parsed loan holders name: " // TRIM(loan%name)

    ! 2) Calculate risk factor
    loan%riskFactor = computeLoanRisk(loan)

    WRITE(*, '(A,F10.2)') "[DEBUG] Calculated risk factor: ", loan%riskFactor

    ! 3) Insert into DB
    CALL dbInsertLoan(loan, newId)
    WRITE(*, '(A,I0)') "[DEBUG] Inserted with ID: ", newId

    loan%id = newId

    ! 4) Convert to JSON
    response = serializeLoanJSON(loan)
  END SUBROUTINE handleCreateLoan
  
  ! GET /loans (List All Loans)
  SUBROUTINE handleListLoans(response)
    CHARACTER(*), INTENT(OUT) :: response

    TYPE(LoanRecord), ALLOCATABLE :: loans(:)
    INTEGER :: count

    CALL dbSelectAllLoans(loans, count)
    response = serializeLoansArrayJSON(loans)
  END SUBROUTINE handleListLoans

  ! GET /loans/<id> (Get a Loan by ID)
    SUBROUTINE handleGetLoan(path, response)
    CHARACTER(*), INTENT(IN)  :: path
    CHARACTER(*), INTENT(OUT) :: response

    INTEGER :: id
    TYPE(LoanRecord) :: loan
    LOGICAL :: found

    ! parse the ID from the path, e.g. /loans/5
    id = parseIdFromPath(path)

    CALL dbSelectLoanById(id, loan, found)

    IF (.NOT. found) THEN
      response = '{"error": "Not Found"}'
    ELSE
      response = serializeLoanJSON(loan)
    END IF
  END SUBROUTINE handleGetLoan

  ! PUT /loans/<id> (Update a Loan by ID)
  SUBROUTINE handleUpdateLoan(path, body, response)
    CHARACTER(*), INTENT(IN)  :: path, body
    CHARACTER(*), INTENT(OUT) :: response

    INTEGER :: id
    TYPE(LoanRecord) :: existingLoan
    TYPE(LoanRecord) :: updatedLoan
    LOGICAL :: found, parseSuccess, success

    id = parseIdFromPath(path)

    ! 1) Get existing loan
    CALL dbSelectLoanById(id, existingLoan, found)
    IF (.NOT. found) THEN
      response = '{"error": "Not Found"}'
      RETURN
    END IF

    ! 2) Parse partial JSON updates into updatedLoan
    updatedLoan = existingLoan
    parseSuccess = parseLoanJSON(body, updatedLoan)  
    ! parseLoanJSON can update only fields found in JSON

    IF (.NOT. parseSuccess) THEN
      response = '{"error": "Invalid JSON"}'
      RETURN
    END IF

    ! 3) Recalculate risk
    updatedLoan%riskFactor = computeLoanRisk(updatedLoan)

    ! 4) Save
    CALL dbUpdateLoanById(updatedLoan, success)
    IF (success) THEN
      response = serializeLoanJSON(updatedLoan)
    ELSE
      response = '{"error": "Update failed"}'
    END IF
  END SUBROUTINE handleUpdateLoan

  ! DELETE /loans/<id> (Delete a Loan by ID)
  SUBROUTINE handleDeleteLoan(path, response)
    CHARACTER(*), INTENT(IN)  :: path
    CHARACTER(*), INTENT(OUT) :: response

    INTEGER :: id
    LOGICAL :: success

    id = parseIdFromPath(path)
    CALL dbDeleteLoanById(id, success)
    IF (success) THEN
      response = '{"status": "Deleted"}'
    ELSE
      response = '{"error": "Delete failed"}'
    END IF
  END SUBROUTINE handleDeleteLoan

END MODULE Handlers
