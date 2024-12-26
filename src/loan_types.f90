MODULE LoanTypes
  IMPLICIT NONE

  TYPE :: LoanRecord
    INTEGER :: id         = 0    ! auto-increment ID from the DB
    CHARACTER(LEN=50) :: name    ! applicant name
    REAL(KIND=8)      :: pastDebts
    REAL(KIND=8)      :: paymentHistory
    REAL(KIND=8)      :: currentIncome
    REAL(KIND=8)      :: timeAtCurrentJob
    REAL(KIND=8)      :: creditUtilization
    INTEGER           :: numberOfOpenAccounts
    REAL(KIND=8)      :: riskFactor
  END TYPE LoanRecord

END MODULE LoanTypes
