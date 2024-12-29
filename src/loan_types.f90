MODULE LoanTypes
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE LoanRecord
    INTEGER(C_INT)                   :: id
    CHARACTER(KIND=C_CHAR, LEN=50)   :: name
    REAL(C_DOUBLE)                   :: pastDebts
    REAL(C_DOUBLE)                   :: paymentHistory
    REAL(C_DOUBLE)                   :: currentIncome
    REAL(C_DOUBLE)                   :: timeAtCurrentJob
    REAL(C_DOUBLE)                   :: creditUtilization
    INTEGER(C_INT)                   :: numberOfOpenAccounts
    REAL(C_DOUBLE)                   :: riskFactor
  END TYPE LoanRecord

END MODULE LoanTypes
