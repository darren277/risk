MODULE RiskCalc
  IMPLICIT NONE
CONTAINS

  REAL(KIND=8) FUNCTION computeLoanRisk(loan) RESULT(risk)
    USE LoanTypes
    IMPLICIT NONE
    TYPE(LoanRecord), INTENT(IN) :: loan
    ! Example coefficients:
    REAL(KIND=8), PARAMETER :: beta0         = 10.0D0
    REAL(KIND=8), PARAMETER :: betaDebts     = -0.2D0
    REAL(KIND=8), PARAMETER :: betaPayHist   = 0.5D0
    REAL(KIND=8), PARAMETER :: betaIncome    = 0.03D0
    REAL(KIND=8), PARAMETER :: betaTimeJob   = 0.1D0

    ! A simple linear model:
    risk = beta0                              &
       + betaDebts   * loan%pastDebts         &
       + betaPayHist * loan%paymentHistory    &
       + betaIncome  * loan%currentIncome     &
       + betaTimeJob * loan%timeAtCurrentJob

  END FUNCTION computeLoanRisk

END MODULE RiskCalc
