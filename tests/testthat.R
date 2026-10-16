library(testthat)
library(PFIM)

# Full suite: local / CI (`devtools::test()` sets NOT_CRAN=true).
# Skipped on CRAN: long ODE FIM and optimization checks.
if ( identical( Sys.getenv( "NOT_CRAN" ), "true" ) )
  test_check( "PFIM" )
