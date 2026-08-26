library(testthat)
library(emp.test)

# Prevent thread overuse (through data.table?) when running tests on CRAN.
Sys.setenv("OMP_THREAD_LIMIT" = 2L)

testthat::test_check("emp.test")
