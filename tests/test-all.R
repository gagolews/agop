# Runs all tests for the package:

require(testthat)

do_test <- function() {
   oldwd <- setwd('tests')
   on.exit(setwd(oldwd))
   test_check('agop')
}

do_test()
