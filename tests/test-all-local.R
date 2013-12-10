# Runs all testthat tests of our package:

do_test <- function() {
   library('testthat')
   oldwd <- setwd('tests')
   on.exit(setwd(oldwd))
   test_check('agop')
}

do_test()
