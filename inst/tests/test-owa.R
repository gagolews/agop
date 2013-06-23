require('testthat')


test_that("owa", {
   
   expect_equivalent(owa(numeric(0)), numeric(0))
   expect_equivalent(owa(1:10), 5.5)
   expect_equivalent(owa(1:10, c(1,rep(0,9))), 10)
   expect_warning(owa(1, 2))
   
})
