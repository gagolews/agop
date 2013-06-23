require('testthat')


test_that("owa, wam", {
   
   expect_equivalent(owa(c(1,2,NA)), NA_real_)
   expect_equivalent(owa(numeric(0)), numeric(0))
   expect_equivalent(owa(1:10), 5.5)
   expect_equivalent(owa(1:10, c(1,rep(0,9))), 10)
   expect_warning(owa(1, 2))
   
   expect_equivalent(wam(c(1,2,NA)), NA_real_)
   expect_equivalent(wam(numeric(0)), numeric(0))
   expect_equivalent(wam(1:10), 5.5)
   expect_equivalent(wam(1:10, c(1,rep(0,9))), 1)
   expect_warning(wam(1, 2))
   
})
