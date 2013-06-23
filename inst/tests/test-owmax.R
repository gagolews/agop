require('testthat')


test_that("owmax, owmin, wmax, wmin", {
   
   expect_equivalent(owmax(c(1,2,NA)), NA_real_)
   expect_equivalent(owmax(numeric(0)), numeric(0))
   expect_equivalent(owmax(1:10, 1:10), 5)
   expect_equivalent(owmax(1:10, 10:1), 10)
   expect_equivalent(owmax(c(1,6,3)), 6)
   
   expect_equivalent(wmax(c(1,2,NA)), NA_real_)
   expect_equivalent(wmax(numeric(0)), numeric(0))
   expect_equivalent(wmax(1:10, 1:10), 10)
   expect_equivalent(wmax(1:10, 10:1), 5)
   expect_equivalent(wmax(c(1,6,3)), 6)
   
   expect_equivalent(wmin(c(1,2,NA)), NA_real_)
   expect_equivalent(wmin(numeric(0)), numeric(0))
   expect_equivalent(wmin(1:10, 1:10), 1)
   expect_equivalent(wmin(1:10, 10:1), 6)
   expect_equivalent(wmin(c(1,6,3)), 1)
   
   expect_equivalent(owmin(c(1,2,NA)), NA_real_)
   expect_equivalent(owmin(numeric(0)), numeric(0))
   expect_equivalent(owmin(1:10, 1:10), 6)
   expect_equivalent(owmin(1:10, 10:1), 1)
   expect_equivalent(owmin(c(1,6,3)), 1)
   
})
