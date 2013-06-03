require('testthat')


test_that("index_h", {
   
   expect_error(index_h(c(-1, 4, 3)))
   expect_error(index_h(c(4, 4, -1)))
   expect_equivalent(index_h(numeric(0)), numeric(0))
   expect_equivalent(index_h(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_h(c(1:10, NA)), NA_real_)
   expect_equivalent(index_h(1:10), 5)
   expect_equivalent(index_h(10:1), 5)
   expect_equivalent(index_h(c(3,3,3)), 3)
   expect_equivalent(index_h(c(3,3,3,3)), 3)
   expect_equivalent(index_h(c(3.1,3.1,3.1,3.1)), 3)
   expect_equivalent(index_h(c(0.99)), 0)
   expect_equivalent(index_h(0), 0)
   expect_equivalent(index_h(rep(1,1)), 1)
   expect_equivalent(index_h(rep(100,100)), 100)
   expect_equivalent(index_h(rep(1000000,1000000)), 1000000)
   
})
