require('testthat')


test_that("index_lp", {
   
   expect_error(index_lp(1:10, -1))
   expect_error(index_lp(c(-1, 4, 3)))
   expect_error(index_lp(c(4, 4, -1)))
   expect_equivalent(index_lp(numeric(0)), 0.0)
   expect_equivalent(index_lp(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_lp(c(1:10, NA)), NA_real_)
   expect_equivalent(index_lp(rep(10,10), Inf, identity), c(10,10))
   expect_equivalent(index_lp(rep(1:10), Inf, identity), c(5,6))
   expect_equivalent(index_lp(rep(1:10), 1,identity), c(10,10))
   
})
