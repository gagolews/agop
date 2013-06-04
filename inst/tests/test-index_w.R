require('testthat')


test_that("index_w", {
   
   expect_error(index_w(c(-1, 4, 3)))
   expect_error(index_w(c(4, 4, -1)))
   expect_equivalent(index_w(numeric(0)), numeric(0))
   expect_equivalent(index_w(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_w(c(1:10, NA)), NA_real_)
   expect_equivalent(index_w(c(1)), 1)
   expect_equivalent(index_w(c(0.5)), 0)
   expect_equivalent(index_w(c(2)), 1)
   expect_equivalent(index_w(c(2,0,0,0,0,0,0,0)), 1)
   expect_equivalent(index_w(10:1), 10)
   expect_equivalent(index_w(110:101), 10)
   expect_equivalent(index_w(c(110:101,0.5)), 10)
   expect_equivalent(index_w(c(110:101,1)), 11)
   expect_equivalent(index_w(10:1+0.5), 10)
   expect_equivalent(index_w(10:1-0.5), 9)
   expect_equivalent(index_w(c(5,4,1,0)), 3)
   expect_equivalent(index_w(rep(10,10)), 10)
   
})
