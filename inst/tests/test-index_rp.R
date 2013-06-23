require('testthat')


test_that("index_rp", {
   
   expect_error(index_rp(1:10, -1))
   expect_error(index_rp(c(-1, 4, 3)))
   expect_error(index_rp(c(4, 4, -1)))
   expect_equivalent(index_rp(numeric(0)), numeric(0))
   expect_equivalent(index_rp(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_rp(c(1:10, NA)), NA_real_)
   expect_equivalent(index_rp(1:10), 5)
   expect_equivalent(index_rp(10:1), 5)
   expect_equivalent(index_rp(c(3,3,3)), 3)
   expect_equivalent(index_rp(c(3,3,3,3)), 3)
   expect_equivalent(index_rp(c(3.1,3.1,3.1,3.1)), 3.1)
   expect_equivalent(index_rp(c(0.99)), 0.99)
   expect_equivalent(index_rp(0), 0)
   expect_equivalent(index_rp(rep(1,1)), 1)
   expect_equivalent(index_rp(rep(100,100)), 100)
   expect_equivalent(index_rp(rep(1000000,1000000)), 1000000)
   
   expect_equivalent(index_rp(c(3,3,3),1), 3)
   expect_equivalent(index_rp(c(1:3),1), 3)
   expect_equivalent(index_rp(c(0),1), 0)
   expect_equivalent(index_rp(c(0.1),1), 0.1)
   expect_equivalent(index_rp(c(2,1),2), sqrt(2))
   
})
