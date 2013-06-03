require('testthat')


test_that("index_h", {
   
   expect_error(index_g(c(-1, 4, 3)))
   expect_error(index_g(c(4, 4, -1)))
   expect_equivalent(index_g(numeric(0)), numeric(0))
   expect_equivalent(index_g(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_g(c(1:10, NA)), NA_real_)
   expect_equivalent(index_g(c(1,3)), 2)
   expect_equivalent(index_g(c(9,0,0)), 3)
   expect_equivalent(index_g(c(9)), 1)
   
})
