require('testthat')


test_that("index_maxprod", {
   
   expect_error(index_maxprod(c(-1, 4, 3)))
   expect_error(index_maxprod(c(4, 4, -1)))
   expect_equivalent(index_maxprod(numeric(0)), numeric(0))
   expect_equivalent(index_maxprod(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_maxprod(c(1:10, NA)), NA_real_)
   expect_equivalent(index_maxprod(c(1)), 1)
   expect_equivalent(index_maxprod(c(2,1,0)), 2)
   expect_equivalent(index_maxprod(c(2,1.5,0)), 3)
   expect_equivalent(index_maxprod(c(2,1.5,0.1,0.1,0.1,0.1,0.1)), 3)
   expect_equivalent(index_maxprod(rep(10,1)), 10)
   expect_equivalent(index_maxprod(rep(10,2)), 20)

   
})
