require('testthat')


test_that("index_g", {
   
   expect_error(index_g(c(-1, 4, 3)))
   expect_error(index_g(c(4, 4, -1)))
   expect_equivalent(index_g(numeric(0)), numeric(0))
   expect_equivalent(index_g(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_g(c(1:10, NA)), NA_real_)
   expect_equivalent(index_g(c(1,3)), 2)
   expect_equivalent(index_g(c(9,0,0)), 3)
   expect_equivalent(index_g(c(9)), 1)
   expect_equivalent(index_g(0), 0)
   expect_equivalent(index_g(rep(1,1)), 1)
   expect_equivalent(index_g(rep(100,100)), 100)
   expect_equivalent(index_g(rep(1000000,1000000)), 1000000)
   
})




test_that("index_g_zi", {
   
   expect_error(index_g_zi(c(-1, 4, 3)))
   expect_error(index_g_zi(c(4, 4, -1)))
   expect_equivalent(index_g_zi(numeric(0)), numeric(0))
   expect_equivalent(index_g_zi(c(NA, 1:10)), NA_real_)
   expect_equivalent(index_g_zi(c(1:10, NA)), NA_real_)
   expect_equivalent(index_g_zi(c(1,3)), 2)
   expect_equivalent(index_g_zi(c(9,0,0)), 3)
   expect_equivalent(index_g_zi(c(9)), 3)
   expect_equivalent(index_g_zi(0), 0)
   expect_equivalent(index_g_zi(rep(1,1)), 1)
   expect_equivalent(index_g_zi(rep(100,100)), 100)
   expect_equivalent(index_g_zi(rep(1000000,1000000)), 1000000)
   
})
