require('testthat')


test_that("pord_weakdom", {
   
   expect_equivalent(pord_weakdom(c(10:1), c(20:1)), TRUE)
   expect_equivalent(pord_weakdom(c(20:1), c(10:1)), FALSE)
   expect_equivalent(pord_weakdom(c(2,2,2), c(3,1)), FALSE)
   expect_equivalent(pord_weakdom(c(3,1), c(2,2,2)), FALSE)

   
})
