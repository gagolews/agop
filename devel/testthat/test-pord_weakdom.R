require('testthat')


test_that("pord_weakdom", {

   expect_equivalent(pord_weakdom(c(10:1), c(20:1)), TRUE)
   expect_equivalent(pord_weakdom(c(20:1), c(10:1)), FALSE)
   expect_equivalent(pord_weakdom(c(2,2,2), c(3,1)), FALSE)
   expect_equivalent(pord_weakdom(c(3,1), c(2,2,2)), FALSE)


   expect_equivalent(pord_weakdom(c(10:1)-10, c(20:1)-10), TRUE)
})


test_that("pord_nd", {

   expect_equivalent(pord_nd(c(10:1), c(10:1)+1), TRUE)
   expect_equivalent(pord_nd(c(10:1), c(10:1)-1), FALSE)
   expect_equivalent(pord_nd(c(10:1), c(10:1)), TRUE)

   expect_equivalent(pord_nd(c(10:1), c(10:3)), FALSE)
   expect_equivalent(pord_nd(c(10:1), c(10:2, 2)), TRUE)
   expect_equivalent(pord_nd(c(10:1), c(1:10)), FALSE)
})
