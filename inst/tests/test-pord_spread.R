require('testthat')


test_that("pord_spread", {

   expect_equivalent(pord_spread(1:10, 101:110), TRUE)
   expect_equivalent(pord_spread(101:110, 1:10), TRUE)
   expect_equivalent(pord_spread(1:10, 2*(1:10)), TRUE)
   expect_equivalent(pord_spread(2*(1:10), 1:10), FALSE)

})

test_that("pord_spreadsym", {

   expect_equivalent(pord_spreadsym(1:10, 101:110), TRUE)
   expect_equivalent(pord_spreadsym(101:110, 1:10), TRUE)
   expect_equivalent(pord_spreadsym(1:10, 2*(1:10)), TRUE)
   expect_equivalent(pord_spreadsym(sample(2*(1:10)), 1:10), FALSE)
   expect_equivalent(pord_spreadsym(sample(2*(1:10)), 1:10), FALSE)

})
