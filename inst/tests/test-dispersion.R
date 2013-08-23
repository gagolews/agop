require('testthat')


test_that("pord_spread, pord_spreadsym", {
   
   expect_equivalent(pord_spread(rep(1,10), rep(-100, 10)), TRUE)
   expect_equivalent(pord_spreadsym(rep(1,10), rep(-100, 10)), TRUE)
   
   expect_equivalent(pord_spread(1:3, c(1,NA,3)), NA)
   expect_equivalent(pord_spreadsym(1:3, c(1,NA,3)), NA)
   
   expect_equivalent(pord_spread(c(1,3,2), c(1,5,2)-1000), TRUE)
   expect_equivalent(pord_spreadsym(c(1,3,2), c(1,5,2)-1000), TRUE)
   
   expect_equivalent(pord_spread(c(1,3,2), c(1,4,3)-1000), TRUE)
   expect_equivalent(pord_spreadsym(c(1,3,2), c(1,4,3)-1000), TRUE)
   
   expect_equivalent(pord_spread(c(1,3,2), c(1,2,5)-1000), FALSE)
   expect_equivalent(pord_spreadsym(c(1,3,2), c(1,2,5)-1000), TRUE)
   
   expect_equivalent(pord_spread(c(1,3,2), c(1,3,2.5)-1000), FALSE)
   expect_equivalent(pord_spreadsym(c(1,3,2), c(1,3,2.5)-1000), FALSE)
   expect_equivalent(pord_spreadsym(c(1,3,2), c(1,2.5,3)-1000), FALSE)
   
   expect_equivalent(pord_spread(1:10, 2*(1:10)-5), TRUE)
   expect_equivalent(pord_spreadsym(1:10, 2*(1:10)-5), TRUE)
   
   expect_equivalent(pord_spread(1:10, 2*(10:1)-5), FALSE)
   expect_equivalent(pord_spreadsym(1:10, 2*(10:1)-5), TRUE)
})
