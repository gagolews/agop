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



test_that("d2owa_checkwts, d2owa", {
   expect_error(d2owa_checkwts(c(-1,-2,3))) 
   expect_equivalent(d2owa_checkwts(c(0.5,0,0.5)), TRUE)
   expect_equivalent(d2owa_checkwts(c(0.5,0,0,0.5)), TRUE)
   expect_equivalent(d2owa_checkwts(c(0.5,0,0,0,0.5)), FALSE)
   expect_equivalent(d2owa_checkwts(rep(1,10)/10), TRUE)
   expect_equivalent(d2owa_checkwts(rep(1,1000)/1000), TRUE)
   expect_equivalent(d2owa_checkwts(rep(1,100000)/100000), TRUE)
   expect_equivalent(d2owa_checkwts(c(0,0,0.5,0.5,0,0,0,0)), TRUE)
   expect_equivalent(d2owa_checkwts(c(0,0,0,0,0,0,0,1)), TRUE)
   expect_equivalent(d2owa_checkwts(c(1,0,0,0,0,0,0,0)), TRUE)
   expect_equivalent(d2owa_checkwts(c(0.9,0,0,0,0,0,0,0.1)), TRUE)
   
   expect_equivalent(d2owa(rep(1,10)), 0)
   expect_equivalent(d2owa(c(1,5,4)) <= d2owa(c(1,6,4)), TRUE)
   expect_equivalent(d2owa(1:10), sd(1:10)*sqrt(9/10))
})