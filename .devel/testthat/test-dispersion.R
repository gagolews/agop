require('testthat')



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
