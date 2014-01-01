require('testthat')


test_that("pareto2", {

   expect_equivalent(ppareto2(0.4), 1-ppareto2(0.4, lower.tail=FALSE))
   expect_true(all(rpareto2(1000) >= 0))
   expect_true(all(diff(order(dpareto2(seq(0.1,10,by=0.1)), decreasing=TRUE)) == 1))
   expect_warning(ppareto2(1, 1:3, 2:3))
   
   q <- 0:100; k <- 1; s <- 2
   expect_equivalent(ppareto2(q, k, s), ifelse(q<0, 0, (1-(s/(s+q))^k)))
   
   q <- 0.5; k <- seq(1, 2, 0.1); s <- 2
   expect_equivalent(ppareto2(q, k, s), (1-(s/(s+q))^k))
   
   q <- c(NA, 1, 2); k <- 1; s <- 2
   expect_equivalent(ppareto2(q, k, s), (1-(s/(s+q))^k))
   
   expect_error(ppareto2(1, -1, 1))
   expect_error(ppareto2(1, 1, c(1,-1)))
   expect_error(ppareto2(1, 1, -1))
   expect_error(ppareto2(1, c(1, -1), 1))
})
