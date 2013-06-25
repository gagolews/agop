require('testthat')


test_that("pareto2", {
   
   expect_equivalent(ppareto2(0.4), 1-ppareto2(0.4, lower.tail=FALSE))
   expect_true(all(rpareto2(1000) >= 0))
   expect_true(all(diff(order(dpareto2(seq(0.1,10,by=0.1)), decreasing=TRUE)) == 1))
   
})
