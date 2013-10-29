require('testthat')


test_that("discr. pareto2", {
   
   expect_equivalent(pdpareto2(0.4), 1-pdpareto2(0.4, lower.tail=FALSE))
   expect_true(all(rdpareto2(1000) >= 0))
   expect_true(all((x <- rdpareto2(1000)) == floor(x)))
   expect_equivalent(cumsum(ddpareto2(0:10, 10, 10)), pdpareto2(0:10, 10, 10))
   expect_equivalent(ddpareto2(c(-1.5, -1, -0.5, 0, 0.5, 1)),
      c(0,0,0,ddpareto2(0),0,ddpareto2(1)))
   expect_equivalent(pdpareto2(1:10), pdpareto2(1:10+0.999))
   expect_true(all(pdpareto2(1:10)>pdpareto2(1:10-0.001)))
#    expect_equivalent(0:10, qdpareto2(pdpareto2(0:10)))
#    expect_equivalent(1:11, qdpareto2(pdpareto2(0:10)+1e-5))
})
