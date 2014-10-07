require('testthat')


test_that("fuzzy_negation", {

   expect_error(fnegation_classic(c()))
   expect_error(fnegation_classic(-1.0))
   expect_error(fnegation_classic(2.0))

   expect_equivalent(fnegation_classic(c(0.0, 1.0)), c(1.0, 0.0))
   expect_equivalent(fnegation_minimal(c(0.0, 1.0)), c(1.0, 0.0))
   expect_equivalent(fnegation_maximal(c(0.0, 1.0)), c(1.0, 0.0))
   expect_equivalent(fnegation_yager(c(0.0, 1.0)), c(1.0, 0.0))

   # testing well-known facts (on random data)
   x <- runif(1000)
   nc <- fnegation_classic(x)
   n0 <- fnegation_minimal(x)
   n1 <- fnegation_maximal(x)
   ny <- fnegation_yager(x)

   expect_true(all(n0 <= nc & nc <= n1))
   expect_true(all(n0 <= ny & ny <= n1))


   # nonincreasing
   y <- pmin(1, x+runif(length(x)))
   expect_true(all(fnegation_classic(x) >= fnegation_classic(y)))
   expect_true(all(fnegation_minimal(x) >= fnegation_minimal(y)))
   expect_true(all(fnegation_maximal(x) >= fnegation_maximal(y)))
   expect_true(all(fnegation_yager(x) >= fnegation_yager(y)))
})
