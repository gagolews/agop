require('testthat')


test_that("check_comonotonicity", {


   expect_identical(check_comonotonicity(1, NA), NA)
   expect_identical(check_comonotonicity(1:2, 1), NA)
   expect_identical(check_comonotonicity(1:2, 1, incompatible_lengths = FALSE), FALSE)

   expect_identical(check_comonotonicity(c(1:100, NA), 1:101), NA)

   expect_identical(check_comonotonicity(c(1, 5, 3, 2, 4), c(10, 100, 10, 10, 50)), TRUE)

   expect_identical(check_comonotonicity(1:100, 101:200), TRUE)
   expect_identical(check_comonotonicity(1:100, 100:1), FALSE)

   set.seed(123)
   for (i in 1:100) {
      x <- sort(sample(runif(1000), replace=TRUE, 1000))
      y <- sort(sample(runif(1000), replace=TRUE, 1000))
      o <- sample(1:length(x))
      expect_identical(check_comonotonicity(x[o], y[o]), TRUE)

      expect_identical(check_comonotonicity(x[o], rep(0, length(x))), TRUE)

      expect_identical(check_comonotonicity(x[o], y[rev(o)]), FALSE)
   }
})

