require('testthat')

test_that("is_reflexive", {
   
   expect_true(is_reflexive(matrix(c(TRUE), nrow=1)))
   expect_true(is_reflexive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_false(is_reflexive(matrix(c(FALSE), nrow=1)))
   expect_true(is_reflexive(matrix(c(1, 0, 0, 1), nrow=2)))
   expect_false(is_reflexive(matrix(c(1, 0, 0, 0), nrow=2)))
   expect_false(is_reflexive(Matrix(c(1, 0, 0, 0), nrow=2)))
   expect_true(is_reflexive(Matrix(c(999, 0, 0, 100), nrow=2)))
   
})
