require('testthat')

test_that("is_reflexive", {
   
   expect_true(is_reflexive(matrix(c(TRUE), nrow=1)))
   expect_true(is_reflexive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_false(is_reflexive(matrix(c(FALSE), nrow=1)))
   expect_true(is_reflexive(matrix(c(1, 0, 0, 1), nrow=2)))
   expect_true(is.na(is_reflexive(matrix(c(1, 0, 0, NA), nrow=2))))
   expect_false(is_reflexive(matrix(c(1, 0, 0, 0), nrow=2)))
   expect_false(is_reflexive(Matrix(c(1, 0, 0, 0), nrow=2)))
   expect_true(is_reflexive(Matrix(c(999, 0, 0, 100), nrow=2)))
   
})

test_that("is_total", {
   
   expect_true(is_total(matrix(c(TRUE), nrow=1)))
   expect_false(is_total(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_false(is_total(matrix(c(TRUE, FALSE, TRUE, FALSE), nrow=2)))
   expect_true(is_total(matrix(c(TRUE, FALSE, TRUE, TRUE), nrow=2)))
   expect_true(is.na(is_total(matrix(c(TRUE,NA,FALSE,TRUE), nrow=2))))
   expect_true(is_total(matrix(c(TRUE,NA,TRUE,TRUE), nrow=2)))
   
})

test_that("is_transitive", {
   
   expect_true(is_transitive(matrix(c(TRUE), nrow=1)))
   expect_true(is_transitive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(is_transitive(matrix(c(FALSE, FALSE, FALSE, FALSE), nrow=2)))
   
   expect_false(is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_false(is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_false(is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_true(is_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
})
