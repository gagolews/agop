require('testthat')

test_that("rel_is_reflexive", {
   
   expect_true(rel_is_reflexive(matrix(c(TRUE), nrow=1)))
   expect_true(rel_is_reflexive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_false(rel_is_reflexive(matrix(c(FALSE), nrow=1)))
   expect_true(rel_is_reflexive(matrix(c(1, 0, 0, 1), nrow=2)))
   expect_true(is.na(rel_is_reflexive(matrix(c(1, 0, 0, NA), nrow=2))))
   expect_false(rel_is_reflexive(matrix(c(1, 0, 0, 0), nrow=2)))
   expect_false(rel_is_reflexive(Matrix(c(1, 0, 0, 0), nrow=2)))
   expect_true(rel_is_reflexive(Matrix(c(999, 0, 0, 100), nrow=2)))
   
})

test_that("rel_is_total", {
   
   expect_true(rel_is_total(matrix(c(TRUE), nrow=1)))
   expect_false(rel_is_total(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_false(rel_is_total(matrix(c(TRUE, FALSE, TRUE, FALSE), nrow=2)))
   expect_false(rel_is_total(matrix(c(TRUE, TRUE, TRUE, FALSE), nrow=2)))
   expect_true(rel_is_total(matrix(c(TRUE, FALSE, TRUE, TRUE), nrow=2)))
   expect_true(is.na(rel_is_total(matrix(c(TRUE,NA,FALSE,TRUE), nrow=2))))
   expect_true(rel_is_total(matrix(c(TRUE,NA,TRUE,TRUE), nrow=2)))
   
})

test_that("rel_is_transitive", {
   
   expect_true(rel_is_transitive(matrix(c(TRUE), nrow=1)))
   expect_true(rel_is_transitive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(rel_is_transitive(matrix(c(FALSE, FALSE, FALSE, FALSE), nrow=2)))
   
   expect_false(rel_is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_false(rel_is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_false(rel_is_transitive(
     matrix(c(0,0,0,1,
              0,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
   
   expect_true(rel_is_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)))
})


test_that("rel_closure_transitive", {
   
   expect_equivalent(rel_closure_transitive(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0),ncol=4, byrow=TRUE)),
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE))
   
   expect_equivalent(rel_closure_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)),
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE))
   
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE))))
   
   # random graphs:
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(runif(625)>0.1, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(runif(625)>0.3, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(runif(625)>0.5, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(runif(625)>0.7, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_transitive(rel_closure_transitive(
     matrix(runif(625)>0.9, ncol=25, byrow=TRUE))))
})


test_that("rel_closure_reflexive", {
   
   expect_equivalent(rel_closure_reflexive(
     matrix(0,ncol=4, nrow=4, byrow=TRUE)),
     matrix(diag(4),ncol=4, byrow=TRUE))
   
   
   A <- structure(as.logical(diag(4)), dimnames=list(LETTERS[1:4], LETTERS[1:4]), dim=c(4,4))
   expect_identical(rel_closure_reflexive(A), A)
   
   # random graphs:
   expect_true(rel_is_reflexive(rel_closure_reflexive(
     matrix(runif(625)>0.1, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_reflexive(rel_closure_reflexive(
     matrix(runif(625)>0.3, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_reflexive(rel_closure_reflexive(
     matrix(runif(625)>0.5, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_reflexive(rel_closure_reflexive(
     matrix(runif(625)>0.7, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.9, ncol=25, byrow=TRUE))))
})



test_that("rel_closure_total_fair", {
   
   expect_equivalent(rel_closure_total_fair(
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0),ncol=4, byrow=TRUE)),
     matrix(c(1,1,0,1,
              1,1,1,1,
              1,0,1,1,
              0,1,1,1),ncol=4, byrow=TRUE))
   
   
   # random graphs:
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.1, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.3, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.5, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.7, ncol=25, byrow=TRUE))))
   
   expect_true(rel_is_total(rel_closure_total_fair(
     matrix(runif(625)>0.9, ncol=25, byrow=TRUE))))
})


