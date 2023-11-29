require('testthat')


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

   A <- structure(as.logical(diag(4)), dimnames=list(LETTERS[1:4], LETTERS[1:4]), dim=c(4,4))
   expect_identical(rel_closure_transitive(A), A)

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


test_that("rel_reduction_transitive", {

   A <- structure(as.logical(diag(4)), dimnames=list(LETTERS[1:4], LETTERS[1:4]), dim=c(4,4))
   expect_identical(rel_reduction_transitive(A), A)
   
   expect_error(rel_reduction_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,1,0,1,
              0,0,0,0), ncol=4, byrow=TRUE)))

   expect_equivalent(rel_reduction_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0), ncol=4, byrow=TRUE)),
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0), ncol=4, byrow=TRUE))

   expect_equivalent(rel_reduction_transitive(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)),
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0),ncol=4, byrow=TRUE))

   A <- matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0),ncol=4, byrow=TRUE)
   
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_transitive(A)))

   # random graphs:
   repeat {
      A <- matrix(runif(25*25)>0.9, ncol=25, byrow=TRUE)
      if (!rel_is_cyclic(A)) break
   }
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_transitive(A)))
   
   repeat {
      A <- matrix(runif(10*10)>0.7, ncol=10, byrow=TRUE)
      if (!rel_is_cyclic(A)) break
   }
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_transitive(A)))
})
