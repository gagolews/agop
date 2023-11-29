require('testthat')




test_that("rel_reduction_hasse", {

   A <- structure(as.logical(diag(4)), dimnames=list(LETTERS[1:4], LETTERS[1:4]), dim=c(4,4))
   expect_identical(rel_reduction_hasse(A), rel_reduction_reflexive(A))

   expect_equivalent(rel_reduction_hasse(
     matrix(c(0,0,0,1,
              1,0,1,1,
              1,0,0,1,
              0,0,0,0), ncol=4, byrow=TRUE)),
     matrix(c(0,0,0,1,
              0,0,1,0,
              1,0,0,0,
              0,0,0,0), ncol=4, byrow=TRUE))

   expect_equivalent(rel_reduction_hasse(
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
   
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_hasse(A)))

   # random graphs:
   A <- rel_reduction_reflexive(matrix(runif(25*25)>0.9, ncol=25, byrow=TRUE))
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_hasse(A)))
   
   A <- rel_reduction_reflexive(matrix(runif(50*50)>0.5, ncol=50, byrow=TRUE))
   expect_equal(rel_closure_transitive(A), rel_closure_transitive(rel_reduction_hasse(A)))
})
