require('testthat')

test_that("rel_is_symmetric", {

   expect_true(rel_is_symmetric(matrix(c(TRUE), nrow=1)))
   expect_true(rel_is_symmetric(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(rel_is_symmetric(matrix(c(FALSE), nrow=1)))
   expect_true(rel_is_symmetric(matrix(c(FALSE, TRUE, TRUE, FALSE), nrow=2)))
   expect_false(rel_is_symmetric(matrix(c(FALSE, TRUE, FALSE, FALSE), nrow=2)))

})



test_that("rel_closure_symmetric", {

   expect_equivalent(rel_closure_symmetric(
         matrix(0,ncol=4, nrow=4, byrow=TRUE)
      ),
      matrix(0,ncol=4, nrow=4, byrow=TRUE))


   A <- structure(as.logical(diag(4)), dimnames=list(LETTERS[1:4], LETTERS[1:4]), dim=c(4,4))
   expect_identical(rel_closure_symmetric(A), A)

   # random graphs:
   expect_true(rel_is_symmetric(rel_closure_symmetric(
     matrix(runif(625)>0.1, ncol=25, byrow=TRUE))))

   expect_true(rel_is_symmetric(rel_closure_symmetric(
     matrix(runif(625)>0.3, ncol=25, byrow=TRUE))))

   expect_true(rel_is_symmetric(rel_closure_symmetric(
     matrix(runif(625)>0.5, ncol=25, byrow=TRUE))))

   expect_true(rel_is_symmetric(rel_closure_symmetric(
     matrix(runif(625)>0.7, ncol=25, byrow=TRUE))))

   expect_true(rel_is_symmetric(rel_closure_symmetric(
     matrix(runif(625)>0.9, ncol=25, byrow=TRUE))))
})
