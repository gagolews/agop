require('testthat')

test_that("rel_is_cyclic", {

   expect_true(is.na(rel_is_cyclic(matrix(c(NA), nrow=1))))
   expect_true(is.na(rel_is_cyclic(matrix(c(0,NA,1,1), nrow=2))))
   
   expect_false(rel_is_cyclic(matrix(c(FALSE), nrow=1)))
   expect_false(rel_is_cyclic(matrix(c(TRUE), nrow=1)))
   expect_false(rel_is_cyclic(diag(10)))
   expect_true(rel_is_cyclic(matrix(c(0,1,1,0),nrow=2)))
   expect_false(rel_is_cyclic(matrix(c(0,1,0,0),nrow=2)))
   expect_true(rel_is_cyclic(matrix(c(0,1,0,
                                      0,0,1,
                                      1,0,0),nrow=3)))
   expect_true(rel_is_cyclic(matrix(c(0,1,0,0,
                                      0,0,1,0,
                                      0,0,0,1,
                                      1,0,0,0),nrow=4)))
   expect_false(rel_is_cyclic(matrix(c(0,1,0,0,
                                      0,1,0,0,
                                      0,0,0,1,
                                      1,0,0,0),nrow=4)))
})
