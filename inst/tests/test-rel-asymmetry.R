require('testthat')

test_that("rel_is_asymmetric", {

   expect_false(rel_is_asymmetric(matrix(c(TRUE), nrow=1)))
   expect_false(rel_is_asymmetric(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(rel_is_asymmetric(matrix(c(FALSE), nrow=1)))
   expect_true(is.na(rel_is_asymmetric(matrix(c(NA, 0, 0, 1), nrow=2))))
   expect_true(rel_is_asymmetric(matrix(c(FALSE, TRUE, FALSE, FALSE), nrow=2)))
   expect_true(rel_is_asymmetric(matrix(c(FALSE, FALSE, TRUE, FALSE), nrow=2)))
   expect_false(rel_is_asymmetric(matrix(c(FALSE, TRUE, TRUE, FALSE), nrow=2)))

})
