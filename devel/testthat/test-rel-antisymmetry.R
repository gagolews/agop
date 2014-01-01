require('testthat')

test_that("rel_is_antisymmetric", {

   expect_false(rel_is_antisymmetric(matrix(c(TRUE), nrow=4, ncol=4)))
   expect_true(rel_is_antisymmetric(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(rel_is_antisymmetric(matrix(c(FALSE), nrow=1)))
   expect_false(is.na(rel_is_antisymmetric(matrix(c(1, NA, 0, 1), nrow=2))))
   expect_false(is.na(rel_is_antisymmetric(matrix(c(1, 0, NA, 1), nrow=2))))
   expect_false(is.na(rel_is_antisymmetric(matrix(c(NA, 0, NA, NA), nrow=2))))
   expect_true(is.na(rel_is_antisymmetric(matrix(c(1, NA, 1, 1), nrow=2))))
   expect_true(is.na(rel_is_antisymmetric(matrix(c(1, 1, NA, 1), nrow=2))))
   expect_true(is.na(rel_is_antisymmetric(matrix(c(1, NA, NA, 1), nrow=2))))
   expect_true(rel_is_antisymmetric(matrix(c(FALSE, TRUE, FALSE, FALSE), nrow=2)))
   expect_true(rel_is_antisymmetric(matrix(c(FALSE, FALSE, TRUE, FALSE), nrow=2)))
   expect_false(rel_is_antisymmetric(matrix(c(FALSE, TRUE, TRUE, FALSE), nrow=2)))

})
