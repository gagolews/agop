require('testthat')

test_that("rel_is_irreflexive", {
   
   expect_false(rel_is_irreflexive(matrix(c(TRUE), nrow=1)))
   expect_false(rel_is_irreflexive(matrix(c(TRUE, FALSE, FALSE, TRUE), nrow=2)))
   expect_true(rel_is_irreflexive(matrix(c(FALSE), nrow=1)))
   expect_true(is.na(rel_is_irreflexive(matrix(c(NA, 0, 0, 1), nrow=2))))

})

