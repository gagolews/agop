require('testthat')


test_that("fuzzy implicaations", {

   expect_error(fimplication_minimum(c(), c(0, 1)))
   expect_error(fimplication_minimum(c(), c()))
   expect_error(fimplication_minimum(c(1), c(0,1)))
   expect_error(fimplication_minimum(c(0,1), c(0,1.1)))
   expect_error(fimplication_minimum(0.6, -1))
   expect_error(fimplication_minimum(2, 0.4))

   # testing well-known facts (on random data)
   x <- runif(1000)
   y <- runif(1000)
   i0  <- fimplication_minimal(x, y)
   i1  <- fimplication_maximal(x, y)
   ikd <- fimplication_kleene(x, y)
   il  <- fimplication_lukasiewicz(x, y)
   irb <- fimplication_reichenbach(x, y)

   expect_true(all(i0 <= ikd & ikd <= i1))
   expect_true(all(i0 <= il  & il  <= i1))
   expect_true(all(i0 <= irb & irb <= i1))

   # boundary conditions
   expect_equivalent(fimplication_minimal(c(1, 0, 1, 0), c(1, 0, 0, 1)), c(1, 1, 0, 1))
   expect_equivalent(fimplication_maximal(c(1, 0, 1, 0), c(1, 0, 0, 1)), c(1, 1, 0, 1))
   expect_equivalent(fimplication_kleene(c(1, 0, 1, 0), c(1, 0, 0, 1)), c(1, 1, 0, 1))
   expect_equivalent(fimplication_lukasiewicz(c(1, 0, 1, 0), c(1, 0, 0, 1)), c(1, 1, 0, 1))
   expect_equivalent(fimplication_reichenbach(c(1, 0, 1, 0), c(1, 0, 0, 1)), c(1, 1, 0, 1))


   # nondecreasing w.r.t. y
   z <- pmin(1, y+runif(length(y)))
   expect_true(all(fimplication_minimal(x, y) <= fimplication_minimal(x, z)))
   expect_true(all(fimplication_maximal(x, y) <= fimplication_maximal(x, z)))
   expect_true(all(fimplication_kleene(x, y) <= fimplication_kleene(x, z)))
   expect_true(all(fimplication_lukasiewicz(x, y) <= fimplication_lukasiewicz(x, z)))
   expect_true(all(fimplication_reichenbach(x, y) <= fimplication_reichenbach(x, z)))

   # nonincreasing w.r.t. x
   z <- pmax(0, x-runif(length(y)))
   expect_true(all(fimplication_minimal(x, y) <= fimplication_minimal(z, y)))
   expect_true(all(fimplication_maximal(x, y) <= fimplication_maximal(z, y)))
   expect_true(all(fimplication_kleene(x, y) <= fimplication_kleene(z, y)))
   expect_true(all(fimplication_lukasiewicz(x, y) <= fimplication_lukasiewicz(z, y)))
   expect_true(all(fimplication_reichenbach(x, y) <= fimplication_reichenbach(z, y)))
})
