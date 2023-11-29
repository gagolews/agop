require('testthat')


test_that("tnorms", {

   expect_error(tnorm_minimum(c(), c(0, 1)))
   expect_error(tnorm_minimum(c(), c()))
   expect_error(tnorm_minimum(c(1), c(0,1)))
   expect_error(tnorm_minimum(c(0,1), c(0,1.1)))

   # testing well-known facts (on random data)
   x <- runif(1000)
   y <- runif(1000)
   tm <- tnorm_minimum(x, y)
   tp <- tnorm_product(x, y)
   tl <- tnorm_lukasiewicz(x, y)
   td <- tnorm_drastic(x, y)
   tf <- tnorm_fodor(x, y)

   expect_true(all(td <= tp & tp <= tm))
   expect_true(all(td <= tl & tl <= tm))
   expect_true(all(td <= tf & tf <= tm))
   expect_true(all(tl <= tp))

   # commutative
   expect_equivalent(tnorm_minimum(x, y), tnorm_minimum(y, x))
   expect_equivalent(tnorm_product(x, y), tnorm_product(y, x))
   expect_equivalent(tnorm_lukasiewicz(x, y), tnorm_lukasiewicz(y, x))
   expect_equivalent(tnorm_drastic(x, y), tnorm_drastic(y, x))
   expect_equivalent(tnorm_fodor(x, y), tnorm_fodor(y, x))

   # 0 annihilator
   expect_equivalent(tnorm_minimum(x, rep(0, length(x))), rep(0, length(x)))
   expect_equivalent(tnorm_product(x, rep(0, length(x))), rep(0, length(x)))
   expect_equivalent(tnorm_lukasiewicz(x, rep(0, length(x))), rep(0, length(x)))
   expect_equivalent(tnorm_drastic(x, rep(0, length(x))), rep(0, length(x)))
   expect_equivalent(tnorm_fodor(x, rep(0, length(x))), rep(0, length(x)))

   # 1 neutral
   expect_equivalent(tnorm_minimum(x, rep(1, length(x))), x)
   expect_equivalent(tnorm_product(x, rep(1, length(x))), x)
   expect_equivalent(tnorm_lukasiewicz(x, rep(1, length(x))), x)
   expect_equivalent(tnorm_drastic(x, rep(1, length(x))), x)
   expect_equivalent(tnorm_fodor(x, rep(1, length(x))), x)

   # nondecreasing
   z <- pmin(1, y+runif(length(y)))
   expect_true(all(tnorm_minimum(x, y) <= tnorm_minimum(x, z)))
   expect_true(all(tnorm_product(x, y) <= tnorm_product(x, z)))
   expect_true(all(tnorm_lukasiewicz(x, y) <= tnorm_lukasiewicz(x, z)))
   expect_true(all(tnorm_drastic(x, y) <= tnorm_drastic(x, z)))
   expect_true(all(tnorm_fodor(x, y) <= tnorm_fodor(x, z)))


   # associative
   z <- runif(length(z))
   expect_equivalent(tnorm_minimum(x, tnorm_minimum(y, z)), tnorm_minimum(tnorm_minimum(x, y), z))
   expect_equivalent(tnorm_product(x, tnorm_product(y, z)), tnorm_product(tnorm_product(x, y), z))
   expect_equivalent(tnorm_lukasiewicz(x, tnorm_lukasiewicz(y, z)), tnorm_lukasiewicz(tnorm_lukasiewicz(x, y), z))
   expect_equivalent(tnorm_drastic(x, tnorm_drastic(y, z)), tnorm_drastic(tnorm_drastic(x, y), z))
   expect_equivalent(tnorm_fodor(x, tnorm_fodor(y, z)), tnorm_fodor(tnorm_fodor(x, y), z))
})
