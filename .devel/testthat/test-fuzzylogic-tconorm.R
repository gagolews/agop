require('testthat')


test_that("tconorms", {

   expect_error(tconorm_minimum(c(), c(0, 1)))
   expect_error(tconorm_minimum(c(), c()))
   expect_error(tconorm_minimum(c(1), c(0,1)))
   expect_error(tconorm_minimum(c(0,1), c(0,1.1)))
   expect_error(tconorm_minimum(-1, 1))

   # testing well-known facts (on random data)
   x <- runif(1000)
   y <- runif(1000)
   tm <- tconorm_minimum(x, y)
   tp <- tconorm_product(x, y)
   tl <- tconorm_lukasiewicz(x, y)
   td <- tconorm_drastic(x, y)
   tf <- tconorm_fodor(x, y)

   expect_true(all(td >= tp & tp >= tm))
   expect_true(all(td >= tl & tl >= tm))
   expect_true(all(td >= tf & tf >= tm))
   expect_true(all(tl >= tp))

   # commutative
   expect_equivalent(tconorm_minimum(x, y), tconorm_minimum(y, x))
   expect_equivalent(tconorm_product(x, y), tconorm_product(y, x))
   expect_equivalent(tconorm_lukasiewicz(x, y), tconorm_lukasiewicz(y, x))
   expect_equivalent(tconorm_drastic(x, y), tconorm_drastic(y, x))
   expect_equivalent(tconorm_fodor(x, y), tconorm_fodor(y, x))

   # 0 annihilator
   expect_equivalent(tconorm_minimum(x, rep(1, length(x))), rep(1, length(x)))
   expect_equivalent(tconorm_product(x, rep(1, length(x))), rep(1, length(x)))
   expect_equivalent(tconorm_lukasiewicz(x, rep(1, length(x))), rep(1, length(x)))
   expect_equivalent(tconorm_drastic(x, rep(1, length(x))), rep(1, length(x)))
   expect_equivalent(tconorm_fodor(x, rep(1, length(x))), rep(1, length(x)))

   # 1 neutral
   expect_equivalent(tconorm_minimum(x, rep(0, length(x))), x)
   expect_equivalent(tconorm_product(x, rep(0, length(x))), x)
   expect_equivalent(tconorm_lukasiewicz(x, rep(0, length(x))), x)
   expect_equivalent(tconorm_drastic(x, rep(0, length(x))), x)
   expect_equivalent(tconorm_fodor(x, rep(0, length(x))), x)

   # nondecreasing
   z <- pmin(1, y+runif(length(y)))
   expect_true(all(tconorm_minimum(x, y) <= tconorm_minimum(x, z)))
   expect_true(all(tconorm_product(x, y) <= tconorm_product(x, z)))
   expect_true(all(tconorm_lukasiewicz(x, y) <= tconorm_lukasiewicz(x, z)))
   expect_true(all(tconorm_drastic(x, y) <= tconorm_drastic(x, z)))
   expect_true(all(tconorm_fodor(x, y) <= tconorm_fodor(x, z)))

   expect_equivalent(tconorm_minimum(x, y),
      1-tnorm_minimum(1-x, 1-y))
   expect_equivalent(tconorm_product(x, y),
      1-tnorm_product(1-x, 1-y))
   expect_equivalent(tconorm_lukasiewicz(x, y),
      1-tnorm_lukasiewicz(1-x, 1-y))
   expect_equivalent(tconorm_drastic(x, y),
      1-tnorm_drastic(1-x, 1-y))
   expect_equivalent(tconorm_fodor(x, y),
      1-tnorm_fodor(1-x, 1-y))

   # associative
   z <- runif(length(z))
   expect_equivalent(tconorm_minimum(x, tconorm_minimum(y, z)), tconorm_minimum(tconorm_minimum(x, y), z))
   expect_equivalent(tconorm_product(x, tconorm_product(y, z)), tconorm_product(tconorm_product(x, y), z))
   expect_equivalent(tconorm_lukasiewicz(x, tconorm_lukasiewicz(y, z)), tconorm_lukasiewicz(tconorm_lukasiewicz(x, y), z))
   expect_equivalent(tconorm_drastic(x, tconorm_drastic(y, z)), tconorm_drastic(tconorm_drastic(x, y), z))
   expect_equivalent(tconorm_fodor(x, tconorm_fodor(y, z)), tconorm_fodor(tconorm_fodor(x, y), z))
})
