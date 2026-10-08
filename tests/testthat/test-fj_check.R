W <- matrix(c(0.5, 0.5, 0,
              0.2, 0.6, 0.2,
              0,   0.3, 0.7), nrow = 3, byrow = TRUE)

test_that("valid inputs are reported as convergent", {
  chk <- fj_check(W, c(0.8, 0.5, 0.9))
  expect_equal(chk$n, 3L)
  expect_false(chk$signed)
  expect_true(chk$converges)
  expect_lte(chk$spectral_radius, chk$norm_inf)
  expect_equal(chk$norm_inf, 0.9)
})

test_that("a scalar lambda is recycled", {
  expect_equal(fj_check(W, 0.5)$norm_inf, 0.5)
})

test_that("DeGroot case (lambda = 1) has spectral radius one", {
  chk <- fj_check(W, 1)
  expect_equal(chk$spectral_radius, 1)
  expect_false(chk$converges)
})

test_that("partially stubborn networks can converge", {
  expect_true(fj_check(W, c(1, 1, 0.5))$converges)
})

test_that("signed matrices are accepted when absolute row sums are <= 1", {
  Ws <- matrix(c(0.6, -0.4, 0,
                 0.3,  0.5, -0.2,
                 0,   -0.3, 0.7), nrow = 3, byrow = TRUE)
  chk <- fj_check(Ws, 0.9)
  expect_true(chk$signed)
  expect_true(chk$converges)
})

test_that("invalid inputs raise errors", {
  expect_error(fj_check(W[1:2, ], 0.5), "square")
  expect_error(fj_check(W, c(0.5, 0.5)), "length")
  expect_error(fj_check(W, 1.2), "\\[0, 1\\]")
  expect_error(fj_check(W, -0.1), "\\[0, 1\\]")
  expect_error(fj_check(W * 0.9, 0.5), "row-stochastic")
  Ws <- matrix(c(0.8, -0.4, 0.5, 0.5), nrow = 2, byrow = TRUE)
  expect_error(fj_check(Ws, 0.5), "absolute row sums")
  expect_error(fj_check("a", 0.5), "numeric matrix")
})
