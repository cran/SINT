W <- matrix(c(0.5, 0.5, 0,
              0.2, 0.6, 0.2,
              0,   0.3, 0.7), nrow = 3, byrow = TRUE)
lambda <- c(0.8, 0.5, 0.9)
y0 <- c(0, 0.5, 1)

test_that("rows of V sum to one for non-negative W", {
  V <- fj_influence(W, lambda)
  expect_equal(rowSums(V), rep(1, 3))
})

test_that("closed-form equilibrium matches the fixed-point equation", {
  y <- fj_equilibrium(W, lambda, y0)
  expect_equal(y, drop(lambda * W %*% y) + (1 - lambda) * y0)
})

test_that("fully anchored agents keep their initial opinions", {
  expect_equal(fj_equilibrium(W, 0, y0), y0)
})

test_that("signed networks have a valid equilibrium", {
  Ws <- matrix(c(0.6, -0.4, 0,
                 0.3,  0.5, -0.2,
                 0,   -0.3, 0.7), nrow = 3, byrow = TRUE)
  y <- fj_equilibrium(Ws, 0.9, y0)
  expect_equal(y, drop(0.9 * Ws %*% y) + 0.1 * y0)
})

test_that("names of W are carried to the results", {
  Wn <- W
  dimnames(Wn) <- list(c("a", "b", "c"), c("a", "b", "c"))
  expect_named(fj_equilibrium(Wn, lambda, y0), c("a", "b", "c"))
  expect_equal(dimnames(fj_influence(Wn, lambda)), dimnames(Wn))
})

test_that("non-convergent dynamics raise an error", {
  expect_error(fj_equilibrium(W, 1, y0), "do not converge")
  expect_error(fj_equilibrium(W, lambda, y0[1:2]), "length")
})
