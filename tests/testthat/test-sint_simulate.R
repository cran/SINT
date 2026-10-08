W <- matrix(c(0.5, 0.5, 0,
              0.2, 0.6, 0.2,
              0,   0.3, 0.7), nrow = 3, byrow = TRUE)
lambda <- c(0.8, 0.5, 0.9)
y0 <- c(0, 0.5, 1)

test_that("constant inputs reproduce fj_simulate", {
  sim <- sint_simulate(W, lambda, y0, steps = 20)
  ref <- fj_simulate(W, lambda, y0, max_steps = 20, tol = 0)
  expect_equal(sim$y, ref$trajectory)
  expect_null(sim$P)
})

test_that("identity response copies the latent trajectory", {
  sim <- sint_simulate(W, lambda, y0, steps = 10,
                       response = function(t, y, P) y)
  expect_equal(sim$P, sim$y)
})

test_that("time-varying inputs are called with the current time", {
  seen <- integer(0)
  lambda_fun <- function(t, y, P) {
    seen <<- c(seen, t)
    if (t == 0) 0 else lambda
  }
  sim <- sint_simulate(W, lambda_fun, y0, steps = 4)
  expect_identical(seen, 0:3)
  expect_equal(sim$y[2, ], y0)
})

test_that("state-dependent W receives the manifest state", {
  W_fun <- function(t, y, P) {
    expect_length(P, 3)
    W
  }
  f <- response_threshold(delta = 0.5, theta = 0.1)
  sim <- sint_simulate(W_fun, lambda, y0, steps = 3, response = f)
  expect_identical(dim(sim$P), c(4L, 3L))
})

test_that("initial manifest state is computed or taken from P0", {
  f <- response_threshold(delta = 0.5, theta = 0.1)
  expect_equal(sint_simulate(W, lambda, y0, 2, response = f)$P[1, ],
               c(-1, 0, 1))
  expect_equal(sint_simulate(W, lambda, y0, 2, response = f,
                             P0 = c(0, 0, 0))$P[1, ], c(0, 0, 0))
})

test_that("stochastic simulations are reproducible", {
  f <- response_threshold(delta = 0.5, theta = 0.4, stochastic = TRUE)
  set.seed(3)
  s1 <- sint_simulate(W, lambda, y0, steps = 5, response = f)
  set.seed(3)
  s2 <- sint_simulate(W, lambda, y0, steps = 5, response = f)
  expect_identical(s1$P, s2$P)
})

test_that("invalid inputs raise errors", {
  expect_error(sint_simulate(W, lambda, y0, steps = 0), "steps")
  expect_error(sint_simulate(W, lambda, y0, steps = 2, response = 1),
               "response")
  expect_error(sint_simulate(function(t, y, P) W * 2, lambda, y0, steps = 2),
               "row-stochastic")
  expect_error(sint_simulate(W, lambda, y0, steps = 2,
                             response = function(t, y, P) 1),
               "length n")
})
