test_that("deterministic logistic response returns probabilities", {
  f <- response_logistic(beta = 2, delta = 0.5, gamma = 1, S = 0.3,
                         stochastic = FALSE)
  y <- c(0.2, 0.5, 0.9)
  expect_equal(f(1, y, NULL), stats::plogis(2 * (y - 0.5) + 0.3))
})

test_that("stochastic logistic response is binary and reproducible", {
  f <- response_logistic(beta = 2, delta = 0.5)
  y <- seq(0, 1, length.out = 20)
  set.seed(1)
  P1 <- f(1, y, NULL)
  set.seed(1)
  P2 <- f(1, y, NULL)
  expect_identical(P1, P2)
  expect_true(all(P1 %in% c(0L, 1L)))
})

test_that("threshold response maps z to -1, 0, +1", {
  f <- response_threshold(delta = 0.5, theta = 0.4)
  expect_identical(f(1, c(0.05, 0.1, 0.5, 0.9, 0.95), NULL),
                   c(-1L, 0L, 0L, 0L, 1L))
})

test_that("pressure shifts the threshold response", {
  f <- response_threshold(delta = 0.5, theta = 0.4, gamma = 0.9, S = 1)
  expect_identical(f(1, c(0.5, -0.1), NULL), c(1L, 0L))
})

test_that("functional pressure uses the previous manifest state", {
  f <- response_threshold(delta = 0.5, theta = 0.4, gamma = 0.9,
                          S = function(P) climate_balance(P, eps = 0.1))
  y <- c(0.5, 0.5, 0.5)
  expect_identical(f(1, y, NULL), c(0L, 0L, 0L))
  expect_identical(f(1, y, c(1, 1, 1)), c(1L, 1L, 1L))
})

test_that("stochastic threshold response is reproducible", {
  f <- response_threshold(delta = 0.5, theta = 0.4, stochastic = TRUE)
  y <- seq(0, 1, length.out = 20)
  set.seed(2)
  P1 <- f(1, y, NULL)
  set.seed(2)
  expect_identical(P1, f(1, y, NULL))
  expect_true(all(P1 %in% c(-1L, 0L, 1L)))
})

test_that("climate_balance follows its definition", {
  expect_equal(climate_balance(c(1, 1, 1, 0, 0), eps = 0.1), 3 / 3.1)
  expect_equal(climate_balance(c(1, -1, 0), eps = 0.1), 0)
  expect_equal(climate_balance(c(0, 0, 0), eps = 0.1), 0)
})

test_that("invalid response arguments raise errors", {
  expect_error(response_threshold(theta = 0), "theta")
  expect_error(response_threshold(theta = 0.4, S = "a"), "S")
  expect_error(response_logistic(delta = c(0, 1)), "delta")
  expect_error(climate_balance(c(1, 0), eps = 0), "eps")
})
