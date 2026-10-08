# Parsegov, Proskurnikov, Tempo and Friedkin (2017), IEEE Transactions on
# Automatic Control 62(5), 2270-2285, doi:10.1109/TAC.2016.2613905.
# W from equation (2), Lambda = I - diag(W), initial opinions from
# equation (15) and equilibria from equation (16) of arXiv:1505.04920v5.

W <- matrix(c(0.220, 0.120, 0.360, 0.300,
              0.147, 0.215, 0.344, 0.294,
              0,     0,     1,     0,
              0.090, 0.178, 0.446, 0.286), nrow = 4, byrow = TRUE)
lambda <- 1 - diag(W)

test_that("the published example converges", {
  expect_true(fj_check(W, lambda)$converges)
})

test_that("equilibria match the published values on issue (a)", {
  y <- fj_equilibrium(W, lambda, c(25, 25, 75, 85))
  expect_equal(round(y), c(60, 60, 75, 75))
})

test_that("equilibria match the published values on issue (b)", {
  y <- fj_equilibrium(W, lambda, c(25, 15, -50, 5))
  expect_equal(round(y, 1), c(-19.3, -21.5, -50, -23.2))
})
