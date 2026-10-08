W <- matrix(c(0.5, 0.5, 0,
              0.2, 0.6, 0.2,
              0,   0.3, 0.7), nrow = 3, byrow = TRUE)
lambda <- c(0.8, 0.5, 0.9)
y0 <- c(0, 0.5, 1)

test_that("iteration converges to the closed-form equilibrium", {
  sim <- fj_simulate(W, lambda, y0)
  expect_true(sim$converged)
  expect_equal(sim$trajectory[sim$steps + 1L, ],
               fj_equilibrium(W, lambda, y0), tolerance = 1e-8)
})

test_that("the trajectory starts at y0 and has steps + 1 rows", {
  sim <- fj_simulate(W, lambda, y0, max_steps = 5)
  expect_equal(sim$trajectory[1L, ], y0)
  expect_equal(nrow(sim$trajectory), sim$steps + 1L)
  expect_equal(sim$steps, 5L)
  expect_false(sim$converged)
})

test_that("DeGroot dynamics reach consensus", {
  sim <- fj_simulate(W, 1, y0, max_steps = 5000)
  expect_true(sim$converged)
  final <- sim$trajectory[sim$steps + 1L, ]
  expect_equal(final, rep(final[1], 3), tolerance = 1e-8)
})

test_that("signed dynamics converge to the closed-form equilibrium", {
  Ws <- matrix(c(0.6, -0.4, 0,
                 0.3,  0.5, -0.2,
                 0,   -0.3, 0.7), nrow = 3, byrow = TRUE)
  sim <- fj_simulate(Ws, 0.9, y0)
  expect_true(sim$converged)
  expect_equal(sim$trajectory[sim$steps + 1L, ],
               fj_equilibrium(Ws, 0.9, y0), tolerance = 1e-8)
})

test_that("invalid max_steps raises an error", {
  expect_error(fj_simulate(W, lambda, y0, max_steps = 0), "max_steps")
})
