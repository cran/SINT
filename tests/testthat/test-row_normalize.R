test_that("non-negative rows become stochastic", {
  V <- matrix(c(2, 1, 1,
                0, 3, 1,
                1, 1, 2), nrow = 3, byrow = TRUE)
  W <- row_normalize(V)
  expect_equal(rowSums(W), rep(1, 3))
  expect_true(fj_check(W, 0.5)$converges)
})

test_that("signed rows get unit absolute sums and keep signs", {
  V <- matrix(c(2, -1, 1,
                -1, 3, 0), nrow = 2, byrow = TRUE)
  W <- row_normalize(V)
  expect_equal(rowSums(abs(W)), rep(1, 2))
  expect_identical(sign(W), sign(V))
})

test_that("zero rows raise an error", {
  expect_error(row_normalize(matrix(c(1, 0, 0, 0), 2)), "non-zero")
})
