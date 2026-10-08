test_that("quota rule counts only responses equal to 1", {
  expect_identical(aggregate_quota(c(1, 1, 0, -1, 1)), 1L)
  expect_identical(aggregate_quota(c(1, 1, 0, 0)), 0L)
  expect_identical(aggregate_quota(c(1, 1, 0, -1, 1), quota = 2 / 3), 0L)
})

test_that("quota rule is applied row-wise to matrices", {
  P <- rbind(c(0, 0, 0), c(1, 1, 0), c(1, 1, 1))
  expect_identical(aggregate_quota(P), c(0L, 1L, 1L))
})

test_that("invalid quota raises an error", {
  expect_error(aggregate_quota(c(1, 0), quota = 1.5), "quota")
})
