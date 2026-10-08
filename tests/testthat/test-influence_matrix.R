A <- matrix(c(0, 1, 1,
              1, 0, 0,
              0, 2, 2), nrow = 3, byrow = TRUE,
            dimnames = list(c("a", "b", "c"), c("a", "b", "c")))

test_that("matrices are row-normalized", {
  W <- influence_matrix(A)
  expect_equal(W, row_normalize(A))
})

test_that("direction = 'influence' transposes the adjacency matrix", {
  expect_equal(influence_matrix(A, direction = "influence"),
               row_normalize(t(A)))
})

test_that("agents without ties get a unit self-weight with a warning", {
  B <- A
  B[2, ] <- 0
  expect_warning(W <- influence_matrix(B), "b")
  expect_equal(unname(W[2, ]), c(0, 1, 0))
})

test_that("igraph graphs are converted with and without weights", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(A, mode = "directed",
                                           weighted = TRUE)
  expect_equal(influence_matrix(g, weights = "weight"), row_normalize(A))
  expect_equal(influence_matrix(g), row_normalize((A > 0) * 1))
})

test_that("network objects are converted with and without weights", {
  skip_if_not_installed("network")
  net <- network::network(A, directed = TRUE, loops = TRUE,
                          ignore.eval = FALSE, names.eval = "weight")
  expect_equal(influence_matrix(net, weights = "weight"), row_normalize(A))
  expect_equal(influence_matrix(net), row_normalize((A > 0) * 1))
})

test_that("invalid inputs raise errors", {
  expect_error(influence_matrix(list()), "igraph")
  expect_error(influence_matrix(A[1:2, ]), "square")
  expect_error(influence_matrix(A, weights = 1), "weights")
})
