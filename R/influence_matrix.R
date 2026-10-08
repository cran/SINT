#' Build an influence matrix from a network
#'
#' Converts a matrix, an `igraph` graph or a `network` object into a
#' row-normalized influence matrix suitable for the other functions of the
#' package. The adjacency matrix, optionally valued with an edge attribute,
#' is normalized with [row_normalize()].
#'
#' With `direction = "attention"`, an edge from \eqn{i} to \eqn{j} means that
#' \eqn{i} attends to \eqn{j}, so that \eqn{w_{ij} > 0}; this matches the
#' row-wise reading of \eqn{W}. With `direction = "influence"`, an edge from
#' \eqn{i} to \eqn{j} means that \eqn{i} influences \eqn{j}, and the adjacency
#' matrix is transposed.
#'
#' For `network` objects, self-ties are present only if the object was
#' created with `loops = TRUE`.
#'
#' Agents with no ties in their row cannot be normalized. They are given a
#' unit self-weight, so that they attend only to themselves, and a warning is
#' issued.
#'
#' @param x A square numeric matrix, an `igraph` graph or a `network` object.
#' @param weights Optional name of a numeric edge attribute holding the tie
#'   weights; if `NULL`, all ties have weight one. Ignored when `x` is a
#'   matrix.
#' @param direction Meaning of an edge from \eqn{i} to \eqn{j}: `"attention"`
#'   (\eqn{i} attends to \eqn{j}) or `"influence"` (\eqn{i} influences
#'   \eqn{j}).
#'
#' @return A numeric matrix whose rows have unit absolute sums, with vertex
#'   names as dimnames when available.
#'
#' @seealso [row_normalize()], [fj_check()]
#'
#' @examples
#' A <- matrix(c(0, 1, 1,
#'               1, 0, 0,
#'               0, 1, 1), nrow = 3, byrow = TRUE)
#' influence_matrix(A)
#' influence_matrix(A, direction = "influence")
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::graph_from_literal(a -+ b, b -+ c, c -+ a, a -+ c)
#'   influence_matrix(g)
#' }
#'
#' @export
influence_matrix <- function(x, weights = NULL,
                             direction = c("attention", "influence")) {
  direction <- match.arg(direction)
  if (!is.null(weights) &&
      (!is.character(weights) || length(weights) != 1L)) {
    stop("`weights` must be NULL or a single attribute name.", call. = FALSE)
  }

  if (inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("Package 'igraph' is required to convert igraph objects.",
           call. = FALSE)
    }
    A <- igraph::as_adjacency_matrix(x, attr = weights, sparse = FALSE)
  } else if (inherits(x, "network")) {
    if (!requireNamespace("network", quietly = TRUE)) {
      stop("Package 'network' is required to convert network objects.",
           call. = FALSE)
    }
    A <- as.matrix(x, matrix.type = "adjacency", attrname = weights)
  } else if (is.matrix(x)) {
    A <- x
  } else {
    stop("`x` must be a matrix, an igraph graph or a network object.",
         call. = FALSE)
  }

  if (!is.numeric(A) || nrow(A) != ncol(A) || any(!is.finite(A))) {
    stop("The adjacency matrix must be square, numeric and finite.",
         call. = FALSE)
  }
  if (direction == "influence") {
    A <- t(A)
  }

  empty <- rowSums(abs(A)) == 0
  if (any(empty)) {
    ids <- if (is.null(rownames(A))) which(empty) else rownames(A)[empty]
    warning(sprintf(
      "Agents without ties were given a unit self-weight: %s.",
      paste(ids, collapse = ", ")
    ), call. = FALSE)
    A[cbind(which(empty), which(empty))] <- 1
  }
  row_normalize(A)
}
