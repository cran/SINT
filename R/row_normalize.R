#' Normalize the rows of an influence matrix
#'
#' Divides each row by the sum of the absolute values of its entries. For a
#' non-negative matrix the result is row-stochastic; for a signed matrix the
#' absolute row sums equal one, as required by [fj_check()].
#'
#' @param V Numeric matrix of unnormalized, possibly signed, weights.
#'
#' @return A numeric matrix with the same dimensions and names as `V`.
#'
#' @examples
#' V <- matrix(c(2, 1, 1,
#'               0, 3, 1,
#'               1, 1, 2), nrow = 3, byrow = TRUE)
#' row_normalize(V)
#'
#' @export
row_normalize <- function(V) {
  if (!is.matrix(V) || !is.numeric(V) || any(!is.finite(V))) {
    stop("`V` must be a finite numeric matrix.", call. = FALSE)
  }
  s <- rowSums(abs(V))
  if (any(s == 0)) {
    stop("Every row of `V` must have at least one non-zero entry.",
         call. = FALSE)
  }
  V / s
}
