#' Check the inputs of a Friedkin-Johnsen model
#'
#' Validates an influence matrix and a susceptibility vector and reports the
#' quantities that govern convergence of the Friedkin-Johnsen dynamics
#' \eqn{y(t+1) = \Lambda W y(t) + (I - \Lambda) y(0)}.
#'
#' A non-negative `W` must be row-stochastic. A signed `W` (at least one
#' negative entry) must have absolute row sums not greater than one.
#'
#' @param W Square numeric influence matrix.
#' @param lambda Numeric vector of susceptibilities in \eqn{[0, 1]}, of length
#'   one (recycled) or equal to `nrow(W)`.
#' @param tol Numerical tolerance used when checking row sums.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`n`}{Number of agents.}
#'     \item{`signed`}{`TRUE` if `W` has negative entries.}
#'     \item{`spectral_radius`}{Spectral radius of \eqn{\Lambda W}.}
#'     \item{`norm_inf`}{Infinity norm of \eqn{\Lambda W}.}
#'     \item{`converges`}{`TRUE` if the spectral radius is below one, so that
#'       the dynamics converge to a unique fixed point.}
#'   }
#'
#' @examples
#' W <- matrix(c(0.5, 0.5, 0,
#'               0.2, 0.6, 0.2,
#'               0,   0.3, 0.7), nrow = 3, byrow = TRUE)
#' fj_check(W, lambda = c(0.8, 0.5, 0.9))
#'
#' @export
fj_check <- function(W, lambda, tol = sqrt(.Machine$double.eps)) {
  if (!is.matrix(W) || !is.numeric(W)) {
    stop("`W` must be a numeric matrix.", call. = FALSE)
  }
  n <- nrow(W)
  if (n == 0L || ncol(W) != n) {
    stop("`W` must be a non-empty square matrix.", call. = FALSE)
  }
  if (any(!is.finite(W))) {
    stop("`W` must contain only finite values.", call. = FALSE)
  }
  lambda <- check_lambda(lambda, n)

  signed <- any(W < 0)
  if (signed) {
    if (any(rowSums(abs(W)) > 1 + tol)) {
      stop("A signed `W` must have absolute row sums not greater than 1.",
           call. = FALSE)
    }
  } else if (any(abs(rowSums(W) - 1) > tol)) {
    stop("A non-negative `W` must be row-stochastic.", call. = FALSE)
  }

  LW <- lambda * W
  rho <- max(Mod(eigen(LW, only.values = TRUE)$values))
  list(
    n = n,
    signed = signed,
    spectral_radius = rho,
    norm_inf = max(rowSums(abs(LW))),
    converges = rho < 1 - tol
  )
}

check_lambda <- function(lambda, n) {
  if (!is.numeric(lambda) || any(!is.finite(lambda))) {
    stop("`lambda` must be a finite numeric vector.", call. = FALSE)
  }
  if (length(lambda) == 1L) {
    lambda <- rep(lambda, n)
  }
  if (length(lambda) != n) {
    stop("`lambda` must have length 1 or `nrow(W)`.", call. = FALSE)
  }
  if (any(lambda < 0 | lambda > 1)) {
    stop("`lambda` must lie in [0, 1].", call. = FALSE)
  }
  lambda
}

check_y0 <- function(y0, n) {
  if (!is.numeric(y0) || !is.null(dim(y0)) || any(!is.finite(y0))) {
    stop("`y0` must be a finite numeric vector.", call. = FALSE)
  }
  if (length(y0) != n) {
    stop("`y0` must have length `nrow(W)`.", call. = FALSE)
  }
  y0
}
