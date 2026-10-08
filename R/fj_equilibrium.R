#' Total influence matrix of a Friedkin-Johnsen model
#'
#' Computes \eqn{V = (I - \Lambda W)^{-1} (I - \Lambda)}, whose entry
#' \eqn{v_{ij}} is the total effect of the initial opinion of agent \eqn{j} on
#' the equilibrium opinion of agent \eqn{i}. When `W` is non-negative, the rows
#' of `V` sum to one.
#'
#' @inheritParams fj_check
#'
#' @return A numeric \eqn{n \times n} matrix.
#'
#' @seealso [fj_equilibrium()], [fj_check()]
#'
#' @examples
#' W <- matrix(c(0.5, 0.5, 0,
#'               0.2, 0.6, 0.2,
#'               0,   0.3, 0.7), nrow = 3, byrow = TRUE)
#' fj_influence(W, lambda = c(0.8, 0.5, 0.9))
#'
#' @export
fj_influence <- function(W, lambda, tol = sqrt(.Machine$double.eps)) {
  chk <- fj_check(W, lambda, tol = tol)
  if (!chk$converges) {
    stop(sprintf(
      "The dynamics do not converge: spectral radius of Lambda W is %.6g.",
      chk$spectral_radius
    ), call. = FALSE)
  }
  lambda <- check_lambda(lambda, chk$n)
  V <- solve(diag(chk$n) - lambda * W, diag(1 - lambda, nrow = chk$n))
  dimnames(V) <- dimnames(W)
  V
}

#' Equilibrium opinions of a Friedkin-Johnsen model
#'
#' Computes the unique fixed point
#' \eqn{y^* = (I - \Lambda W)^{-1} (I - \Lambda) y(0)} of the dynamics
#' \eqn{y(t+1) = \Lambda W y(t) + (I - \Lambda) y(0)}. An error is raised when
#' the spectral radius of \eqn{\Lambda W} is not below one.
#'
#' @inheritParams fj_check
#' @param y0 Numeric vector of initial opinions, of length `nrow(W)`.
#'
#' @return A numeric vector of equilibrium opinions.
#'
#' @seealso [fj_influence()], [fj_simulate()]
#'
#' @examples
#' W <- matrix(c(0.5, 0.5, 0,
#'               0.2, 0.6, 0.2,
#'               0,   0.3, 0.7), nrow = 3, byrow = TRUE)
#' fj_equilibrium(W, lambda = c(0.8, 0.5, 0.9), y0 = c(0, 0.5, 1))
#'
#' @export
fj_equilibrium <- function(W, lambda, y0, tol = sqrt(.Machine$double.eps)) {
  V <- fj_influence(W, lambda, tol = tol)
  y0 <- check_y0(y0, nrow(W))
  y <- drop(V %*% y0)
  names(y) <- rownames(W)
  y
}
