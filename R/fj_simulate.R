#' Simulate the Friedkin-Johnsen dynamics
#'
#' Iterates \eqn{y(t+1) = \Lambda W y(t) + (I - \Lambda) y(0)} until the
#' largest absolute change between two steps falls below `tol` or `max_steps`
#' is reached. Unlike [fj_equilibrium()], the iteration is carried out even
#' when the dynamics do not converge.
#'
#' @inheritParams fj_equilibrium
#' @param max_steps Maximum number of iterations.
#' @param tol Convergence tolerance on the largest absolute change between two
#'   consecutive steps.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`trajectory`}{Matrix with one row per time step, from \eqn{t = 0}
#'       to the last step, and one column per agent.}
#'     \item{`steps`}{Number of iterations performed.}
#'     \item{`converged`}{`TRUE` if the stopping criterion was met before
#'       `max_steps`.}
#'   }
#'
#' @seealso [fj_equilibrium()]
#'
#' @examples
#' W <- matrix(c(0.5, 0.5, 0,
#'               0.2, 0.6, 0.2,
#'               0,   0.3, 0.7), nrow = 3, byrow = TRUE)
#' sim <- fj_simulate(W, lambda = c(0.8, 0.5, 0.9), y0 = c(0, 0.5, 1))
#' tail(sim$trajectory, 1)
#'
#' @export
fj_simulate <- function(W, lambda, y0, max_steps = 1000L, tol = 1e-10) {
  chk <- fj_check(W, lambda)
  n <- chk$n
  lambda <- check_lambda(lambda, n)
  y0 <- check_y0(y0, n)
  if (!is.numeric(max_steps) || length(max_steps) != 1L || max_steps < 1) {
    stop("`max_steps` must be a positive number.", call. = FALSE)
  }
  max_steps <- as.integer(max_steps)

  LW <- lambda * W
  anchor <- (1 - lambda) * y0
  trajectory <- matrix(NA_real_, nrow = max_steps + 1L, ncol = n,
                       dimnames = list(NULL, rownames(W)))
  trajectory[1L, ] <- y0
  y <- y0
  converged <- FALSE
  steps <- 0L
  while (steps < max_steps) {
    y_new <- drop(LW %*% y) + anchor
    steps <- steps + 1L
    trajectory[steps + 1L, ] <- y_new
    if (max(abs(y_new - y)) < tol) {
      converged <- TRUE
      break
    }
    y <- y_new
  }

  list(
    trajectory = trajectory[seq_len(steps + 1L), , drop = FALSE],
    steps = steps,
    converged = converged
  )
}
