#' Simulate latent and manifest opinion dynamics
#'
#' Simulates a Friedkin-Johnsen process whose influence matrix and
#' susceptibilities may depend on time and on the current state, coupled with
#' an optional response function that maps latent opinions to manifest
#' responses. At each step from \eqn{t} to \eqn{t + 1}:
#' \deqn{y(t+1) = \Lambda(t) W(t) y(t) + (I - \Lambda(t)) y(0)}
#' \deqn{P(t+1) = f(t + 1, y(t+1), P(t))}
#' where \eqn{f} is `response`.
#'
#' `W` and `lambda` may be fixed objects or functions with arguments
#' `(t, y, P)`, called with the current time \eqn{t}, the latent state
#' \eqn{y(t)} and the manifest state \eqn{P(t)} (`NULL` when there is no
#' response function). They must return inputs valid for [fj_check()].
#'
#' `response` is a function with arguments `(t, y, P)`, called with the new
#' time \eqn{t + 1}, the new latent state \eqn{y(t+1)} and the previous
#' manifest state \eqn{P(t)}; see [response_logistic()] and
#' [response_threshold()]. When `P0` is `NULL`, the initial manifest state is
#' computed as `response(0, y0, NULL)`.
#'
#' External sources with fixed opinions can be represented as agents with
#' susceptibility 0 and a unit self-weight.
#'
#' @inheritParams fj_equilibrium
#' @param W Influence matrix, or a function `(t, y, P)` returning one.
#' @param lambda Susceptibility vector, or a function `(t, y, P)` returning
#'   one.
#' @param steps Number of steps to simulate.
#' @param response Optional response function; if `NULL`, only the latent
#'   dynamics are simulated.
#' @param P0 Optional initial manifest state.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`y`}{Matrix of latent opinions, one row per time step from
#'       \eqn{t = 0} to `steps`, one column per agent.}
#'     \item{`P`}{Matrix of manifest responses with the same layout, or
#'       `NULL` when `response` is `NULL`.}
#'   }
#'
#' @seealso [fj_simulate()], [response_threshold()], [aggregate_quota()]
#'
#' @examples
#' W <- matrix(c(0.5, 0.5, 0,
#'               0.2, 0.6, 0.2,
#'               0,   0.3, 0.7), nrow = 3, byrow = TRUE)
#' f <- response_threshold(delta = 0.5, theta = 0.1, gamma = 0.2,
#'                         S = function(P) climate_balance(P, eps = 0.1))
#' sim <- sint_simulate(W, lambda = 0.8, y0 = c(0.1, 0.5, 0.9),
#'                      steps = 10, response = f)
#' sim$P
#' aggregate_quota(sim$P)
#'
#' @export
sint_simulate <- function(W, lambda, y0, steps, response = NULL, P0 = NULL) {
  if (!is.numeric(steps) || length(steps) != 1L || steps < 1) {
    stop("`steps` must be a positive number.", call. = FALSE)
  }
  steps <- as.integer(steps)
  if (!is.null(response) && !is.function(response)) {
    stop("`response` must be a function or NULL.", call. = FALSE)
  }
  W_fun <- is.function(W)
  lambda_fun <- is.function(lambda)

  n <- if (W_fun) length(y0) else nrow(W)
  y0 <- check_y0(y0, n)
  if (!W_fun && !lambda_fun) {
    fj_check(W, lambda)
  }

  y_traj <- matrix(NA_real_, nrow = steps + 1L, ncol = n)
  y_traj[1L, ] <- y0
  P_traj <- NULL
  P <- NULL
  if (!is.null(response)) {
    P <- if (is.null(P0)) response(0L, y0, NULL) else P0
    check_P(P, n)
    P_traj <- matrix(NA_real_, nrow = steps + 1L, ncol = n)
    P_traj[1L, ] <- P
  }

  y <- y0
  for (t in seq_len(steps) - 1L) {
    W_t <- if (W_fun) W(t, y, P) else W
    lambda_t <- if (lambda_fun) lambda(t, y, P) else lambda
    if (W_fun || lambda_fun) {
      if (!is.matrix(W_t) || nrow(W_t) != n) {
        stop(sprintf("`W` returned an invalid matrix at t = %d.", t),
             call. = FALSE)
      }
      fj_check(W_t, lambda_t)
    }
    lambda_t <- check_lambda(lambda_t, n)
    y <- drop((lambda_t * W_t) %*% y) + (1 - lambda_t) * y0
    y_traj[t + 2L, ] <- y
    if (!is.null(response)) {
      P <- response(t + 1L, y, P)
      check_P(P, n)
      P_traj[t + 2L, ] <- P
    }
  }

  agent_names <- if (!W_fun) rownames(W) else names(y0)
  dimnames(y_traj) <- list(NULL, agent_names)
  if (!is.null(P_traj)) {
    dimnames(P_traj) <- list(NULL, agent_names)
  }
  list(y = y_traj, P = P_traj)
}

check_P <- function(P, n) {
  if (!is.numeric(P) || length(P) != n) {
    stop("The response function must return a numeric vector of length n.",
         call. = FALSE)
  }
  invisible(TRUE)
}
