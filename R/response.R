#' Logistic response function
#'
#' Builds a response function mapping latent opinions to binary manifest
#' responses through
#' \eqn{\Pr(P_i = 1) = \mathrm{logit}^{-1}(\beta_i (y_i - \delta) + \gamma_i S_i)}.
#'
#' `S` may be a fixed number (or vector) or a function of the previous
#' manifest state `P`, such as [climate_balance()]. When `S` is a function and
#' no previous manifest state exists (`P` is `NULL`), `S` is taken as 0.
#'
#' @param beta Discrimination \eqn{\beta}, of length one or \eqn{n}.
#' @param delta Adhesion threshold \eqn{\delta}.
#' @param gamma Sensitivity \eqn{\gamma} to normative pressure, of length one
#'   or \eqn{n}.
#' @param S Normative pressure: a number, a vector of length \eqn{n}, or a
#'   function of the previous manifest state.
#' @param stochastic If `TRUE`, responses are drawn from Bernoulli
#'   distributions; if `FALSE`, the response probabilities are returned.
#'
#' @return A function with arguments `(t, y, P)` for use in [sint_simulate()].
#'
#' @seealso [response_threshold()], [sint_simulate()]
#'
#' @examples
#' f <- response_logistic(beta = 4, delta = 0.5, stochastic = FALSE)
#' f(1, y = c(0.2, 0.5, 0.9), P = NULL)
#'
#' @export
response_logistic <- function(beta = 1, delta = 0, gamma = 0, S = 0,
                              stochastic = TRUE) {
  check_response_args(beta, delta, gamma, S)
  force(stochastic)
  function(t, y, P) {
    p <- stats::plogis(beta * (y - delta) + gamma * eval_pressure(S, P))
    if (stochastic) {
      as.integer(stats::runif(length(y)) < p)
    } else {
      p
    }
  }
}

#' Double-threshold response function
#'
#' Builds a response function mapping latent opinions to ternary manifest
#' responses. With \eqn{z_i = \beta_i (y_i - \delta) + \gamma_i S_i}, the
#' response is \eqn{+1} if \eqn{z_i > \theta}, \eqn{-1} if
#' \eqn{z_i < -\theta} and 0 (silence) otherwise.
#'
#' In the stochastic version a standard logistic error is added to
#' \eqn{z_i}, which yields an ordered logit model with thresholds
#' \eqn{\pm\theta}. `S` is handled as in [response_logistic()].
#'
#' @inheritParams response_logistic
#' @param theta Positive half-width \eqn{\theta} of the silence band.
#' @param stochastic If `TRUE`, a logistic error is added to \eqn{z_i}.
#'
#' @return A function with arguments `(t, y, P)` for use in [sint_simulate()].
#'
#' @seealso [response_logistic()], [climate_balance()], [sint_simulate()]
#'
#' @examples
#' f <- response_threshold(delta = 0.5, theta = 0.4)
#' f(1, y = c(0.05, 0.5, 0.95), P = NULL)
#'
#' @export
response_threshold <- function(beta = 1, delta = 0, theta, gamma = 0, S = 0,
                               stochastic = FALSE) {
  check_response_args(beta, delta, gamma, S)
  if (!is.numeric(theta) || length(theta) != 1L || !(theta > 0)) {
    stop("`theta` must be a single positive number.", call. = FALSE)
  }
  force(stochastic)
  function(t, y, P) {
    z <- beta * (y - delta) + gamma * eval_pressure(S, P)
    if (stochastic) {
      z <- z + stats::rlogis(length(y))
    }
    as.integer(ifelse(z > theta, 1L, ifelse(z < -theta, -1L, 0L)))
  }
}

#' Normative climate from manifest responses
#'
#' Computes \eqn{S = (n^+ - n^-) / (n^+ + n^- + \epsilon)}, where \eqn{n^+}
#' and \eqn{n^-} count the responses equal to \eqn{+1} and \eqn{-1}.
#'
#' @param P Vector of manifest responses in \eqn{\{-1, 0, +1\}}.
#' @param eps Positive regularization constant \eqn{\epsilon}.
#'
#' @return A single number in \eqn{(-1, 1)}.
#'
#' @examples
#' climate_balance(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0), eps = 0.1)
#'
#' # As a normative pressure in a response function
#' f <- response_threshold(delta = 0.5, theta = 0.4, gamma = 0.9,
#'                         S = function(P) climate_balance(P, eps = 0.1))
#'
#' @export
climate_balance <- function(P, eps) {
  if (!is.numeric(P)) {
    stop("`P` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L || !(eps > 0)) {
    stop("`eps` must be a single positive number.", call. = FALSE)
  }
  n_pos <- sum(P == 1)
  n_neg <- sum(P == -1)
  (n_pos - n_neg) / (n_pos + n_neg + eps)
}

check_response_args <- function(beta, delta, gamma, S) {
  if (!is.numeric(beta) || any(!is.finite(beta))) {
    stop("`beta` must be a finite numeric vector.", call. = FALSE)
  }
  if (!is.numeric(delta) || length(delta) != 1L || !is.finite(delta)) {
    stop("`delta` must be a single finite number.", call. = FALSE)
  }
  if (!is.numeric(gamma) || any(!is.finite(gamma))) {
    stop("`gamma` must be a finite numeric vector.", call. = FALSE)
  }
  if (!is.function(S) && (!is.numeric(S) || any(!is.finite(S)))) {
    stop("`S` must be a finite numeric vector or a function.", call. = FALSE)
  }
  invisible(TRUE)
}

eval_pressure <- function(S, P) {
  if (!is.function(S)) {
    return(S)
  }
  if (is.null(P)) {
    return(0)
  }
  S(P)
}
