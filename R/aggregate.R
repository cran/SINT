#' Aggregate manifest responses with a quota rule
#'
#' Returns \eqn{\Theta(\sum_i 1(P_i = 1) - q n)}, where \eqn{\Theta} is the
#' Heaviside step function (1 for positive arguments, 0 otherwise). Only
#' responses equal to 1 count in favor; abstentions (0) and opposing
#' responses (-1) count as not in favor. With `quota = 0.5` this is the
#' simple majority rule.
#'
#' @param P Vector of manifest responses, or a matrix with one row per time
#'   step as returned by [sint_simulate()].
#' @param quota Quota \eqn{q} in \eqn{[0, 1]}.
#'
#' @return An integer (0 or 1), or an integer vector with one outcome per row
#'   when `P` is a matrix.
#'
#' @examples
#' aggregate_quota(c(1, 1, 0, -1, 1))
#' aggregate_quota(c(1, 1, 0, -1, 1), quota = 2/3)
#'
#' @export
aggregate_quota <- function(P, quota = 0.5) {
  if (!is.numeric(P)) {
    stop("`P` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(quota) || length(quota) != 1L || quota < 0 || quota > 1) {
    stop("`quota` must be a single number in [0, 1].", call. = FALSE)
  }
  if (is.matrix(P)) {
    return(as.integer(rowSums(P == 1) > quota * ncol(P)))
  }
  as.integer(sum(P == 1) > quota * length(P))
}
