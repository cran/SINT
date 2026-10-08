## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SINT)

## ----specify------------------------------------------------------------------
raw <- matrix(c(3, 1, 0, 0,
                1, 2, 1, 0,
                0, 1, 2, 1,
                0, 0, 1, 3), nrow = 4, byrow = TRUE,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
W <- row_normalize(raw)
W

lambda <- c(0.9, 0.6, 0.6, 0.9)
y0 <- c(0, 0.3, 0.7, 1)

## ----specify-igraph, eval = requireNamespace("igraph", quietly = TRUE)--------
g <- igraph::graph_from_adjacency_matrix(raw, mode = "directed",
                                         weighted = TRUE)
all.equal(influence_matrix(g, weights = "weight"), W)

## ----check--------------------------------------------------------------------
fj_check(W, lambda)

## ----check-partial------------------------------------------------------------
fj_check(W, c(1, 1, 0.5, 1))$converges

## ----check-degroot------------------------------------------------------------
fj_check(W, 1)$converges

## ----equilibrium--------------------------------------------------------------
fj_equilibrium(W, lambda, y0)

## ----influence----------------------------------------------------------------
V <- fj_influence(W, lambda)
round(V, 3)

## ----influence-summary--------------------------------------------------------
rowSums(V)
diag(V)
colSums(V)

## ----published----------------------------------------------------------------
W_p <- matrix(c(0.220, 0.120, 0.360, 0.300,
                0.147, 0.215, 0.344, 0.294,
                0,     0,     1,     0,
                0.090, 0.178, 0.446, 0.286), nrow = 4, byrow = TRUE)
lambda_p <- 1 - diag(W_p)
lambda_p

## ----published-equilibria-----------------------------------------------------
round(fj_equilibrium(W_p, lambda_p, c(25, 25, 75, 85)), 1)
round(fj_equilibrium(W_p, lambda_p, c(25, 15, -50, 5)), 1)

## ----simulate, fig.width = 6, fig.height = 4----------------------------------
sim <- fj_simulate(W, lambda, y0)
sim$steps
sim$converged
tail(sim$trajectory, 1)

matplot(0:sim$steps, sim$trajectory, type = "l", lty = 1,
        xlab = "t", ylab = "opinion")

## ----simulate-degroot---------------------------------------------------------
tail(fj_simulate(W, 1, y0, max_steps = 5000)$trajectory, 1)

## ----signed-------------------------------------------------------------------
raw_signed <- matrix(c( 3,  1, -1,  0,
                        1,  2,  0, -1,
                       -1,  0,  2,  1,
                        0, -1,  1,  3), nrow = 4, byrow = TRUE)
Ws <- row_normalize(raw_signed)
fj_check(Ws, lambda)
fj_equilibrium(Ws, lambda, y0)

## ----source-------------------------------------------------------------------
raw_src <- cbind(raw, S = c(2, 0, 0, 0))
raw_src <- rbind(raw_src, S = c(0, 0, 0, 0, 1))
W_src <- row_normalize(raw_src)
lambda_src <- c(lambda, 0)
y0_src <- c(y0, 1)
names(y0_src) <- rownames(W_src)

fj_equilibrium(W_src, lambda_src, y0_src)

## ----time---------------------------------------------------------------------
lambda_campaign <- function(t, y, P) {
  if ((t + 1) %in% 1:3) c(0.98, 0.6, 0.6, 0.9, 0) else lambda_src
}
sim_t <- sint_simulate(W_src, lambda_campaign, y0_src, steps = 8)
round(sim_t$y, 3)

## ----logistic-----------------------------------------------------------------
f_log <- response_logistic(beta = 6, delta = 0.5, stochastic = FALSE)
round(f_log(1, y = c(0.1, 0.5, 0.9), P = NULL), 3)

set.seed(1)
f_bin <- response_logistic(beta = 6, delta = 0.5)
f_bin(1, y = c(0.1, 0.5, 0.9), P = NULL)

## ----threshold----------------------------------------------------------------
f_thr <- response_threshold(delta = 0.5, theta = 0.3)
f_thr(1, y = c(0.1, 0.5, 0.9), P = NULL)

## ----climate------------------------------------------------------------------
f_clim <- response_threshold(delta = 0.5, theta = 0.3, gamma = 0.4,
                             S = function(P) climate_balance(P, eps = 0.1))
f_clim(1, y = c(0.45, 0.5, 0.55), P = NULL)
f_clim(1, y = c(0.45, 0.5, 0.55), P = c(1, 1, 1))

## ----response-sim-------------------------------------------------------------
agents <- 1:4
f_group <- response_threshold(
  delta = 0.5, theta = 0.3, gamma = 0.4,
  S = function(P) climate_balance(P[agents], eps = 0.1)
)
respond <- function(t, y, P) {
  out <- f_group(t, y, P)
  out[-agents] <- 0L
  out
}
sim_r <- sint_simulate(W_src, lambda_campaign, y0_src, steps = 8,
                       response = respond)
sim_r$P[, agents]

## ----visibility---------------------------------------------------------------
W_visible <- function(t, y, P) {
  visibility <- 0.2 + 0.8 * abs(P)
  row_normalize(sweep(raw_src, 2, visibility, `*`))
}
sim_v <- sint_simulate(W_visible, lambda_campaign, y0_src, steps = 8,
                       response = respond)
sim_v$P[, agents]

## ----aggregate----------------------------------------------------------------
aggregate_quota(sim_r$P[, agents])
aggregate_quota(sim_r$P[, agents], quota = 3 / 4)

