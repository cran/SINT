
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SINT

SINT provides tools for specifying, analyzing and simulating models of
social influence network theory based on the Friedkin-Johnsen model. The
latent dynamics

$$y(t+1) = \Lambda W y(t) + (I - \Lambda) y(0)$$

can be analyzed in closed form (equilibrium, total influence matrix,
convergence diagnostics, also for signed networks) or simulated with
influence weights and susceptibilities that depend on time and on the
state of the system. An optional response stage maps latent opinions to
manifest responses, which can then be aggregated into collective
outcomes.

## Installation

You can install the development version of SINT from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("Silvestro26/SINT")
```

## Example

Three agents with influence matrix `W`, susceptibilities `lambda` and
initial opinions `y0`:

``` r
library(SINT)

W <- matrix(c(0.5, 0.5, 0,
              0.2, 0.6, 0.2,
              0,   0.3, 0.7), nrow = 3, byrow = TRUE)
lambda <- c(0.8, 0.5, 0.9)
y0 <- c(0, 0.5, 1)

fj_check(W, lambda)$converges
#> [1] TRUE
fj_equilibrium(W, lambda, y0)
#> [1] 0.3295820 0.4943730 0.6310289
```

The total influence matrix gives the weight of each initial opinion in
each equilibrium opinion:

``` r
round(fj_influence(W, lambda), 3)
#>       [,1]  [,2]  [,3]
#> [1,] 0.373 0.595 0.032
#> [2,] 0.059 0.892 0.048
#> [3,] 0.043 0.651 0.305
```

See `vignette("SINT")` for a complete guide to the package.
