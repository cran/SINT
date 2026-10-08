# SINT 0.1.0

* Friedkin-Johnsen core: `fj_check()` validates inputs and reports
  convergence diagnostics, `fj_equilibrium()` and `fj_influence()` compute
  the fixed point and the total influence matrix, `fj_simulate()` iterates
  the dynamics. Signed influence matrices are supported.
* `sint_simulate()` simulates latent and manifest dynamics with influence
  matrices and susceptibilities that may depend on time and state.
* Response functions `response_logistic()` and `response_threshold()`, with
  `climate_balance()` for endogenous normative pressure.
* `aggregate_quota()` maps manifest responses to collective outcomes.
* `row_normalize()` builds influence matrices from unnormalized weights.
* `influence_matrix()` converts matrices, 'igraph' graphs and 'network'
  objects into influence matrices.
* User guide vignette, `vignette("SINT")`.
