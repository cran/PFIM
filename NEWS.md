# PFIM 8.0

PFIM 8.0 is a full rewrite of the package on the S7 object system, with C++
(Rcpp/Armadillo) kernels for the FIM and the optimization algorithms.

## Breaking changes

- New S7 object model (`Evaluation`, `Optimization`, `Model`, `Fim`,
  covariates, library of models). Requires **R >= 4.5.0**.
- Simplified public API: user verbs are `run()`, `Report()`, the `get*()` /
  `plot*()` accessors and the class constructors. Pipeline functions are now
  internal.
  - `Dcriterion()` -> `getDcriterion()`
  - `plotSEFIM()` / `plotRSEFIM()` -> `plotSE()` / `plotRSE()`
  - `plotWeightsMultiplicativeAlgorithm()` -> `plotWeights()`
  - `plotFrequenciesFedorovWynnAlgorithm()` -> `plotFrequencies()`
  - `plotEvaluationResults()` / `plotEvaluationSI()` -> `plotEvaluation()` /
    `plotSensitivityIndices()`
  - `generateReportEvaluation()` / `generateReportOptimization()` -> `Report()`
  - `evaluateDesign()`, `optimizeDesign()` and related helpers -> `run()`
- Individual and Bayesian FIMs are aggregated across arms and covariate strata
  as a covariance mixture (\eqn{\bar C=\sum w_s M_s^{-1}}), not as a sum of
  Fisher matrices. Results are therefore not directly comparable with PFIM 6
  or PopED in that case. Non-identifiable protocols with positive weight give
  `Inf` SE.

## New features

- Population, individual and Bayesian FIM with categorical covariates
  (`CategoricalCovariate`) and inter-occasion variability
  (`CategoricalCovariateWithIOV`, `gamma` on parameters).
- Covariate effect tests: `covariateTest()` (alias `tost()`) and
  `saveCovariateTest()`.
- Five optimizers with C++ kernels: Multiplicative, Fedorov-Wynn, PSO, PGBO
  and Simplex. Joint discrete optimization over several arms in one design.
- Discrete allocation uses Hamilton (largest-remainder) rounding so subject
  counts sum exactly to the study size.
- Sampling windows for continuous optimizers (`samplingsWindows`,
  `numberOfTimesByWindows`) with consistency checks on the initial design.
- New residual error hierarchy: `Constant`, `Proportional`, `Combined1`,
  `Combined2` (additive + proportional, PopED-style).
- Observed responses can be defined as transformations of the model states
  through `outputs` in `Evaluation()` / `Optimization()` (e.g.
  `C = A/Vd`, `Wout = log10(W)`). Sampling times are declared on the model
  states (`SamplingTimes(outcome = "A")`) and residual error models on the
  observed responses (`Constant(output = "C")`), so the error is applied on
  the transformed scale (e.g. additive error on `log10(W)`).
- `Report()` generates HTML reports for evaluations and optimizations.
- Session options: `pfim_get_option()`, `pfim_set_option()`,
  `pfim_reset_session()`.
- New vignettes `Example01` to `Example04` and `LibraryOfModels`. `Example04`
  reproduces the PopED G-CSF / filgrastim PK-PD workshop (Design 1 RSE), which
  is also checked by the test suite.

## Performance

- FIM computation, residual variance and optimization algorithms run in C++.
- Caching of FIMs, ODE solutions and finite-difference gradients; continuous
  optimizers reuse one evaluation context per search.

## Bug fixes (relative to PFIM 7)

- Population FIM with fixed typical value and estimated IIV (`fixedMu = TRUE`,
  \eqn{\omega > 0}): the gradient of \eqn{\mu} is now computed, so the
  \eqn{\omega^2} term is kept in the variance and \eqn{\omega^2} remains
  estimable. `fixedMu` and `fixedOmega` are now independent.
- Rank-deficient FIMs (e.g. one sample for two parameters) now report `Inf`
  SE and `Inf` condition number on non-identifiable directions instead of
  large finite values.
- Residual-error parameters are labelled \eqn{\sigma} (SD scale) instead of
  \eqn{\sigma^2}; values were already on the SD scale.
- ODE finite-difference steps are no longer inflated by the solver tolerance,
  which biased the FIM.
- Multiplicative algorithm: returned weights are certified (the optimality
  condition is checked before the update). Both the mixture and the realised
  D-criteria are returned (`getMixtureDcriterion()`,
  `getRealisedDcriterion()`). In joint (multi-outcome) optimization, the
  weights are restricted to the winning protocol, and the study size is split
  across its arms as in Fedorov-Wynn.
- Fedorov-Wynn: the default optimality gap is `1e-4`, matching the weight
  precision. A run that stops on the cycle budget is reported as
  `incomplete`, with a warning, and no longer as `success`.
- Discrete optimizers (Multiplicative, Fedorov-Wynn): when
  `administrationsConstraints` is omitted, the dose grid is taken from the
  arm's current administration.
- Simplex: increasing `numberOfTimesByWindows` no longer leaves the search
  stuck at its starting point. An infeasible start raises an error and is no
  longer reported as `converged = TRUE`.
- Continuous optimizers (Simplex, PSO, PGBO): `converged` is `NA` when
  `tolerance <= 0` (the stopping rule is disabled). A warning is issued when
  the arm sampling times differ from `initialSamplings`, which is the actual
  starting point.
- Analytic models: equations of non-administered outcomes that depend on `t`
  are evaluated at their own sampling times. Disease-progression and placebo
  models from the library evaluate correctly again.
- A covariate effect set to 0 is kept in the population FIM, so its SE under
  the null hypothesis is available. A warning is issued.
- `Report()` restores the knitr chunk options and no longer closes the
  user's graphics devices.
- Library of models: corrected closed forms for two-compartment bolus and
  infusion models, one-compartment infusion at steady state, exponential
  baseline models, sigmoid turnover and Michaelis-Menten ODE models (central
  volume `V1`). Unknown model names now raise an error.
- Constructors reject `NA` / non-finite parameters, unknown initial
  conditions and undeclared covariate categories with a clear `PFIM:` error.

## Contributors

- Romain Leroux (`aut`) designed and wrote the entire source code of
  PFIM 8.0 (R/S7 code, C++ kernels, testthat and documentation).
- France Mentré (`cre`, <pfim@inserm.fr>) is the package maintainer.
- Vignettes:
  - `Example01`, `Example02`: Romain Leroux and Jérémy Seurat.
  - `Example03`: Antoine Croxo and Romain Leroux.  
  - `Example04`, `LibraryOfModels`: Romain Leroux.
- Thanks to Antoine Croxo and Lucie Fayette for beta-testing PFIM 8.0 
and running the example scripts to help verify their correct execution.
