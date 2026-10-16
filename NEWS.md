# PFIM 8.0.1

Maintenance release addressing macOS compatibility issues identified during CRAN checks. No changes to the functionality, algorithms or public API introduced in PFIM 8.0. It uses S7 objects in R and compiled Rcpp/Armadillo kernels for Fisher information matrix (FIM) calculations and optimization. This release includes categorical covariates, inter-occasion variability (IOV), covariate-effect tests, expanded design constraints and model support, numerical corrections, and compatibility adjustments for CRAN and macOS. It requires R >= 4.5.0 and C++17 for source builds.

## Architecture and user interface

- Rebuilt the package around S7 classes for evaluations, optimizations, models, model parameters, FIM types, designs, study arms, covariates, residual errors and optimization algorithms. Modular R code is backed by compiled C++ routines.

- Simplified the public interface around the `Evaluation()` and `Optimization()` constructors, `run()`, `Report()` and dedicated `get*()`/`plot*()` accessors. Internal evaluation and optimization pipeline functions are no longer part of the public workflow.

- Renamed key entry points: `evaluateDesign()` and `optimizeDesign()` to `run()`; `Dcriterion()` to `getDcriterion()`; `plotSEFIM()` and `plotRSEFIM()` to `plotSE()` and `plotRSE()`; `plotWeightsMultiplicativeAlgorithm()` to `plotWeights()`; `plotFrequenciesFedorovWynnAlgorithm()` to `plotFrequencies()`; `plotEvaluationResults()` and `plotEvaluationSI()` to `plotEvaluation()` and `plotSensitivityIndices()`; and `generateReportEvaluation()`/`generateReportOptimization()` to `Report()`.

- Added session controls (`pfim_get_option()`, `pfim_set_option()`, `pfim_reset_session()`) and registration interfaces for extending model, FIM and optimizer types.

## Covariates, inter-occasion variability and inference

- Added categorical between-subject covariates (`Covariate`, `CategoricalCovariate`) with category proportions, effects on typical model parameters, and additive or exponential covariate-effect models (`modelCovariatesEquation`). Within-subject covariate sequences can represent changes across occasions, including crossover-style designs.

- Added inter-occasion variability through `CategoricalCovariateWithIOV`, with occasion-level variance parameters (`gamma^2`) for selected model parameters. `Evaluation()` and `Optimization()` support `numberOfOccasions`; when possible, the number is inferred from the model layout.

- Extended population-FIM construction to covariate-by-occasion gradient structures and dedicated C++ combination routines. Fixed typical parameters (`fixedMu`) and fixed inter-individual variance parameters (`fixedOmega`) are accounted for separately while preserving the contributions required to estimate remaining variance components.

- Extended individual and Bayesian FIM evaluation to covariate/IOV designs. When combining study arms or covariate strata, these FIM types use a weighted mixture of stratum covariance matrices: `C_bar = sum_s w_s * M_s^(-1)`, rather than simply summing the FIMs. The resulting precision estimates are not, in general, numerically comparable with those from PFIM 6 or PopED for these designs. Protocols with positive weight that are non-identifiable give infinite standard errors.

- Added `covariateTest()` (alias `tost()`) for effects represented in the population FIM after `run()`. Available tables address statistical significance, equivalence/non-relevance through two one-sided tests (TOST), and clinical relevance; outputs include effect ratios, relevant log-ratio bounds, power and required sample size for a target power. `saveCovariateTest()` exports the tables. These tests do not apply to individual/Bayesian subject-level precision matrices lacking the required covariate-effect block.

- Enabled covariate-aware design evaluation and optimization, with covariate tables in HTML evaluation reports. Population `plotSE()` and `plotRSE()` include typical-parameter and covariate-effect blocks in the FIM parameter order.

## Design optimization and sampling constraints

- Added or reimplemented five optimizers with compiled numerical kernels: Multiplicative and Fedorov-Wynn for discrete candidate designs, and Simplex, particle swarm optimization (PSO) and population genetics-based optimization (PGBO) for continuous sampling-time optimization.

- Supported joint discrete optimization across multiple arms within one design and candidate grids for sampling times and administrations. Where no `administrationsConstraints` is supplied, discrete optimizers use the arm's existing administration as the dose grid.

- Used Hamilton's largest-remainder method to turn discrete design weights into integer subject counts whose sum equals the requested study size.

- Added sampling windows (`samplingsWindows`) and counts per window (`numberOfTimesByWindows`) for continuous optimization, including checks for consistency with initial sampling schedules and feasibility constraints.

- For the multiplicative algorithm, checked the optimality condition before updating the returned weights. Exposed mixture and realized D-criteria separately through `getMixtureDcriterion()` and `getRealisedDcriterion()`. In joint multi-outcome optimization, weights are restricted to the selected protocol, with subjects distributed across that protocol's arms.

- Set the default Fedorov-Wynn optimality gap to `1e-4`. Reaching the cycle limit without meeting the stopping criterion is reported as `incomplete`, with a warning, rather than as a successful optimization.

- Corrected Simplex behaviour when `numberOfTimesByWindows` increases and rejected infeasible initial designs instead of reporting convergence. Continuous optimizers report `converged = NA` if `tolerance <= 0` disables their tolerance-based stopping rule; they warn when arm sampling times differ from `initialSamplings`, the actual optimization starting point.

## Models, residual errors and numerical reliability

- Supported custom analytical and ODE models as well as an expanded PK/PD library, including bolus, infusion, absorption, steady-state, multi-compartment, turnover and Michaelis-Menten models.

- Added algebraic observed responses through `outputs` in `Evaluation()` and `Optimization()`. Sampling times refer to the underlying model states, while residual error definitions refer to observed outputs, including transformed scales such as concentration or log-transformed response.

- Implemented `Constant`, `Proportional`, `Combined1` and `Combined2` residual-error models. Residual parameters are labelled as standard deviations (`sigma`), not variances, consistent with their stored values.

- Corrected population-FIM variance calculations when a typical parameter is fixed (`fixedMu = TRUE`) but its inter-individual variance remains estimable: the model gradient is retained where needed for the variance contribution. `fixedMu` and `fixedOmega` are treated independently.

- Improved treatment of rank-deficient information matrices: non-identifiable directions report infinite standard errors, and rank deficiency is reflected in the condition number rather than masked by very large finite values.

- Separated ODE finite-difference step selection from solver tolerance to avoid inflating derivative steps and biasing FIM calculations.

- Corrected analytic-model evaluation for time-dependent, non-administered outcomes at their own sampling times. Corrected formulas for selected two-compartment bolus/infusion models, one-compartment steady-state infusion, exponential baselines, sigmoid turnover and Michaelis-Menten ODE models using central volume `V1`. Unknown model names are rejected.

- Preserved zero-valued covariate effects as estimable population-FIM columns, allowing standard errors to be computed under the null hypothesis, with a warning.

- Improved input checks for non-finite/`NA` parameters, unknown initial conditions and undeclared covariate categories, with explicit `PFIM:` errors.

## Performance, reporting and package compatibility

- Moved core FIM assembly, residual-variance calculations and optimization computations into C++ with Rcpp/Armadillo. Added caches for FIMs, ODE trajectories and finite-difference gradients; continuous optimizers reuse a common evaluation context during a search. Performance depends on the model and design; no universal acceleration factor is asserted.

- Added HTML reporting for evaluations and optimizations through `Report()`. Reporting restores knitr chunk options and preserves the user's open graphics devices.

- Aligned population SE/RSE plot ordering with `show()`, including typical parameters (`mu`), covariate effects (`beta`), inter-individual variance (`omega^2`), occasion-level parameters (`gamma`) and residual error (`sigma`).

- Improved compatibility with current S7 and R-devel by registering optional S7 class unions with S4 and using quoted expressions for empty property defaults.

- Included prebuilt HTML vignettes and a vignette index (`inst/doc/meta/vignette.rds`) to address CRAN incoming package checks.

- Skipped side-car `Report()` HTML while vignettes are woven under `R CMD check`. The thirteen reports (vignettes 1–3 and 5) are written only to a temporary directory and are not part of the vignette. Cached evaluation and optimization results are still loaded, and plots are still drawn. `R CMD build` outside a check still writes the reports.

- Skipped the G-CSF / PopED 11-state ODE FIM comparison on CRAN (`PFIM_GCSF_FORCE_RUN`). It still runs when `NOT_CRAN=true`.

- Relaxed the **test comparison tolerance** for reference Michaelis-Menten ODE FIM determinants from `1e-6` to `1e-3` relative error to accommodate cross-platform ODE and finite-difference variability observed on arm64 macOS. This change concerns regression-test acceptance, not the requested accuracy of the underlying numerical algorithms.

## Documentation and verification examples

- Added `Vignette_01_discrete_design` (discrete optimization), `Vignette_02_continuous_design` (continuous optimization), `Vignette_03_covariates_analytical_pk` (categorical covariates, IOV and effect tests), `Vignette_04_tim_waterhouse_pediatric` (a pediatric sparse-design example based on Tim Waterhouse/PopED), `Vignette_05_gcse_pkpd` (a G-CSF/filgrastim PK/PD example with a PopED Design 1 RSE comparison), and `LibraryOfModels`.

- Added or updated automated tests for analytic/ODE FIMs, optimization branches, covariates/IOV and numerical reference cases, including the G-CSF Design 1 comparison. Numerical comparison to a reference case is not a claim that all models or algorithms have been independently validated.

## Contributors

- **Romain Leroux** (`aut`): Author and developer of the entire PFIM 8 codebase, including all R/S7 and C++ source code, numerical algorithms, automated tests and technical documentation. 

- **France Mentré** (`cre`): CRAN package maintainer.

- **Jérémy Seurat** (`ctb`): contributions (two vignettes).

- **Antoine Croxo** (`ctb`): contributions (one vignette), beta-testing.

- **Lucie Fayette** (`ctb`): contributions (one vignette), beta-testing and logo design.

- The pediatric sparse-design example (`Vignette_04_tim_waterhouse_pediatric`) and the G-CSF/filgrastim PK/PD example (`Vignette_05_gcse_pkpd`) follow Tim Waterhouse's webinar material *A Gentle Introduction to Optimal Design for Pharmacometric Models* (<https://github.com/timwaterhouse/optimal-design-2023>).
