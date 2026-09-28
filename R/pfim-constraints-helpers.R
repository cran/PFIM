#' Constraint tables for optimization reports.
#'
#' Builds \code{kableExtra} tables of arm-level dose/sampling constraints for
#' discrete (Fedorov-Wynn / Multiplicative) and continuous (PSO / PGBO / Simplex)
#' optimizers. Uses \code{getArmConstraints()} from \code{pfim-arm-constraints.R}.
#' @include pfim-arm-constraints.R
#' @include pfim-utils.R
#' @name pfim-constraints-helpers
NULL

#' Build a kableExtra table from the arm constraints of the first design.
#' @noRd
#' @keywords internal
.constraintsKbl = function( arms, optimizationAlgorithm, colNames ) {
  df = .constraintsArmsTable( map( pluck( arms, 1L ), \( arm ) getArmConstraints( arm, optimizationAlgorithm ) ) )
  colnames( df ) = colNames
  kbl( df, align = c( "l", rep( "c", ncol( df ) - 1L ) ) ) |>
    kable_styling( bootstrap_options = "hover", full_width = FALSE,
                   position = "center", font_size = 13 ) |>
    .pfimKableHeaderGray()
}

#' kableExtra table of discrete-optimizer arm constraints for reports.
#' @param optimizationAlgorithm An optimization algorithm object (Simplex, Fedorov-Wynn, etc.).
#' @param arms List of \code{Arm} objects from the design.
#' @return A \code{kableExtra} table.
#' @noRd
#' @keywords internal
.pfimConstraintsTableDiscrete = function( optimizationAlgorithm, arms ) {
  .constraintsKbl(
    arms, optimizationAlgorithm,
    c( "Arms name", "Number of subjects", "Outcome",
       "Initial samplings", "Fixed times",
       "Number of samplings optimisable", "Dose constraints" )
  )
}

#' kableExtra table of continuous-optimizer arm constraints for reports.
#' @param optimizationAlgorithm A continuous optimizer object (PSO, PGBO, Simplex on windows).
#' @param arms List of \code{Arm} objects from the design.
#' @return A \code{kableExtra} table.
#' @noRd
#' @keywords internal
.pfimConstraintsTableContinuous = function( optimizationAlgorithm, arms ) {
  .constraintsKbl(
    arms, optimizationAlgorithm,
    c( "Arms name", "Number of subjects", "Outcome",
       "Initial samplings", "Samplings windows", "Number of times by windows", "Min sampling" )
  )
}
