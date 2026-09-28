# Flat sampling layout shared by the Simplex, PSO and PGBO optimizers.

#' Flatten sampling times across arms/outcomes for metaheuristic search.
#'
#' Builds a single decision vector plus per-group windows and feasibility
#' checkers used by Simplex / PSO / PGBO. R=>C++ flat-sampling contract
#' (C++ argument names in parentheses):
#' \itemize{
#'   \item \code{initialFlat} (\code{initial_pos}): concatenation of
#'     \code{samplings} in arm order.
#'   \item \code{windowsList} (\code{windows_list}): length =
#'     \code{length(initialFlat)}; entry \eqn{j} is the window matrix for
#'     coordinate \eqn{j} (PSO clamps here; PGBO/Simplex rely on R validity
#'     callbacks instead).
#'   \item \code{sortingGroups} (\code{sorting_groups}): 1-based index vectors
#'     (one per arm/outcome); C++ sorts within each group so spacing
#'     constraints see ordered times.
#'   \item \code{groupSpecs}: R-only metadata for constraint checks / reseeding.
#' }
#' @noRd
#' @keywords internal
.buildFlatSamplingLayout = function( design ) {
  arms = prop( design, "arms" )
  # Flatten (arm x samplingTimes) into one decision vector + group metadata.
  entries = arms |>
    map( function( arm ) {
      armName = prop( arm, "name" )
      map( prop( arm, "samplingTimes" ), function( st ) {
        outcome    = prop( st, "outcome" )
        samps      = prop( st, "samplings" )
        constraint = pluck(
          keep( prop( arm, "samplingTimesConstraints" ), \( x ) prop( x, "outcome" ) == outcome ),
          1L
        )
        win       = prop( constraint, "samplingsWindows" )
        winMatrix = do.call( rbind, map( win, unlist ) )
        list(
          samps     = samps,
          nSamps    = length( samps ),
          winMatrix = winMatrix,
          spec      = list(
            arm = arm, armName = armName, outcome = outcome, constraint = constraint
          )
        )
      } )
    } ) |>
    list_flatten()

  nSamps = map_int( entries, "nSamps" )
  ends   = cumsum( nSamps )
  starts = ends - nSamps + 1L
  list(
    initialFlat   = list_c( map( entries, "samps" ) ),
    # Repeat each group's window matrix once per coordinate in that group.
    windowsList   = list_c( map2( entries, nSamps, \( x, y ) rep( list( x$winMatrix ), y ) ) ),
    sortingGroups = map2( starts, ends, \( x, y ) seq( x, y ) ),
    groupSpecs    = map( entries, "spec" )
  )
}

#' Draw a feasible flat sampling vector from window constraints.
#' @noRd
#' @keywords internal
.sampleFlatFromConstraints = function(
    layout,
    sampler = generateSamplingsFromSamplingConstraints ) {
  # Independent draws per arm/outcome group; sort for spacing checks.
  draws = map( layout$groupSpecs, \( spec ) sort( sampler( spec$constraint ) ) )
  flat  = layout$initialFlat
  flat[ unlist( layout$sortingGroups ) ] = unlist( draws )
  flat
}

#' Check one arm/outcome group of a flat sampling vector against constraints.
#' @noRd
#' @keywords internal
.checkFlatValidGroup = function( layout, flatPos, groupId ) {
  spec      = layout$groupSpecs[[ groupId ]]
  idx       = layout$sortingGroups[[ groupId ]]
  samplings = flatPos[ idx ]
  all( unlist( checkSamplingTimeConstraintsForMetaheuristic(
    spec$constraint, spec$arm, samplings, spec$outcome
  ) ) )
}

#' Whether every group in a flat sampling vector satisfies its constraints.
#' @noRd
#' @keywords internal
.isFlatValid = function( layout, flatPos ) {
  every( seq_along( layout$groupSpecs ), \( g ) .checkFlatValidGroup( layout, flatPos, g ) )
}

# Large fitness penalty / tiny D-criterion stand-ins for infeasible metaheuristic
# candidates. Keep these in sync with C++ kernels that treat huge cost / tiny D
# as "invalid design" sentinels (not real information).
.metaheuristicFitnessPenalty = 1e7
.metaheuristicInvalidDcriterion = 1e-30
