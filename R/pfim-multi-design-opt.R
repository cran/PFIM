#' @include Optimization.R
#' @include pfim-fim-cache.R
#' @keywords internal
NULL

# Multi-design optimization driver (continuous and discrete).
#
# When `length(designs) > 1`, run one independent search per design. The primary
# `optimisationDesign` / algorithm outputs stay on design 1; other designs are
# stored under `optimisationAlgorithmOutputs$perDesign`.

#' Run an optimizer once per design, then reassemble the Optimization.
#'
#' @param optimizationObject An \code{Optimization} with one or more designs.
#' @param optimizationAlgorithm Algorithm object (Multiplicative, FW, PSO, ...).
#' @param optimizeOneDesign Function(\code{optimizationObject}, \code{algorithm})
#'   that optimizes the single design currently in \code{projectProp(..., "designs")}.
#' @param beginCache Open a FIM cache scope first. Discrete optimizers pass
#'   \code{FALSE}: \code{generateFimsFromConstraints} opens its own.
#' @return Updated \code{Optimization} with all designs optimized.
#' @noRd
#' @keywords internal
.pfimOptimizeDesigns = function( optimizationObject, optimizationAlgorithm, optimizeOneDesign, beginCache = TRUE ) {
  if ( isTRUE( beginCache ) )
    .pfimFimCacheBegin()
  designs = projectProp( optimizationObject, "designs" )
  if ( length( designs ) <= 1L )
    return( optimizeOneDesign( optimizationObject, optimizationAlgorithm ) )

  state = reduce(
    seq_along( designs ),
    function( state, i ) {
      # Restrict the project to one design for this search.
      projectProp( state$obj, "designs" ) = list( designs[[ i ]] )
      obj       = optimizeOneDesign( state$obj, optimizationAlgorithm )
      optDesign = projectProp( obj, "designs" )[[ 1L ]]
      outputs   = prop( obj, "optimisationAlgorithmOutputs" )
      per       = list(
        name               = prop( optDesign, "name" ),
        optimisationDesign = prop( obj, "optimisationDesign" ),
        optimalArms        = pluck( outputs, "optimalArms" )
      )
      list(
        obj       = obj,
        optimized = c( state$optimized, list( optDesign ) ),
        perDesign = c( state$perDesign, list( per ) ),
        # Design 1 carries the primary outputs.
        primary   = state$primary %||% list(
          optimisationDesign           = prop( obj, "optimisationDesign" ),
          optimisationAlgorithmOutputs = outputs
        )
      )
    },
    .init = list( obj = optimizationObject, optimized = list(), perDesign = list(), primary = NULL )
  )

  optimizationObject = state$obj
  projectProp( optimizationObject, "designs" ) = state$optimized
  prop( optimizationObject, "optimisationDesign" ) = state$primary$optimisationDesign
  outputs = state$primary$optimisationAlgorithmOutputs
  outputs$perDesign = state$perDesign
  prop( optimizationObject, "optimisationAlgorithmOutputs" ) = outputs
  optimizationObject
}
