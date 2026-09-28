# Plot helpers for evaluation reports (response and sensitivity curves).
#
# Re-evaluate on a dense time grid (step 0.05), overlay design
# sampling times as red markers. FIM evaluation itself stays at design times.

#' Cap for densify via \code{updateSamplingTimes} (long horizons stay tractable).
#' @noRd
#' @keywords internal
.pfimDefaultPlotMaxPoints = function() 400L

#' Time grid densifier for plot re-evaluation (keeps design times, adds steps).
#'
#' Uses \code{seq(0, tmax, by = 0.05)} when that stays under
#' \code{maxPoints}; otherwise the step grows so the curve remains smooth.
#' @noRd
#' @keywords internal
.pfimDensePlotTimes = function(
    times,
    maxPoints = .pfimDefaultPlotMaxPoints(),
    minStep   = 0.05 ) {
  times = sort( unique( as.numeric( times ) ) )
  if ( !length( times ) ) return( times )
  tmax = max( times, 0 )
  nGrid = as.integer( floor( tmax / minStep ) ) + 1L
  if ( nGrid > maxPoints )
    minStep = tmax / max( maxPoints - 1L, 1L )
  sort( unique( c( times, seq( 0, tmax, by = minStep ) ) ) )
}

#' Copy of an arm with densified sampling grids for response / SI plots.
#' @noRd
#' @keywords internal
.pfimDensifyArmForPlots = function( arm ) {
  updateSamplingTimes( arm, getSamplingData( arm ) )
}

#' Model bound to a (possibly densified) plot arm; optional FD stencil for SI.
#' @noRd
#' @keywords internal
.pfimPreparePlotModel = function( model, plotArm, needFd = FALSE ) {
  modelPlot = .pfimPrepareModelForEvaluation( model, plotArm )
  if ( isTRUE( needFd ) ) {
    gp = prop( modelPlot, "parametersForComputingGradient" )
    if ( is.null( gp ) || is.null( gp$shifted ) )
      modelPlot = finiteDifferenceHessian( modelPlot )
  }
  modelPlot
}

#' Sampling grids of an arm keyed by response names (RespPK) when outputs map
#' to state names (Cc).
#' @noRd
#' @keywords internal
.pfimArmSamplingsByResponse = function( arm, model, outputNames ) {
  .odeSamplingsByOutput(
    getSamplingData( arm )$samplingTimes,
    unlist( outputNames, use.names = FALSE ),
    .getModelOutputFormulas( model )
  )
}

#' Evaluated arms from \code{run()} when available (carry cached model/gradient results).
#' @noRd
#' @keywords internal
.pfimEvaluatedArmsForDesign = function( pfimproject, design ) {
  evaluated = prop( pfimproject, "evaluationDesign" )
  if ( length( evaluated ) ) {
    idx = match( prop( design, "name" ), map_chr( evaluated, \( x ) prop( x, "name" ) ) )
    if ( !is.na( idx ) ) {
      arms = prop( evaluated[[ idx ]], "evaluationArms" )
      if ( length( arms ) ) return( arms )
    }
  }
  prop( design, "arms" )
}

#' Minimum number of distinct sampling times required for response/SI plots.
#' @noRd
#' @keywords internal
.pfimMinSamplingTimesForPlots = function() 2L

#' Whether an arm has enough sampling times to build response/SI plots.
#' @noRd
#' @keywords internal
.pfimArmPlotsEnabled = function( arm, model, outputNames ) {
  if ( !length( unlist( outputNames ) ) ) return( FALSE )
  counts = map_int(
    .pfimArmSamplingsByResponse( arm, model, outputNames ),
    \( x ) length( unique( stats::na.omit( as.numeric( x ) ) ) )
  )
  all( counts >= .pfimMinSamplingTimesForPlots() )
}

#' Plots of one arm nested as \code{list(<design> = list(<arm> = plots))}.
#' @noRd
#' @keywords internal
.pfimArmPlotResult = function( designName, arm, plots = list() ) {
  set_names( list( set_names( list( plots ), prop( arm, "name" ) ) ), designName )
}

#' Drop covariate beta columns from sensitivity-index parameter names.
#' @noRd
#' @keywords internal
.pfimSiParamNamesNoBeta = function( names ) {
  if ( !length( names ) ) return( names )
  names[
    !startsWith( names, "beta_" ) &
      !startsWith( names, "\u03b2_" )
  ]
}

#' Proportion-weighted gradient matrix for one model output (covariate structure).
#' @param gradients Nested (covariate x occasion) gradients of an arm.
#' @noRd
#' @keywords internal
.pfimAggregateGradientForOutput = function( gradients, outName ) {
  zeroGrad = pluck( gradients, 1L, "gradients", 1L, "gradient", outName ) * 0
  matList = map( gradients, function( combinationData ) {
    nOcc     = length( combinationData$gradients )
    meanGrad = reduce(
      map( combinationData$gradients, \( x ) x$gradient[[ outName ]] ),
      `+`
    ) / nOcc
    combinationData$proportion * meanGrad
  } )
  reduce( matList, `+`, .init = zeroGrad )
}

#' Convert freshly computed gradients to per-output data frames with time.
#'
#' Nested gradients are aggregated per output; their columns are named after
#' the aggregated matrix, falling back to \code{parametersNames}.
#' @noRd
#' @keywords internal
.pfimSiFramesFromGradients = function(
    evaluationModelGradient, model, arm, outputNames, parametersNames ) {
  outNames = unlist( outputNames, use.names = FALSE )
  stList = .pfimArmSamplingsByResponse( arm, model, outNames )
  if ( .isNestedArmEvaluation( evaluationModelGradient ) ) {
    gradByOutput = map( outNames, \( out ) .pfimAggregateGradientForOutput( evaluationModelGradient, out ) )
    colNames = colnames( gradByOutput[[ 1L ]] )
    if ( !length( colNames ) )
      colNames = parametersNames
    frames = map2( outNames, gradByOutput, function( out, gmat ) {
      df = as.data.frame( gmat, stringsAsFactors = FALSE )
      colnames( df ) = colNames
      df$time = stList[[ out ]][ seq_len( nrow( gmat ) ) ]
      df
    } )
    set_names( frames, outNames )
  } else {
    map( outNames, function( out ) {
      grad = evaluationModelGradient[[ out ]]
      df   = as.data.frame( grad, stringsAsFactors = FALSE )
      df$time = stList[[ out ]][ seq_len( nrow( df ) ) ]
      df
    } ) |> set_names( outNames )
  }
}
