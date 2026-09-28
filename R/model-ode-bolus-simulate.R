# ODE bolus simulation: sampling extraction, deSolve call, and cached admin replay.

#' Map each output name to its sampling-time vector from the arm.
#'
#' Resolves via output-formula targets when the sampling outcome differs from
#' the reported output name.
#' @param samplingTimes List of \code{SamplingTimes} objects.
#' @param outputNames Character vector of model outputs.
#' @param outputFormula Optional output-formula mapping.
#' @return Named list of numeric sampling vectors.
#' @noRd
#' @keywords internal
.odeSamplingsByOutput = function( samplingTimes, outputNames, outputFormula = list() ) {
  sampOutcomes = map_chr( samplingTimes, \( x ) prop( x, "outcome" ) )
  set_names(
    map( outputNames, function( outName ) {
      x = if ( outName %in% names( outputFormula ) ) outputFormula[[ outName ]]
      target = if ( is.character( x ) && length( x ) == 1L ) x
        else if ( is.symbol( x ) ) as.character( x )
        else outName
      idx = match( target, sampOutcomes )
      if ( is.na( idx ) ) idx = match( outName, sampOutcomes )
      if ( is.na( idx ) ) idx = 1L
      prop( samplingTimes[[ idx ]], "samplings" )
    } ),
    outputNames
  )
}

#' Extract outputs at requested sampling times.
#' @param evaluationModelTmp Data frame returned by ODE integration.
#' @param samplingTimes List of \code{SamplingTimes} objects.
#' @param outputNames Character vector of requested outputs.
#' @param outputFormula Optional parsed output formulas.
#' @return Named list of output data frames by outcome.
#' @noRd
#' @keywords internal
.odeExtractOutputAtSamplingTimes = function(
    evaluationModelTmp, samplingTimes, outputNames, outputFormula = list() ) {
  samplingsList = .odeSamplingsByOutput( samplingTimes, outputNames, outputFormula )
  times = evaluationModelTmp$time
  set_names(
    map( outputNames, function( outName ) {
      colName  = .odeColumnForOutput( outName, evaluationModelTmp, outputFormula )
      reqTimes = samplingsList[[ outName ]]
      idx      = map_int( reqTimes, \( rt ) which.min( abs( times - rt ) ) )
      data.frame( time = reqTimes, evaluationModelTmp[ idx, colName, drop = FALSE ] ) |>
        set_names( c( "time", outName ) )
    } ),
    outputNames
  )
}

# deSolve call (three bolus modes); events for doseEvent/bolusIc only.
#' Simulate ODE trajectories for bolus administration modes.
#' @param model \code{ModelODE} object with prepared administration fields
#'   (samplings, dose events, solver inputs).
#' @param mode Character mode among bolus administration strategies.
#' @return Data frame of integrated states and outputs.
#' @noRd
#' @keywords internal
.odeSimulateBolus = function( model, mode ) {
  odeSolverParameters = prop( model, "odeSolverParameters" )
  rawSamplings        = prop( model, "samplings" )
  eventsDf            = if ( mode != "doseInEq" ) prop( model, "doseEvent" ) else NULL
  simKey              = .pfimOdeSimCacheKey( mode, rawSamplings, eventsDf )
  simTimes            = .pfimOdeSimTimesCached( simKey, rawSamplings, eventsDf )
  odeFn               = if ( mode == "doseInEq" )
    prop( model, "modelODEDoseInEquations" ) else prop( model, "modelODE" )
  parms = if ( mode == "doseInEq" ) prop( model, "solverInputs" ) else NULL
  tol   = .pfimDeSolveTolerances( odeSolverParameters )

  ode(
    prop( model, "initialConditions" ),
    simTimes,
    odeFn,
    parms,
    events = if ( !is.null( eventsDf ) ) list( data = eventsDf ) else NULL,
    atol   = tol$atol,
    rtol   = tol$rtol
  ) |> as.data.frame()
}

#' Evaluate bolus ODE model outputs at arm sampling times.
#' @param model \code{ModelODE} object with prepared administration state.
#' @param arm \code{Arm} object providing requested sampling times.
#' @return Named list of output data frames.
#' @noRd
#' @keywords internal
.odeEvaluateModelCore = function( model, arm ) {
  mode = .odeBolusMode( model )
  evaluationModelTmp = .odeSimulateBolus( model, mode )
  .odeExtractOutputAtSamplingTimes(
    evaluationModelTmp,
    prop( arm, "samplingTimes" ),
    prop( model, "outputNames" ),
    .getModelOutputFormulas( model )
  )
}

#' Evaluate bolus ODE model with optional covariate expansion.
#' @param model \code{ModelODE} object.
#' @param arm \code{Arm} object used for evaluation.
#' @return Output structure from direct or covariate-expanded evaluation.
#' @noRd
#' @keywords internal
.odeEvaluateBolus = function( model, arm ) {
  if ( usesCovariateOccasionStructure( model ) )
    evaluateModelWithCovariates( model, arm, .odeEvaluateModelCore )
  else
    .odeEvaluateModelCore( model, arm )
}

# Fast path for gradient cache: reapply stored administration template to new mu.
#' Reapply cached ODE administration entry to current parameters.
#' @param model \code{ModelODE} object to update.
#' @param arm \code{Arm} object used for initial-condition context.
#' @param entry Cached administration entry list.
#' @return Updated \code{ModelODE} object.
#' @noRd
#' @keywords internal
.odeApplyAdminEntry = function( model, arm, entry ) {
  mu = .extractMu( prop( model, "modelParameters" ) )
  mode = entry$type
  if ( mode == "doseInEq" ) {
    .odeFinalizeAdministration(
      model, mode, mu,
      evaluateInitialConditions( model, arm ), entry$samplings,
      entry$wrapper, entry$functionArguments, entry$functionArgumentsSymbols, entry$outputFormula,
      solverInputs = entry$solverInputs,
      outcomesWithAdministration = entry$outcomesWithAdministration
    )
  } else {
    doseEvent = entry$doseEventTemplate
    initialConditions = if ( mode == "bolusIc" ) {
      ic = .odeInitialConditionsBolus( model, arm, doseEvent )
      env = .odeIcEvalEnv( as.list( mu ) )
      doseEvent = .applyInitialConditionsToEvent(
        doseEvent, entry$initialConditionsParsed, env
      )
      ic
    } else {
      .odeInitialConditionsDoseEvent( model, arm, doseEvent )
    }
    .odeFinalizeAdministration(
      model, mode, mu, initialConditions, entry$samplings,
      entry$wrapper, entry$functionArguments, entry$functionArgumentsSymbols, entry$outputFormula,
      doseEvent = doseEvent
    )
  }
}
