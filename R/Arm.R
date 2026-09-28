#' @title Arm
#' @description
#' One experimental arm: subjects, dosing, sampling times, and slots filled during
#' design evaluation (model predictions, gradients, variance, FIM).
#' @param name Character string: arm identifier.
#' @param size Numeric: number of subjects in the arm.
#' @param administrations List of \code{Administration} objects.
#' @param initialConditions List of ODE initial conditions (name = state).
#'   Values may be numeric or a character expression in typical values and
#'   \code{dose_<state>} (e.g. \code{"dose_Cc/V"}). Expressions are evaluated
#'   in a sealed environment (no session symbols). List order does not set
#'   the deSolve state order (that follows \code{Deriv_*} declaration).
#'   In event-based bolus mode the value for a compartment dosed at \code{t = 0}
#'   is replaced by 0: the dose mass enters through the event table.
#' @param initialCondition Alias of \code{initialConditions}.
#' @param samplingTimes List of \code{SamplingTimes} objects.
#' @param administrationsConstraints List of \code{AdministrationConstraints} objects.
#' @param samplingTimesConstraints List of \code{SamplingTimeConstraints} objects.
#' @param evaluationModel Model predictions at sampling times (filled by \code{evaluateArm}).
#' @param evaluationGradients Gradients of responses w.r.t. parameters (nested if covariates/IOV).
#' @param evaluationVariance Residual variance structure for the arm.
#' @param evaluationFim FIM object after \code{evaluateFim}.
#' @include Model.R
#' @include Fim.R
#' @include Administration.R
#' @include AdministrationConstraints.R
#' @return An S7 object of class \code{Arm}.
#' @examples
#' \donttest{
#' source(system.file("examples", "evaluation-minimal.R", package = "PFIM"))
#' arm = prop(prop(ev, "designs")[[1L]], "arms")[[1L]]
#' length(prop(prop(arm, "samplingTimes")[[1L]], "samplings"))
#' }
#' @export

Arm = new_class("Arm", package = "PFIM",

                properties = list(
                  name = new_property(class_character, default = character(0)),
                  size = new_property(class_double, default = numeric(0)),
                  administrations = new_property(class_list, default = list()),
                  initialConditions = new_property(class_list, default = list()),
                  samplingTimes = new_property(class_list, default = list()),
                  administrationsConstraints = new_property(class_list, default = list()),
                  samplingTimesConstraints = new_property(class_list, default = list()),
                  evaluationModel = new_property(class_list, default = list()),
                  evaluationGradients = new_property(class_list, default = list()),
                  evaluationVariance = new_property(class_list, default = list()),
                  evaluationFim = new_property(NULL | Fim, default = NULL)
                ),
                validator = function( self ) {
                  n = prop( self, "name" )
                  if ( length( n ) > 1L )
                    return( "Arm: name must be a single string." )
                  if ( .pfimIsBlankScalar( n ) )
                    return( "Arm: name must be a non-empty string." )
                  size = prop( self, "size" )
                  if ( length( size ) == 1L && ( !is.finite( size ) || size < 0 ) )
                    return( "Arm: size must be non-negative." )
                  .validateS7List( prop( self, "administrations" ), Administration,
                                   "Arm:administrations", "Administration" ) %||%
                    .validateS7List( prop( self, "samplingTimes" ), SamplingTimes,
                                     "Arm:samplingTimes", "SamplingTimes" ) %||%
                    .validateS7List( prop( self, "administrationsConstraints" ), AdministrationConstraints,
                                     "Arm:administrationsConstraints", "AdministrationConstraints" ) %||%
                    .validateS7List( prop( self, "samplingTimesConstraints" ), SamplingTimeConstraints,
                                     "Arm:samplingTimesConstraints", "SamplingTimeConstraints" )
                },
                constructor = function(
                  name                         = character( 0 ),
                  size                         = numeric( 0 ),
                  administrations              = list(),
                  initialConditions            = list(),
                  initialCondition             = NULL,
                  samplingTimes                = list(),
                  administrationsConstraints   = list(),
                  samplingTimesConstraints     = list(),
                  evaluationModel              = list(),
                  evaluationGradients          = list(),
                  evaluationVariance           = list(),
                  evaluationFim                = NULL
                ) {
                  if ( !is.null( initialCondition ) ) {
                    if ( length( initialConditions ) )
                      .pfimStop( "Arm: use initialConditions or initialCondition, not both." )
                    initialConditions = initialCondition
                  }
                  new_object(
                    S7_object(),
                    name                       = name,
                    size                       = size,
                    administrations            = administrations,
                    initialConditions          = initialConditions,
                    samplingTimes              = samplingTimes,
                    administrationsConstraints = administrationsConstraints,
                    samplingTimesConstraints   = samplingTimesConstraints,
                    evaluationModel            = evaluationModel,
                    evaluationGradients        = evaluationGradients,
                    evaluationVariance         = evaluationVariance,
                    evaluationFim              = evaluationFim
                  )
                })


#' Report rows from each arm of the first design on a project.
#' @param pfimproject \code{Evaluation} or \code{Optimization} project.
#' @return Flat list of \code{getArmData()} results.
#' @noRd
#' @keywords internal
.armsDataFromEvaluation = function( pfimproject ) {
  design = prop( pfimproject, "designs" )[[ 1L ]]
  list_flatten( map( prop( design, "arms" ), getArmData ) )
}



#' Evaluate model, gradients, variance and FIM for one arm
#' @param arm An \code{Arm} object.
#' @param ... Model and Fim prototype (see methods).
#' @usage evaluateArm(arm, ...)
#' @name evaluateArm
#' @keywords internal
evaluateArm = new_generic( "evaluateArm", c( "arm" ) )

#' Sampling times and grid extent for evaluation plots
#' @param arm An \code{Arm} object.
#' @param ... Not used.
#' @name getSamplingData
#' @keywords internal
getSamplingData = new_generic( "getSamplingData", c( "arm" ) )

#' Refine the time grid used in response and SI plots
#' @param arm An \code{Arm} object.
#' @param ... Output of \code{\link{getSamplingData}}.
#' @usage updateSamplingTimes(arm, ...)
#' @name updateSamplingTimes
#' @keywords internal
updateSamplingTimes = new_generic( "updateSamplingTimes", c( "arm" ) )

#' Model response plots for one arm
#' @param arm An \code{Arm} object.
#' @param model A \code{Model} object.
#' @param fim A \code{Fim} object.
#' @param ... Design label and plot options (see methods).
#' @usage processArmEvaluationResults(arm, model, fim, ...)
#' @name processArmEvaluationResults
#' @keywords internal
processArmEvaluationResults = new_generic( "processArmEvaluationResults", c( "arm", "model", "fim" ) )

#' Sensitivity-index plots for one arm
#' @param arm An \code{Arm} object.
#' @param model A \code{Model} object.
#' @param fim A \code{Fim} object.
#' @param ... Design label and plot options (see methods).
#' @usage processArmEvaluationSI(arm, model, fim, ...)
#' @name processArmEvaluationSI
#' @keywords internal
processArmEvaluationSI = new_generic( "processArmEvaluationSI", c( "arm", "model", "fim" ) )

#' ggplot of predicted responses with sampling points
#' @param arm An \code{Arm} object.
#' @param ... Method-specific plotting inputs (see methods).
#' @usage plotEvaluationResults(arm, ...)
#' @name plotEvaluationResults
#' @keywords internal
plotEvaluationResults = new_generic( "plotEvaluationResults", c( "arm" ) )

#' ggplot of parameter sensitivities over time
#' @param arm An \code{Arm} object.
#' @param ... Method-specific sensitivity-index plotting inputs (see methods).
#' @usage plotEvaluationSI(arm, ...)
#' @name plotEvaluationSI
#' @keywords internal
plotEvaluationSI = new_generic( "plotEvaluationSI", c( "arm" ) )

#' Dose and sampling summary for reports
#' @param arm An \code{Arm} object.
#' @param ... Not used.
#' @name getArmData
#' @keywords internal
getArmData = new_generic( "getArmData", c( "arm" ) )

#' Tabular administration settings for reports
#' @param arm An \code{Arm} object.
#' @param ... Not used.
#' @name armAdministration
#' @keywords internal
armAdministration = new_generic( "armAdministration", c( "arm", "designName" ) )

#' Evaluate model, gradients, variance and FIM for one arm.
#'
#' With covariates/IOV, uses the occasion-averaged evaluation core; otherwise
#' runs the flat path. The FIM template is reset by \code{.duplicateFim()}
#' before \code{evaluateFim()}.
#'
#' Flat path reuses the FD grid's nominal column as \code{evaluationModel}
#' (one fewer full model solve per arm - same pattern as the covariate path).
#' @return \code{Arm} populated with model, gradient, variance, and FIM results.
#' @name evaluateArm
#' @keywords internal
method( evaluateArm, Arm ) = function( arm, model, fim ) {
  model = .pfimPrepareModelForEvaluation( model, arm )
  if ( usesCovariateOccasionStructure( model ) && .hasCovariateEvaluationCore( model ) ) {
    # Nested predictions/gradients over covariate x occasion combinations.
    out = .evaluateCovariateOccasions(
      model, arm,
      evaluateModelCore         = .covariateEvaluationCore( model ),
      evaluateModelGradientCore = evaluateModelGradientCore
    )
    arm = set_props(
      arm,
      evaluationModel     = out$evaluationModel,
      evaluationGradients = out$evaluationGradients,
      evaluationVariance  = out$evaluationVariance
    )
  } else {
    # FD column 0 is the nominal evaluation - avoid a second evaluateModel().
    fdOut = .evaluateModelGradientCoreInner( model, arm )
    arm = set_props( arm, evaluationModel = fdOut$nominalEvaluation, evaluationGradients = fdOut$gradTheta )
    # The residual variance reads the evaluationModel just stored.
    prop( arm, "evaluationVariance" ) = evaluateModelVariance( model, arm )
  }
  prop( arm, "evaluationFim" ) = evaluateFim( .duplicateFim( fim ), model, arm )
  arm
}

#' Default method for \code{Arm}.
#' @param arm First argument of generic.
#' @return List of administration rows formatted for reports.
#' @name armAdministration
#' @keywords internal
method( armAdministration, list( Arm, class_character ) ) = function( arm, designName ) {
  armName   = prop( arm, "name" )
  armSize   = round( prop( arm, "size" ), 2 )
  adminList = prop( arm, "administrations" )
  map( adminList, function( adm ) {
    list(
      "Design name"        = designName,
      "Arms name"          = armName,
      "Number of subjects" = as.character( armSize ),
      "Outcome"            = prop( adm, "outcome" ),
      "Dose"               = as.character( prop( adm, "dose" ) ),
      "Time of dose"       = if ( length( prop( adm, "timeDose" ) ) > 0L )
        as.character( prop( adm, "timeDose" ) ) else ".",
      "tau"                = as.character( prop( adm, "tau" ) ),
      "Tinf"               = if ( length( prop( adm, "Tinf" ) ) > 0L )
        as.character( prop( adm, "Tinf" ) ) else "."
    )
  } )
}

#' Build dose / sampling summary rows for HTML and console reports.
#' @param arm First argument of generic.
#' @return List of arm-level dose and sampling summaries by outcome.
#' @name getArmData
#' @keywords internal
method( getArmData, Arm ) = function( arm ) {
  armName         = prop( arm, "name" )
  armSize         = round( prop( arm, "size" ), 2 )
  administrations = prop( arm, "administrations" )
  doseList        = map( administrations, function( adm ) {
    list( outcome = prop( adm, "outcome" ), dose = prop( adm, "dose" ) )
  } )
  # Map outcome -> dose string; sampling outcomes without a dose show ".".
  doseDict = set_names(
    map( doseList, \( x ) paste( x$dose, collapse = ", " ) ),
    map_chr( doseList, \( x ) x$outcome )
  )
  samplingList     = prop( arm, "samplingTimes" )
  samplingOutcomes = map_chr( samplingList, \( x ) prop( x, "outcome" ) )
  samplingTimes    = map( samplingList, \( x ) prop( x, "samplings" ) )
  map2( samplingOutcomes, samplingTimes, function( outc, samps ) {
    doseVal = if ( outc %in% names( doseDict ) ) doseDict[[ outc ]] else "."
    list(
      "Arms name"          = armName,
      "Number of subjects" = as.character( armSize ),
      "Outcome"            = outc,
      "Dose"               = doseVal,
      "Sampling times"     = paste0( "(", paste( round( samps, 2 ), collapse = ", " ), ")" )
    )
  } )
}

#' Default method for \code{Arm}.
#' @param arm First argument of generic.
#' @return List containing per-outcome sampling objects and numeric grids.
#' @name getSamplingData
#' @keywords internal
method( getSamplingData, Arm ) = function( arm ) {
  samplingTimes = prop( arm, "samplingTimes" )
  samplings = map( samplingTimes, \( x ) prop( x, "samplings" ) ) |>
    set_names( map_chr( samplingTimes, \( x ) prop( x, "outcome" ) ) )
  list(
    samplingTimes = samplingTimes,
    samplings     = samplings
  )
}

#' Densify sampling grids for response / SI plots.
#'
#' Keeps design sampling times and inserts intermediate steps via
#' \code{minPlotStep} / \code{maxPlotPoints} (\code{seq(0, tmax, 0.05)} when it
#' fits). Does not change the design used for FIM evaluation.
#' @return \code{Arm} with enriched sampling grids.
#' @name updateSamplingTimes
#' @keywords internal
method( updateSamplingTimes, Arm ) = function(
    arm, samplingData,
    maxPlotPoints = .pfimDefaultPlotMaxPoints(),
    minPlotStep   = 0.05 ) {
  prop( arm, "samplingTimes" ) = map( samplingData$samplingTimes, function( st ) {
    outcome = prop( st, "outcome" )
    times   = samplingData$samplings[[ outcome ]]
    if ( is.null( times ) )
      times = prop( st, "samplings" )
    prop( st, "samplings" ) = if ( length( times ) ) {
      .pfimDensePlotTimes( times, maxPoints = maxPlotPoints, minStep = minPlotStep )
    } else {
      prop( st, "samplings" )
    }
    st
  } )
  arm
}

#' Normalize plot axis units for arm evaluation / SI figures.
#'
#' Missing units become a single space (keeps axis labels aligned without
#' showing \code{NULL}). A scalar outcome unit is recycled across outputs.
#' @noRd
#' @keywords internal
.normalizePlotOptions = function( plotOptions, outputNames ) {
  unitTime = plotOptions$unitTime
  if ( is.null( unitTime ) || length( unitTime ) == 0L )
    unitTime = " "
  else if ( length( unitTime ) > 1L )
    unitTime = unitTime[[ 1L ]]
  out = unlist( outputNames, use.names = FALSE )
  if ( length( out ) == 0L ) out = names( outputNames )
  raw = plotOptions$unitOutcomes
  if ( is.null( raw ) || length( raw ) == 0L ) {
    unitOutcomes = set_names( rep( " ", length( out ) ), out )
  } else {
    raw = as.character( unlist( raw, use.names = FALSE ) )
    if ( length( raw ) == 1L && length( out ) > 1L )
      raw = rep( raw, length( out ) )
    unitOutcomes = set_names( raw[ seq_along( out ) ], out )
  }
  list( unitTime = unitTime, unitOutcomes = unitOutcomes )
}

#' Response plots for one arm.
#'
#' Re-evaluates the model on a dense time grid and overlays the
#' design sampling times as red markers. The FIM arm is not modified.
#' @return Nested list of response plots by design and arm.
#' @name processArmEvaluationResults
#' @keywords internal
method( processArmEvaluationResults, list( Arm, Model, Fim ) ) = function(
    arm, model, fim, designName, plotOptions ) {
  outputNames = as.list( prop( model, "outputNames" ) )
  if ( !.pfimArmPlotsEnabled( arm, model, outputNames ) )
    return( .pfimArmPlotResult( designName, arm ) )
  plotArm         = .pfimDensifyArmForPlots( arm )
  modelPlot       = .pfimPreparePlotModel( model, plotArm, needFd = FALSE )
  evaluationModel = evaluateModel( modelPlot, plotArm )
  if ( .isNestedEvaluationModel( evaluationModel ) )
    evaluationModel = .aggregateEvaluationModelForPlot( evaluationModel )
  plotEvaluationResults(
    arm, evaluationModel, outputNames,
    .pfimArmSamplingsByResponse( arm, model, outputNames ), designName, plotOptions
  )
}

#' Sensitivity-index plots for one arm.
#'
#' Re-evaluates gradients on the same dense grid as the response plots.
#' Covariate \code{beta_*} columns are omitted from sensitivity plots.
#' @return Nested list of sensitivity-index plots by design and arm.
#' @name processArmEvaluationSI
#' @keywords internal
method( processArmEvaluationSI, list( Arm, Model, Fim ) ) = function(
    arm, model, fim, designName, plotOptions ) {
  outputNames = as.list( prop( model, "outputNames" ) )
  if ( !.pfimArmPlotsEnabled( arm, model, outputNames ) )
    return( .pfimArmPlotResult( designName, arm ) )
  parametersNames         = prop( model, "modelParameters" ) |> map_chr( \( x ) prop( x, "name" ) )
  plotArm                 = .pfimDensifyArmForPlots( arm )
  modelPlot               = .pfimPreparePlotModel( model, plotArm, needFd = TRUE )
  rawGrad                 = evaluateModelGradient( modelPlot, plotArm )
  evaluationModelGradient = .pfimSiFramesFromGradients(
    rawGrad, modelPlot, plotArm, outputNames, parametersNames
  )
  # Nested (covariate / occasion) gradients carry their own parameter columns.
  if ( .isNestedArmEvaluation( rawGrad ) )
    parametersNames = setdiff( colnames( evaluationModelGradient[[ 1L ]] ), "time" )
  plotEvaluationSI(
    arm, evaluationModelGradient, parametersNames, outputNames,
    .pfimArmSamplingsByResponse( arm, model, outputNames ), designName, plotOptions
  )
}

#' Default method for \code{Arm}.
#' @return Nested list of ggplot objects for model responses.
#' @name plotEvaluationResults
#' @keywords internal
method( plotEvaluationResults, Arm ) = function(
    arm, evaluationModel, outputNames, samplingsByResponse, designName, plotOptions ) {
  units     = .normalizePlotOptions( plotOptions, outputNames )
  unitXAxis = units$unitTime
  unitYAxis = units$unitOutcomes
  armName   = prop( arm, "name" )
  respNames = unlist( outputNames, use.names = FALSE )
  plots     = map2(
    respNames, samplingsByResponse[ respNames ],
    function( outputName, sampling ) {
      data = evaluationModel[[ outputName ]]
      samplingPoints = data[ data$time %in% sampling, ]
      ggplot( data, aes( x = .data$time, y = .data[[ outputName ]] ) ) +
        geom_line( linewidth = 0.6 ) +
        geom_point( data = samplingPoints, color = "#C62828", size = 2.6 ) +
        labs(
          x = paste0(
            "Time (", unitXAxis, ")\n\nDesign: ",
            sub( "_", " ", designName ), "      Arm: ", armName
          ),
          y = paste0( outputName, " (", unitYAxis[[ outputName ]], ")\n" )
        ) +
        scale_x_continuous(
          breaks = pretty_breaks( n = 10 ),
          sec.axis = sec_axis( ~ . * 1, breaks = round( sampling, 2 ), name = "Sampling times" )
        ) +
        scale_y_continuous( breaks = pretty_breaks( n = 10 ) ) +
        .pfimSamplingAxisTheme()
    }
  )
  .pfimArmPlotResult( designName, arm, set_names( plots, outputNames ) )
}

#' Default method for \code{Arm}.
#' @return Nested list of ggplot objects for sensitivity indices.
#' @name plotEvaluationSI
#' @keywords internal
method( plotEvaluationSI, Arm ) = function(
    arm, evaluationModelGradient, parametersNames, outputNames,
    samplingsByResponse, designName, plotOptions ) {
  unitXAxis       = .normalizePlotOptions( plotOptions, outputNames )$unitTime
  armName         = prop( arm, "name" )
  respNames       = unlist( outputNames, use.names = FALSE )
  parametersNames = .pfimSiParamNamesNoBeta( parametersNames )
  plots           = map2(
    respNames, samplingsByResponse[ respNames ],
    function( outputName, sampling ) {
      gradientData = evaluationModelGradient[[ outputName ]]
      paramCols    = intersect( parametersNames, colnames( gradientData ) )
      if ( !length( paramCols ) ) return( list() )
      minYAxis = min( gradientData[ , paramCols, drop = FALSE ], na.rm = TRUE )
      maxYAxis = max( gradientData[ , paramCols, drop = FALSE ], na.rm = TRUE )
      map( paramCols, function( parameterName ) {
        data = as_tibble( gradientData[ , c( "time", parameterName ) ] )
        names( data )[ 2L ] = "parameterValue"
        samplingPoints = data[ data$time %in% sampling, ]
        ggplot( data, aes( x = .data$time, y = .data$parameterValue ) ) +
          geom_line( linewidth = 0.6 ) +
          geom_point( data = samplingPoints, color = "red", size = 2.2 ) +
          labs(
            y = .pfimParsePlotmath( .pfimSensitivityYLab( parameterName ) ),
            x = .pfimParsePlotmath( .pfimSensitivityXLab(
              unitXAxis, designName, armName, outputName, parameterName
            ) )
          ) +
          scale_x_continuous(
            breaks = pretty_breaks( n = 10 ),
            sec.axis = sec_axis( ~ ., breaks = round( sampling, 2 ), name = "Sampling times" )
          ) +
          scale_y_continuous( breaks = pretty_breaks( n = 10 ), limits = c( minYAxis, maxYAxis ) ) +
          .pfimSamplingAxisTheme()
      } ) |> set_names( paramCols )
    }
  )
  .pfimArmPlotResult( designName, arm, set_names( plots, outputNames ) )
}
