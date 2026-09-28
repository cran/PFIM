# Observation variance + sigma derivatives for the population FIM lambda block.
#
# Multi-output residual blocks are stacked with Matrix::bdiag. Per-output
# dV/dsigma slices are embedded as sparse diagonals so large designs
# (many samples x outputs) never allocate dense zero fills during assembly.

#' Core residual variance for one (flat) evaluationModel list.
#' @noRd
#' @keywords internal
evaluateModelVarianceCore = function( model, evaluationModel ) {
  modelErrors = prop( model, "modelError" )
  outputNames = prop( model, "outputNames" )

  # One evaluateErrorModelDerivatives call per configured outcome.
  errs = keep( modelErrors, \( x ) prop( x, "output" ) %in% outputNames )
  errorDerivativesList = set_names(
    map( errs, function( err ) {
      out = prop( err, "output" )
      evaluateErrorModelDerivatives(
        err,
        evaluationModel[[ out ]][ , out, drop = TRUE ]
      )
    } ),
    map_chr( errs, \( x ) prop( x, "output" ) )
  )

  # Require a residual-error model for every outcome. A missing modelError used
  # to fall back to Diagonal(n) (= R = I), silently matching a unit residual.
  missingOut = setdiff( outputNames, map_chr( errs, \( x ) prop( x, "output" ) ) )
  if ( length( missingOut ) )
    .pfimStop(
      "No modelError for output(s): ", paste( missingOut, collapse = ", " ),
      ". Provide a residual-error model (Constant, Proportional, Combined1/2)."
    )
  if ( !length( errs ) && length( outputNames ) )
    .pfimStop(
      "modelError is empty but the model has outputs. ",
      "Provide at least one residual-error model."
    )

  blocks = map( outputNames, function( outName ) {
    as.matrix( errorDerivativesList[[ outName ]]$errorVariance )
  } )
  errorVariance = if ( length( blocks ) )
    bdiag( blocks ) else Matrix::Diagonal( n = 0L )

  nByOut = map_int( outputNames, \( outName ) length( evaluationModel[[ outName ]]$time ) )
  totalSamplings = sum( nByOut )
  samplingOffsets = set_names(
    c( 0L, cumsum( nByOut )[ -length( nByOut ) ] ),
    outputNames
  )

  # Flatten per-output estimable sigma derivatives into FIM column order.
  sigmaDerivatives = outputNames |>
    map( function( outName ) {
      offset = samplingOffsets[[ outName ]]
      map(
        pluck( errorDerivativesList, outName, "sigmaDerivatives", .default = list() ),
        \( x ) .pfimEmbedDiagonalBlock( x, offset, totalSamplings )
      )
    } ) |>
    .pfimFlatten()

  list( errorVariance = errorVariance, sigmaDerivatives = sigmaDerivatives )
}

method( evaluateModelVariance, Model ) = function( model, arm ) {
  evaluationModel = prop( arm, "evaluationModel" )
  if ( !usesCovariateOccasionStructure( model ) )
    evaluateModelVarianceCore( model, evaluationModel )
  else
    evaluateModelVarianceWithCovariates( model, evaluationModel )
}

#' Residual variance under covariate x occasion nesting.
#' @noRd
#' @keywords internal
evaluateModelVarianceWithCovariates = function( model, evaluationModelWithCovariates ) {
  map( evaluationModelWithCovariates, \( combo ) list(
    combination = combo$combination,
    proportion  = combo$proportion,
    variances   = map( combo$evaluations, \( occ ) list(
      occasion = occ$occasion,
      variance = evaluateModelVarianceCore( model, occ$evaluation )
    ) )
  ) )
}
