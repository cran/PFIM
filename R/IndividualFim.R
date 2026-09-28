#' @title IndividualFim
#' @description
#' Individual (subject-level) Fisher information matrix.
#'
#' Across covariate strata (and arms at design level), subject FIMs are combined
#' by averaging covariances:
#' \eqn{\bar C = \sum_s w_s M_s^{-1}}, \eqn{M_{\mathrm{eff}} = \bar C^{-1}}.
#' A positive weight on a non-identifiable protocol yields \code{Inf} SE.
#' This mixture is not comparable to summing Fisher matrices (PFIM 6 / PopED).
#' Covariate \code{beta} effects are not subject parameters and are omitted.
#' With IOV, each occasion uses
#' \eqn{V_k = R_k + F_k\,\mathrm{diag}(\gamma^2)\,F_k^\top} for the
#' \eqn{\mu} and residual \eqn{\sigma} blocks.
#'
#' Arm-level matrices are per-subject.
#'
#' @inheritParams Fim
#' @details
#' Block-diagonal structure for a single stratum:
#' \deqn{M_I = \mathrm{bdiag}(M_\mu, M_\sigma)}
#' where \eqn{M_\mu = G^\top V^{-1} G} and \eqn{M_\sigma} is the residual
#' variance-effects block (with the same \eqn{V} under IOV).
#' @return An \code{IndividualFim} object (filled by \code{run()}).
#' @examples
#' \donttest{
#' source(system.file("examples", "evaluation-minimal.R", package = "PFIM"))
#' }
#' @include Fim.R
#' @include pfim-fim-report-render.R
#' @include PFIMProject.R
#' @export

IndividualFim = new_class( "IndividualFim", package = "PFIM", parent = Fim )
S4_register( IndividualFim )

#' Variance block of the individual FIM (\eqn{\partial V / \partial \sigma} form).
#'
#' Flat path: one residual \eqn{V}. Nested cov/IOV path: harmonic mean of
#' per-stratum blocks (see \code{.evaluateIndBayesMixtureFim}).
#' @name evaluateVarianceFIM
#' @keywords internal

method( evaluateVarianceFIM, list( IndividualFim, Model, Arm ) ) = function( fim, model, arm ) {

  if ( .isNestedArmEvaluation( prop( arm, "evaluationVariance" ) ) ) {
    feCols = .pfimSubjectFeCols( .fimFixedEffectColumnNames( model, arm ) )
    return( .evaluateIndBayesMixtureFim( model, arm, feCols, bayesian = FALSE ) )
  }

  ev    = getArmEvaluationVarianceFlat( arm )
  V     = as.matrix( ev$errorVariance )
  V_inv = tryCatch(
    .safeCholInv( V ),
    error = function( e ) {
      W = .pfimPsdPseudoInverse( V )
      W[ !is.finite( W ) ] = 0
      0.5 * ( W + t( W ) )
    }
  )

  list( MFVar = .computeMFVar( V_inv, ev$sigmaDerivatives ), V = V, V_inv = V_inv )
}

#' Compute the individual FIM for one arm.
#'
#' Fixed-effect block plus residual variance block; not scaled by arm size.
#' Cov/IOV strata use the harmonic-mean subject FIM. No \eqn{\omega}/\eqn{\gamma}
#' block - only \eqn{\mu} and residual \eqn{\sigma} (\eqn{\beta} omitted).
#' @name evaluateFim
#' @keywords internal

method( evaluateFim, list( IndividualFim, Model, Arm ) ) = function( fim, model, arm ) {

  feCols = .pfimSubjectFeCols( .fimFixedEffectColumnNames( model, arm ) )

  if ( .isNestedArmEvaluation( prop( arm, "evaluationGradients" ) ) ) {
    harm = .evaluateIndBayesMixtureFim( model, arm, feCols, bayesian = FALSE )
    prop( fim, "fisherMatrix" ) = harm$fisherMatrix
    return( fim )
  }

  varBlock = evaluateVarianceFIM( fim, model, arm )
  G        = .gradientMatrix( arm, feCols, model )
  MFbeta   = crossprod( G, varBlock$V_inv ) %*% G

  prop( fim, "fisherMatrix" ) = as.matrix( bdiag( MFbeta, varBlock$MFVar ) )
  fim
}

#' Attach evaluated individual FIM results (labels, SE/RSE, condition numbers).
#'
#' Label order: Greek-prefixed \code{mu}, then \code{sigma} (no \code{beta}).
#' @name setEvaluationFim
#' @keywords internal

method( setEvaluationFim, IndividualFim ) = function( fim, evaluation ) {

  greek  = .greekConsole
  fe     = .fimFixedEffectLabels( evaluation, greek )
  sigma  = .fimSigmaBlockLabels( evaluation, greek )

  # Subject FIM omits covariate beta columns.
  allNames = c( fe$columnNamesMu, sigma$columnNamesSigma )
  pVals    = c( fe$muValues, sigma$sigmaValues )

  M = prop( fim, "fisherMatrix" )

  if ( ncol( M ) != length( allNames ) )
    .pfimInternalStop( sprintf(
      "IndividualFim setEvaluationFim: FIM dim %d != %d column names.",
      ncol( M ), length( allNames )
    ) )

  dimnames( M ) = list( allNames, allNames )

  feNames         = fe$columnNamesMu
  fixedEffects    = M[ feNames, feNames, drop = FALSE ]
  varianceEffects = M[ sigma$columnNamesSigma, sigma$columnNamesSigma, drop = FALSE ]

  se = .fimBuildSeAndRse( M, allNames, pVals, absDenominator = TRUE )
  .fimStoreEvaluationResult(
    fim, M, fixedEffects, se, varianceEffects = varianceEffects
  )
}

#' Print FIM summaries to the console
#' @name showFIM
#' @export

method( showFIM, IndividualFim ) = function( fim ) {

  .hdr = function( t ) cat( sprintf(
    "\n*************************************** \n %s \n*************************************** \n\n", t
  ))

  .hdr( "Individual Fisher Matrix" );        print( prop( fim, "fisherMatrix"   ) )
  .hdr( "Fixed effects (\u03bc)" );          print( prop( fim, "fixedEffects"   ) )
  .hdr( "Variance components (\u03c3)" );    print( prop( fim, "varianceEffects") )

  M   = prop( fim, "fisherMatrix" )
  cn1 = prop( fim, "condNumberFixedEffects"    )
  cn2 = prop( fim, "condNumberVarianceEffects" )

  cat( "\n*********************************************** \n",
       " Determinant, condition numbers and D-criterion \n",
       "*********************************************** \n\n" )
  cat( "Determinant:",  as.numeric( .fimDeterminant( M ) ),       "\n" )
  cat( "D-criterion:",  as.numeric( Dcriterion(fim) ), "\n" )
  cat( "Condition number (fixed effects):",    as.numeric( cn1 ), "\n" )
  cat( "Condition number (variance effects):", as.numeric( cn2 ), "\n" )
  .hdr( "Parameters estimation" )
  .printFimSeAndRse( fim )

  .fimShowSymbolLegend(
    rownames( prop( fim, "fisherMatrix" ) ),
    includeSigma = TRUE,
    muLabel = "\u03bc  = fixed effects",
    sigmaLabel = "\u03c3  = residual error"
  )
  invisible( fim )
}

# SE / RSE bar charts
#' Build Individual FIM SE/RSE bar plot data and figure.
#' @param fim \code{IndividualFim} object.
#' @param evaluation \code{PFIMProject} providing labels and fitted FIM.
#' @param metric \code{"SE"} or \code{"RSE"}.
#' @return \code{ggplot} object of parameter uncertainty bars.
#' @noRd
#' @keywords internal
.individualSEPlot = function( fim, evaluation, metric ) {
  fim    = setEvaluationFim( prop( evaluation, "fim" ), evaluation )
  seDF   = prop( fim, "SEAndRSE" )$SEAndRSE
  greekC = .greekConsole
  fe     = .fimFixedEffectLabels( evaluation, greekC )
  sigma  = .fimSigmaBlockLabels( evaluation, greekC )
  facet  = function( key ) .pfimSeRseFacetLabel( metric, key )

  # SE/RSE barplots: mu / sigma (beta omitted from subject FIM).
  nMu    = length( fe$columnNamesMu )
  nSigma = length( sigma$columnNamesSigma )
  idx    = seq_len( nMu + nSigma )
  # Full console names so x-axis plotmath matches the SE/RSE report table.
  paramLabels = c( fe$columnNamesMu, sigma$columnNamesSigma )
  cats = c(
    rep( facet( "mu"    ), nMu ),
    rep( facet( "sigma" ), nSigma )
  )
  yVals = if ( metric == "SE" ) seDF$SE[ idx ] else seDF$RSE[ idx ]
  df = data.frame( Parameter = paramLabels, y = yVals, cat = cats )
  names( df )[ 2L ] = metric
  .fimSeRseBarPlot( df, metric, unique( cats ) )
}

#' @keywords internal
method( plotSEFIM,  list( IndividualFim, PFIMProject ) ) =
  function( fim, evaluation ) .individualSEPlot( fim, evaluation, "SE"  )

#' @keywords internal
method( plotRSEFIM, list( IndividualFim, PFIMProject ) ) =
  function( fim, evaluation ) .individualSEPlot( fim, evaluation, "RSE" )

#' FIM tables for HTML reports
#' @name tablesForReport
#' @keywords internal

method( tablesForReport, list( IndividualFim, PFIMProject ) ) = function( fim, evaluation ) {
  .fimTablesForReportStandard( fim, evaluation )
}

#' Render the evaluation HTML report
#' @name generateReportEvaluation
#' @keywords internal
method( generateReportEvaluation, IndividualFim ) =
  .renderEvalReport( "EvaluationIndividualFIM.Rmd" )
