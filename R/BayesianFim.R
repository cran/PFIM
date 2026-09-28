#' @title BayesianFim
#' @description
#' Bayesian Fisher information matrix with shrinkage on random effects.
#'
#' First-order linearization on the random-effect scale. Covariate \code{beta} is not a
#' subject parameter (MAP conditions on \eqn{\beta}) and is omitted. Across
#' strata / arms, subject FIMs keep their prior and are aggregated by averaging
#' covariances: \eqn{\bar C = \sum w_s M_s^{-1}}, \eqn{M_{\mathrm{eff}}=\bar C^{-1}}.
#' A positive weight on a non-identifiable protocol yields \code{Inf} SE for the
#' null-space parameters. This covariance mixture is not comparable to summing
#' Fisher matrices (PFIM 6 / PopED on this case).
#'
#' With IOV (\eqn{\gamma>0}), each occasion uses the marginal FO residual
#' \eqn{V_k = R_k + F_k\,\mathrm{diag}(\gamma^2)\,F_k^\top} on the \eqn{\eta}-scale
#' (equivalent to the \eqn{\eta} block of the augmented \eqn{(\eta,\kappa)} system).
#'
#' @inheritParams Fim
#' @return A \code{BayesianFim} object (filled by \code{run()}).
#' @examples
#' \dontrun{
#' vignette("Example01")
#' }
#' @include Fim.R
#' @include pfim-fim-report-render.R
#' @include PFIMProject.R
#' @export

BayesianFim = new_class( "BayesianFim",
                         package    = "PFIM",
                         parent     = Fim
)
S4_register( BayesianFim )

# Estimable mu names with Greek prefix (setEvaluationFim, plots, ...).
#' Build estimable parameter labels for Bayesian outputs.
#' @param parameters List of \code{ModelParameter} objects.
#' @param greekPrefix Character prefix added to each parameter name.
#' @return Character vector of estimable parameter labels.
#' @noRd
#' @keywords internal
.bayesianEstimableParamNames = function( parameters, greekPrefix ) {
  .pfimBayesianEtaParameters( parameters ) |>
    map_chr( \( x ) prop( x, "name" ) ) |>
    map_chr( \( x ) paste0( greekPrefix, x ) )
}

#' Prior covariance \eqn{\Omega} on the random-effect (\eqn{\eta}) scale.
#'
#' For the Bayesian FIM, \eqn{\Omega = \mathrm{diag}(\omega^2)} even when
#' the data block is transformed by \eqn{M = \mathrm{diag}(\mu)} (LogNormal).
#' Always returns a matrix (including the 1x1 case) for \code{.safeSolve()}.
#' @noRd
#' @keywords internal
.bayesianOmega = function( omegaPk ) {
  n = length( omegaPk )
  if ( !n )
    return( matrix( numeric( 0L ), 0L, 0L ) )
  diag( omegaPk^2, nrow = n )
}

#' Prior precision \eqn{\Omega^{-1} = \mathrm{diag}(1/\omega^2)}.
#' Prefer this over \code{.safeSolve(.bayesianOmega(...))} - Omega is diagonal.
#' @noRd
#' @keywords internal
.bayesianOmegaInv = function( omegaPk ) {
  n = length( omegaPk )
  if ( !n )
    return( matrix( numeric( 0L ), 0L, 0L ) )
  diag( 1 / ( omegaPk^2 ), nrow = n )
}

#' Shrinkage (%) from PK FIM block and prior covariance on \eqn{\eta}.
#'
#' \eqn{W = M_{\mathrm{BF}}^{-1} \Omega^{-1}} (ratio of posterior
#' to prior variance on the random-effect scale), reported as percent.
#' @noRd
.bayesianShrinkageFromPrior = function( MF_pk, priorVariance ) {
  # priorVariance is Omega (diagonal); invert elementwise rather than via chol.
  omegaInv = if ( is.matrix( priorVariance ) && nrow( priorVariance ) == ncol( priorVariance ) )
    diag( 1 / diag( priorVariance ), nrow = nrow( priorVariance ) )
  else
    .safeCholInv( priorVariance )
  # Singular Bayesian FIM: same pseudo-inverse SE path as .fimBuildSeAndRse.
  MF_inv = tryCatch(
    .safeCholInv( MF_pk ),
    error = function( e ) {
      W = .pfimPsdPseudoInverse( MF_pk )
      W[ !is.finite( W ) ] = 0
      0.5 * ( W + t( W ) )
    }
  )
  as.vector( diag( MF_inv %*% omegaInv ) * 100 )
}

#' @noRd
.bayesianShrinkageValues = function( shrinkage ) {
  if ( !is.matrix( shrinkage ) )
    return( as.numeric( shrinkage ) )
  if ( nrow( shrinkage ) == 1L )
    return( as.numeric( shrinkage[ 1L, , drop = TRUE ] ) )
  if ( ncol( shrinkage ) == 1L )
    return( as.numeric( shrinkage[ , 1L, drop = TRUE ] ) )
  as.numeric( shrinkage )
}

#' Console mu-labels aligned with stored Bayesian shrinkage.
#' @noRd
#' @keywords internal
.bayesianShrinkageMuLabels = function( shrinkage, evaluation ) {
  muPrefix = .greekConsole[ "mu" ]
  cols     = character( 0L )
  if ( is.matrix( shrinkage ) ) {
    if ( nrow( shrinkage ) == 1L && !is.null( colnames( shrinkage ) ) )
      cols = colnames( shrinkage )
    else if ( ncol( shrinkage ) == 1L && !is.null( rownames( shrinkage ) ) )
      cols = rownames( shrinkage )
  }
  if ( !length( cols ) || identical( cols, "Shrinkage" ) ) {
    n    = length( .bayesianShrinkageValues( shrinkage ) )
    est  = .bayesianEstimableParamNames( prop( evaluation, "modelParameters" ), muPrefix )
    fe   = .fimFixedEffectLabels( evaluation )$columnNamesMu
    cols = if ( length( est ) == n ) est else if ( length( fe ) == n ) fe else est
  }
  .stripGreekPrefix( cols, muPrefix )
}

#' Map one shrinkage label onto an SE/RSE row (exact name, else mu-prefixed bare name).
#' @noRd
#' @keywords internal
.bayesianMatchShrinkageRow = function( name, rn, muPrefix ) {
  j = match( name, rn )
  if ( !is.na( j ) )
    return( j )
  match( paste0( muPrefix, .stripGreekPrefix( name, muPrefix ) ), rn )
}

#' Align Bayesian shrinkage values to SE/RSE table rows (mu only; beta stays NA).
#' @noRd
#' @keywords internal
.bayesianShrinkageReportColumn = function( seDF, shrinkage ) {
  rn     = .pfimSeRownames( seDF, "BayesianFim tablesForReport" )
  out    = rep( NA_real_, nrow( seDF ) )
  shVals = .bayesianShrinkageValues( shrinkage )
  if ( !length( shVals ) )
    return( out )

  greek   = .greekConsole
  muIdx   = which( startsWith( rn, greek[ "mu" ] ) )
  shMat   = as.matrix( shrinkage )
  shNames = colnames( shMat )

  if ( length( shNames ) == length( shVals ) && length( shNames ) ) {
    j = map_int( shNames, \( nm ) .bayesianMatchShrinkageRow( nm, rn, greek[ "mu" ] ) )
    ok = !is.na( j )
    out[ j[ ok ] ] = shVals[ ok ]
    unmatched = sum( !ok )
    if ( unmatched > 0L )
      .pfimWarn(
        sprintf(
          "BayesianFim tablesForReport: %d shrinkage label(s) did not match FIM rows.",
          unmatched
        )
      )
  } else if ( length( shVals ) == length( muIdx ) ) {
    out[ muIdx ] = shVals
  } else {
    .pfimWarn(
      sprintf(
        "BayesianFim tablesForReport: shrinkage length (%d) != mu rows (%d); leaving NA.",
        length( shVals ), length( muIdx )
      )
    )
  }
  out
}

#' @noRd
.bayesianShrinkageMatrix = function( shrinkage, shrinkageCols ) {
  matrix(
    .bayesianShrinkageValues( shrinkage ),
    nrow = 1L,
    dimnames = list( "Shrinkage", shrinkageCols )
  )
}

#' PK prior covariance and shrinkage from a Bayesian FIM block.
#' @noRd
#' @keywords internal
.bayesianShrinkage = function( fisherMatrix, model ) {

  parameters    = .pfimBayesianEtaParameters( prop( model, "modelParameters" ) )
  omegaPk       = .paramOmegas( parameters )
  priorVariance = .bayesianOmega( omegaPk )
  nEta          = length( omegaPk )
  MF_pk         = as.matrix( fisherMatrix )
  if ( nrow( MF_pk ) != nEta || ncol( MF_pk ) != nEta ) {
    # Legacy layouts with trailing beta / fixed columns: take the leading eta block.
    if ( nrow( MF_pk ) >= nEta )
      MF_pk = MF_pk[ seq_len( nEta ), seq_len( nEta ), drop = FALSE ]
  }
  .bayesianShrinkageFromPrior( MF_pk, priorVariance )
}

#' Variance / data block of the Bayesian FIM (mu only; no beta, no sigma).
#'
#' Flat: \eqn{G^\top V^{-1} G}. Nested cov/IOV: harmonic mean of subject
#' FIMs (prior included per stratum).
#' @name evaluateVarianceFIM
#' @keywords internal

method( evaluateVarianceFIM, list( BayesianFim, Model, Arm ) ) = function( fim, model, arm ) {

  feCols = .pfimBayesianMuCols( model, arm )

  if ( .isNestedArmEvaluation( prop( arm, "evaluationGradients" ) ) ) {
    harm = .evaluateIndBayesMixtureFim( model, arm, feCols, bayesian = TRUE )
    return( list( MFbeta = harm$fisherMatrix, V = NULL, complete = TRUE ) )
  }

  gradient = .gradientMatrix( arm, feCols, model )
  V        = as.matrix( getArmEvaluationVarianceFlat( arm )$errorVariance )
  V_inv    = tryCatch(
    .safeCholInv( V ),
    error = function( e ) {
      W = .pfimPsdPseudoInverse( V )
      W[ !is.finite( W ) ] = 0
      0.5 * ( W + t( W ) )
    }
  )
  MFbeta   = crossprod( gradient, V_inv ) %*% gradient

  list( MFbeta = MFbeta, V = V, complete = FALSE )
}

#' Compute the Bayesian FIM for one arm.
#'
#' FO Bayesian FIM: \eqn{M^\top M_{\mathrm{data}} M + \Omega^{-1}} for every
#' parameter with \eqn{\omega > 0} (\eqn{\beta} omitted; fix flags do not drop
#' eta). Shrinkage (\%) uses \eqn{M_{\mathrm{BF}}^{-1}\Omega^{-1}}.
#' @name evaluateFim
#' @keywords internal

method( evaluateFim, list( BayesianFim, Model, Arm ) ) = function( fim, model, arm ) {

  varBlock = evaluateVarianceFIM( fim, model, arm )
  MFbeta   = as.matrix( varBlock$MFbeta )
  M        = if ( isTRUE( varBlock$complete ) ) MFbeta else .pfimBayesianSubjectFim( MFbeta, model )

  parameters    = .pfimBayesianEtaParameters( prop( model, "modelParameters" ) )
  priorVariance = .bayesianOmega( .paramOmegas( parameters ) )

  set_props( fim, fisherMatrix = M, shrinkage = .bayesianShrinkageFromPrior( M, priorVariance ) )
}

#' Attach evaluated Bayesian FIM results (labels, SE/RSE, shrinkage matrix).
#'
#' SE/RSE use \code{.pfimBayesianSeRse}: for LogNormal, SE on \eqn{\theta} is
#' \eqn{\mu\cdot\mathrm{SE}_\eta} and RSE is \eqn{100\cdot\mathrm{SE}_\eta}.
#' @name setEvaluationFim
#' @keywords internal

method( setEvaluationFim, BayesianFim ) = function( fim, evaluation ) {

  parameters = prop( evaluation, "modelParameters" )
  greek      = .greekConsole
  allNames   = .bayesianEstimableParamNames( parameters, greek[ "mu" ] )

  fisherMatrix = prop( fim, "fisherMatrix" )
  if ( ncol( fisherMatrix ) != length( allNames ) )
    .pfimInternalStop( sprintf(
      "BayesianFim setEvaluationFim: FIM dim %d != %d column names.",
      ncol( fisherMatrix ), length( allNames )
    ) )
  dimnames( fisherMatrix ) = list( allNames, allNames )

  shrinkage = .bayesianShrinkageValues( prop( fim, "shrinkage" ) )
  se        = .pfimBayesianSeRse( fisherMatrix, parameters, allNames )
  .fimStoreEvaluationResult(
    fim, fisherMatrix, fisherMatrix, se,
    shrinkage = .bayesianShrinkageMatrix( shrinkage, allNames )
  )
}

#' Print FIM summaries to the console
#' @name showFIM
#' @export

method( showFIM, BayesianFim ) = function( fim ) {

  fisherMatrix           = prop( fim, "fisherMatrix" )
  fixedEffects           = prop( fim, "fixedEffects" )
  shrinkage              = prop( fim, "shrinkage" )
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )
  dcrit                  = Dcriterion( fim )

  cat( "\n*************************************** \n Bayesian Fisher Matrix \n*************************************** \n\n" )
  print( fisherMatrix )
  cat( "\n*************************************** \n Fixed effects \n*************************************** \n\n" )
  print( fixedEffects )
  cat( "\n*********************************************** \n Determinant, condition numbers and D-criterion \n*********************************************** \n\n" )
  cat( c( "Determinant:",  as.numeric( .fimDeterminant( fisherMatrix ) ) ), "\n" )
  cat( c( "D-criterion:",  as.numeric( dcrit              ) ), "\n" )
  cat( c( "Condition number of the fixed effects:", as.numeric( condNumberFixedEffects ), "\n" ) )
  cat( "\n*************************************** \n Shrinkage \n*************************************** \n\n" )
  print( shrinkage )
  cat( "\n*************************************** \n Parameters estimation \n*************************************** \n\n" )
  .printFimSeAndRse( fim )

  .fimShowSymbolLegend( rownames( fisherMatrix ), muLabel = "\u03bc  = fixed effects" )
  invisible( fim )
}

#' ggplot data for Bayesian SE/RSE barplots (mu and beta facets).
#' @param evaluation A \code{PFIMProject} object.
#' @param metric Character scalar, \code{"SE"} or \code{"RSE"}.
#' @return List with \code{data} (plot-ready \code{data.frame}) and \code{facetLevels}.
#' @noRd
#' @keywords internal
.bayesianSeRsePlotData = function( evaluation, metric ) {
  fim  = setEvaluationFim( prop( evaluation, "fim" ), evaluation )
  seDF = prop( fim, "SEAndRSE" )$SEAndRSE
  # Labels follow FIM rows (omega-estimable mus), not all mu-estimable names.
  df = .fimSeRseGroupedFrame(
    seDF, .pfimSeRownames( seDF, "Bayesian SE/RSE plot" ), metric, c( "mu", "beta" )
  )
  list( data = df, facetLevels = unique( df$cat ) )
}

.bayesianSeRseBarPlot = function( evaluation, metric ) {
  px = .bayesianSeRsePlotData( evaluation, metric )
  .fimSeRseBarPlot( px$data, metric, px$facetLevels )
}

method( plotSEFIM, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation )
  .bayesianSeRseBarPlot( evaluation, "SE" )

method( plotRSEFIM, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation )
  .bayesianSeRseBarPlot( evaluation, "RSE" )

#' Default method for \code{BayesianFim}.
#' @param fim First argument of generic.
#' @param evaluation \code{PFIMProject} providing model parameter labels.
#' @return \code{ggplot} object showing Bayesian shrinkage by parameter.
#' @name plotShrinkage
#' @export
method( plotShrinkage, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation ) {
  fim         = setEvaluationFim( prop( evaluation, "fim" ), evaluation )
  shrinkage   = prop( fim, "shrinkage" )
  paramLabels = .bayesianShrinkageMuLabels( shrinkage, evaluation )
  shrinkVals  = .bayesianShrinkageValues( shrinkage )

  data = data.frame( Parameter = paramLabels, Shrinkage = shrinkVals )
  ggplot( data, aes( x = .data$Parameter, y = .data$Shrinkage ) ) +
    geom_col( show.legend = FALSE, width = .pfimBarWidth, fill = "grey35" ) +
    labs( x = "Parameter", y = "Shrinkage (%)" ) +
    .pfimBaseTheme()
}

#' FIM tables for HTML reports
#' @name tablesForReport
#' @keywords internal

method( tablesForReport, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation ) {

  fim = setEvaluationFim( fim, evaluation )

  SEAndRSE               = prop( fim, "SEAndRSE" )$SEAndRSE
  fisherMatrix           = prop( fim, "fisherMatrix" )
  fixedEffects           = as.matrix( prop( fim, "fixedEffects" ) )
  shrinkage              = prop( fim, "shrinkage" )
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )

  columnNamesFe = .fimFixedEffectLatexLabels( evaluation, fixedEffects = fixedEffects )
  colnames( fixedEffects ) = columnNamesFe
  rownames( fixedEffects ) = columnNamesFe
  shrinkCol = .bayesianShrinkageReportColumn( SEAndRSE, shrinkage )

  list(
    fixedEffectsTable = .kblReportStyled( fixedEffects ),
    FIMCriteriaTable  = .fimCriteriaKable(
      .fimDeterminant( fisherMatrix ), Dcriterion( fim ), condNumberFixedEffects
    ),
    SEAndRSETable     = .fimSeRseKable( columnNamesFe, SEAndRSE, shrinkCol )
  )
}

# Report rendering methods

#' Render the evaluation HTML report
#' @name generateReportEvaluation
#' @keywords internal
method( generateReportEvaluation, BayesianFim ) =
  .renderEvalReport( "EvaluationBayesianFIM.Rmd" )
