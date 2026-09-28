# Subject-level FIM helpers (individual / Bayesian).
#
# Population FIM adds information across subjects. Individual and Bayesian FIMs
# describe one subject's precision: when strata (arms x covariate combinations)
# have different protocols, average the covariances then invert:
#   C_bar = sum_s w_s M_s^{-1},   M_eff = C_bar^{-1}.
# Arithmetic mixing of FIMs underestimates shrinkage (Jensen).

#' Subject-level covariance from a (possibly singular) FIM.
#'
#' Correlation-scale eigen-decomposition: identifiable directions get the
#' corresponding pseudoinverse; directions in the numerical null space mark
#' touched parameters with \code{Inf} variance and zero cross-covariances
#' (same contract as \code{.fimPinvCovarianceDiagonal}).
#' @noRd
#' @keywords internal
.pfimSubjectCovariance = function( M ) {
  .pfimPsdPseudoInverse( M )
}

#' Invert a covariance that may contain \code{Inf} on non-identifiable parameters.
#'
#' Only the finite principal block is inverted; other rows/columns stay 0 in the
#' information matrix (no information).
#' @noRd
#' @keywords internal
.pfimCovarianceToFim = function( C ) {
  C = as.matrix( C )
  p = nrow( C )
  if ( !p )
    return( matrix( numeric( 0L ), 0L, 0L ) )
  finite = which( is.finite( diag( C ) ) )
  M = matrix( 0, p, p )
  if ( !length( finite ) )
    return( M )
  Cf = C[ finite, finite, drop = FALSE ]
  Cf[ !is.finite( Cf ) ] = 0
  Mf = tryCatch( .safeCholInv( Cf ), error = function( e ) NULL )
  if ( is.null( Mf ) ) {
    # Singular finite block: correlation-scale pseudoinverse; Inf -> 0 information.
    Mf = .pfimPsdPseudoInverse( Cf )
    Mf[ !is.finite( Mf ) ] = 0
  }
  M[ finite, finite ] = Mf
  0.5 * ( M + t( M ) )
}

#' Harmonic-mean (covariance-mixture) design FIM from subject-level blocks.
#'
#' @param fisherBlocks List of square subject FIMs (same dimension).
#' @param weights Non-negative weights summing to 1 (stratum proportions).
#' @return List with \code{fisherMatrix} (= \eqn{\bar C^{-1}}) and \code{covariance}
#'   (= \eqn{\bar C}).
#' @noRd
#' @keywords internal
.pfimHarmonicMeanFim = function( fisherBlocks, weights ) {
  n = length( fisherBlocks )
  if ( !n )
    return( list(
      fisherMatrix = matrix( numeric( 0L ), 0L, 0L ),
      covariance   = matrix( numeric( 0L ), 0L, 0L )
    ) )
  weights = as.numeric( weights )
  if ( length( weights ) != n )
    .pfimStop( "subject-level FIM weights do not match the number of protocols." )
  if ( any( !is.finite( weights ) ) || any( weights < 0 ) )
    .pfimStop( "subject-level FIM weights must be finite and non-negative." )
  wsum = sum( weights )
  if ( !( wsum > 0 ) )
    .pfimStop( "subject-level FIM weights must sum to a positive value." )
  weights = weights / wsum

  blocks = map( fisherBlocks, as.matrix )
  p = nrow( blocks[[ 1L ]] )
  if ( !all( map_lgl( blocks, \( M ) nrow( M ) == p && ncol( M ) == p ) ) )
    .pfimStop( "subject-level FIM blocks must have the same dimensions." )

  # Zero-weight strata are skipped (0 * Inf would be NaN); Inf variance with a
  # positive weight stays Inf.
  active = weights > 0
  weightedCov = map2( blocks[ active ], weights[ active ],
                      \( M, w ) w * .pfimSubjectCovariance( M ) )
  C_bar = reduce( weightedCov, `+`, .init = matrix( 0, p, p ) )
  # Clean NaN from Inf - Inf or 0 * Inf edge cases.
  C_bar[ is.nan( C_bar ) ] = Inf
  C_bar = 0.5 * ( C_bar + t( C_bar ) )
  # Parameters that are Inf in any stratum stay Inf (null cross-cov).
  infIdx = which( !is.finite( diag( C_bar ) ) )
  C_bar = .pfimZeroCrossInfDiag( C_bar, infIdx )
  M_eff = .pfimCovarianceToFim( C_bar )
  list( fisherMatrix = M_eff, covariance = C_bar )
}

#' Drop covariate beta columns from fixed-effect names (Ind / Bayes subject FIM).
#' @noRd
#' @keywords internal
.pfimSubjectFeCols = function( feCols ) {
  feCols[ !startsWith( feCols, "beta_" ) ]
}

#' Bayesian FE column names for eta parameters (\eqn{\omega > 0}; beta excluded).
#'
#' Flat models use bare parameter names; covariate/IOV layouts use \code{mu_*}
#' prefixes. Unlike population FIM, \code{fixedMu}/\code{fixedOmega} do not drop
#' an eta when \eqn{\omega > 0}.
#' @noRd
#' @keywords internal
.pfimBayesianMuCols = function( model, arm = NULL ) {
  parameters = prop( model, "modelParameters" )
  eta = .pfimBayesianEtaParameters( parameters )
  if ( !length( eta ) )
    return( character( 0L ) )
  etaBare = map_chr( eta, \( x ) prop( x, "name" ) )

  if ( !is.null( arm ) && usesCovariateOccasionStructure( model ) ) {
    cn = colnames( getArmEvaluationGradientsMatrix( model, arm, evalModel = model ) )
    if ( is.null( cn ) ) cn = character( 0L )
    wanted = paste0( "mu_", etaBare )
    hit = wanted[ wanted %in% cn ]
    if ( length( hit ) ) return( hit )
    # Fallback: bare names present in the gradient layout.
    hit = etaBare[ etaBare %in% cn ]
    if ( length( hit ) ) return( hit )
    return( wanted )
  }

  # Flat path: gradient columns are bare parameter names.
  etaBare
}

#' TRUE when FIM is individual or Bayesian (subject-level precision).
#' @noRd
#' @keywords internal
.pfimIsSubjectLevelFim = function( fim ) {
  S7::S7_inherits( fim, IndividualFim ) || S7::S7_inherits( fim, BayesianFim )
}

#' Bayesian subject FIM on the random-effect scale.
#'
#' Data block \eqn{G^\top V^{-1} G} for every \eqn{\omega > 0} parameter, then
#' \eqn{M^\top M_{\mathrm{pk}} M + \Omega^{-1}} with \eqn{M=\mathrm{diag}(\mu)}
#' for LogNormal. Fix flags do not remove eta.
#' @noRd
#' @keywords internal
.pfimBayesianSubjectFim = function( M_data, model ) {
  parameters = .pfimBayesianEtaParameters( prop( model, "modelParameters" ) )
  n = length( parameters )
  if ( !n )
    return( matrix( numeric( 0L ), 0L, 0L ) )
  if ( nrow( M_data ) != n || ncol( M_data ) != n )
    .pfimInternalStop(
      "Bayesian data FIM dimension does not match the number of IIV parameters."
    )
  mu = map_dbl( parameters, function( p ) {
    d = prop( p, "distribution" )
    adjustGradient( d, 1, prop( d, "mu" ) )
  } )
  muD = diag( mu, nrow = n )
  as.matrix( t( muD ) %*% M_data %*% muD + .bayesianOmegaInv( .paramOmegas( parameters ) ) )
}

#' Bayesian SE / RSE from an eta-scale FIM.
#'
#' For LogNormal, the FIM is on \eqn{\eta}; displayed SE on \eqn{\theta} is
#' \eqn{\mu\cdot\mathrm{SE}_\eta} and RSE is \eqn{100\cdot\mathrm{SE}_\eta}.
#' For Normal, SE stays on the natural scale with RSE \eqn{100\cdot\mathrm{SE}/|\mu|}.
#' @noRd
#' @keywords internal
.pfimBayesianSeRse = function( fisherMatrix, parameters, allNames ) {
  M = as.matrix( fisherMatrix )
  p = nrow( M )
  if ( p != length( allNames ) )
    .pfimInternalStop( "Bayesian SE labels do not match the FIM dimension." )

  covMat = tryCatch( .safeCholInv( M ), error = function( e ) NULL )
  singular = is.null( covMat )
  if ( singular ) {
    seEta = sqrt( .fimPinvCovarianceDiagonal( M ) )
  } else {
    seEta = sqrt( pmax( diag( covMat ), 0 ) )
  }
  seEta[ !is.finite( seEta ) ] = Inf

  # Map greek-prefixed mu labels back to ModelParameter objects.
  greekMu = .greekConsole[ "mu" ]
  bare = .stripGreekPrefix( allNames, greekMu )
  paramByName = set_names( parameters, map_chr( parameters, \( x ) prop( x, "name" ) ) )

  objs    = unname( paramByName[ bare ] )
  missing = map_lgl( objs, is.null )
  isLogn  = map_lgl( objs, \( x ) !is.null( x ) && S7_inherits( prop( x, "distribution" ), LogNormal ) )
  mu      = map_dbl( objs, \( x ) if ( is.null( x ) ) NA_real_ else .paramDist( x, "mu" ) )

  seTheta = seEta
  rse     = rep( NA_real_, p )
  rse[ missing ] = ifelse( is.finite( seEta[ missing ] ), NA_real_, Inf )

  logn = !missing & isLogn
  seTheta[ logn ] = abs( mu[ logn ] ) * seEta[ logn ]
  rse[ logn ]      = 100 * seEta[ logn ]

  nat = !missing & !isLogn
  seTheta[ nat ] = seEta[ nat ]
  rse[ nat ] = ifelse( abs( mu[ nat ] ) > 0, 100 * seEta[ nat ] / abs( mu[ nat ] ), Inf )

  seDF = data.frame(
    parametersValues = mu,
    SE               = seTheta,
    RSE              = rse
  )
  rownames( seDF ) = allNames
  list(
    SE       = seDF[ , c( "parametersValues", "SE"  ), drop = FALSE ],
    RSE      = seDF[ , c( "parametersValues", "RSE" ), drop = FALSE ],
    table    = seDF,
    SEAndRSE = seDF,
    singular = singular
  )
}

#' Index of the best subject-level protocol by D-criterion.
#'
#' For individual / Bayesian designs the D-criterion is convex in the weights of
#' a covariance mixture, so the optimum is a single protocol (vertex).
#' @param fisherMatrices List of candidate subject FIMs.
#' @return List with \code{index} (1-based) and \code{Dcriterion}.
#' @noRd
#' @keywords internal
.pfimBestSubjectProtocol = function( fisherMatrices ) {
  if ( !length( fisherMatrices ) )
    .pfimInternalStop( "no candidate protocols for subject-level D-criterion." )
  dvals = map_dbl( fisherMatrices, \( M ) .fimDcriterionFromMatrix( as.matrix( M ) ) )
  idx   = which.max( dvals )
  list( index = as.integer( idx ), Dcriterion = unname( dvals[ idx ] ), dvals = dvals )
}
