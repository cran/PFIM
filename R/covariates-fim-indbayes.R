# Individual / Bayesian FIM as a mixture over covariate combinations and occasions.

#' Stack per-output gradient frames for one (combo, occasion) leaf.
#' @noRd
#' @keywords internal
.indBayesOccasionGradientMat = function( comboGrad, occ, outputNames, feCols ) {
  dfs = map( outputNames, \( x ) comboGrad$gradients[[ occ ]]$gradient[[ x ]] )
  mat = as.matrix( list_rbind( dfs ) )
  mat[ , feCols, drop = FALSE ]
}

#' Gradient columns needed to inflate residual V by IOV (every gamma>0).
#' @noRd
#' @keywords internal
.indBayesIovGradientCols = function( iovIdx, sampleGrad, bareNames ) {
  if ( !length( iovIdx ) || !length( sampleGrad ) )
    return( character( 0L ) )
  hits = map_chr( iovIdx, function( ii ) {
    muCol = paste0( "mu_", bareNames[[ ii ]] )
    if ( muCol %in% sampleGrad )
      return( muCol )
    if ( bareNames[[ ii ]] %in% sampleGrad )
      return( bareNames[[ ii ]] )
    NA_character_
  } )
  unique( hits[ !is.na( hits ) ] )
}

#' FO IOV inflation: \eqn{V = R + \sum_j \gamma_j^2 (\mu_j g_j)(\mu_j g_j)^\top}.
#'
#' One BLAS \code{tcrossprod} after column scaling; same algebra as a loop of
#' rank-1 updates.
#' @noRd
#' @keywords internal
.indBayesIovInflatedV = function( R, G_all, gradCols, bareNames, gammaAll, muChain ) {
  bare = sub( "^mu_", "", gradCols )
  ii   = match( bare, bareNames )
  keep = !is.na( ii ) & gammaAll[ ii ] > 0
  if ( !any( keep ) )
    return( R )
  scale = muChain[ ii[ keep ] ] * gammaAll[ ii[ keep ] ]
  R + tcrossprod( sweep( G_all[ , keep, drop = FALSE ], 2L, scale, `*` ) )
}

#' Reorder occasion gradient columns to the requested FE layout.
#' @noRd
#' @keywords internal
.indBayesAlignFeGradient = function( G_all, gradCols, feCols ) {
  feIn = which( gradCols %in% feCols )
  G = G_all[ , feIn, drop = FALSE ]
  G[ , match( feCols, gradCols[ feIn ] ), drop = FALSE ]
}

#' Individual / Bayesian Fisher pieces for every occasion of one combination.
#' @noRd
#' @keywords internal
.indBayesOccasionBlocks = function( comboGrad, comboVar, nOcc, outputNames,
                                    gradCols, feCols, hasIov, bareNames,
                                    gammaAll, muChain, bayesian ) {
  map( seq_len( nOcc ), function( k ) {
    G_all = .indBayesOccasionGradientMat( comboGrad, k, outputNames, gradCols )
    R = as.matrix( pluck( comboVar, "variances", k, "variance", "errorVariance" ) )
    V = if ( hasIov )
      .indBayesIovInflatedV( R, G_all, gradCols, bareNames, gammaAll, muChain )
    else
      R
    V_inv = tryCatch(
      .safeCholInv( V ),
      error = function( e ) {
        W = .pfimPsdPseudoInverse( V )
        W[ !is.finite( W ) ] = 0
        0.5 * ( W + t( W ) )
      }
    )
    G = .indBayesAlignFeGradient( G_all, gradCols, feCols )
    Mb = crossprod( G, V_inv ) %*% G
    Mv = if ( !bayesian ) {
      dV = pluck( comboVar, "variances", k, "variance", "sigmaDerivatives" )
      if ( length( dV ) ) .computeMFVar( V_inv, dV ) else NULL
    } else {
      NULL
    }
    list( Mb = Mb, Mv = Mv )
  } )
}

#' Stack occasion Fisher blocks into one subject FIM for a covariate combination.
#' @noRd
#' @keywords internal
.indBayesComboSubjectFim = function( blocks, model, bayesian ) {
  Mc_b = reduce( map( blocks, "Mb" ), `+` )
  if ( bayesian )
    return( .pfimBayesianSubjectFim( Mc_b, model ) )
  mvs = compact( map( blocks, "Mv" ) )
  Mc_v = if ( length( mvs ) )
    reduce( mvs, `+` )
  else
    matrix( numeric( 0L ), 0L, 0L )
  as.matrix( Matrix::bdiag( Mc_b, Mc_v ) )
}

#' Individual / Bayesian subject FIM: harmonic mean over covariate strata.
#'
#' Per covariate combination \eqn{c}, occasions are stacked (sum of Fisher
#' blocks for one subject). Across combinations, precision is averaged:
#' \eqn{\bar C = \sum_c \pi_c M_c^{-1}}, \eqn{M_{\mathrm{eff}} = \bar C^{-1}}.
#' Covariate \code{beta} columns are dropped (MAP conditions on \eqn{\beta}).
#'
#' When \code{bayesian = TRUE}, each stratum uses
#' \code{.pfimBayesianSubjectFim()} (data FIM + \eqn{\Omega^{-1}}).
#' With IOV (\eqn{\gamma > 0}), both individual and Bayesian paths use
#' \eqn{V_k = R_k + F_k \mathrm{diag}(\gamma^2) F_k^\top}. The sum runs over
#' every parameter with \eqn{\gamma > 0} (even without IIV); \eqn{M_{\mathrm{data}}}
#' keeps only the requested \code{feCols} (eta / mu columns).
#'
#' @param model \code{Model} with nested arm evaluations.
#' @param arm Evaluated \code{Arm}.
#' @param feCols Character vector of estimable FE column names (beta stripped).
#' @param bayesian If \code{TRUE}, build Bayesian subject FIMs.
#' @return List with \code{fisherMatrix} (and \code{covariance}); legacy
#'   \code{MFbeta}/\code{MFVar} when \code{bayesian = FALSE} for callers that
#'   still split FE / residual blocks after a flat path.
#' @noRd
#' @keywords internal
.evaluateIndBayesMixtureFim = function( model, arm, feCols, bayesian = FALSE ) {

  feCols           = .pfimSubjectFeCols( feCols )
  allGradientsData = prop( arm, "evaluationGradients" )
  varianceResults  = prop( arm, "evaluationVariance" )
  outputNames      = prop( model, "outputNames" )
  if ( length( outputNames ) == 0L && length( allGradientsData ) > 0L )
    outputNames = names( pluck( allGradientsData, 1L, "gradients", 1L, "gradient" ) )

  parameters = prop( model, "modelParameters" )
  muChain    = .pfimPopMuChainFactors( parameters )
  bareNames  = map_chr( parameters, \( x ) prop( x, "name" ) )
  gammaAll   = .paramGammas( parameters )
  iovIdx     = which( gammaAll > 0 )
  hasIov     = length( iovIdx ) > 0L

  # Column names available in the nested gradient frames.
  sampleGrad = character( 0L )
  if ( length( allGradientsData ) && length( outputNames ) ) {
    sampleGrad = tryCatch( {
      colnames( as.matrix( list_rbind( map(
        outputNames,
        \( x ) allGradientsData[[ 1L ]]$gradients[[ 1L ]]$gradient[[ x ]]
      ) ) ) )
    }, error = function( e ) character( 0L ) )
  }

  # IOV noise needs gradients for every gamma>0 parameter (even without eta / IIV).
  iovCols = .indBayesIovGradientCols( iovIdx, sampleGrad, bareNames )
  gradCols = unique( c( feCols, iovCols ) )
  if ( length( sampleGrad ) )
    gradCols = gradCols[ gradCols %in% sampleGrad ]
  if ( !length( gradCols ) )
    gradCols = feCols

  pieces = compact( map( seq_along( allGradientsData ), function( iter ) {
    comboGrad = allGradientsData[[ iter ]]
    nOcc = length( comboGrad$gradients )
    if ( !nOcc )
      return( NULL )
    blocks = .indBayesOccasionBlocks(
      comboGrad, varianceResults[[ iter ]], nOcc, outputNames,
      gradCols, feCols, hasIov, bareNames, gammaAll, muChain, bayesian
    )
    list(
      mat    = .indBayesComboSubjectFim( blocks, model, bayesian ),
      weight = comboGrad$proportion
    )
  } ) )
  mats    = map( pieces, "mat" )
  weights = map_dbl( pieces, "weight" )

  if ( !length( mats ) ) {
    empty = matrix( numeric( 0L ), 0L, 0L )
    return( list(
      fisherMatrix = empty, covariance = empty,
      MFbeta = empty, MFVar = empty
    ) )
  }

  harm = .pfimHarmonicMeanFim( mats, weights )
  # Split FE / residual for Individual callers that still expect two blocks.
  if ( !bayesian && length( mats ) ) {
    nFe = length( feCols )
    M = harm$fisherMatrix
    if ( nrow( M ) > nFe ) {
      harm$MFbeta = M[ seq_len( nFe ), seq_len( nFe ), drop = FALSE ]
      harm$MFVar  = M[ ( nFe + 1L ):nrow( M ), ( nFe + 1L ):ncol( M ), drop = FALSE ]
    } else {
      harm$MFbeta = M
      harm$MFVar  = matrix( numeric( 0L ), 0L, 0L )
    }
  }
  harm
}
