# Population FO variance FIM: simple (flat arm) vs covariatexoccasion mixture.
# Combination kernel lives in computePopFimCombo_Rcpp; this file is layout,
# lambda-dropping, and M = Sum_c pi_c M_c.

#' Chain-rule scale df/deta|₀ per parameter (uses \code{adjustGradient()}).
#'
#' FO pop FIM works on the random-effect (\eqn{\eta}) scale: LogNormal needs
#' \eqn{\times\mu}, Normal is \eqn{1}. Applied to mu-gradient rows only; beta
#' rows stay unscaled.
#' @noRd
#' @keywords internal
.pfimPopMuChainFactors = function( parameters ) {
  map_dbl( parameters, function( p ) {
    d = prop( p, "distribution" )
    # value= constants have no distribution and are not random effects.
    if ( is.null( d ) ) return( 1 )
    adjustGradient( d, 1, prop( d, "mu" ) )
  } )
}

#' Outer product of a numeric vector with itself (\eqn{v v^\top}).
#' @noRd
#' @keywords internal
.outerColPopFim = function( v ) tcrossprod( v )

#' Split mu-gradient columns by occasion widths.
#'
#' Multi-occasion IOV: observation columns are concatenated per occasion;
#' \code{occColWidths} restores that layout so \eqn{\gamma_i^2} outer products
#' stay block-diagonal across occasions (not pooled into one dense V_IOV).
#' @noRd
#' @keywords internal
.splitGradMuByOccasion = function( gradientsAdjustedMu, occColWidths ) {
  if ( length( occColWidths ) < 2L ) return( list( gradientsAdjustedMu ) )
  offsets = c( 0L, cumsum( occColWidths ) )
  map( seq_along( occColWidths ), function( k ) {
    cols = ( offsets[[ k ]] + 1L ):offsets[[ k + 1L ]]
    gradientsAdjustedMu[ , cols, drop = FALSE ]
  } )
}

#' Block-diagonal IOV outer products across occasions for one gamma parameter.
#'
#' For parameter index \code{gammaIdx}, each occasion contributes
#' \eqn{g_{i,k} g_{i,k}^\top}; occasions are independent under FO IOV so the
#' sum is \code{bdiag}, not a full Kronecker coupling.
#' @noRd
#' @keywords internal
.iovOuterBlocks = function( gammaIdx, occGradsT ) {
  map( occGradsT, \( x ) .outerColPopFim( x[ , gammaIdx, drop = FALSE ] ) ) |>
    .pfimBlockDiag()
}

#' Dense block-diagonal matrix; a single block is returned without a sparse round trip.
#' @noRd
#' @keywords internal
.pfimBlockDiag = function( blocks ) {
  if ( length( blocks ) == 1L ) as.matrix( blocks[[ 1L ]] ) else as.matrix( bdiag( blocks ) )
}

#' One covariate x occasion block (MFbeta + MFVar); R reference for C++ parity tests.
#'
#' Formals mirror \code{computePopFimCombo_Rcpp()} so tests call both kernels
#' with the same argument list.
#' @param mu_values Per-parameter chain-rule factors (LogNormal: \eqn{\mu};
#'   Normal: \eqn{1}); from \code{.pfimPopMuChainFactors()}.
#' @noRd
#' @keywords internal
.computePopFimCombo_R = function( gradients,
                                   mu_values,
                                   omega_iiv,
                                   gamma_values,
                                   error_variance,
                                   occ_col_widths,
                                   sigma_derivatives,
                                   has_iov ) {
  nOmega     = length( mu_values )
  nRows      = nrow( gradients )
  nOccasions = length( occ_col_widths )

  # Rows: mu block first, then optional beta rows from covariate effects.
  gradientsMu   = gradients[ seq_len( nOmega ), , drop = FALSE ]
  gradientsBeta = if ( nRows > nOmega )
    gradients[ ( nOmega + 1L ):nRows, , drop = FALSE ]
  else
    matrix( 0, nrow = 0L, ncol = ncol( gradients ) )

  # Random-effect cov on eta: multi-occ expands gamma into occasion blocks;
  # single-occ pools omega²+gamma² on the diagonal; no-IOV is diag(omega²) only.
  nOmegaRows = length( omega_iiv )
  OMEGA = if ( has_iov && nOccasions > 1L ) {
    diag( c( omega_iiv, rep( gamma_values^2, nOccasions ) ) )
  } else if ( has_iov ) {
    diag( omega_iiv + gamma_values^2 )
  } else {
    diag( omega_iiv, nrow = nOmegaRows )
  }

  # eta-scale for IIV/IOV terms; MFbeta still uses raw G (mu + beta) vs V⁻¹.
  gradientsAdjustedMu = gradientsMu * mu_values
  gradientsAdjusted   = if ( nrow( gradientsBeta ) > 0L )
    rbind( gradientsAdjustedMu, gradientsBeta )
  else
    gradientsAdjustedMu

  if ( has_iov && nOccasions > 1L ) {
    # Multi-occ FO: V = G_etaᵀ diag(omega²) G_eta + Sum_i gamma_i² bdiag_k(g_{i,k} g_{i,k}ᵀ) + R.
    # (Cannot use a single Gᵀ Ω G with Ω = diag(omega, gamma⊗I_occ) without occasion splits.)
    scaledMu     = gradientsAdjustedMu * sqrt( omega_iiv )
    V_iiv        = as.matrix( crossprod( scaledMu ) )
    occGradsT    = map( .splitGradMuByOccasion( gradientsAdjustedMu, occ_col_widths ), t )
    gammaIndices = which( gamma_values > 0 )

    V_iov = if ( length( gammaIndices ) > 0L ) {
      reduce( map( gammaIndices, function( i )
        gamma_values[ i ]^2 * .iovOuterBlocks( i, occGradsT )
      ), `+` )
    } else {
      matrix( 0, nrow = nrow( error_variance ), ncol = ncol( error_variance ) )
    }

    V     = as.matrix( V_iiv + V_iov + error_variance )
    V_inv = .safeCholInv( V )

    # MFbeta = G V⁻¹ Gᵀ; G includes unscaled beta rows when present.
    MFbeta_full = ( gradients %*% V_inv ) %*% t( gradients )
    gradMuT     = t( gradientsAdjustedMu )

    # MFVar needs dV/dgamma_i (= occasion outer blocks) then dV/dσ_k.
    dV_gamma = map( gammaIndices, function( i )
      .iovOuterBlocks( i, occGradsT )
    )
    dV_full  = c( dV_gamma, sigma_derivatives )
  } else {
    # One occasion (or no IOV): classic V = G_etaᵀ Ω G_eta + R.
    G_mu  = gradientsAdjusted[ seq_len( nOmegaRows ), , drop = FALSE ]
    tmp   = OMEGA %*% G_mu
    V     = as.matrix( crossprod( tmp, G_mu ) + error_variance )
    V_inv = .safeCholInv( V )

    MFbeta_full = ( gradients %*% V_inv ) %*% t( gradients )
    gradMuT     = t( gradientsAdjustedMu )

    gammaIndices = if ( has_iov ) which( gamma_values > 0 ) else integer( 0L )
    dV_full      = sigma_derivatives
  }

  # W = eta-scaled mu grads (and pooled gamma cols when n_occ ≤ 1); dV_full carries σ / multi-occ gamma.
  W = gradMuT[ , seq_len( nOmega ), drop = FALSE ]
  if ( has_iov && nOccasions <= 1L && length( gammaIndices ) > 0L )
    W = cbind( W, gradMuT[ , gammaIndices, drop = FALSE ] )

  MFVar = computeMFVar_mixed_Rcpp( V_inv, W, map( dV_full, as.matrix ) )

  # Combination FIM is block-diagonal: FE first, then variance (omega / gamma / σ).
  nFixed = nrow( MFbeta_full )
  nVar   = nrow( MFVar )
  out    = matrix( 0, nFixed + nVar, nFixed + nVar )
  out[ seq_len( nFixed ), seq_len( nFixed ) ] = MFbeta_full
  out[ nFixed + seq_len( nVar ), nFixed + seq_len( nVar ) ] = MFVar
  out
}

#' Standard population path: fixed mu, diagonal IIV, no occasion structure.
#'
#' Builds \eqn{V = G_\eta^\top \Omega G_\eta + R}, then the fixed-effect block
#' \eqn{G V^{-1} G^\top} and the mixed variance block via Rcpp.
#' @param model A \code{Model} object.
#' @param arm An \code{Arm} with flat gradients and variance.
#' @param isFixedMu Logical vector of fixed mu flags. Fixed omegas are dropped
#'   by the caller (\code{PopulationFim} \code{evaluateFim}).
#' @return List with \code{MFbeta} and \code{MFVar} matrices.
#' @noRd
#' @keywords internal
.evaluateVarianceFIMPopSimple = function( model, arm, isFixedMu ) {
  parameters     = prop( model, "modelParameters" )
  parameterNames = map_chr( parameters, \( x ) prop( x, "name" ) )

  allGradientsData = prop( arm, "evaluationGradients" )
  varianceResults  = prop( arm, "evaluationVariance" )
  outputNames      = prop( model, "outputNames" )

  omegaIIV      = .modelOmegaIIVVariance( model )
  muChain       = .pfimPopMuChainFactors( parameters )
  nOmega        = length( parameterNames )
  errorVariance = as.matrix( varianceResults$errorVariance )

  # Stack outputs: rows = parameters, cols = observations (combo kernel layout).
  gradients = do.call( cbind, map( outputNames, \( nm ) t( allGradientsData[[ nm ]] ) ) )

  # Simple path = one occasion, no IOV - same Armadillo kernel as the cov path.
  fisher = as.matrix( computePopFimCombo_Rcpp(
    gradients,
    unname( muChain ),
    unname( omegaIIV ),
    rep( 0, nOmega ),
    errorVariance,
    as.integer( ncol( gradients ) ),
    map( varianceResults$sigmaDerivatives, as.matrix ),
    has_iov = FALSE
  ) )

  # Kernel returns full mu block; drop fixed-mu here. Lambda drop is deferred to
  # PopulationFim::evaluateFim (simple path) so sigma columns stay until then.
  muIdxKeep = seq_len( nOmega )[ !isFixedMu ]
  varIdx    = if ( nrow( fisher ) > nOmega )
    ( nOmega + 1L ):nrow( fisher ) else integer( 0L )
  list(
    MFbeta = fisher[ muIdxKeep, muIdxKeep, drop = FALSE ],
    MFVar  = if ( length( varIdx ) )
      fisher[ varIdx, varIdx, drop = FALSE ]
    else
      matrix( numeric( 0 ), 0L, 0L )
  )
}

#' Covariate / occasion path: sum Fisher blocks over covariate combinations.
#'
#' Pop FIM averages at the FIM level: \eqn{M = \sum_c \pi_c M_c} (not a single
#' expected gradient). Individual/Bayesian use the same mixture contract via
#' \code{.evaluateIndBayesMixtureFim()}.
#' @noRd
#' @keywords internal
.evaluateVarianceFIMPopCovariateOccasion = function( model, arm,
                                                       isFixedMu,
                                                       isFixedOmega,
                                                       isFixedLambda = NULL ) {
  parameters     = prop( model, "modelParameters" )
  parameterNames = map_chr( parameters, \( x ) prop( x, "name" ) )

  allGradientsData = prop( arm, "evaluationGradients" )
  varianceResults  = prop( arm, "evaluationVariance" )
  outputNames      = prop( model, "outputNames" )

  omegaIIV    = .modelOmegaIIVVariance( model )
  gammaValues = .paramGammas( parameters )
  hasIOV      = any( gammaValues > 0 )
  muChain     = .pfimPopMuChainFactors( parameters )

  nOmega = length( parameterNames )
  nGamma = sum( gammaValues > 0 )
  nSigma = length(
    pluck( varianceResults, 1L, "variances", 1L, "variance", "sigmaDerivatives" )
  )
  numberOfOccasions = pluck( varianceResults, 1L, "variances" ) |>
    map_chr( "occasion" ) |> unique() |> length()
  # IOV gamma block only when at least one gamma > 0
  nGammaEff = if ( hasIOV ) nGamma else 0L
  nLambda   = nOmega + nGammaEff + nSigma

  computeFisherOneCombination = function( iter ) {
    # Per combo: occasion grads/R are concatenated (bdiag R); kernel gets occ widths.
    gradientsByOccasion = map( seq_len( numberOfOccasions ), function( occ ) {
      do.call( cbind, map( outputNames,
                           \( x ) t( pluck( allGradientsData, iter, "gradients", occ, "gradient", x ) ) ) )
    })

    varianceByOccasion = map(
      seq_len( numberOfOccasions ),
      \( x ) as.matrix( pluck( varianceResults, iter, "variances", x, "variance", "errorVariance" ) )
    )

    # dR/dσ_k must match V's occasion block-diag layout.
    sigmaDerivatives = map( seq_len( nSigma ), function( i ) {
      sdo = map( seq_len( numberOfOccasions ),
                 \( x ) pluck( varianceResults, iter, "variances", x, "variance", "sigmaDerivatives", i ) )
      .pfimBlockDiag( sdo )
    })

    block = computePopFimCombo_Rcpp(
      do.call( cbind, gradientsByOccasion ),
      unname( muChain ),
      unname( omegaIIV ),
      unname( gammaValues ),
      .pfimBlockDiag( varianceByOccasion ),
      map_int( gradientsByOccasion, ncol ),
      sigmaDerivatives,
      has_iov = hasIOV
    )
    as.matrix( block ) * allGradientsData[[ iter ]]$proportion
  }

  # Mixture of FIMs (not of gradients): M = Sum_c pi_c M_c.
  # n_combo is the covariate Cartesian product (small); reduce is the contract.
  fisherMatrix = reduce(
    map( seq_along( allGradientsData ), computeFisherOneCombination ),
    `+`
  )

  # Kernel layout: [mu x nOmega | beta | lambda = omega(+gamma)+σ]; drop fixed mu from FE,
  # drop omega² only when fixedOmega (not when fixedMu).
  nBetaTotal = nrow( fisherMatrix ) - nLambda - nOmega
  nMuAndBeta = nOmega + nBetaTotal
  muIdxKeep  = seq_len( nOmega )[ !isFixedMu ]
  betaIdx    = if ( nBetaTotal > 0L ) ( nOmega + 1L ):nMuAndBeta else integer( 0L )
  varIdx     = ( nMuAndBeta + 1L ):nrow( fisherMatrix )

  MFbeta        = fisherMatrix[ c( muIdxKeep, betaIdx ), c( muIdxKeep, betaIdx ), drop = FALSE ]
  MFVarFull     = fisherMatrix[ varIdx, varIdx, drop = FALSE ]
  isFixedLambda = isFixedLambda %||% isFixedOmega
  if ( length( isFixedLambda ) != nOmega )
    .pfimInternalStop(
      "isFixedLambda length (", length( isFixedLambda ),
      ") must match the number of parameters (", nOmega, ")."
    )
  # Gamma/sigma columns are never flagged fixed via isFixedLambda (length = nOmega).
  keepLambda = c( !isFixedLambda, rep( TRUE, nGammaEff + nSigma ) )

  list(
    MFbeta = MFbeta,
    MFVar  = MFVarFull[ keepLambda, keepLambda, drop = FALSE ]
  )
}

#' Dispatch population variance FIM between simple and covariate/occasion paths.
#'
#' Simple path: flat arm grads, no beta/IOV occasion layout. Complex path:
#' nested comboxoccasion evaluations and lambda-dropping via \code{isFixedLambda}.
#' @noRd
#' @keywords internal
.evaluateVarianceFIMPop = function( model, arm,
                                    isFixedMu     = NULL,
                                    isFixedOmega  = NULL,
                                    isFixedLambda = NULL ) {
  parameters    = prop( model, "modelParameters" )
  isFixedMu     = isFixedMu %||% map_lgl( parameters, .paramMuFixed )
  isFixedOmega  = isFixedOmega %||% map_lgl( parameters, .paramOmegaFixed )
  isFixedLambda = isFixedLambda %||% isFixedOmega

  if ( !usesCovariateOccasionStructure( model ) )
    .evaluateVarianceFIMPopSimple( model, arm, isFixedMu )
  else
    .evaluateVarianceFIMPopCovariateOccasion( model, arm, isFixedMu, isFixedOmega, isFixedLambda )
}
