# Wald power and required sample size for the covariate significance / relevance tests.

#' Two-sided Wald power for testing \eqn{H_0: \beta = 0}.
#' @noRd
#' @keywords internal
.powerSignificance = function( beta, SE, zHalf ) {
  1 - pnorm( zHalf - beta / SE ) + pnorm( -zHalf - beta / SE )
}

#' Sample size so that Wald power for \eqn{\beta} reaches target \code{PS}.
#'
#' Inverts the critical-value equation for the SE that yields power \code{PS},
#' then converts with \eqn{N = \sigma^2_{\mathrm{unit}} / \mathrm{SE}^2}.
#' @noRd
#' @keywords internal
.nRequiredSignificance = function( beta, sigma2Unit, zHalf, PS ) {
  if ( beta == 0 ) return( NA_real_ )
  seS = if ( beta > 0 ) {
    beta  / ( zHalf - qnorm( 1 - PS ) )
  } else {
    -beta / ( zHalf + qnorm( PS ) )
  }
  if ( seS <= 0 ) return( NA_real_ )
  sigma2Unit / seS^2
}

#' TOST power for clinical non-relevance (\eqn{\beta} inside [\code{Binf}, \code{Bsup}]).
#' @noRd
#' @keywords internal
.powerNonRelevance = function( beta, SE, Binf, Bsup, z_one ) {
  # When the CI width needed for TOST exceeds the equivalence window, power is 0.
  if ( 2 * z_one >= ( Bsup - Binf ) / SE ) return( 0 )
  pnorm( -z_one + ( Bsup - beta ) / SE ) -
    pnorm(  z_one + ( Binf - beta ) / SE )
}

#' Expand an upper search bound until \code{f(N) >= 0} (sample-size root finding).
#' @noRd
#' @keywords internal
.nBracketSampleSizeRoot = function( f, N_min = 2, N_max = 1e7, growth = 2 ) {
  steps      = ceiling( log( N_max / N_min, base = growth ) )
  candidates = unique( pmin( N_min * growth^( 0:steps ), N_max ) )
  detect( candidates, \( N ) f( N ) >= 0 ) %||% N_max
}

#' Sample size for TOST non-relevance power (uniroot on power − target).
#' @noRd
#' @keywords internal
.nRequiredNonRelevance = function( beta, sigma2Unit, Binf, Bsup, z_one, PS ) {
  if ( beta <= Binf || beta >= Bsup ) return( NA_real_ )

  f = function( N ) {
    .powerNonRelevance( beta, sqrt( sigma2Unit / N ), Binf, Bsup, z_one ) - PS
  }
  pMax = .powerNonRelevance( beta, 1e-12, Binf, Bsup, z_one )

  if ( f( 2 ) >= 0 ) return( 2 )
  if ( pMax < PS ) return( NA_real_ )
  # Near-asymptotic power: report Inf rather than a huge finite N.
  if ( pMax - PS < 0.005 ) return( Inf )

  N_hi = .nBracketSampleSizeRoot( f )
  if ( f( N_hi ) < 0 ) return( NA_real_ )

  tryCatch(
    uniroot( f, interval = c( 2, N_hi ) )$root,
    error = function( e ) NA_real_
  )
}

#' Power that \eqn{\beta} lies outside the equivalence window (clinical relevance).
#' @noRd
#' @keywords internal
.powerRelevance = function( beta, SE, Binf, Bsup, z_one ) {
  pnorm( -z_one + ( Binf - beta ) / SE ) +
    1 - pnorm(  z_one + ( Bsup - beta ) / SE )
}

#' Sample size for clinical-relevance power (beta outside [\code{Binf}, \code{Bsup}]).
#' @noRd
#' @keywords internal
.nRequiredRelevance = function( beta, sigma2Unit, Binf, Bsup, z_one, PS ) {
  if ( beta >= Binf && beta <= Bsup ) return( NA_real_ )
  seR = if ( beta > Bsup ) {
    ( Bsup - beta ) / ( qnorm( 1 - PS ) - z_one )
  } else {
    ( Binf - beta ) / ( qnorm( PS ) + z_one )
  }
  if ( is.na( seR ) || seR <= 0 ) return( NA_real_ )
  sigma2Unit / seR^2
}

