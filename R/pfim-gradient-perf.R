# FD / ODE gradient performance caches (session options under perf.*).
#
# Speeds repeated gradient evaluations during continuous optimization:
#   - `.pfimFdSchemeCache`  - quadratic/linear FD stencil (indMat, XcolsInv)
#     keyed on nFree + perf.fdLinearOnly (not on mu)
#   - `.pfimGradAdminCache` - ODE administration wrappers (dose/sampling layout)
#   - C++ ODE sim-time grid cache (ode-cache.cpp) via `pfimOdeSimTimesCached_Rcpp`
#
# Keys are hashed when long (Windows env name limit). Clear with
# `.pfimClearGradientPerfCaches()` / `pfim_reset_session()`.

.pfimFdSchemeCache  = new.env( parent = emptyenv() )
.pfimGradAdminCache = new.env( parent = emptyenv() )

#' Read a boolean performance option (default FALSE unless overridden).
#' @noRd
#' @keywords internal
.pfimPerfOption = function( name, default = FALSE ) {
  isTRUE( pfim_get_option( name, default ) )
}

# Hash long cache keys (Windows env name limit ~255 chars for bindings).
#' @noRd
#' @keywords internal
.pfimEnvCacheKey = function( x ) {
  pfimEnvCacheKey_Rcpp( as.character( x ) )
}

#' Compact signature of parameter names, mu values, and fixed-mu flags.
#' @noRd
#' @keywords internal
.pfimMuSignature = function( parameters ) {
  paste(
    map_chr( parameters, \( x ) prop( x, "name" ) ),
    format(
      map_dbl( parameters, function( p ) {
        d = prop( p, "distribution" )
        if ( is.null( d ) ) {
          v = prop( p, "value" )
          if ( length( v ) == 1L && is.finite( v ) ) v else NA_real_
        } else prop( d, "mu" )
      } ),
      digits = 12, scientific = TRUE
    ),
    map_lgl( parameters, .paramMuFixed ),
    sep = "=",
    collapse = "|"
  )
}

# Expand free-parameter FD shifts back to full-length parameter columns.
# Parameters outside freeIdx keep their nominal value; freeIdx is estimable mu,
# any omega>0, and any gamma>0.
#' @noRd
#' @keywords internal
.pfimFdShiftedFull = function( pars, freeIdx, shiftedFree ) {
  shifted = matrix( as.numeric( pars ), nrow = length( pars ), ncol = ncol( shiftedFree ) )
  shifted[ freeIdx, ] = shiftedFree
  shifted
}

#' Indices of parameters shifted by the FD grid.
#'
#' Estimable \eqn{\mu}, any parameter with \eqn{\omega > 0} (Bayesian MAP
#' needs \eqn{\partial f/\partial\mu} even when \code{fixedMu}), and any
#' parameter with \eqn{\gamma > 0} (IOV residual still needs that gradient
#' when \eqn{\mu} is fixed and \eqn{\omega=0}). Population FIM later drops
#' fixed-\eqn{\mu} columns.
#' @noRd
#' @keywords internal
.pfimFdFreeIndices = function( parameters ) {
  which( map_lgl( parameters, function( p ) {
    .paramMuEstimable( p ) || .paramHasIiv( p ) || .paramHasIov( p )
  } ) )
}

# Quadratic FD stencil on free parameters; full-length shifted columns for evaluateModel().
# Linear-only mode (perf.fdLinearOnly) drops cross terms and returns incr for central differences.
# Scheme cache key is nFree + linearOnly only - mu enters later via incr * indMat.

.pfimFdSchemeKey = function( nFree, linearOnly ) {
  paste0( "n", as.integer( nFree ), if ( isTRUE( linearOnly ) ) "_lin" else "_quad" )
}

#' Quadratic / linear FD design matrix and its inverse (independent of mu).
#' @noRd
#' @keywords internal
.pfimFdStencilScheme = function( nFree, linearOnly ) {
  cache = .pfimPerfOption( "perf.fdCache", TRUE )
  key   = .pfimFdSchemeKey( nFree, linearOnly )
  if ( cache ) {
    cached = .pfimFdSchemeCache[[ key ]]
    if ( !is.null( cached ) ) return( cached )
  }

  # indMat columns: 0 | +e_i | -e_i | (+e_i+e_j for i<j when quadratic).
  # Xcols rows match those columns: [1 | s | s^2 | s_i*s_j]; XcolsInv recovers coeffs.
  baseInd = diag( nFree )
  if ( isTRUE( linearOnly ) ) {
    scheme = list(
      indMat   = cbind( 0, baseInd, -baseInd ),
      XcolsInv = NULL
    )
  } else {
    extraCols = if ( nFree > 1L )
      map( seq_len( nFree - 1L ), \( x ) baseInd[ , x ] + baseInd[ , -seq_len( x ) ] )
    else list()
    indMat  = do.call( cbind, c( list( 0, baseInd, -baseInd ), extraCols ) )
    indMatT = t( indMat )
    Xcols   = c(
      list( 1, indMatT, indMatT^2 ),
      if ( nFree > 1L )
        map( seq_len( nFree - 1L ), \( x ) indMatT[ , x ] * indMatT[ , -seq_len( x ) ] )
      else list()
    )
    scheme = list(
      indMat   = indMat,
      XcolsInv = .safeSolve( do.call( cbind, Xcols ) )
    )
  }

  if ( cache )
    .pfimCacheStore( .pfimFdSchemeCache, key, scheme )
  scheme
}

#' Compact id for FD scaling warnings (stable across equal mus).
#' @noRd
#' @keywords internal
.pfimFdWarnId = function( x ) {
  .pfimCacheHash( signif( as.numeric( x ), 6L ) )
}

.pfimFiniteDifferenceGrid = function( pars, freeIdx,
                                      odeSolverParameters = NULL,
                                      scaleToOdeTol = FALSE ) {
  nFree = length( freeIdx )

  if ( nFree == 0L ) {
    return( list(
      shifted  = matrix( pars, ncol = 1L ),
      XcolsInv = matrix( 1, 1, 1 ),
      frac     = 1,
      freeIdx  = freeIdx
    ) )
  }

  parsFree = pars[ freeIdx ]
  # eps^(1/3) is optimal for a centred first derivative.
  relStep = .Machine$double.eps^( 1 / 3 )
  # Absolute step at mu == 0: use 1e-4 (not floor*relStep ≈ 6e-10, which is
  # drowned by floating-point noise). Near-zero |mu| < 1e-4 keeps floor*relStep
  # with a one-shot warning.
  absMu  = abs( parsFree )
  incr   = absMu * relStep
  zeroMu = absMu < .Machine$double.eps
  incr[ zeroMu ] = 1e-4
  tinyIdx = !zeroMu & absMu < 1e-4
  if ( any( tinyIdx ) ) {
    incr[ tinyIdx ] = 1e-4 * relStep
    .pfimWarnOnce(
      "fdAbsFloor",
      "Finite-difference |mu| < 1e-4 uses an absolute step floor of 1e-4; ",
      "truncation error can dominate. Scale parameters to O(1) ",
      "(e.g. rates in 1/h rather than 1/min).",
      id = .pfimFdWarnId( parsFree[ tinyIdx ] )
    )
  }
  # For ODE models, ensure the absolute FD step stays well above solver noise.
  # Do NOT replace eps^(1/3) by odeTol^(1/3): that inflates steps by orders of
  # magnitude (e.g. 1e-8^(1/3) ≈ 2e-3) and systematically biases the FIM.
  if ( isTRUE( scaleToOdeTol ) ) {
    odeTol = {
      tols = .pfimDeSolveTolerances( odeSolverParameters )
      min( as.numeric( tols$atol ), as.numeric( tols$rtol ) )
    }
    if ( !is.finite( odeTol ) || odeTol <= 0 )
      odeTol = 1e-8
    minIncr = 10 * odeTol
    raisedIdx = incr < minIncr
    if ( any( raisedIdx ) ) {
      incr = pmax( incr, minIncr )
      if ( isTRUE( pfim_get_option( "verbose", FALSE ) ) )
        .pfimWarnOnce(
          "fdStepRaised",
          "Finite-difference step raised to 10 * ODE tolerance; ",
          "gradients (and the FIM) may still be inaccurate if parameters ",
          "are tiny relative to atol/rtol. Tighten odeSolverParameters if needed.",
          id = .pfimFdWarnId( parsFree[ raisedIdx ] )
        )
    }
  }

  linearOnly  = .pfimPerfOption( "perf.fdLinearOnly", TRUE )
  scheme      = .pfimFdStencilScheme( nFree, linearOnly )
  shiftedFree = parsFree + incr * scheme$indMat

  # frac aligns with Xcols column scaling so XcolsInv %*% f / frac yields
  # unscaled polynomial coeffs (constant, first derivs, second, cross).
  if ( linearOnly ) {
    return( list(
      shifted    = .pfimFdShiftedFull( pars, freeIdx, shiftedFree ),
      XcolsInv   = NULL,
      linearOnly = TRUE,
      incr       = incr,
      frac       = c( 1, incr, incr ),
      freeIdx    = freeIdx
    ) )
  }

  extraFrac = if ( nFree > 1L )
    map( seq_len( nFree - 1L ), \( x ) incr[ x ] * incr[ -seq_len( x ) ] )
  else list()

  list(
    shifted  = .pfimFdShiftedFull( pars, freeIdx, shiftedFree ),
    XcolsInv = scheme$XcolsInv,
    frac     = c( 1, incr, incr^2, unlist( extraFrac ) ),
    freeIdx  = freeIdx
  )
}

# Signature of doses, sampling times, and initial-condition expressions for an arm.
#' @noRd
#' @keywords internal
.pfimArmAdminSignature = function( arm ) {
  adms = prop( arm, "administrations" )
  sts  = prop( arm, "samplingTimes" )
  ic   = prop( arm, "initialConditions" )

  doseSig = map_chr( adms, function( adm ) {
    dosing = .alignAdministrationDosing( adm )
    paste(
      prop( adm, "outcome" ),
      paste( dosing$timeDose, collapse = "," ),
      paste( dosing$dose, collapse = "," ),
      paste( dosing$Tinf, collapse = "," ),
      prop( adm, "tau" ),
      sep = ":"
    )
  } )

  sampSig = map_chr( sts, function( st ) {
    paste( prop( st, "outcome" ), paste( prop( st, "samplings" ), collapse = "," ), sep = ":" )
  } )

  icSig = if ( length( ic ) )
    paste( names( ic ), map_chr( ic, as.character ), sep = "=", collapse = ";" )
  else ""

  paste( paste( doseSig, collapse = ";" ), paste( sampSig, collapse = ";" ), icSig, sep = "||" )
}

#' Cache key for ODE admin layout (model class + param names + arm admin signature).
#' Intentionally omits mu: hit path refreshes mus via .pfimRefreshOdeAdministration.
#' @noRd
#' @keywords internal
.pfimGradAdminCacheKey = function( model, arm ) {
  cls = class( model )[[ 1L ]]
  parameterNames = paste( map_chr( prop( model, "modelParameters" ), \( x ) prop( x, "name" ) ), collapse = "," )
  .pfimEnvCacheKey( paste( cls, parameterNames, .pfimArmAdminSignature( arm ), sep = "::" ) )
}

#' Copy of a \code{ModelParameter} with its typical value set to \code{mu}.
#' @noRd
#' @keywords internal
.pfimParameterWithMu = function( param, mu ) {
  distribution = prop( param, "distribution" )
  if ( is.null( distribution ) )
    return( set_props( param, value = mu ) )
  set_props( param, distribution = set_props( distribution, mu = mu ) )
}

#' Overwrite each parameter mu with the corresponding shifted value.
#' @noRd
#' @keywords internal
.pfimShiftModelParameters = function( parameters, shiftedCol ) {
  map2( parameters, as.numeric( shiftedCol ), .pfimParameterWithMu )
}

#' TRUE for model classes that benefit from administration caching.
#' @noRd
#' @keywords internal
.pfimUsesAdminCache = function( model ) {
  .pfimIsOdeModel( model )
}

#' Build a hash key for the C++ ODE sim-time cache from mode, samples, and events.
#' @noRd
#' @keywords internal
.pfimOdeSimCacheKey = function( mode, rawSamplings, eventsDf ) {
  parts = c( mode, paste( rawSamplings, collapse = "," ) )
  if ( !is.null( eventsDf ) )
    parts = c( parts, paste( eventsDf$time, eventsDf$value, sep = ":", collapse = "," ) )
  pfimCacheHash_Rcpp( parts )
}

#' Cached unique sorted ODE simulation times (samples ∪ event times).
#' @noRd
#' @keywords internal
.pfimOdeSimTimesCached = function( cacheKey, rawSamplings, eventsDf ) {
  eventTimes = if ( !is.null( eventsDf ) ) eventsDf$time else NULL
  maxEntries = pfim_get_option( "perf.odeTimesCache.maxEntries", 2048L )
  if ( is.null( maxEntries ) || !is.finite( maxEntries ) )
    maxEntries = -1L
  pfimOdeSimTimesCached_Rcpp(
    cacheKey,
    rawSamplings,
    eventTimes,
    enabled = .pfimPerfOption( "perf.odeTimesCache", TRUE ),
    max_entries = as.integer( maxEntries )
  )
}

# First call builds administration; later calls refresh mu/dose only from the cached entry.
#' @noRd
#' @keywords internal
.pfimDefineModelAdministrationCached = function( model, arm ) {
  if ( !.pfimPerfOption( "perf.adminCache", TRUE ) || !.pfimUsesAdminCache( model ) )
    return( defineModelAdministration( model, arm ) )

  key   = .pfimGradAdminCacheKey( model, arm )
  entry = .pfimGradAdminCache[[ key ]]

  if ( is.null( entry ) ) {
    # Miss: build full administration and store a reusable entry.
    if ( S7::S7_inherits( model, ModelODEInfusionDoseInEquation ) ) {
      parts = .odeInfusionAdminParts( model, arm )
      .pfimCacheStore( .pfimGradAdminCache, key, .pfimInfusionAdminEntryFromParts( parts ) )
      return( .odeInfusionApplyParts( model, parts ) )
    }
    model = defineModelAdministration( model, arm )
    .pfimCacheStore( .pfimGradAdminCache, key, .pfimCaptureOdeAdminEntry( model, arm ) )
    return( model )
  }

  # Hit: reapply cached wrapper / dose tables with current mu values.
  .pfimRefreshOdeAdministration( model, arm, entry )
}

# Parsed wrapper / RHS / dose tables - enough to rebuild after a mu shift.
#' @noRd
#' @keywords internal
.pfimCaptureOdeAdminEntry = function( model, arm ) {
  if ( !S7::S7_inherits( model, ModelODE ) ) return( list( type = "full" ) )
  mode = tryCatch( .odeBolusMode( model ), error = function( e ) "full" )
  if ( identical( mode, "full" ) ) return( list( type = "full" ) )

  entry = list(
    type                       = mode,
    samplings                  = prop( model, "samplings" ),
    wrapper                    = prop( model, "wrapper" ),
    functionArguments          = prop( model, "functionArguments" ),
    functionArgumentsSymbols   = prop( model, "functionArgumentsSymbol" ),
    outputFormula              = .getOutputFormulaParsed( model )
  )
  if ( mode == "doseInEq" ) {
    entry$solverInputs               = prop( model, "solverInputs" )
    entry$outcomesWithAdministration = prop( model, "outcomesWithAdministration" )
  } else {
    entry$doseEventTemplate = .odeDoseEventFromArm( arm, prop( model, "samplings" ), model )
    if ( mode == "bolusIc" )
      entry$initialConditionsParsed = .parseInitialConditionExprs( prop( arm, "initialConditions" ) )
  }
  entry
}

#' Reapply a cached admin entry (or fall back to a full defineModelAdministration).
#' @noRd
#' @keywords internal
.pfimRefreshOdeAdministration = function( model, arm, entry ) {
  if ( entry$type == "full" )
    defineModelAdministration( model, arm )
  else if ( entry$type == "infusionDoseInEq" )
    .odeInfusionApplyAdminEntry( model, arm, entry )
  else
    .odeApplyAdminEntry( model, arm, entry )
}

#' Bind arm administration to a model (uses admin cache when enabled).
#' @noRd
#' @keywords internal
.pfimPrepareModelForEvaluation = function( model, arm ) {
  .pfimDefineModelAdministrationCached( model, arm )
}

#' Clear all R and C++ gradient / ODE performance caches.
#' @noRd
#' @keywords internal
.pfimClearGradientPerfCaches = function() {
  .pfimEnvClear( .pfimFdSchemeCache )
  .pfimEnvClear( .pfimGradAdminCache )
  pfimOdeSimTimesCacheClear_Rcpp()
  .pfimEnvClear( .pfimOutputFormulaCache )
  .pfimEnvClear( .pfimParsedOutputFormulaCache )
  invisible( NULL )
}
