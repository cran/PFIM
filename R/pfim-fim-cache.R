# FIM caches: design-level Fisher matrices and covariate x occasion gradients.
#
# Design cache keys: scope :: model signature :: designs signature.
# Cov/occasion cache keys: scope :: model :: arm admin :: combination :: occasion.
# Scope is set by .pfimFimCacheBegin() at the start of each optimization run
# (fim.cache.scope). LRU eviction uses fim.cache.maxEntries when set.
# Continuous-optimizer helpers live in pfim-continuous-opt.R.

.pfimFimDesignCache = new.env( parent = emptyenv() )
.pfimCovOccasionCache = new.env( parent = emptyenv() )

#' Read (or initialise) the LRU key order for a cache environment.
#' @noRd
#' @keywords internal
.pfimCacheLru = function( env ) {
  if ( !exists( ".__lru", envir = env, inherits = FALSE ) )
    assign( ".__lru", character( 0L ), envir = env )
  get( ".__lru", envir = env, inherits = FALSE )
}

#' Mark \code{key} as most recently used (move to end of LRU list).
#' @noRd
#' @keywords internal
.pfimCacheLruTouch = function( env, key ) {
  if ( !exists( key, envir = env, inherits = FALSE ) )
    return( invisible( NULL ) )
  lru = .pfimCacheLru( env )
  assign( ".__lru", c( lru[ lru != key ], key ), envir = env )
  invisible( NULL )
}

#' Cache binding names excluding the internal \code{.__lru} bookkeeping key.
#' @noRd
#' @keywords internal
.pfimCacheKeys = function( env ) {
  keys = ls( env, all.names = TRUE )
  keys[ keys != ".__lru" ]
}

#' Drop least-recently-used entries until size \eqn{\le} \code{fim.cache.maxEntries}.
#' @noRd
#' @keywords internal
.pfimCacheTrimEnv = function( env ) {
  max = pfim_get_option( "fim.cache.maxEntries", NULL )
  if ( is.null( max ) || !is.finite( max ) )
    return( invisible( NULL ) )
  max = as.integer( max )
  if ( max <= 0L )
    return( invisible( NULL ) )
  lru = .pfimCacheLru( env )
  lru = lru[ lru %in% .pfimCacheKeys( env ) ]
  overflow = length( lru ) - max
  if ( overflow > 0L ) {
    rm( list = lru[ seq_len( overflow ) ], envir = env )
    lru = lru[ -seq_len( overflow ) ]
    assign( ".__lru", lru, envir = env )
  }
  invisible( NULL )
}

#' Store a value, touch LRU, then trim if over the max-entries cap.
#' @noRd
#' @keywords internal
.pfimCacheStore = function( env, key, value ) {
  assign( key, value, envir = env )
  .pfimCacheLruTouch( env, key )
  .pfimCacheTrimEnv( env )
}

#' Active FIM cache scope: option, else project id, else \code{"global"}.
#' @noRd
#' @keywords internal
.pfimActiveCacheScope = function( evaluation = NULL ) {
  scope = pfim_get_option( "fim.cache.scope", NULL )
  if ( .pfimIsNonEmptyScalar( scope ) )
    return( scope )
  if ( !is.null( evaluation ) )
    return( .pfimProjectScopeId( projectOf( evaluation ) ) )
  "global"
}

#' Remove all keys starting with \code{prefix} and update the LRU list.
#' @noRd
#' @keywords internal
.pfimCacheRmByPrefix = function( env, prefix ) {
  keys = ls( env, all.names = TRUE )
  if ( !length( keys ) )
    return( invisible( NULL ) )
  drop = keys[ startsWith( keys, prefix ) ]
  if ( !length( drop ) )
    return( invisible( NULL ) )
  rm( list = drop, envir = env )
  if ( exists( ".__lru", envir = env, inherits = FALSE ) ) {
    lru = get( ".__lru", envir = env, inherits = FALSE )
    assign( ".__lru", lru[ !( lru %in% drop ) ], envir = env )
  }
  invisible( NULL )
}

#' Stable cache-scope id for a project (stored on \code{cacheScope} / registry).
#' @noRd
#' @keywords internal
.pfimProjectScopeId = function( project ) {
  project = projectOf( project )
  scope = projectProp( project, "cacheScope" )
  if ( .pfimIsNonEmptyScalar( scope ) )
    return( scope )

  key = .pfimProjectScopeKey( project )
  if ( exists( key, envir = .pfimProjectScopeRegistry, inherits = FALSE ) )
    return( get( key, envir = .pfimProjectScopeRegistry ) )

  scope = .pfimNextCacheScope()
  assign( key, scope, envir = .pfimProjectScopeRegistry )
  projectProp( project, "cacheScope" ) = scope
  scope
}

#' Drop all address -> scope mappings (session reset).
#' @noRd
#' @keywords internal
.pfimClearProjectScopeRegistry = function() {
  rm( list = ls( .pfimProjectScopeRegistry, all.names = TRUE ),
      envir = .pfimProjectScopeRegistry )
  invisible( NULL )
}

#' Stable hash of arbitrary R objects for cache keys.
#' @noRd
#' @keywords internal
.pfimCacheHash = function( ... ) {
  rlang::hash( list( ... ) )
}

#' Attach an evaluated design and its FIM onto a copy of an Evaluation.
#' @noRd
#' @keywords internal
.pfimEvaluationFromDesign = function( evaluation, design, evaluatedDesign ) {
  set_props(
    evaluation,
    designs          = list( design ),
    evaluationDesign = list( evaluatedDesign ),
    fim              = prop( evaluatedDesign, "fim" )
  )
}

# --- model signature (rebuildEvalModel / eval-model cache; also scopes design cache indirectly)

#' Hash of model equations, parameters, errors, covariates, FIM type, etc.
#'
#' When \code{model.cache.signature} is FALSE, falls back to the project scope id.
#' @noRd
#' @keywords internal
.pfimModelCacheId = function( pfimproject ) {
  if ( !isTRUE( pfim_get_option( "model.cache.signature", TRUE ) ) )
    return( .pfimProjectScopeId( projectOf( pfimproject ) ) )

  params = projectProp( pfimproject, "modelParameters" )
  paramSig = map( params, function( p ) {
    d = prop( p, "distribution" )
    list(
      name       = prop( p, "name" ),
      distClass  = if ( is.null( d ) ) NA_character_ else S7::prop( S7::S7_class( d ), "name" ),
      mu         = if ( is.null( d ) ) NA_real_ else prop( d, "mu" ),
      omega      = if ( is.null( d ) ) NA_real_ else prop( d, "omega" ),
      gamma      = .paramGamma( p ),
      fixedMu    = isTRUE( prop( p, "fixedMu" ) ),
      fixedOmega = isTRUE( prop( p, "fixedOmega" ) )
    )
  } )
  errors = projectProp( pfimproject, "modelError" )
  errorSig = map( errors, function( e ) {
    c(
      prop( e, "output" ),
      S7::prop( S7::S7_class( e ), "name" ),
      prop( e, "sigmaInter" ),
      .pfimPropOr( e, "sigmaSlope", 0 ),
      .pfimPropOr( e, "cError", 1 ),
      .modelErrorVarianceForm( e ),
      isTRUE( .pfimPropOr( e, "sigmaInterFixed", FALSE ) ),
      isTRUE( .pfimPropOr( e, "sigmaSlopeFixed", FALSE ) )
    )
  } )
  lib = projectProp( pfimproject, "modelFromLibrary" )
  libKey = if ( length( lib ) ) unlist( lib, use.names = TRUE ) else character( 0L )
  covs = projectProp( pfimproject, "modelCovariates" )
  covSig = map( covs, function( cov ) {
    list(
      name                  = prop( cov, "name" ),
      effects               = prop( cov, "effects" ),
      categories            = .pfimPropOr( cov, "categories", NULL ),
      categoriesProportions = .pfimPropOr( cov, "categoriesProportions", NULL ),
      sequences             = .pfimPropOr( cov, "sequences", NULL ),
      sequencesProportions  = .pfimPropOr( cov, "sequencesProportions", NULL )
    )
  } )
  .pfimCacheHash(
    projectProp( pfimproject, "fimType" ),
    projectProp( pfimproject, "modelClass" ),
    projectProp( pfimproject, "modelEquations" ),
    projectProp( pfimproject, "outputs" ),
    projectProp( pfimproject, "odeSolverParameters" ),
    libKey,
    paramSig,
    errorSig,
    covSig,
    projectProp( pfimproject, "modelCovariatesEquation" ),
    projectProp( pfimproject, "numberOfOccasions" )
  )
}

#' Active evaluation for covariate/occasion cache keys (set by \code{run()}).
#' @noRd
#' @keywords internal
.pfimCacheEvaluation = function() {
  pfim_get_option( "fim.cache.evaluation", NULL )
}

#' Set the Evaluation used when building covariate/occasion cache keys.
#' @noRd
#' @keywords internal
.pfimSetCacheEvaluation = function( evaluation ) {
  pfim_set_option( fim.cache.evaluation = evaluation )
  invisible( NULL )
}

# --- design-level FIM cache (doses + sampling times per arm)

#' Hash of arm sizes, doses, infusion times, sampling grids, and ODE ICs.
#' @noRd
#' @keywords internal
.pfimDesignSignature = function( design ) {
  arms = prop( design, "arms" )
  parts = map( arms, function( arm ) {
    adms   = prop( arm, "administrations" )
    sts    = prop( arm, "samplingTimes" )
    dosing = map( adms, function( a ) {
      d = .alignAdministrationDosing( a )
      list(
        dose     = as.numeric( d$dose ),
        timeDose = as.numeric( d$timeDose ),
        Tinf     = as.numeric( d$Tinf ),
        tau      = as.numeric( prop( a, "tau" ) )
      )
    } )
    samps = map( sts, \( s ) as.numeric( prop( s, "samplings" ) ) )
    ic = prop( arm, "initialConditions" )
    if ( length( ic ) && !is.null( names( ic ) ) )
      ic = ic[ order( names( ic ) ) ]
    list(
      name   = prop( arm, "name" ),
      size   = as.numeric( prop( arm, "size" ) ),
      dosing = dosing,
      samps  = samps,
      ic     = ic
    )
  } )
  .pfimCacheHash( parts )
}

#' Combined signature for a list of designs.
#' @noRd
#' @keywords internal
.pfimDesignsSignature = function( designs ) {
  .pfimCacheHash( map( designs, .pfimDesignSignature ) )
}

#' Full design-FIM cache key: scope :: model id :: designs signature.
#' @noRd
#' @keywords internal
.pfimFimCacheKey = function( evaluation ) {
  paste0(
    .pfimActiveCacheScope( evaluation ), "::",
    .pfimModelCacheId( evaluation ), "::",
    .pfimDesignsSignature( prop( evaluation, "designs" ) )
  )
}

#' Open a fresh optimization cache scope and clear any leftover entries for it.
#' @noRd
#' @keywords internal
.pfimFimCacheBegin = function() {
  scope = .pfimNextCacheScope()
  pfim_set_option( fim.cache.scope = scope )
  .pfimFimCacheClear( scope )
  invisible( scope )
}

#' Drop design and cov/occasion cache entries for one scope (or the active one).
#' @noRd
#' @keywords internal
.pfimFimCacheClear = function( scope = NULL ) {
  scope = scope %||% pfim_get_option( "fim.cache.scope", NULL )
  if ( is.null( scope ) || !.pfimIsNonEmptyScalar( scope ) )
    return( invisible( NULL ) )
  .pfimCacheRmByPrefix( .pfimFimDesignCache, paste0( scope, "::" ) )
  .pfimCovOccasionCacheClear( scope )
  invisible( NULL )
}

#' Flush optimization-scope FIM caches before final design re-evaluation.
#' @noRd
#' @keywords internal
.pfimOptimizationCacheFlush = function() {
  .pfimFimCacheClear( pfim_get_option( "fim.cache.scope" ) )
}

#' Increment a session hit-counter option by one.
#' @noRd
#' @keywords internal
.pfimCacheBump = function( name ) {
  key = .pfimNormalizeOptionName( name )
  assign( key, pfim_get_option( name, 0L ) + 1L, envir = .pfimSession )
}

# --- covariate x occasion cache (gradient path)

#' Whether the covariate/occasion gradient cache is enabled.
#' @noRd
#' @keywords internal
.pfimCovOccasionCacheEnabled = function() {
  isTRUE( pfim_get_option( "covariate.occasion.cache", TRUE ) )
}

#' Order-independent hash of a named numeric vector.
#' @noRd
#' @keywords internal
.pfimValuesSignature = function( namedValues ) {
  vals = unlist( namedValues, use.names = TRUE )
  ord = order( names( vals ) )
  .pfimCacheHash( names( vals )[ ord ], vals[ ord ] )
}

#' Cache key for one arm x covariate combination x occasion evaluation.
#' @noRd
#' @keywords internal
.pfimCovOccasionCacheKey = function( arm, combination, occasion, occasionParams, model = NULL ) {
  eval = .pfimCacheEvaluation()
  modelId = if ( !is.null( eval ) ) {
    .pfimModelCacheId( eval )
  } else if ( !is.null( model ) ) {
    paste(
      class( model )[[ 1L ]],
      .pfimMuSignature( prop( model, "modelParameters" ) ),
      sep = "::"
    )
  } else {
    .pfimInternalStop( "Covariate occasion cache requires an evaluation context or model." )
  }
  raw = paste(
    .pfimActiveCacheScope( eval ), modelId, .pfimArmAdminSignature( arm ),
    .pfimCacheHash( combination ), occasion, .pfimValuesSignature( occasionParams ),
    sep = "::"
  )
  .pfimEnvCacheKey( raw )
}

#' Lookup covariate/occasion cache; NULL on miss or incomplete payload.
#' @noRd
#' @keywords internal
.pfimCovOccasionCacheGet = function( arm, combination, occasion, occasionParams,
                                     wantModel, wantGrad, model = NULL ) {
  if ( !.pfimCovOccasionCacheEnabled() )
    return( NULL )

  key = .pfimCovOccasionCacheKey( arm, combination, occasion, occasionParams, model )
  if ( !exists( key, envir = .pfimCovOccasionCache, inherits = FALSE ) )
    return( NULL )

  cached = get( key, envir = .pfimCovOccasionCache )
  # miss if caller needs a slot that was never stored
  if ( wantModel && is.null( cached$evaluation ) )
    return( NULL )
  if ( wantGrad && is.null( cached$gradient ) )
    return( NULL )

  .pfimCacheBump( "covariate.occasion.cache.hits" )
  .pfimCacheLruTouch( .pfimCovOccasionCache, key )
  cached
}

#' Merge model and/or gradient slots into the covariate/occasion cache entry.
#' @noRd
#' @keywords internal
.pfimCovOccasionCacheSet = function( arm, combination, occasion, occasionParams, occ,
                                     wantModel, wantGrad, model = NULL ) {
  if ( !.pfimCovOccasionCacheEnabled() )
    return( invisible( NULL ) )

  key = .pfimCovOccasionCacheKey( arm, combination, occasion, occasionParams, model )
  cached = if ( exists( key, envir = .pfimCovOccasionCache, inherits = FALSE ) )
    get( key, envir = .pfimCovOccasionCache ) else list()

  if ( wantModel ) {
    cached$evaluation = occ$evaluation
    cached$variance = occ$variance
  }
  if ( wantGrad )
    cached$gradient = occ$gradient
  cached$occasion = occasion

  .pfimCacheStore( .pfimCovOccasionCache, key, cached )
  invisible( key )
}

#' Clear covariate/occasion cache entirely, or only keys for one scope.
#' @noRd
#' @keywords internal
.pfimCovOccasionCacheClear = function( scope = NULL ) {
  if ( is.null( scope ) )
    .pfimEnvClear( .pfimCovOccasionCache )
  else
    .pfimCacheRmByPrefix( .pfimCovOccasionCache, paste0( scope, "::" ) )
  invisible( NULL )
}

#' Store a completed Evaluation in the design FIM cache (no-op if caching off).
#' @noRd
#' @keywords internal
.pfimFimCacheRegister = function( evaluation ) {
  if ( !isTRUE( pfim_get_option( "fim.cache", TRUE ) ) )
    return( invisible( NULL ) )
  key = .pfimFimCacheKey( evaluation )
  .pfimCacheStore( .pfimFimDesignCache, key, evaluation )
  invisible( key )
}

#' Run Evaluation with design-FIM caching.
#' @noRd
#' @keywords internal
.pfimRunEvaluationCached = function( evaluation ) {
  .pfimSetCacheEvaluation( evaluation )
  if ( !isTRUE( pfim_get_option( "fim.cache", TRUE ) ) )
    return( run( evaluation ) )

  key = .pfimFimCacheKey( evaluation )
  if ( exists( key, envir = .pfimFimDesignCache, inherits = FALSE ) ) {
    .pfimCacheBump( "fim.cache.hits" )
    .pfimCacheLruTouch( .pfimFimDesignCache, key )
    return( get( key, envir = .pfimFimDesignCache ) )
  }

  result = run( evaluation )
  .pfimCacheStore( .pfimFimDesignCache, key, result )
  result
}

#' FIM cache sizes and hit counts
#' @return A list of cache statistics.
#' @keywords internal
pfim_cache_stats = function() {
  .pfimFimCacheStats()
}

#' Internal list of FIM / gradient / ODE cache sizes and hit counters.
#' @noRd
#' @keywords internal
.pfimFimCacheStats = function() {
  list(
    enabled = isTRUE( pfim_get_option( "fim.cache", TRUE ) ),
    maxEntries = pfim_get_option( "fim.cache.maxEntries", NULL ),
    scope = pfim_get_option( "fim.cache.scope", NA ),
    size = length( .pfimCacheKeys( .pfimFimDesignCache ) ),
    hits = pfim_get_option( "fim.cache.hits", 0L ),
    covOccasionCacheSize = length( .pfimCacheKeys( .pfimCovOccasionCache ) ),
    covOccasionCacheHits = pfim_get_option( "covariate.occasion.cache.hits", 0L ),
    fdCacheSize = length( ls( .pfimFdSchemeCache, all.names = TRUE ) ),
    adminCacheSize = length( ls( .pfimGradAdminCache, all.names = TRUE ) ),
    odeTimesCacheSize = pfimOdeSimTimesCacheSize_Rcpp()
  )
}

#' Wipe all FIM-related caches and gradient perf caches (session reset).
#' @noRd
#' @keywords internal
.pfimClearFimCaches = function() {
  .pfimEnvClear( .pfimFimDesignCache )
  .pfimEnvClear( .pfimCovOccasionCache )
  .pfimEnvClear( .pfimEvalModelCache )
  .pfimClearProjectScopeRegistry()
  pfim_set_option( fim.cache.scope = NULL, fim.cache.evaluation = NULL )
  .pfimClearGradientPerfCaches()
  invisible( NULL )
}
