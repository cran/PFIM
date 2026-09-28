#' FIM type and optimizer registries.
#'
#' Package-private environments map string names to factory functions:
#' \itemize{
#'   \item \code{.pfimFimTypeRegistry} - \code{"population"}, \code{"individual"},
#'     \code{"bayesian"} (and any type registered with \code{pfim_register_fim_type})
#'   \item \code{.pfimFimDuplicatorRegistry} - how to clone a FIM while clearing
#'     computed slots (SE, matrices, ...) for design-level copies
#'   \item \code{.pfimOptimizerRegistry} - S7 optimizer constructors used by
#'     \code{\link{Optimization}} / \code{run()}
#' }
#' Built-ins are registered at load time via \code{.pfimInitBuiltinRegistries()}.
#' @name pfim-registry
#' @include PopulationFim.R
#' @include IndividualFim.R
#' @include BayesianFim.R
#' @include MultiplicativeAlgorithm.R
#' @include FedorovWynnAlgorithm.R
#' @include PSOAlgorithm.R
#' @include PGBOAlgorithm.R
#' @include SimplexAlgorithm.R
#' @keywords internal
NULL

.pfimFimTypeRegistry = new.env( parent = emptyenv() )
.pfimFimDuplicatorRegistry = new.env( parent = emptyenv() )
.pfimOptimizerRegistry = new.env( parent = emptyenv() )

#' Copy a FIM and clear all computed numerical slots.
#'
#' Used as the default \code{duplicator} when registering a FIM type so that
#' design-level copies start empty (no leftover Fisher matrix / SE / shrinkage).
#' @param fim A \code{Fim} S7 object.
#' @return A copy with numeric result slots reset.
#' @noRd
#' @keywords internal
.fimDuplicateReset = function( fim ) {
  set_props(
    fim,
    fisherMatrix              = numeric( 0 ),
    fixedEffects              = numeric( 0 ),
    varianceEffects           = numeric( 0 ),
    SEAndRSE                  = list(),
    condNumberFixedEffects    = 0.0,
    condNumberVarianceEffects = 0.0,
    shrinkage                 = numeric( 0 )
  )
}

#' Register a custom FIM type
#'
#' Extends PFIM with a new \code{fimType} string for \code{\link{Evaluation}} /
#' \code{\link{Optimization}}. The factory is called whenever a project needs a
#' fresh FIM of that type. Overwrites silently if \code{name} already exists.
#'
#' @param name Character label (e.g. \code{"population"}).
#' @param factory Zero-argument function returning a \code{Fim} object.
#' @param duplicator Optional \code{function(fim)} for design-level FIM copies;
#'   defaults to \code{.fimDuplicateReset}.
#' @return Invisibly, \code{NULL}.
#' @export
pfim_register_fim_type = function( name, factory, duplicator = .fimDuplicateReset ) {
  if ( !is.character( name ) || !.pfimIsNonEmptyScalar( name ) )
    .pfimStop( "name must be a non-empty character string." )
  if ( !is.function( factory ) )
    .pfimStop( "factory must be a function." )
  if ( !is.function( duplicator ) )
    .pfimStop( "duplicator must be a function." )
  key = tolower( name )
  assign( key, factory, envir = .pfimFimTypeRegistry )
  sample = factory()
  cls    = S7::prop( S7::S7_class( sample ), "name" )
  assign( cls, duplicator, envir = .pfimFimDuplicatorRegistry )
  invisible( NULL )
}

#' Register a custom optimizer
#'
#' Makes \code{optimizer = name} valid on \code{\link{Optimization}}. The factory
#' must return an S7 optimizer object with an \code{optimizeDesign} method.
#' Overwrites silently if \code{name} already exists.
#'
#' @param name Character optimizer name (e.g. \code{"PSOAlgorithm"}).
#' @param factory Zero-argument function returning an optimizer object.
#' @return Invisibly, \code{NULL}.
#' @export
pfim_register_optimizer = function( name, factory ) {
  if ( !is.character( name ) || !.pfimIsNonEmptyScalar( name ) )
    .pfimStop( "name must be a non-empty character string." )
  if ( !is.function( factory ) )
    .pfimStop( "factory must be a function." )
  assign( name, factory, envir = .pfimOptimizerRegistry )
  invisible( NULL )
}

#' Instantiate a FIM from a registered \code{fimType} string.
#' @param fimType Character; compared case-insensitively to registry keys.
#' @return A new \code{Fim} S7 object.
#' @noRd
#' @keywords internal
.pfimInstantiateFimType = function( fimType ) {
  key = tolower( fimType )
  if ( !exists( key, envir = .pfimFimTypeRegistry, inherits = FALSE ) )
    .pfimStop(
      sprintf(
        "Invalid fimType '%s' (population, individual, Bayesian, or pfim_register_fim_type).",
        fimType
      )
    )
  get( key, envir = .pfimFimTypeRegistry )()
}

#' Instantiate an optimizer from a registered name.
#' @param name Exact registry key (e.g. \code{"MultiplicativeAlgorithm"}).
#' @return A new optimizer S7 object.
#' @noRd
#' @keywords internal
.pfimInstantiateOptimizer = function( name ) {
  if ( !exists( name, envir = .pfimOptimizerRegistry, inherits = FALSE ) )
    .pfimStop(
      sprintf(
        "Unknown optimizer '%s' (pfim_register_optimizer).",
        name
      )
    )
  get( name, envir = .pfimOptimizerRegistry )()
}

#' Required / optional \code{optimizerParameters} per built-in algorithm.
#'
#' Used by \code{.pfimCheckOptimizerParameters} at \code{Optimization()} construction.
#' Custom optimizers registered via \code{pfim_register_optimizer} are not listed
#' here and skip this validation.
#' @return Named list of \code{list(required=, optional=)} specs.
#' @noRd
#' @keywords internal
.pfimOptimizerParamSpecs = function() {
  list(
    MultiplicativeAlgorithm = list(
      required = c( "lambda", "delta", "numberOfIterations", "weightThreshold" ),
      optional = "showProcess"
    ),
    FedorovWynnAlgorithm = list(
      required = c( "elementaryProtocols", "numberOfSubjects", "proportionsOfSubjects" ),
      optional = "showProcess"
    ),
    PSOAlgorithm = list(
      required = c(
        "maxIteration", "populationSize", "seed",
        "personalLearningCoefficient", "globalLearningCoefficient"
      ),
      optional = c( "showProcess", "tolerance", "stallIterations" )
    ),
    PGBOAlgorithm = list(
      required = c( "maxIteration", "N", "muteEffect", "purgeIteration", "seed" ),
      optional = c( "showProcess", "fitBase", "cauchyProb", "tolerance", "stallIterations" )
    ),
    SimplexAlgorithm = list(
      required = c( "pctInitialSimplexBuilding", "tolerance", "maxIteration" ),
      optional = "showProcess"
    )
  )
}

#' Validate \code{optimizerParameters} against the built-in spec for \code{optimizer}.
#'
#' Checks unknown names (with fuzzy \code{agrep} suggestions) and missing required
#' keys. Returns \code{params} unchanged for empty optimizer or custom names.
#' @param optimizer Character optimizer class name.
#' @param params Named list (may be \code{NULL}).
#' @return The validated \code{params} list (invisibly).
#' @noRd
#' @keywords internal
.pfimCheckOptimizerParameters = function( optimizer, params ) {
  if ( !length( optimizer ) || !.pfimIsNonEmptyScalar( optimizer[[ 1L ]] ) )
    return( invisible( params ) )
  spec = .pfimOptimizerParamSpecs()[[ optimizer ]]
  if ( is.null( spec ) )
    return( invisible( params ) )

  params  = params %||% list()
  allowed = c( spec$required, spec$optional )
  unknown = setdiff( names( params ), allowed )
  if ( length( unknown ) ) {
    parts = map_chr( unknown, function( nm ) {
      hit = agrep( nm, allowed, ignore.case = TRUE, max.distance = 0.2, value = TRUE )
      paste0( "'", nm, "'", if ( length( hit ) ) paste0( " (did you mean '", hit[ 1L ], "'?)" ) else "" )
    } )
    .pfimStop(
      "Unknown optimizerParameters for ", optimizer, ": ",
      paste( parts, collapse = ", " ),
      ". Use Optimization(..., optimizerParameters = list(...))."
    )
  }

  missing = setdiff( spec$required, names( params ) )
  if ( length( missing ) ) {
    .pfimStop(
      "Missing optimizerParameters for ", optimizer, ": ",
      paste( missing, collapse = ", " ),
      ". See ?Optimization."
    )
  }

  .pfimValidateOptimizerParamValues( optimizer, params )
  invisible( params )
}

#' Value-range checks for built-in optimizer parameters (after name checks).
#' @noRd
#' @keywords internal
.pfimValidateOptimizerParamValues = function( optimizer, params ) {
  posInt = function( x, label ) {
    if ( !is.numeric( x ) || length( x ) != 1L || !is.finite( x ) || x < 1 || x != as.integer( x ) )
      .pfimStop( optimizer, ": ", label, " must be a positive integer." )
  }
  posNum = function( x, label ) {
    if ( !is.numeric( x ) || length( x ) != 1L || !is.finite( x ) || x <= 0 )
      .pfimStop( optimizer, ": ", label, " must be a finite number > 0." )
  }
  nonneg = function( x, label ) {
    if ( !is.numeric( x ) || length( x ) != 1L || !is.finite( x ) || x < 0 )
      .pfimStop( optimizer, ": ", label, " must be a finite number >= 0." )
  }

  if ( !is.null( params$maxIteration ) )
    posInt( params$maxIteration, "maxIteration" )
  if ( !is.null( params$N ) )
    posInt( params$N, "N" )
  if ( !is.null( params$populationSize ) )
    posInt( params$populationSize, "populationSize" )
  if ( !is.null( params$purgeIteration ) )
    posInt( params$purgeIteration, "purgeIteration" )
  if ( !is.null( params$numberOfIterations ) )
    posInt( params$numberOfIterations, "numberOfIterations" )
  if ( !is.null( params$stallIterations ) )
    posInt( params$stallIterations, "stallIterations" )
  if ( !is.null( params$numberOfSubjects ) )
    posInt( params$numberOfSubjects, "numberOfSubjects" )
  if ( !is.null( params$muteEffect ) )
    posNum( params$muteEffect, "muteEffect" )
  if ( !is.null( params$lambda ) )
    posNum( params$lambda, "lambda" )
  if ( !is.null( params$fitBase ) )
    posNum( params$fitBase, "fitBase" )
  if ( !is.null( params$delta ) )
    nonneg( params$delta, "delta" )
  if ( !is.null( params$tolerance ) )
    nonneg( params$tolerance, "tolerance" )
  if ( !is.null( params$seed ) ) {
    if ( !is.numeric( params$seed ) || length( params$seed ) != 1L || !is.finite( params$seed ) )
      .pfimStop( optimizer, ": seed must be a finite numeric scalar." )
  }
  if ( !is.null( params$showProcess ) ) {
    if ( !is.logical( params$showProcess ) || length( params$showProcess ) != 1L )
      .pfimStop( optimizer, ": showProcess must be a single logical." )
  }
  if ( !is.null( params$weightThreshold ) ) {
    wt = params$weightThreshold
    if ( !is.numeric( wt ) || length( wt ) != 1L || !is.finite( wt ) || wt < 0 || wt >= 1 )
      .pfimStop( optimizer, ": weightThreshold must be in [0, 1)." )
  }
  if ( !is.null( params$pctInitialSimplexBuilding ) ) {
    pct = params$pctInitialSimplexBuilding
    if ( !is.numeric( pct ) || length( pct ) != 1L || !is.finite( pct ) ||
         pct <= 0 || pct >= 100 )
      .pfimStop( optimizer, ": pctInitialSimplexBuilding must be in (0, 100)." )
  }
  if ( !is.null( params$cauchyProb ) ) {
    cp = params$cauchyProb
    if ( !is.numeric( cp ) || length( cp ) != 1L || !is.finite( cp ) || cp <= 0 || cp >= 1 )
      .pfimStop( optimizer, ": cauchyProb must be in (0, 1). Default: 0.8" )
  }
  if ( !is.null( params$proportionsOfSubjects ) ) {
    pr = params$proportionsOfSubjects
    if ( !is.numeric( pr ) || !length( pr ) || any( !is.finite( pr ) ) || any( pr < 0 ) )
      .pfimStop( optimizer, ": proportionsOfSubjects must be finite and non-negative." )
    .validateUnitProportions( pr, "proportionsOfSubjects" )
  }
  if ( !is.null( params$elementaryProtocols ) ) {
    ep = params$elementaryProtocols
    if ( !is.list( ep ) || !length( ep ) )
      .pfimStop( optimizer, ": elementaryProtocols must be a non-empty list." )
  }
  phi1 = params$personalLearningCoefficient
  phi2 = params$globalLearningCoefficient
  if ( !is.null( phi1 ) )
    posNum( phi1, "personalLearningCoefficient" )
  if ( !is.null( phi2 ) )
    posNum( phi2, "globalLearningCoefficient" )
  if ( !is.null( phi1 ) && !is.null( phi2 ) ) {
    phi = phi1 + phi2
    if ( !is.finite( phi ) || phi <= 4 )
      .pfimStop(
        optimizer, ": personalLearningCoefficient + globalLearningCoefficient must be > 4 ",
        "(Clerc & Kennedy 2002). Current phi = ",
        if ( is.finite( phi ) ) sprintf( "%.4f", phi ) else "NA",
        "; typical default is 4.1."
      )
  }
  invisible( NULL )
}

#' Discrete vs continuous optimizer constraint checks (before search).
#'
#' Discrete (Fedorov-Wynn / Multiplicative) enumerate \code{initialSamplings}.
#' Continuous (PSO / PGBO / Simplex) need \code{samplingsWindows}. Missing
#' \code{AdministrationConstraints} is allowed for discrete: the current arm
#' dose is used as a singleton grid.
#' @noRd
#' @keywords internal
.pfimDiscreteOptimizerNames = function() {
  c( "MultiplicativeAlgorithm", "FedorovWynnAlgorithm" )
}

.pfimContinuousOptimizerNames = function() {
  c( "PSOAlgorithm", "PGBOAlgorithm", "SimplexAlgorithm" )
}

.pfimValidateOptimizerConstraints = function( optimization ) {
  optimizer = projectProp( optimization, "optimizer" )
  if ( !.pfimIsNonEmptyScalar( optimizer ) ) return( invisible( NULL ) )
  designs = projectProp( optimization, "designs" )
  if ( !length( designs ) ) return( invisible( NULL ) )
  discrete = optimizer %in% .pfimDiscreteOptimizerNames()
  continuous = optimizer %in% .pfimContinuousOptimizerNames()
  if ( !discrete && !continuous ) return( invisible( NULL ) )

  walk( designs, function( design ) {
    walk( prop( design, "arms" ), function( arm ) {
      armName = prop( arm, "name" )
      scs = prop( arm, "samplingTimesConstraints" )
      if ( discrete ) {
        if ( !length( scs ) )
          .pfimStop(
            optimizer, ": arm '", armName,
            "' needs SamplingTimeConstraints(initialSamplings=...) ",
            "to enumerate the discrete sampling grid."
          )
        walk( scs, function( sc ) {
          if ( !length( prop( sc, "initialSamplings" ) ) )
            .pfimStop(
              optimizer, ": SamplingTimeConstraints for '",
              prop( sc, "outcome" ),
              "' needs initialSamplings (discrete candidate times)."
            )
        } )
      }
      if ( continuous && length( scs ) ) {
        walk( scs, function( sc ) {
          if ( !length( prop( sc, "samplingsWindows" ) ) )
            .pfimStop(
              optimizer, ": SamplingTimeConstraints for '",
              prop( sc, "outcome" ),
              "' needs samplingsWindows. For a discrete sampling grid use ",
              "FedorovWynnAlgorithm or MultiplicativeAlgorithm."
            )
        } )
      }
    } )
  } )
  invisible( NULL )
}

#' Register built-in FIM types and optimizers (called once at package load).
#' @noRd
#' @keywords internal
.pfimInitBuiltinRegistries = function() {
  pfim_register_fim_type( "population", function() PopulationFim() )
  pfim_register_fim_type( "individual", function() IndividualFim() )
  pfim_register_fim_type( "bayesian", function() BayesianFim() )
  pfim_register_optimizer( "MultiplicativeAlgorithm", function() MultiplicativeAlgorithm() )
  pfim_register_optimizer( "FedorovWynnAlgorithm", function() FedorovWynnAlgorithm() )
  pfim_register_optimizer( "PSOAlgorithm", function() PSOAlgorithm() )
  pfim_register_optimizer( "PGBOAlgorithm", function() PGBOAlgorithm() )
  pfim_register_optimizer( "SimplexAlgorithm", function() SimplexAlgorithm() )
}

.pfimInitBuiltinRegistries()
