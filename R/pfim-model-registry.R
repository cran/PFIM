# Model-class registry: detect() tried in registration order.
#
# `pfim_register_model_class()` stores factory + optional detect() handlers.
# Resolution: explicit `modelClass` on the project, else first matching detect(),
# else legacy fallback in model-type-dispatch.R.
#
# Built-in catch-alls (ModelAnalytic / ModelODEDoseNotInEquations) have no
# detect() so custom classes registered after package load can still match.
# Specific builtins stay before custom entries when registered at load time;
# call pfim_register_model_class() early (or before a catch-all) for overrides.

#' @include model-type-dispatch.R
#' @include ModelAnalytic.R
#' @include ModelAnalyticSteadyState.R
#' @include ModelAnalyticInfusion.R
#' @include ModelAnalyticInfusionSteadyState.R
#' @include ModelODE.R
#' @include model-ode-bolus.R
#' @include ModelODEInfusion.R
#' @include ModelODEInfusionDoseInEquation.R
#' @keywords internal
NULL

.pfimModelClassRegistry = new.env( parent = emptyenv() )

#' Ordered list of registered model class names (registration order).
#' @noRd
#' @keywords internal
.pfimModelClassOrder = function() {
  if ( exists( "__order__", envir = .pfimModelClassRegistry, inherits = FALSE ) )
    get( "__order__", envir = .pfimModelClassRegistry )
  else
    character( 0L )
}

#' Append a class name to the registry order (no duplicates).
#' @noRd
#' @keywords internal
.pfimModelClassOrderAppend = function( className ) {
  ord = .pfimModelClassOrder()
  if ( !className %in% ord )
    assign( "__order__", c( ord, className ), envir = .pfimModelClassRegistry )
}

#' Invoke a registered detect() handler (pfimproject or legacy eq/ic).
#'
#' Prefers the project object whenever the signature looks project-oriented
#' (\code{function(project)}, \code{function(project, ...)}, \code{function(...)}).
#' Only plain two-argument \code{function(equations, ic)} stays on the legacy path -
#' so \code{function(project, ...)} is never fed equation lists.
#' @noRd
#' @keywords internal
.pfimCallModelDetect = function( detectFn, pfimproject ) {
  fmls = formals( detectFn )
  nms  = names( fmls )
  if ( is.null( nms ) ) nms = rep( "", length( fmls ) )
  n = length( fmls )

  projectish = c( "pfimproject", "project", "evaluation", "optimization", "x", "object" )
  legacyEq   = c( "equations", "eq", "modelEquations" )

  # Explicit legacy: two+ named args that are not a project (and no ...).
  if ( n >= 2L && !( "..." %in% nms ) &&
       !nms[[ 1L ]] %in% projectish &&
       ( nms[[ 1L ]] %in% legacyEq || nms[[ 1L ]] == "" ) ) {
    eqs = projectProp( pfimproject, "modelEquations" )
    ic  = .initialConditionsFromProject( pfimproject )
    return( isTRUE( detectFn( eqs, ic ) ) )
  }

  # Default / documented API: pass the project.
  isTRUE( detectFn( pfimproject ) )
}

#' Register a model class
#' @param className Character S7 class name (e.g. \code{"ModelAnalytic"}).
#' @param factory Zero-argument function returning a \code{Model} object.
#' @param detect Optional \code{function(pfimproject)} returning logical.
#'   Legacy \code{function(equations, initialConditions)} is still accepted.
#' @return Invisibly, \code{NULL}.
#' @export
pfim_register_model_class = function( className, factory, detect = NULL ) {
  if ( !is.character( className ) || !.pfimIsNonEmptyScalar( className ) )
    .pfimStop( "className must be a non-empty character string." )
  if ( !is.function( factory ) )
    .pfimStop( "factory must be a function." )
  if ( !is.null( detect ) && !is.function( detect ) )
    .pfimStop( "detect must be NULL or a function." )
  .pfimModelClassAssign( className, factory, detect )
}

#' Store a registry entry and append it to the resolution order.
#' @param detectFeatures Optional predicate on \code{.detectModelFeatures()}
#'   output; lets built-ins share one feature scan per resolution.
#' @noRd
#' @keywords internal
.pfimModelClassAssign = function( className, factory, detect, detectFeatures = NULL ) {
  assign(
    className,
    list( factory = factory, detect = detect, detectFeatures = detectFeatures ),
    envir = .pfimModelClassRegistry
  )
  .pfimModelClassOrderAppend( className )
  invisible( NULL )
}

#' Register a built-in class whose detection is a predicate on model features.
#' @noRd
#' @keywords internal
.pfimRegisterBuiltinModelClass = function( className, factory, detectFeatures = NULL ) {
  detect = if ( is.function( detectFeatures ) )
    function( pfimproject ) detectFeatures( .detectModelFeaturesFromProject( pfimproject ) )
  .pfimModelClassAssign( className, factory, detect, detectFeatures )
}

#' Resolve model class name for a project
#' @param pfimproject A \code{PFIMProject}, \code{Evaluation}, or \code{Optimization}.
#' @return Character class name with attribute \code{source} (\code{"registry"} or \code{"legacy"}).
#' @export
pfim_resolve_model_class = function( pfimproject ) {
  info = .pfimResolveModelClassInfo( pfimproject )
  structure( info$class, source = info$source )
}

#' Resolve class name and provenance (explicit / registry / legacy).
#' @noRd
#' @keywords internal
.pfimResolveModelClassInfo = function( pfimproject ) {
  explicit = projectProp( pfimproject, "modelClass" )
  if ( .pfimIsNonEmptyScalar( explicit ) ) {
    if ( !exists( explicit, envir = .pfimModelClassRegistry, inherits = FALSE ) )
      .pfimStop( "Unknown model class: ", explicit )
    return( list( class = explicit, source = "explicit" ) )
  }

  # First matching detect() wins (specific builtins first; catch-alls have none).
  order    = .pfimModelClassOrder()
  features = .detectModelFeaturesFromProject( pfimproject )
  idx      = detect_index( order, function( nm ) {
    entry = get( nm, envir = .pfimModelClassRegistry )
    if ( is.function( entry$detectFeatures ) )
      isTRUE( entry$detectFeatures( features ) )
    else
      is.function( entry$detect ) && .pfimCallModelDetect( entry$detect, pfimproject )
  } )
  if ( idx > 0L )
    return( list( class = order[[ idx ]], source = "registry" ) )

  list( class = .selectModelClass( features ), source = "legacy" )
}

#' Instantiate a registered model class via its factory.
#' @noRd
#' @keywords internal
.pfimInstantiateModelClass = function( className ) {
  if ( !exists( className, envir = .pfimModelClassRegistry, inherits = FALSE ) )
    .pfimStop( "Unknown model class: ", className )
  get( className, envir = .pfimModelClassRegistry )$factory()
}

#' Persist resolved class name onto the project when modelClass was empty.
#' @noRd
#' @keywords internal
.pfimSyncModelClass = function( pfimproject, className ) {
  current = projectProp( pfimproject, "modelClass" )
  if ( !.pfimIsNonEmptyScalar( current ) )
    projectProp( pfimproject, "modelClass" ) = className
  invisible( className )
}

#' Register built-in analytic and ODE model classes (most specific first).
#'
#' Catch-all classes are registered without \code{detect} so legacy selection
#' (and user-registered detectors) can still run.
#' @noRd
#' @keywords internal
.pfimInitBuiltinModelRegistry = function() {
  .pfimRegisterBuiltinModelClass(
    "ModelODEBolus", function() ModelODEBolus(),
    \( f ) f$isODE && f$doseInInitialConditions
  )
  .pfimRegisterBuiltinModelClass(
    "ModelODEInfusionDoseInEquation", function() ModelODEInfusionDoseInEquation(),
    \( f ) f$isODE && f$hasInfusion && f$doseInEquation
  )
  .pfimRegisterBuiltinModelClass(
    "ModelODEDoseInEquations", function() ModelODEDoseInEquations(),
    \( f ) f$isODE && f$doseInEquation
  )
  # Catch-all ODE: no detect - legacy .selectModelClass picks it when needed.
  .pfimRegisterBuiltinModelClass( "ModelODEDoseNotInEquations", function() ModelODEDoseNotInEquations() )
  .pfimRegisterBuiltinModelClass(
    "ModelAnalyticInfusionSteadyState", function() ModelAnalyticInfusionSteadyState(),
    \( f ) !f$isODE && f$hasInfusion && f$doseInEquation && f$hasTau
  )
  .pfimRegisterBuiltinModelClass(
    "ModelAnalyticInfusion", function() ModelAnalyticInfusion(),
    \( f ) !f$isODE && f$hasInfusion && f$doseInEquation
  )
  .pfimRegisterBuiltinModelClass(
    "ModelAnalyticSteadyState", function() ModelAnalyticSteadyState(),
    \( f ) !f$isODE && f$hasTau
  )
  # Catch-all analytic: no detect - leaves room for custom detect() handlers.
  .pfimRegisterBuiltinModelClass( "ModelAnalytic", function() ModelAnalytic() )
}

.pfimInitBuiltinModelRegistry()
