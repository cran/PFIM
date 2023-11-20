#' Class "Model"
#'
#' @description The class \code{Model} defines information concerning the construction of a model.
#'
#' @name Model-class
#' @aliases Model
#' @docType class
#' @include GenericMethods.R
#' @include LibraryOfPKPDModels.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Model} can be created by calls of the form \code{Model(...)} where
#' (...) are the parameters for the \code{Model} objects.
#'
#' @section Slots for \code{Administration} objects:
#'  \describe{
#'    \item{\code{name}:}{A string giving the name of the model.}
#'    \item{\code{description}:}{A list of string giving the description of the model.}
#'    \item{\code{equations}:}{A list giving the equations of the model.}
#'    \item{\code{outcomes}:}{A list giving the outcomes of the model.}
#'    \item{\code{outcomesForEvaluation}:}{A list giving the outcomes used for the evaluation of the model.}
#'    \item{\code{parameters}:}{A list giving the parameters of the model.}
#'    \item{\code{modelError}:}{A list giving the model error of the model.}
#'    \item{\code{initialConditions}:}{A list giving the initial conditions of the model.}
#'    \item{\code{odeSolverParameters}:}{A list giving the parameters for the solver of the model.}
#'    \item{\code{modelFromLibrary}:}{A list giving the model equations when the model is constructed from the library of model.}
#'  }

Model = setClass("Model",
                 representation = representation(
                   name = "character",
                   description = "list",
                   equations = "list",
                   outcomes = "list",
                   outcomesForEvaluation = "list",
                   parameters ="list",
                   modelError = "list",
                   initialConditions = "list",
                   odeSolverParameters = "list",
                   modelFromLibrary = "list"))

setMethod( f="initialize",
           signature="Model",
           definition= function (.Object, name, description, equations, outcomes, outcomesForEvaluation, parameters,
                                 modelError, initialConditions, odeSolverParameters,modelFromLibrary )
           {
             if(!missing(name))
             {
               .Object@name = name
             }
             if(!missing( description ) )
             {
               .Object@description = description
             }
             if(!missing( equations ) )
             {
               .Object@equations = equations
             }
             if(!missing( outcomes ) )
             {
               .Object@outcomes = outcomes
             }
             if(!missing( outcomesForEvaluation ) )
             {
               .Object@outcomesForEvaluation = outcomesForEvaluation
             }
             if(!missing( parameters ) )
             {
               .Object@parameters = parameters
             }
             if(!missing( initialConditions ) )
             {
               .Object@initialConditions = initialConditions
             }
             if(!missing( modelError ) )
             {
               .Object@modelError = modelError
             }
             if(!missing( odeSolverParameters ) )
             {
               .Object@odeSolverParameters = odeSolverParameters
             }
             if(!missing( modelFromLibrary ) )
             {
               .Object@modelFromLibrary = modelFromLibrary
             }

             validObject(.Object)
             return (.Object )
           }
)

# ======================================================================================================
# getName
# ======================================================================================================

#' @rdname getName
#' @export

setMethod("getName",
          "Model",
          function(object) {
            return( object@name )
          })

# ======================================================================================================
# setName
# ======================================================================================================

#' @rdname setName
#' @export

setMethod("setName",
          "Model",
          function(object, name) {
            object@name = name
            return( object )
          })

#' Get the description of a model.
#'
#' @name getDescription
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the description of a model.
#' @export

setGeneric(
  "getDescription",
  function(object) {
    standardGeneric("getDescription")
  })

#' @rdname getDescription
#' @export

setMethod("getDescription",
          "Model",
          function(object) {
            return( object@description )
          })

#' Set the description of a model.
#'
#' @name setDescription
#' @param object An object from the class \linkS4class{Model}.
#' @param description A list giving the description of a model.
#' @return The model with the updated description.
#' @export

setGeneric(
  "setDescription",
  function(object, description) {
    standardGeneric("setDescription")
  })

#' @rdname setDescription
#' @export

setMethod("setDescription",
          "Model",
          function(object, description) {
            object@description = description
            return( object )
          })

#' Get the equations of a model.
#'
#' @name getEquations
#' @param object An object from the class \linkS4class{Model}.
#' @return The list giving the equations of the model.
#' @export

setGeneric(
  "getEquations",
  function(object) {
    standardGeneric("getEquations")
  })

#' @rdname getEquations
#' @export

setMethod("getEquations",
          "Model",
          function(object) {
            return( object@equations )
          })

#' Set the equations of a model.
#'
#' @name setEquations
#' @param object An object from the class \linkS4class{Model}.
#' @param equations A list giving the equations of the model.
#' @return The model with the updated equations.
#' @export

setGeneric(
  "setEquations",
  function(object,equations) {
    standardGeneric("setEquations")
  })

#' @rdname setEquations
#' @export

setMethod("setEquations",
          "Model",
          function(object,equations) {
            if ( !is.null( equations ) )
            {
              object@equations = equations
            }else{
              object@equations = list()
            }

            return( object )
          })

#' Set  a model from the library of model
#'
#' @name setModelFromLibrary
#' @param object An object from the class \linkS4class{Model}.
#' @param modelFromLibrary An object from the class \linkS4class{Model}.
#' @return The model with the updated model from library of models.
#' @export

setGeneric(
  "setModelFromLibrary",
  function( object, modelFromLibrary ) {
    standardGeneric("setModelFromLibrary")
  })

#' @rdname setModelFromLibrary
#' @export

setMethod("setModelFromLibrary",
          "Model",
          function( object, modelFromLibrary ) {
            if ( !is.null( modelFromLibrary ) )
            {
              object@modelFromLibrary = modelFromLibrary
            }else{
              object@modelFromLibrary = list()
            }

            return( object )
          })


#' Get the outcomes of a model.
#'
#' @name getOutcomes
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the outcomes of the model.
#' @export

setGeneric(
  "getOutcomes",
  function(object) {
    standardGeneric("getOutcomes")
  })

#' @rdname getOutcomes
#' @export

setMethod("getOutcomes",
          "Model",
          function(object) {
            return( object@outcomes )
          })

#' Set the outcomes of a model.
#'
#' @name setOutcomes
#' @param object An object from the class \linkS4class{Model}.
#' @param outcomes A list giving the outcomes of the model.
#' @return The model with the updated outcomes.
#' @export

setGeneric(
  "setOutcomes",
  function(object,outcomes) {
    standardGeneric("setOutcomes")
  })

#' @rdname setOutcomes
#' @export

setMethod("setOutcomes",
          "Model",
          function(object,outcomes) {
            object@outcomes = outcomes

            outcomesNames = names( outcomes )

            if ( !is.null( outcomesNames ) )
            {
              names(object@outcomes) = names(outcomes)

            }else if ( is.null( outcomesNames ) )
            {
              names(object@outcomes) = outcomes
            }

            return( object )
          })

#' Get the outcomes of a model used for the evaluation (is scales outcomes).
#'
#' @name getOutcomesForEvaluation
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the outcomes of a model used for the evaluation (is scales outcomes).
#' @export

setGeneric(
  "getOutcomesForEvaluation",
  function(object) {
    standardGeneric("getOutcomesForEvaluation")
  })

#' @rdname getOutcomesForEvaluation
#' @export

setMethod("getOutcomesForEvaluation",
          "Model",
          function(object) {
            return( object@outcomesForEvaluation )
          })

#' Set the outcomes of a model used for the evaluation (is scales outcomes).
#'
#' @name setOutcomesForEvaluation
#' @param object An object from the class \linkS4class{Model}.
#' @param outcomes A list giving the outcomes of a model used for the evaluation (is scales outcomes).
#' @return The model with the updated outcomes for the evaluation.
#' @export

setGeneric(
  "setOutcomesForEvaluation",
  function(object,outcomes) {
    standardGeneric("setOutcomesForEvaluation")
  })

#' @rdname setOutcomesForEvaluation
#' @export

setMethod("setOutcomesForEvaluation",
          "Model",
          function(object,outcomes) {
            object@outcomesForEvaluation = outcomes
            return( object )
          })

# ======================================================================================================
# getParameters
# ======================================================================================================

#' @rdname getParameters
#' @export

setMethod("getParameters",
          "Model",
          function(object) {
            return( object@parameters )
          })

# ======================================================================================================
# setParameters
# ======================================================================================================

#' @rdname setParameters
#' @export

setMethod("setParameters",
          "Model",
          function(object,parameters) {
            object@parameters = parameters
            return( object )
          })

# ======================================================================================================
# getModelError
# ======================================================================================================

#' @rdname getModelError
#' @export

setMethod("getModelError",
          "Model",
          function(object) {
            return( object@modelError )
          })

#' Set the model error.
#'
#' @name setModelError
#' @param object An object from the class \linkS4class{Model}.
#' @param modelError  An object from the class \linkS4class{ModelError}.
#' @return The model with the updated model error.
#' @export

setGeneric(
  "setModelError",
  function(object, modelError) {
    standardGeneric("setModelError")
  })

#' @rdname setModelError
#' @export

setMethod("setModelError",
          "Model",
          function(object, modelError ) {
            object@modelError = modelError
            return( object )
          })

# ======================================================================================================
# getInitialConditions
# ======================================================================================================

#' @rdname getInitialConditions
#' @export

setMethod("getInitialConditions",
          "Model",
          function(object) {
            return( object@initialConditions )
          })


#' @rdname setInitialConditions
#' @export

setMethod("setInitialConditions",
          "Model",
          function(object, initialConditions) {
            object@initialConditions = initialConditions
            return( object )
          })

# ======================================================================================================
# getOdeSolverParameters
# ======================================================================================================

#' @rdname getOdeSolverParameters
#' @export

setMethod("getOdeSolverParameters",
          "Model",
          function(object) {
            return( object@odeSolverParameters )
          })

#' Set the parameters of the ode solver.
#'
#' @name setOdeSolverParameters
#' @param object An object from the class \linkS4class{Model}.
#' @param odeSolverParameters A list giving the parameters of the ode solver.
#' @return The model with the updated parameters of the ode solver.
#' @export

setGeneric("setOdeSolverParameters",
           function(object,odeSolverParameters)
           {
             standardGeneric("setOdeSolverParameters")
           })

#' @rdname setOdeSolverParameters
#' @export

setMethod("setOdeSolverParameters",
          "Model",
          function(object,odeSolverParameters) {
            object@odeSolverParameters = odeSolverParameters
            return( object )
          })

#' Get a model from the library of models.
#'
#' @name getModelFromLibrary
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a model from the the library of models.
#' @export

setGeneric(
  "getModelFromLibrary",
  function(object) {
    standardGeneric("getModelFromLibrary")
  })

#' @rdname getModelFromLibrary
#' @export

setMethod("getModelFromLibrary",
          "Model",
          function(object) {
            return( object@modelFromLibrary )
          })

#' Convert an analytic model to a ode model.
#'
#' @name convertPKModelAnalyticToPKModelODE
#' @param object An object from the class \linkS4class{Model}.
#' @return A ode model.
#' @export

setGeneric("convertPKModelAnalyticToPKModelODE",
           function( object )
           {
             standardGeneric("convertPKModelAnalyticToPKModelODE")
           })

#' Get the number of parameters.
#'
#' @name getNumberOfParameters
#' @param object An object from the class \linkS4class{Model}.
#' @return A numeric giving the number of parameters of the model.
#' @export

setGeneric("getNumberOfParameters",
           function(object)
           {
             standardGeneric("getNumberOfParameters")
           })

#' @rdname getNumberOfParameters
#' @export

setMethod("getNumberOfParameters",
          signature("Model"),
          function(object)
          {
            return(length(object@parameters))
          })

#' Test if a mode is ode.
#'
#' @name isModelODE
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a Boolean giving if the mode is ode or not.
#' @export

setGeneric("isModelODE",
           function(object)
           {
             standardGeneric("isModelODE")
           })

#' @rdname isModelODE
#' @export

setMethod(f="isModelODE",
          signature="Model",
          definition = function(object)
          {
            equations = getEquations( object )
            isModelODE = any( grepl( "Deriv", c( names( sapply( equations, tail, 1 ) ) ) ) )
            return(isModelODE)
          })

#' Test if a mode is analytic.
#'
#' @name isModelAnalytic
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a Boolean giving if the mode is analytic or not.
#' @export

setGeneric("isModelAnalytic",
           function(object)
           {
             standardGeneric("isModelAnalytic")
           })

#' @rdname isModelAnalytic
#' @export

setMethod(f="isModelAnalytic",
          signature="Model",
          definition = function(object)
          {
            equations = getEquations( object )
            isModelAnalytic = !any( grepl( "Deriv", c( names( sapply( equations, tail, 1 ) ) ) ) )
            return(isModelAnalytic)
          })

#' Test if the dose is in the equations of the model.
#'
#' @name isDoseInEquations
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a Boolean giving if the dose is in the equations of the model.
#' @export

setGeneric("isDoseInEquations",
           function(object)
           {
             standardGeneric("isDoseInEquations")
           })

#' @rdname isDoseInEquations
#' @export

setMethod(f="isDoseInEquations",
          signature="Model",
          definition = function(object)
          {
            equations = getEquations( object )
            isDoseInEquations = any( grepl( "dose", equations ) )
            return(isDoseInEquations)
          })

#' Test if a mode is infusion
#'
#' @name isModelInfusion
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a Boolean giving if the mode is infusion or not.
#' @export

setGeneric("isModelInfusion",
           function(object)
           {
             standardGeneric("isModelInfusion")
           })

#' @rdname isModelInfusion
#' @export

setMethod(f="isModelInfusion",
          signature="Model",
          definition = function(object)
          {
            equations = getEquations( object )
            isInfusion = any( grepl( "Tinf", equations ) )
            return(isInfusion)
          })

#' Test if a mode is steady state.
#'
#' @name isModelSteadyState
#' @param object An object from the class \linkS4class{Model}.
#' @return Return a Boolean giving if the mode is steady state or not.
#' @export

setGeneric("isModelSteadyState",
           function(object)
           {
             standardGeneric("isModelSteadyState")
           })

#' @rdname isModelSteadyState
#' @export

setMethod(f="isModelSteadyState",
          signature="Model",
          definition = function(object)
          {
            equations = getEquations( object )
            isSteadyState = any( grepl( "tau", equations ) )
            return(isSteadyState)
          })

#' Test if a mode is bolus.
#'
#' @name isModelBolus
#' @param object An object from the class \linkS4class{Model}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return Return a Boolean giving if the mode is bolus or not.
#' @export

setGeneric("isModelBolus",
           function( object, designs )
           {
             standardGeneric("isModelBolus")
           })

#' @rdname isModelBolus
#' @export

setMethod(f="isModelBolus",
          signature = "Model",
          definition = function( object, designs )
          {
            arms = lapply( designs, function(x) getArms(x) )
            initialConditions = unlist( lapply( unlist( arms ), function(x) getInitialConditions(x) ) )
            isDoseInInitialConditions = any( grepl( "dose", initialConditions ) )
            isModelBolus = !isDoseInEquations( object ) & isDoseInInitialConditions

            return(isModelBolus)
          })

#' Define a PKPD model.
#'
#' @name definePKPDModel
#' @param PKModel An object from the class \linkS4class{Model}.
#' @param PDModel An object from the class \linkS4class{Model}.
#' @param outcomes A list giving the outcomes of the  PKPD model.
#' @return A model giving a PKPD model.
#' @export

setGeneric("definePKPDModel",
           function( PKModel, PDModel, outcomes )
           {
             standardGeneric("definePKPDModel")
           })

#' Define a PK model.
#'
#' @name definePKModel
#' @param object An object from the class \linkS4class{Model}.
#' @param outcomes A list giving the outcomes of the  PK model.
#' @return A model giving a PK model.
#' @export

setGeneric("definePKModel",
           function( object, outcomes )
           {
             standardGeneric("definePKModel")
           })

#' Define a model.
#'
#' @name defineModel
#' @param object An object from the class \linkS4class{Model}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return A model defined either from the library of models or user defined.
#' @export

setGeneric(
  "defineModel",
  function(object, designs) {
    standardGeneric("defineModel")
  })

#' @rdname defineModel
#' @export

setMethod("defineModel",
          "Model",
          function(object, designs) {

            equations = getEquations( object )
            modelFromLibrary = getModelFromLibrary( object )

            if( length( equations ) == 0 )
            {
              # ==============================================
              # define model from library of models
              # ==============================================

              modelFromLibrary = getModelFromLibrary( object )

              object = defineModelFromLibraryOfModels( object, designs )

            } else if ( length( modelFromLibrary ) == 0 )
            {
              # ==============================================
              # define model from user defined models
              # ==============================================

              object = defineModelUserDefined( object, designs )
            }

            return( object )
          })

#' Define a model from the library of models.
#'
#' @name defineModelFromLibraryOfModels
#' @param object An object from the class \linkS4class{Model}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return A model defined from the library of models.
#' @export

setGeneric(
  "defineModelFromLibraryOfModels",
  function( object, designs ) {

    standardGeneric("defineModelFromLibraryOfModels")
  })

#' @rdname defineModelFromLibraryOfModels
#' @export

setMethod(f = "defineModelFromLibraryOfModels",
          signature="Model",
          definition= function( object, designs )
          {
            # ==============================================
            # model parameters, error model and outcomes
            # ==============================================

            modelParameters = getParameters( object )
            parametersNames = unlist( lapply( modelParameters, function(x) getName( x ) ) )
            modelParameters = modelParameters[ order( parametersNames ) ]

            modelError = getModelError( object )
            modelOutcomes = getOutcomes( object )

            # ==============================================
            # get model from LibraryOfPKPDModels
            # ==============================================

            modelNamesFromLibrary = getModelFromLibrary( object )

            # ==============================================
            # define PK or PKPD models
            # ==============================================

            numberOfModels = length( modelNamesFromLibrary )

            if ( numberOfModels == 2 )
            {
              PKModelName = modelNamesFromLibrary$PKModel
              PDModelName = modelNamesFromLibrary$PDModel

              PKModel = getPKModel( LibraryOfPKPDModels(), PKModelName )
              PDModel = getPDModel( LibraryOfPKPDModels(), PDModelName )

              model = definePKPDModel( PKModel, PDModel, modelOutcomes )

            }else if( numberOfModels == 1 )
            {
              PKModelName = modelNamesFromLibrary$PKModel
              PKModel = getPKModel( LibraryOfPKPDModels(), PKModelName )

              model = definePKModel( PKModel, modelOutcomes )
            }

            # ==============================================
            # set model components
            # ==============================================

            model = setParameters( model, modelParameters )
            model = setModelError( model, modelError )

            return( model )
          })

#' Define a user defined model.
#'
#' @name defineModelUserDefined
#' @param object An object from the class \linkS4class{Model}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return A model giving a user defined model.
#' @export

setGeneric(
  "defineModelUserDefined",
  function(object, designs) {
    standardGeneric("defineModelUserDefined")
  })

#' @rdname defineModelUserDefined
#' @export

setMethod(f = "defineModelUserDefined",
          signature="Model",
          definition= function( object, designs )
          {
            # ==============================================
            # define type of the model
            # ==============================================

            newModel = defineModelType( object, designs )

            # ==============================================
            # set model name
            # ==============================================

            newModel = setName( newModel, getName( object ) )

            # ==============================================
            # set description
            # ==============================================

            newModel = setDescription( newModel, getDescription( object ) )

            # ==============================================
            # set model error
            # ==============================================

            newModel = setModelError( newModel, getModelError( object ) )

            # ==============================================
            # set equations
            # ==============================================

            newModel = setEquations( newModel, getEquations( object ) )

            # ==============================================
            # set outcomes
            # ==============================================

            newModel = setOutcomes( newModel, as.list( getOutcomes( object ) ) )

            # ==============================================
            # set parameters names
            # ==============================================

            parameters = getParameters( object )
            parametersNames = unlist( lapply( parameters, function(x) getName( x ) ) )
            parameters = parameters[ order( parametersNames ) ]
            newModel = setParameters( newModel, parameters )

            # ==============================================
            # set Ode Solver Parameters
            # ==============================================

            newModel = setOdeSolverParameters( newModel, getOdeSolverParameters( object ) )

            return( newModel )
          })

#' Define the type of a model.
#'
#' @name defineModelType
#' @param object An object from the class \linkS4class{Model}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return Return a model defined as analytic, ode, etc.
#' @export

setGeneric(
  "defineModelType",
  function(object, designs ) {

    standardGeneric("defineModelType")
  })

#' @rdname defineModelType
#' @export

setMethod(f = "defineModelType",
          signature="Model",
          definition= function(object, designs)
          {
            # ==============================================
            # All possible models
            # ==============================================

            # model = paste0( ModelODE, DoseInEquations, Infusion )
            # model = paste0( ModelODE, DoseInEquations )
            # model = paste0( ModelODE, DoseNotInEquations )
            # model = paste0( ModelODE, Bolus )
            # model = paste0( ModelAnalytic )
            # model = paste0( ModelAnalytic, Bolus )
            # model = paste0( ModelAnalytic, Bolus, SteadyState )
            # model = paste0( ModelAnalytic, Infusion )
            # model = paste0( ModelAnalytic, Infusion, SteadyState )
            # model = paste0( ModelODE, Infusion )
            # model = paste0( ModelAnalytic, SteadyState )

            # ==========================================================================
            # tests model for ode analytic, dose in equations, infusion and steady state
            # ==========================================================================

            ModelODE = NULL
            ModelAnalytic = NULL
            DoseInEquations = NULL
            DoseNotInEquations = NULL
            Bolus = NULL
            Infusion = NULL
            SteadyState = NULL

            if ( isModelODE( object ) )
            {
              ModelODE = "ModelODE"
            }

            if ( isModelAnalytic( object ) )
            {
              ModelAnalytic = "ModelAnalytic"
            }

            if ( isDoseInEquations( object ) & isModelODE( object ) )
            {
              DoseInEquations = "DoseInEquations"
            }

            if ( !isDoseInEquations( object ) & isModelODE( object ) )
            {
              DoseNotInEquations = "DoseNotInEquations"
            }

            if ( isModelBolus( object, designs ) )
            {
              Bolus = "Bolus"
              DoseNotInEquations = NULL
            }

            if ( isModelInfusion( object ) )
            {
              Infusion = "Infusion"
            }

            if ( isModelSteadyState ( object ) )
            {
              SteadyState = "SteadyState"
            }

            # ==============================================
            # define new model
            # ==============================================

            nameNewModel = paste0( ModelODE, ModelAnalytic,  Bolus, Infusion, DoseInEquations, DoseNotInEquations, SteadyState )

            newModel = new( nameNewModel )

            return( newModel )
          })

#' Evaluate a model.
#'
#' @name EvaluateModel
#' @param object An object from the class \linkS4class{Model}.
#' @param arm An object from the class \linkS4class{Arm}.
#' @return A list giving the results of the model evaluation.
#' @export

setGeneric("EvaluateModel",
           function( object, arm )
           {
             standardGeneric("EvaluateModel")
           })

#' Define the parameters for computing the gradients of a model.
#'
#' @name parametersForComputingGradient
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the parameters for computing the gradients of a model.
#' @export

setGeneric("parametersForComputingGradient",
           function(object)
           {
             standardGeneric("parametersForComputingGradient")
           }
)

#' @rdname parametersForComputingGradient
#' @export

setMethod(f = "parametersForComputingGradient",
          signature="Model",
          definition= function(object)
          {
            minAbsPar = 0
            .relStep = .Machine$double.eps^(1/3)

            valuePars = c()
            modelParameters = getParameters( object )

            for ( modelParameter in modelParameters )
            {
              distribution = getDistribution( modelParameter )
              mu = getMu( distribution )
              valuePars = c( valuePars, mu )
            }

            pars = as.numeric(valuePars)
            npar = length(pars)
            incr = pmax(abs(pars), minAbsPar) * .relStep
            baseInd = diag(npar)
            frac = c(1, incr, incr^2)
            cols = list(0, baseInd, -baseInd)

            for ( i in seq_along(pars)[ -npar ] ) {
              cols = c( cols, list( baseInd[ , i ] + baseInd[ , -(1:i) ] ) )
              frac = c( frac, incr[ i ] * incr[ -(1:i) ] )
            }

            indMat = do.call( "cbind", cols)
            shifted = pars + incr * indMat
            indMat = t(indMat)
            Xcols = list(1, indMat, indMat^2)

            for ( i in seq_along(pars)[ - npar ] ) {
              Xcols = c( Xcols, list( indMat[ , i ] * indMat[ , -(1:i) ] ) )
            }

            return( list( Xcols = Xcols, shifted = shifted, frac = frac ) )
          })

#' Evaluate the variance of a model.
#'
#' @name EvaluateVarianceModel
#' @param object An object from the class \linkS4class{Model}.
#' @param arm An object from the class \linkS4class{Arm}.
#' @param evaluationModel A list giving the outputs of the model evaluation.
#' @return Return a list giving the results of the evaluation of the model variance.
#' @export

setGeneric("EvaluateVarianceModel",
           function( object, arm, evaluationModel )
           {
             standardGeneric("EvaluateVarianceModel")
           }
)

#' @rdname EvaluateVarianceModel
#' @export

setMethod(f = "EvaluateVarianceModel",
          signature="Model",
          definition= function(object, arm, evaluationModel )
          {
            errorModelDerivatives = list()
            errorVariances = list()
            sigmaDerivatives = list()

            modelError = getModelError( object )

            outcomeNames = names( getOutcomes( object ) )

            totalNumberOfSamplingTimes = length( ( unlist( lapply( evaluationModel$evaluationOutcomes, '[', c( "time" ) ) ) ) )

            k = 1

            for ( outcomeName in outcomeNames )
            {
              modelErrorIndex = which( sapply( modelError, function (x) getOutcome(x) == outcomeName) )

              if ( length( modelErrorIndex ) != 0 )
              {
                # ==============================================
                # Evaluate Error Model Derivatives
                # ==============================================

                evaluationResponseTmp = evaluationModel$evaluationOutcomes[[outcomeName]]

                errorModelDerivatives[[outcomeName]] = EvaluateErrorModelDerivatives( modelError[[modelErrorIndex]],
                                                                                      evaluationResponseTmp[,outcomeName] )

                # ==============================================
                # Error variances
                # ==============================================

                errorVariances = append( errorVariances, bdiag( errorModelDerivatives[[outcomeName]]$errorVariance ) )
                errorVariances = bdiag( errorVariances )

                # ==============================================
                # Sigma derivatives
                # ==============================================

                for( errorModelDerivative in errorModelDerivatives[[outcomeName]]$sigmaDerivatives )
                {
                  sigmaDerivativesTmp = matrix( 0, ncol = totalNumberOfSamplingTimes, nrow = totalNumberOfSamplingTimes )

                  numberOfSampleResponse = length( evaluationResponseTmp[,outcomeName] )

                  range = k:( k + numberOfSampleResponse - 1 )

                  sigmaDerivativesTmp[ range, range ] = errorModelDerivative

                  sigmaDerivatives = c( sigmaDerivatives, list( sigmaDerivativesTmp ) )
                }
                k = k + numberOfSampleResponse
              }
            }

            return( list( errorVariances = errorVariances, sigmaDerivatives = sigmaDerivatives ) )

          })

#' Get the fixed parameters.
#'
#' @name getFixedParameters
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the fixed parameters of the model.
#' @export

setGeneric(
  "getFixedParameters",
  function(object) {
    standardGeneric("getFixedParameters")
  })

#' @rdname getFixedParameters
#' @export

setMethod("getFixedParameters",
          signature("Model"),
          function(object)
          {
            fixedMu = list()
            fixedOmega = list()
            omega = list()
            mu = list()

            parameters = getParameters( object )

            for ( parameter in parameters )
            {
              parameterName = getName( parameter )

              fixedMu[[parameterName]] = getFixedMu( parameter )
              fixedOmega[[parameterName]] = getFixedOmega( parameter )

              omega[[parameterName]] = getOmega( parameter )
              mu[[parameterName]] = getMu( parameter )

              if ( mu[[parameterName]] == 0 )
              {
                fixedMu[[parameterName]] = TRUE
                parameter = setFixedMu( parameter, TRUE )
              }

              if ( omega[[parameterName]] == 0 )
              {
                fixedOmega[[parameterName]] = TRUE
                parameter = setFixedOmega( parameter, TRUE )
              }
            }

            indexParameterfixedMu = which( fixedMu == TRUE )
            indexParametersMuZero = which( mu == 0 )

            indexParameterfixedOmega = which( fixedOmega == TRUE )
            indexParametersOmegaZero = which( omega == 0 )

            parameterfixedMu = unique( c( indexParameterfixedMu, indexParametersMuZero ) )
            parameterfixedOmega = unique( c( indexParameterfixedOmega, indexParametersOmegaZero ) )

            return( list( parameterfixedMu = parameterfixedMu,
                          parameterfixedOmega = parameterfixedOmega ) )

          })

#' Get the values of the model error parameters.
#'
#' @name getModelErrorParametersValues
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the values of the model error parameters.
#' @export

setGeneric(
  "getModelErrorParametersValues",
  function(object) {
    standardGeneric("getModelErrorParametersValues")
  })

#' @rdname getModelErrorParametersValues
#' @export

setMethod("getModelErrorParametersValues",
          signature("Model"),
          function(object)
          {
            modelError = getModelError( object )

            sigmaInterSlope = c()

            for ( modelErrorResponse in modelError )
            {
              responseName = getOutcome( modelErrorResponse )

              sigmaInterSlope = c( sigmaInterSlope, getSigmaInter( modelErrorResponse ), getSigmaSlope( modelErrorResponse ) )
            }

            sigmaInterSlope = sigmaInterSlope[ sigmaInterSlope != 0 ]

            return(sigmaInterSlope)
          })

#' Get the values of the model parameters.
#'
#' @name getModelParametersValues
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the values of the model parameters.
#' @export

setGeneric(
  "getModelParametersValues",
  function(object) {
    standardGeneric("getModelParametersValues")
  })

#' @rdname getModelParametersValues
#' @export

setMethod("getModelParametersValues",
          signature("Model"),
          function(object)
          {
            mu = c()
            omega = c()
            sigma = c()
            sigmaInterSlope = c()
            parameters = getParameters( object )
            # fixed mu and omega
            for ( parameter in parameters )
            {
              mu = c( mu, getMu( parameter ) )
              omega = c( omega, getOmega(  parameter ) )
            }
            return( modelParametersValues = list( mu = mu, omega = omega ) )
          })

#' Generate the tables for model parameters for the evaluation report.
#'
#' @name reportTablesModelParameters
#' @param object An object from the class \linkS4class{Model}.
#' @return A kable table for the evaluation report.
#' @export

setGeneric(
  "reportTablesModelParameters",
  function(object) {
    standardGeneric("reportTablesModelParameters")
  })

#' @rdname reportTablesModelParameters
#' @export

setMethod("reportTablesModelParameters",
          signature("Model"),
          function(object)
          {
            # ==============================================
            # get parameters
            # ==============================================

            parameters = getParameters( object )
            numberOfParameters = getNumberOfParameters( object )

            # =====================================================
            # get parameters names, distribution, values, mu, omega
            # =====================================================

            parameterNames = unlist( lapply( parameters, function(x) getName(x) ) )

            parameterDistributions = unlist( lapply( parameters, function(x) getDistribution(x) ) )
            parameterDistributions = unlist( lapply( parameterDistributions,function(x) is(x)[1] ) )
            mu = unlist( lapply( parameters, function(x) getMu(x) ) )
            omega = unlist( lapply( parameters, function(x) getOmega(x) ) )

            fixedParameters = getFixedParameters( object )
            indexfixedMu = fixedParameters$parameterfixedMu
            indexfixedOmega = fixedParameters$parameterfixedOmega

            # =====================================================
            # vectors for fixed true/false
            # =====================================================

            vecFixedMu = rep( "no", numberOfParameters )
            vecFixedOmega = rep( "no", numberOfParameters )

            if ( length( indexfixedMu ) !=0 )
            {
              vecFixedMu[indexfixedMu] = "yes"
            }
            if ( length( indexfixedOmega ) !=0 )
            {
              vecFixedOmega[indexfixedOmega] = "yes"
            }

            tableParameters = data.frame( parameterNames = parameterNames,
                                          mu = round(mu,2), omega = round(omega**2,2),
                                          parameterDistributions = parameterDistributions,
                                          vecFixedMu = vecFixedMu, vecFixedOmega = vecFixedOmega)

            rownames( tableParameters ) = NULL
            colnames( tableParameters ) = c("Parameters","
                                            ${\\mu}$","${\\omega^2}$","Distribution",
                                            paste0("${\\mu}$ ","fixed"),paste0("${\\omega^2}$ ","fixed") )

            tableParameters = knitr::kable( tableParameters ) %>%
              kable_styling( font_size = 12,
                             latex_options = c("hold_position","striped", "condensed", "bordered" ),
                             full_width = T)


            return( tableParameters )
          })

#' Generate the tables for model errors for the evaluation report.
#'
#' @name reportTablesModelError
#' @param object An object from the class \linkS4class{Model}.
#' @return A kable table for the evaluation report.
#' @export

setGeneric(
  "reportTablesModelError",
  function(object) {
    standardGeneric("reportTablesModelError")
  })

#' @rdname reportTablesModelError
#' @export

setMethod("reportTablesModelError",
          signature("Model"),
          function( object )
          {
            modelError = getModelError( object )

            # =====================================================
            # get outcome, sigma slope and sigma inter
            # =====================================================

            sigmaSlope = unlist( lapply( modelError,function(x) getSigmaSlope(x) ) )
            sigmaInter = unlist( lapply( modelError,function(x) getSigmaInter(x) ) )
            ouctomes = unlist( lapply( modelError,function(x) getOutcome(x) ) )

            sigmaSlope = round( sigmaSlope, 3 )
            sigmaInter = round( sigmaInter, 3 )

            tablesModelError = data.frame( ouctomes = ouctomes, sigmaSlope = sigmaSlope, sigmaInter = sigmaInter )

            rownames( tablesModelError ) = NULL
            colnames( tablesModelError ) = c("Response"," ${\\sigma}_{slope}$","${\\sigma}_{inter}$" )

            tablesModelError = knitr::kable( tablesModelError ) %>%
              kable_styling( font_size = 12,
                             latex_options = c("hold_position","striped", "condensed", "bordered" ),
                             full_width = T)

            return( tablesModelError )
          })

##########################################################################################################
# END Class "Model"
##########################################################################################################
