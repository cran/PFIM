#' Class "Arm"
#'
#' @description
#' The class \code{Arm} combines the treatment and the sampling schedule.
#'
#' @name Arm-class
#' @aliases Arm
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Arm} can be created by calls of the form \code{Arm(...)} where (...) are the parameters for the \code{Arm} objects.
#'
#' @section Slots for the \code{Arm} objects:
#' \describe{
#' \item{\code{name}:}{A string giving the name of the arm.}
#' \item{\code{size}:}{An integer giving the number of subjects in the arm. By default set to 1.}
#' \item{\code{administrations}:}{A list of the administrations.}
#' \item{\code{initialConditions}:}{A list of the initial conditions.}
#' \item{\code{samplingTimes}:}{A list of the sampling times.}
#' \item{\code{administrationsConstraints}:}{A list of the administrations constraints.}
#' \item{\code{samplingTimesConstraints}:}{A list of the sampling times constraints.}
#' }

Arm = setClass(
  Class = "Arm",
  representation = representation(
    name = "character",
    size = "numeric",
    administrations  = "list",
    initialConditions = "list",
    samplingTimes = "list",
    administrationsConstraints = "list",
    samplingTimesConstraints = "list"
  ))

setMethod(f="initialize",
          signature="Arm",
          definition= function (.Object, name, size, administrations, initialConditions, samplingTimes, administrationsConstraints, samplingTimesConstraints )
          {
            if(!missing(name))
            {
              .Object@name = name
            }
            if(!missing(size))
            {
              .Object@size = size
            }
            if(!missing(administrations))
            {
              .Object@administrations = unlist(administrations)
            }
            if(!missing(samplingTimes))
            {
              .Object@samplingTimes = unlist(samplingTimes)
            }
            if(!missing(initialConditions))
            {
              .Object@initialConditions = initialConditions
            }
            if(!missing(administrationsConstraints))
            {
              .Object@administrationsConstraints = administrationsConstraints
            }
            if(!missing(samplingTimesConstraints))
            {
              .Object@samplingTimesConstraints = samplingTimesConstraints
            }

            validObject(.Object)
            return (.Object )
          }
)

# ======================================================================================================
# get, set the name of an arm
# ======================================================================================================

#' @rdname getName
#' @export

setMethod("getName",
          "Arm",
          function(object)
          {
            name = object@name
            return( name )
          })

#' @rdname setName
#' @export

setMethod("setName",
          "Arm",
          function(object,name)
          {
            object@name = name
            return( object )
          })

# ======================================================================================================
# get the size of an arm
# ======================================================================================================

#' @rdname getSize
#' @export

setMethod("getSize",
          "Arm",
          function( object )
          {
            return( object@size )
          }
)

#' Set the size of an arm.
#'
#' @title setSize
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param size A numeric giving the new size of the object \code{Arm}.
#' @return The object \code{Arm} object with its new size.
#' @export

setGeneric(
  "setSize",
  function(object,size) {
    standardGeneric("setSize")
  })

#' @rdname setSize
#' @export

setMethod("setSize",
          "Arm",
          function( object, size )
          {
            object@size = size
            return( object )
          }
)

#' Get all the administration for an arm.
#'
#' @title getAdministrations
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A list \code{administrations} of objects from the class \code{Administration} class giving
#' the parameters of the administration for the object \code{Arm}.
#' @export

setGeneric(
  "getAdministrations",
  function(object) {
    standardGeneric("getAdministrations")
  })

#' @rdname getAdministrations
#' @export

setMethod("getAdministrations",
          "Arm",
          function( object )
          {
            return( object@administrations )
          })

#' Set all the administration for an arm.
#'
#' @title setAdministrations
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param administrations A list \code{administrations} of objects from the class \code{Administration} class giving
#' the parameters of the administration for the object \code{Arm}.
#' @return The object \code{Arm} with the list \code{administrations} of objects from the class \code{Administration} class giving
#' the parameters of the administration for the object \code{Arm}.
#' @export

setGeneric(
  "setAdministrations",
  function(object, administrations) {
    standardGeneric("setAdministrations")
  })

#' @rdname setAdministrations
#' @export

setMethod("setAdministrations",
          "Arm",
          function( object, administrations )
          {
            object@administrations = administrations
            return( object )
          })

#' Get the vectors of sampling times for an arm.
#'
#' @title getSamplingTimes
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return The list \code{samplingTimes} for the object \code{Arm}.
#' @export

setGeneric(
  "getSamplingTimes",
  function(object) {
    standardGeneric("getSamplingTimes")
  })

#' @rdname getSamplingTimes
#' @export

setMethod("getSamplingTimes",
          "Arm",
          function( object )
          {
            return( object@samplingTimes )
          })

#' Set the vectors of sampling times for an arm.
#'
#' @title setSamplingTimes
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param samplingTimes The list containing the new sampling times.
#' @return An object \code{Arm} from the class \linkS4class{Arm} with the new sampling times \code{samplingTimes}.
#' @export

setGeneric(
  "setSamplingTimes",
  function(object, samplingTimes) {
    standardGeneric("setSamplingTimes")
  })

#' @rdname setSamplingTimes
#' @export

setMethod("setSamplingTimes",
          "Arm",
          function( object, samplingTimes )
          {
            object@samplingTimes = samplingTimes
            return( object )
          })

#' Get the initial condition for the evaluation of an ode model.
#'
#' @title getInitialConditions
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return The list \code{initialConditions} for the object \code{Arm}.
#' @exportMethod getInitialConditions

setGeneric(
  "getInitialConditions",
  function(object) {
    standardGeneric("getInitialConditions")
  })

#' @rdname getInitialConditions
#' @export

setMethod("getInitialConditions",
          "Arm",
          function( object )
          {
            return( object@initialConditions )
          }
)

#' @rdname setInitialConditions
#' @export

setMethod("setInitialConditions",
          "Arm",
          function( object, initialConditions )
          {
            object@initialConditions = initialConditions
            return( object )
          }
)

#' Get the administrations constraints.
#'
#' @title getAdministrationsConstraints
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return The list \code{administrationsConstraints}.
#' @export

setGeneric("getAdministrationsConstraints",
           function(object)
           {
             standardGeneric("getAdministrationsConstraints")
           }
)

#' @rdname getAdministrationsConstraints
#' @export

setMethod(f="getAdministrationsConstraints",
          signature="Arm",
          definition = function(object)
          {
            return( object@administrationsConstraints )
          }
)

#' Get the sampling times constraints.
#'
#' @title getSamplingTimesConstraints
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return The list \code{getSamplingTimesConstraints}.
#' @export

setGeneric("getSamplingTimesConstraints",
           function(object)
           {
             standardGeneric("getSamplingTimesConstraints")
           }
)

#' @rdname getSamplingTimesConstraints
#' @export

setMethod(f="getSamplingTimesConstraints",
          signature="Arm",
          definition = function(object)
          {
            return( object@samplingTimesConstraints )
          }
)

#' Set the sampling times constraints.
#'
#' @title setSamplingTimesConstraints
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param samplingTimesConstraints  An object \code{SamplingTimeConstraints} from the class \linkS4class{SamplingTimeConstraints}.
#' @return The arm with the new sampling time constraints.
#' @export

#' @rdname setSamplingTimesConstraints
#' @export

setGeneric("setSamplingTimesConstraints",
           function( object, samplingTimesConstraints )
           {
             standardGeneric("setSamplingTimesConstraints")
           }
)

#' @rdname setSamplingTimesConstraints
#' @export
#'
setMethod(f="setSamplingTimesConstraints",
          signature="Arm",
          definition = function( object, samplingTimesConstraints )
          {
            object@samplingTimesConstraints = samplingTimesConstraints
            return( object )
          }
)

#' Get the sampling times by outcome.
#'
#' @title getSamplingTime
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A string giving the name of the outcome.
#' @return The element of the list \code{samplingTimes} containing the sampling times of the outcome \code{outcome}
#' @export

setGeneric(
  "getSamplingTime",
  function(object,outcome) {
    standardGeneric("getSamplingTime")
  })

#' @rdname getSamplingTime
#' @export

setMethod("getSamplingTime",
          "Arm",
          function( object, outcome )
          {
            samplingTimes = getSamplingTimes( object )

            outcomes = lapply( samplingTimes, function (x) getOutcome(x))

            indexOutcome = which( outcomes == outcome )

            samplingTimes = samplingTimes[[indexOutcome]]

            return( samplingTimes )
          }
)

#' Get the sampling times constraints by outcome.
#'
#' @title getSamplingTimeConstraint
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A string giving the name of the outcome.
#' @return The element of the list \code{samplingTimesConstraints} containing the sampling times constraints of the outcome \code{outcome}
#' @export

setGeneric(
  "getSamplingTimeConstraint",
  function(object,outcome) {
    standardGeneric("getSamplingTimeConstraint")
  })

#' @rdname getSamplingTimeConstraint
#' @export

setMethod("getSamplingTimeConstraint",
          "Arm",
          function( object, outcome )
          {
            samplingTimesConstraints = getSamplingTimesConstraints( object )

            outcomes = lapply( samplingTimesConstraints, function (x) getOutcome(x) )

            indexOutcome = which( outcomes == outcome )

            samplingTimeConstraint = samplingTimesConstraints[[indexOutcome]]

            return( samplingTimeConstraint )
          }
)

#' Set the sampling time of an arm.
#'
#' @title setSamplingTime
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param samplingTime An object \code{samplingTime} from the class \linkS4class{SamplingTimes}.
#' @return An object \code{Arm} from the class \linkS4class{Arm} with the new sampling time \code{samplingTime}.
#' @export

setGeneric(
  "setSamplingTime",
  function(object, samplingTime ) {
    standardGeneric("setSamplingTime")
  })

#' @rdname setSamplingTime
#' @export

setMethod("setSamplingTime",
          "Arm",
          function( object, samplingTime )
          {
            outcomeName = getOutcome( samplingTime )

            samplingTimes = getSamplingTimes( object )

            indexOutcome = lapply( samplingTimes, function(x) getOutcome(x) )

            indexOutcome = which( indexOutcome == outcomeName )

            samplingTimes[[indexOutcome]] = samplingTime

            object = setSamplingTimes( object, samplingTimes )

            return( object )
          }
)

#' Get the administrations by outcome.
#'
#' @title getAdministration
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A string giving the name of the outcome.
#' @return The element of the list \code{administrations} containing the administration of the outcome \code{outcome}
#' @export

setGeneric(
  "getAdministration",
  function(object, outcome) {
    standardGeneric("getAdministration")
  })

#' @rdname getAdministration
#' @export

setMethod("getAdministration",
          "Arm",
          function( object, outcome )
          {
            administration = list()

            administrations = getAdministrations( object )

            outcomeWithAdministration = lapply( administrations, function(x) getOutcome(x) )

            indexOutcome = which( outcomeWithAdministration == outcome )

            if ( length( indexOutcome ) !=0 )
            {
              administration = administrations[[indexOutcome]]
            }

            return( administration )

          }
)

#' Get the administration constraints by outcome.
#'
#' @title getAdministrationConstraint
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A string giving the name of the outcome.
#' @return The element of the list \code{getAdministrationConstraint} containing the administration constraints of the outcome \code{outcome}
#' @export

setGeneric(
  "getAdministrationConstraint",
  function(object, outcome) {
    standardGeneric("getAdministrationConstraint")
  })

#' @rdname getAdministrationConstraint
#' @export

setMethod("getAdministrationConstraint",
          "Arm",
          function( object, outcome )
          {
            administrationConstraint = list()

            administrationsConstraints = getAdministrationsConstraints( object )

            outcomeWithAdministration = lapply( administrationsConstraints, function(x) getOutcome(x) )

            indexOutcome = which( outcomeWithAdministration == outcome )

            if ( length( indexOutcome ) !=0 )
            {
              administrationConstraint = administrationsConstraints[[indexOutcome]]
            }

            return( administrationConstraint )

          }
)

#' Evaluate an arm.
#' @title EvaluateArm
#' @param object An object \code{arm} from the class \linkS4class{Arm}.
#' @param model An object \code{model} from the class \linkS4class{Model}.
#' @param fim An object \code{fim} from the class \linkS4class{Fim}.
#' @return The object \code{fim} containing the Fisher Information Matrix
#' the two lists \code{evaluationOutcomes}, \code{outcomesGradient} containing the results of
#' the evaluation of the outcome and the sensitivity indices.
#' @export

#' @rdname EvaluateArm
#' @export

setGeneric("EvaluateArm",
           function( object, model, fim )
           {
             standardGeneric("EvaluateArm")
           }
)

#' @rdname EvaluateArm
#' @export

setMethod(f="EvaluateArm",
          signature = "Arm",
          definition = function( object, model, fim )
          {
            # ==========================================================
            # evaluate model equations for each arm
            # responses and gradients
            # ==========================================================

            evaluationModel = EvaluateModel( model, object )

            # ==========================================================
            # model error variances
            # evaluation FIM
            # ==========================================================

            evaluateVarianceModel = EvaluateVarianceModel( model, object, evaluationModel )

            fim = EvaluateFisherMatrix( fim, model, object, evaluationModel, evaluateVarianceModel )

            return( list( fim = fim,
                          evaluationOutcomes = evaluationModel$evaluationOutcomes,
                          outcomesGradient =  evaluationModel$outcomesGradient ) )
          })

##########################################################################################################
# END Class "Arm"
##########################################################################################################



