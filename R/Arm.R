#####################################################################################################################################
#' Class "Arm"
#'
#' @description
#' The class "\code{Arm} combines the treatment (the class \linkS4class{Administration}) and the sampling schedule
#' (the class \linkS4class{SamplingTimes}).
#'
#' @name Arm-class
#' @aliases Arm
#' [,Arm-method [<-,Arm-method
#' @docType class
#' @exportClass Arm
#'
#' @section Objects from the class:
#' Objects form the class \code{Arm} can be created by calls of the form \code{Arm(...)} where (...) are the parameters for the \code{Arm} objects.
#'
#'@section Slots for the \code{Arm} objects:
#' \describe{
#' \item{\code{arm_size}:}{An integer the number of subjects in the arm. By default set to 1.}
#' \item{\code{administrations}:}{A list of \linkS4class{Administration} objects.}
#' \item{\code{cond_init}:}{A list of the initial conditions.}
#' \item{\code{samplings}:}{A list of \linkS4class{SamplingTimes} objects.}
#' \item{\code{constraints}:}{A list of \linkS4class{SamplingConstraint} objects.}
#' \item{\code{sampling_constraints}:}{A list of objects from \linkS4class{SamplingConstraint} class.}
#' }
#'
#####################################################################################################################################

Arm <- setClass(
  Class = "Arm",
  representation = representation(
    name = "character",
    arm_size = "numeric",
    administrations  = "list",
    cond_init = "list",
    samplings = "list",
    sampling_constraints = "list"
  ),
  prototype = prototype(
    samplings = list()
  ))

setMethod(
  f="initialize",
  signature="Arm",
  definition= function (.Object, name, arm_size, administrations, samplings, cond_init, sampling_constraints )
  {
    if(!missing(name))
      .Object@name<-name
    if(!missing(arm_size))
      .Object@arm_size<-arm_size
    if(!missing(administrations))
      .Object@administrations<-administrations
    if(!missing(samplings))
      .Object@samplings<-samplings
    if(!missing(sampling_constraints))
      .Object@sampling_constraints<-sampling_constraints
    if(!missing(cond_init))
      .Object@cond_init<-cond_init
    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################

#' Get the response name given the indice of the response.
#'
#' @name getResponseNameByIndice
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcomeIndice A numeric giving the indice of the response in the \code{Arm} object.
#' @return A character string giving the name of the response.

setGeneric("getResponseNameByIndice",
           function(object, outcomeIndice)
           {
             standardGeneric("getResponseNameByIndice")
           }
)

setMethod("getResponseNameByIndice",
          "Arm",
          function(object, outcomeIndice)
          {
            return(getNameSampleTime(object@samplings[[outcomeIndice]]))
          }
)

##########################################################################################################

#' Add sampling constraints to an arm.
#'
#' @name addSamplingConstraints
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param sampling_constraints A \code{SamplingConstraint} object giving the sampling constraints added to the \code{Arm} object.
#' @return The object \code{Arm} with the new sampling constraints.

setGeneric("addSamplingConstraints",
           function(object, sampling_constraints)
           {
             standardGeneric("addSamplingConstraints")
           }
)

setMethod("addSamplingConstraints",
          signature="Arm",
          function(object, sampling_constraints)
          {

            object@sampling_constraints <- append(object@sampling_constraints,sampling_constraints)

            return(object)
          }
)

##########################################################################################################

#' Get the sampling constraints of an arm.
#'
#' @name getSamplingConstraintsInArm
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param responseName A character string giving the name of the response.
#' @return An object \code{constraintsOfTheResponse} from the class \code{SamplingConstraint}.
#' giving the sampling constraints of the object \code{Arm}.

setGeneric("getSamplingConstraintsInArm",
           function(object, responseName)
           {
             standardGeneric("getSamplingConstraintsInArm")
           }
)

setMethod (f="getSamplingConstraintsInArm",
           signature="Arm",
           definition = function(object, responseName)
           {
             constraintsOfTheResponse <- list()
             for (i in object@sampling_constraints){
               constraintsOfTheResponse <- append( constraintsOfTheResponse,getSamplingConstraints(i,responseName) )
             }
             return(constraintsOfTheResponse)
           }
)

##########################################################################################################

#' Modify the sampling times of an arm.
#'
#' @name modifySamplingTimes
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A character string giving the name of the outcome ie the name of the response.
#' @param samplingTimes A vector of numeric giving the new sampling times.
#' @return The object \code{Arm} object with its new sampling times.

setGeneric("modifySamplingTimes",
           function(object, outcome, samplingTimes)
           {
             standardGeneric("modifySamplingTimes")
           }
)

setMethod("modifySamplingTimes",
          "Arm",
          function(object, outcome, samplingTimes)
          {
            if ( outcome %in% names( object@samplings ) )
            {
              object@samplings[[ outcome ]] <- SamplingTimes( outcome = outcome, samplingTimes )
            }
            return(object)
          }
)

##########################################################################################################

#' Get the name of the arm.
#'
#' @name getNameArm
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A character string \code{name} giving the name of the object \code{Arm}.

setGeneric("getNameArm",
           function(object)
           {
             standardGeneric("getNameArm")
           }
)

setMethod("getNameArm",
          "Arm",
          function(object)
          {
            return(object@name)
          }
)

##########################################################################################################

#' Get the size of an arm.
#'
#' @name getArmSize
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A numeric \code{arm_size} giving the size of the object \code{Arm}.

setGeneric("getArmSize",
           function(object)
           {
             standardGeneric("getArmSize")
           }
)
setMethod("getArmSize",
          "Arm",
          function(object)
          {
            return(object@arm_size)
          }
)

##########################################################################################################

#' Set the size of an arm.
#'
#' @name setArmSize
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param value A numeric giving the new size of the object \code{Arm}.
#' @return The object \code{Arm} object with its new size.

setGeneric("setArmSize",
           function(object, value)
           {
             standardGeneric("setArmSize")
           }
)
setMethod( f="setArmSize",
           signature="Arm",
           definition = function(object, value)
           {
             object@arm_size <- value
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Get the vectors of sampling times for an arm.
#'
#' @name getSamplings
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A list of objects \code{samplings} from the class \code{SamplingTimes} giving the vector.
#' of sampling times for the object \code{Arm}.

setGeneric("getSamplings",
           function(object)
           {
             standardGeneric("getSamplings")
           }
)
setMethod("getSamplings",
          "Arm",
          function(object)
          {
            return(object@samplings)
          }
)

##########################################################################################################

#' Get the number of sampling times in a arm.
#'
#' @name getNumberOfSamplings
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A numeric giving the number of sampling times in the \code{Arm} object.

setGeneric("getNumberOfSamplings",
           function(object)
           {
             standardGeneric("getNumberOfSamplings")
           }
)
setMethod("getNumberOfSamplings",
          "Arm",
          function( object)
          {
            return( length( object@samplings))
          }
)

##########################################################################################################

#' Get the initial conditions in a arm for an ODE model
#'
#' @name getCondInit
#' @param object An object \code{Arm} from the class \linkS4class{Arm}
#' @return A list \code{cond_init} giving the initial conditions for ODE model in the object \code{Arm}

setGeneric("getCondInit",
           function(object)
           {
             standardGeneric("getCondInit")
           }
)
setMethod("getCondInit",
          "Arm",
          function(object)
          {
            return(object@cond_init)
          }
)

##########################################################################################################

#' Set the sampling times for an arm.
#'
#' @name setSamplings<-
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param value The sampling times given by the objects from the class \code{SamplingTimes}.
#' @return The object \code{Arm} with its new sampling times.

setGeneric("setSamplings<-",
           function(object, value)
           {
             standardGeneric("setSamplings<-")
           }
)

setReplaceMethod( f="setSamplings",
                  signature="Arm",
                  definition = function(object, value)
                  {
                    object@samplings <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Set the initial conditions of an Arm for an ODE model.
#'
#' @name setInitialConditions
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param values A list of numeric giving the values of the initial conditions.
#' @return The object \code{Arm} with the new initial conditions for an ODE model.

setGeneric("setInitialConditions",
           function(object, values)
           {
             standardGeneric("setInitialConditions")
           }
)

setMethod( f="setInitialConditions",
           signature="Arm",
           definition = function(object, values)
           {
             object@cond_init <- values
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Add sampling time for an arm and for a response.
#'
#' @name addSampling
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param value An object from the class \code{SamplingTimes}.
#' @return The object \code{Arm} with its the new sampling times.

setGeneric("addSampling",
           function(object, value)
           {
             standardGeneric("addSampling")
           }
)

setMethod( f="addSampling",
           signature = "Arm",
           definition = function(object, value)
           {
             name = ( value@outcome )

             if(length(object@samplings[[ name ]]) == 0)
               object@samplings[[ name ]] <- value
             else
               cat("You have already used this name for a sampling.\n")
             validObject(object)
             return( object )
           }
)

##########################################################################################################

#' Add sampling times for an arm and for a response.
#'
#' @name addSamplings
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param listOfSamplings The objects from the class \code{SamplingTimes}.
#' @return The object \code{Arm} with its new sampling times.

setGeneric("addSamplings",
           function(object, listOfSamplings)
           {
             standardGeneric("addSamplings")
           }
)

setMethod( f="addSamplings",
           signature = "Arm",
           definition = function(object, listOfSamplings)
           {

             for (i in 1:length(listOfSamplings)){
               name = ( listOfSamplings[[i]]@outcome )

               if(length(object@samplings[[ name ]]) == 0)
                 object@samplings[[ name ]] <- listOfSamplings[[i]]
               else
                 cat("You have already used this name for a sampling.\n")
               validObject(object)
             }
             return( object )
           }
)

##########################################################################################################

#' Get the parameters of the administration for an arm.
#'
#' @name getAdministration
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @return A list \code{administrations} of objects from the class \code{Administration} class giving
#' the parameters of the administration for the object \code{Arm}.

setGeneric("getAdministration",
           function(object)
           {
             standardGeneric("getAdministration")
           }
)
setMethod(f = "getAdministration",
          signature = "Arm",
          definition = function(object)
          {
            return(object@administrations)
          }
)

##########################################################################################################

#' Get the parameters of the administration for an arm given the response of the model.
#'
#' @name getAdministrationByOutcome
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param outcome A character string giving the the name of the response of the model.
#' @return A list of objects from \code{Administration} class giving the parameters of the
#' administration for the object \code{Arm}.

setGeneric("getAdministrationByOutcome",
           function(object, outcome)
           {
             standardGeneric("getAdministrationByOutcome")
           }
)
setMethod(f = "getAdministrationByOutcome",
          signature = "Arm",
          definition = function(object, outcome)
          {
            for( adm in object@administrations )
              if ( getNameAdministration( adm ) == outcome )
                return( adm )
            print( paste( "Cannot find the outcome ", outcome ) )
            return( NA )
          }
)

##########################################################################################################

#' Add an administration to an arm.
#'
#' @name addAdministration
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param value An object from the class \linkS4class{Administration}.
#' @return The object \code{Arm} with its administration.

setGeneric("addAdministration",
           function( object, value )
           {
             standardGeneric("addAdministration")
           }
)

setMethod( f="addAdministration",
           signature = "Arm",
           definition = function(object, value)
           {

             name = ( value@outcome )
             if(length(object@administrations[[ name ]]) == 0)
               object@administrations[[ name ]] <- value
             else
               cat("You have already used this name for an administration.\n")

             validObject(object)
             return( object )
           }
)

##########################################################################################################

#' Evaluate a statistical model for all the administrations and all the sampling times of an arm.
#'
#' @name EvaluateStatisticalModel
#' @param object An object \code{Arm} from the class \linkS4class{Arm}.
#' @param statistical_model An object from the class \code{StatisticalModel} or \code{ODEStatisticalModel}.
#' @param fim Character string giving the type of the Fisher Information Matrix: "PopulationFim", "IndividualFim", "BayesianFim".
#' @return A list giving the evaluated Fisher Information Matrix, the concentration,
#' the sensitivity indices and to sampling times used for plotting the outputs.

setGeneric("EvaluateStatisticalModel",
           function(object, statistical_model, fim )
           {
             standardGeneric("EvaluateStatisticalModel")
           }
)

setMethod(f="EvaluateStatisticalModel",
          signature=  "Arm",
          definition=function(object, statistical_model, fim)
          {

            result = Evaluate( statistical_model, object@administrations, object@samplings, object@cond_init, fim )

            resultFim = FinalizeFIMForOneElementaryDesign( result$fim, object )
            concentrationModel = result$predictedResponses
            sensitivityIndicesModel = result$sensitivityIndicesModel


            return( list( resultFim = resultFim,
                          concentrationModel = concentrationModel,
                          sensitivityIndicesModel = sensitivityIndicesModel ) )
          })

##########################################################################################################
# END Class "Arm"
##########################################################################################################















