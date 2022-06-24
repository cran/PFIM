##################################################################################
#' Class "SamplingTimes"
#' @description
#' Class "SamplingTimes" stores information concerning sampling times.
#'
#' @name SamplingTimes-class
#' @aliases SamplingTimes
#' [,Sampling-method [<-,Sampling-method
#' @docType class
#' @exportClass SamplingTimes
#'
#' @section Objects from the Class:
#' Objects form the Class \code{SamplingTimes} can be created by calls of the form \code{SamplingTimes(...)} where
#' (...) are the parameters for the \code{SamplingTimes} objects.
#'
#'@section Slots for the \code{SamplingTimes} objects:
#' \describe{
#' \item{\code{outcome}:}{A character string giving either a compartment name or number (character or integer, TBD with model) (nombre de reponses "1", "2").}
#' \item{\code{sample_time}:}{A list of discrete vectors giving the times when sampling design is performed. }
#' \item{\code{initialTime}:}{A numeric giving the initial time of the vecotr of sampling times.}
#' }
##################################################################################

SamplingTimes <- setClass(Class="SamplingTimes",
                          representation=representation
                          (
                            outcome = "character",    # either a compartment name or number (character or integer, TBD with model) (nombre de reponses "1", "2")
                            sample_time="vector",     # sampling times (t_ij)
                            initialTime="numeric"
                          ),
                          prototype=prototype
                          (#default options
                            outcome = "1" #The Sampling class should necessarily contain outcome (outcome can be null but if it is the case, the default value equal to 1 is given)
                          ))

# Initialize method
setMethod(
  f="initialize",
  signature="SamplingTimes",
  definition= function (.Object, outcome, sample_time, initialTime )
  {
    if(!missing(outcome))
      .Object@outcome<-outcome

    if(!missing(sample_time))
      .Object@sample_time = sample_time

    if(!missing(initialTime))
      .Object@initialTime <- initialTime
    else
      .Object@initialTime = 0
    validObject(.Object)
    return (.Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of the response of the \code{SamplingTimes} object.
#'
#' @rdname getNameSampleTime
#' @param object A \code{SamplingTimes} object.
#' @return A character string \code{outcome} giving the name of the response of the model.

setGeneric("getNameSampleTime",
           function(object)
           {
             standardGeneric("getNameSampleTime")
           }
)

setMethod("getNameSampleTime",
          "SamplingTimes",
          function(object)
          {
            return(object@outcome)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the sample time of the response of the \code{SamplingTimes} object.
#'
#' @name getSampleTime
#' @param object A \code{getSampleTime} object.
#' @return A vector \code{sample_time} giving the sample time.


setGeneric("getSampleTime",
           function(object)
           {
             standardGeneric("getSampleTime")
           }
)

setMethod("getSampleTime",
          "SamplingTimes",
          function(object)
          {
            return(object@sample_time)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the sample time of the response of the \code{SamplingTimes} object.
#'
#' @name setSampleTime
#' @param object A \code{SamplingTimes} object.
#' @param values A vector giving the new values of the sampling times.
#' @return The \code{SamplingTimes} object with the new sample times.


setGeneric("setSampleTime",
           function(object, values )
           {
             standardGeneric("setSampleTime")
           }
)

setMethod("setSampleTime",
          "SamplingTimes",
          function(object, values)
          {
            object@sample_time<-values
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the number of times in a \code{SamplingTimes} object.
#'
#' @name getNumberTime
#' @param object A \code{SamplingTimes} object.
#' @return A numeric  giving the number of times.

setGeneric("getNumberTime",
           function(object)
           {
             standardGeneric("getNumberTime")
           }
)
setMethod("getNumberTime",
          "SamplingTimes",
          function(object)
          {
            return( length( object@sample_time ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the initial time of a \code{SamplingTimes} object.
#'
#' @name getInitialTime
#' @param object \code{SamplingTimes} object.
#' @return A numeric \code{initialTime} giving the inital time of a \code{SamplingTimes} object.

setGeneric("getInitialTime",
           function(object)
           {
             standardGeneric("getInitialTime")
           }
)
setMethod("getInitialTime",
          "SamplingTimes",
          function(object)
          {
            return(object@initialTime)
          }
)

# ########################################################################################################################
# END Class SamplingTimes
# ########################################################################################################################




