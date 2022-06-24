##################################################################################
#' Class "SamplingConstraint"
#'
#' @description Class "SamplingConstraint" storing information concerning sampling constraint.
#'
#' @name SamplingConstraint-class
#' @aliases SamplingConstraint
#' @docType class
#' @exportClass SamplingConstraint
#'
#' @section Objects from the class: \code{SamplingConstraint} objects are typically created by calls to \code{SamplingConstraint} and contain the following slots:
#'
#'@section Slots for \code{SamplingConstraint}  objects:
#' \describe{
#' \item{\code{response}:}{A character string for the name of the response of the model.}
#' \item{\code{numberOptimisability}:}{A boolean that gives TRUE for optimizing the number of times and FALSE for fixing the number of times.}
#' \item{\code{numberOfSamplingTimes}:}{A vector of the number of sampling times.}
#' \item{\code{fixedTimes}:}{A vector of the number of fixed times.}
#' \item{\code{continuousSamplingTimes}:}{A list of the continuous sampling times.}
#' \item{\code{discretSamplingTimes}:}{A list of the discrete sampling times.}
#' \item{\code{min_delay}:}{A numeric giving the minimal interval in the sampling times.}
#' }
##################################################################################

SamplingConstraint<- setClass(
  Class = "SamplingConstraint",
  slots = c(
    response = "character",
    numberOfSamplingTimes="vector",
    fixedTimes = "vector",
    continuousSamplingTimes="list",
    discretSamplingTimes="list",
    min_delay = "numeric"
  ),
  prototype=prototype(
    continuousSamplingTimes = list(),
    discretSamplingTimes = list()
  ),
  validity = function(object)
  {
    return(TRUE)
  }
)


setMethod(
  f="initialize",
  signature="SamplingConstraint",
  definition= function (.Object,
                        response,
                        numberOfSamplingTimes,
                        fixedTimes,
                        continuousSamplingTimes,
                        discretSamplingTimes,
                        min_delay)
  {
    .Object@min_delay <- 0

    if(!missing(response))
      .Object@response<-response

    if(!missing(numberOfSamplingTimes))
      .Object@numberOfSamplingTimes<-numberOfSamplingTimes

    if(!missing(fixedTimes))
      .Object@fixedTimes<-fixedTimes

    if(!missing(continuousSamplingTimes))
      .Object@continuousSamplingTimes<-continuousSamplingTimes

    if(!missing(discretSamplingTimes))
      .Object@discretSamplingTimes<-discretSamplingTimes

    if ( !missing( min_delay ) )
      .Object@min_delay <- min_delay
    validObject(.Object)
    return (.Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the constraint on minimal time delay in sampling times.
#'
#' @name isLessThanDelay
#' @param object A \code{SamplingConstraint} object.
#' @param samplingTimes A \code{SamplingTimes} object.
#' @return A boolean that give TRUE/FALSE if the constraint on minimal delay is satisfied.

setGeneric( "isLessThanDelay",
            function( object, samplingTimes )
            {
              standardGeneric( "isLessThanDelay")
            }
)
setMethod( "isLessThanDelay",
           "SamplingConstraint",
           function(object, samplingTimes )
           {


             if ( object@min_delay <=0 )
               return( FALSE )

             if ( sum( diff( samplingTimes ) <= object@min_delay ) !=0 )
             {

               return( TRUE )
             }
             return( FALSE )
           })

# -------------------------------------------------------------------------------------------------------------------
#' Set the number of sampling times that are optimisable.
#'
#' @name numberOfSamplingTimesIsOptimisable
#' @param object A \code{SamplingConstraint} object.
#' @param FixedNumberTimes A numeric giving the number of sampling times to be fixed.
#' @return Set the number of sampling times that are optimisable in the constraints.


setGeneric("numberOfSamplingTimesIsOptimisable",
           function(object,FixedNumberTimes)
           {
             standardGeneric("numberOfSamplingTimesIsOptimisable")
           }
)
setMethod ("numberOfSamplingTimesIsOptimisable",
           signature=c("SamplingConstraint"),
           function(object,FixedNumberTimes)
           {
             object@numberOfSamplingTimes <- FixedNumberTimes

             return(object)
           }
)

# ------------------------------------------------------------------------------
#' Set the allowed continuous sampling times.
#'
#' @rdname allowedContinuousSamplingTimes
#' @param object A \code{SamplingConstraint} object.
#' @param allowedTimes A list giving the vectors for the allowed continuous Sampling Times.
#' @return The object \code{SamplingConstraint} object with the allowed continuous Sampling Times.

setGeneric("allowedContinuousSamplingTimes",
           function(object,allowedTimes)
           {
             standardGeneric("allowedContinuousSamplingTimes")
           }
)

setMethod (f="allowedContinuousSamplingTimes",
           signature=c("SamplingConstraint","list"),
           definition=function(object,allowedTimes)
           {
             object@continuousSamplingTimes <- allowedTimes
             return(object)
           }

)

# ------------------------------------------------------------------------------
#' Set the allowed discret sampling times.

#' @param object A \code{SamplingConstraint} object.
#' @param allowedTimes A list giving the vectors for the allowed Continous Sampling Times.
#' @return The object \code{SamplingConstraint} object with the allowed Continous Sampling Times.
#' @rdname allowedDiscretSamplingTimes
#' @docType methods

setGeneric("allowedDiscretSamplingTimes",
           function(object,allowedTimes)
           {
             standardGeneric("allowedDiscretSamplingTimes")
           }
)

setMethod ("allowedDiscretSamplingTimes",
           signature=c("SamplingConstraint","list"),
           function(object,allowedTimes)
           {
             object@discretSamplingTimes <- allowedTimes
             return(object)
           }

)

# -------------------------------------------------------------------------------------------------------------------
#' Set the value for the fixed times.
#'
#' @name FixTimeValues
#' @param object A \code{SamplingConstraint} object.
#' @param value A vector of numeric giving the values for the fixed times.
#' @return The \code{SamplingConstraint} object with the values for the fixed times.

setGeneric("FixTimeValues",
           function(object, value)
           {
             standardGeneric("FixTimeValues")
           }
)
setMethod (
  f = "FixTimeValues",
  signature=c("SamplingConstraint","vector"),
  definition = function(object, value)
  {
    object@fixedTimes <- value
    return(object)
  }

)

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of the response for the SamplingConstraint.
#'
#' @rdname getResponseName
#' @param object \code{SamplingConstraint} object.
#' @return The character string \code{response} giving the name of the response for the \code{SamplingConstraint} object.

setMethod("getResponseName",
          "SamplingConstraint",
          function(object)
          {
            return(object@response)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the optimisability of a \code{SamplingConstraint} object.
#'
#' @name getOptimisability
#' @param object \code{SamplingConstraint} object.
#' @return A boolean giving the optimisability of a \code{SamplingConstraint} object.

setGeneric("getOptimisability",
           function(object)
           {
             standardGeneric("getOptimisability")
           }
)
setMethod("getOptimisability",
          "SamplingConstraint",
          function(object)
          {
            return(object@numberOptimisability)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the number of sampling times.
#'
#' @name getnumberOfSamplingTimes
#' @param object A \code{SamplingConstraint} object.
#' @return A numeric giving the number of sampling times in the \code{SamplingConstraint} object.

setGeneric("getnumberOfSamplingTimes",
           function(object)
           {
             standardGeneric("getnumberOfSamplingTimes")
           }
)
setMethod("getnumberOfSamplingTimes",
          "SamplingConstraint",
          function(object)
          {
            return(object@numberOfSamplingTimes)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the allowed Continuous SamplingTimes
#'
#' @name getallowedContinuousSamplingTimes
#' @param object A \code{SamplingConstraint} object.
#' @return A list giving the allowed Continuous SamplingTimes for the \code{SamplingConstraint} object.

setGeneric("getallowedContinuousSamplingTimes",
           function(object)
           {
             standardGeneric("getallowedContinuousSamplingTimes")
           }
)
setMethod("getallowedContinuousSamplingTimes",
          "SamplingConstraint",
          function(object)
          {
            return(object@continuousSamplingTimes)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the constraint on the sampling times bounds.
#'
#' @name isTimeInBetweenBounds
#' @param object A \code{SamplingConstraint} object.
#' @param time A \code{SamplingTimes} object.
#' @return A boolean that give TRUE/FALSE if the constraint on on the sampling times bounds are satisfied.

setGeneric("isTimeInBetweenBounds",
           function(object, time)
           {
             standardGeneric("isTimeInBetweenBounds")
           }
)
setMethod("isTimeInBetweenBounds",
          "SamplingConstraint",
          function(object, time)
          {


            if ( length( object@continuousSamplingTimes ) == 0 )
              return( TRUE )

            for ( interval in object@continuousSamplingTimes )
            {

              if ( time >= interval[1] && time <= interval[2] )
              {
                return( TRUE )
              }

            }

            return( FALSE )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the allowed discret sampling simes
#'
#' @name getallowedDiscretSamplingTimes
#' @param object \code{SamplingConstraint} object.
#' @return The allowed discret sampling times of the \code{SamplingConstraint} object.

setGeneric("getallowedDiscretSamplingTimes",
           function(object)
           {
             standardGeneric("getallowedDiscretSamplingTimes")
           }
)
setMethod("getallowedDiscretSamplingTimes",
          "SamplingConstraint",
          function(object)
          {
            return(object@discretSamplingTimes)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the fixed times.
#'
#' @name getfixedTimes
#' @param object A \code{SamplingConstraint} object.
#' @return The fixed times of the \code{SamplingConstraint} object.

setGeneric("getfixedTimes",
           function(object)
           {
             standardGeneric("getfixedTimes")
           }
)
setMethod("getfixedTimes",
          "SamplingConstraint",
          function(object)
          {
            return(object@fixedTimes)
          }
)

# ########################################################################################################################
# END Class SamplingConstraint
# ########################################################################################################################


