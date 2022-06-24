#####################################################################################################################################
#' Class "ContinuousConstraint" representing the constraints for a variable
#'
#' @description
#' The class  \code{ContinuousConstraint} stores constraints for a variable.
#' Constraints are given either as a continuous range (any value between a min and a max boundary is admissible)
#' or a discrete set of values (any value belonging to the set is admissible).
#'
#' @name ContinuousConstraint-class
#' @aliases ContinuousConstraint
#' [,ContinuousConstraint-method [<-,ContinuousConstraint-method
#' @docType class
#' @include Constraint.R
#' @exportClass ContinuousConstraint
#'
#' @section Objects from the class: Constraint objects are typically created by calls to \code{constraint}
#' and contain the following slots:
#'
#' \describe{
#' \item{\code{type}:}{A character string, one of 'continuous' or 'discrete'.}
#' \item{\code{range}:}{A numeric vector with two values giving the min/max of the continuous range.}
#' \item{\code{minimalDelay}:}{A numeric value giving the minimal timestep between two sampling times.}
#' }
#####################################################################################################################################

ContinuousConstraint<-setClass(

  Class = "ContinuousConstraint",
  contains = "Constraint",

  representation = representation(
    range = "vector",
    minimalDelay="numeric"),

  validity = function(object)
  {
    if(length(object@range)!=2)
      stop ("[ ContinuousConstraint : validation ] A continuous Constraint must have an element containing the min and max of the admissible values.")
    if(object@range[1]>=object@range[2])
      stop ("[ ContinuousConstraint : validation ] The range argument should be of the form c(a,b) with a<b.")
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature = "ContinuousConstraint",
  definition = function ( .Object, range, minimalDelay )
  {
    .Object@minimalDelay <- 0

    if ( !missing( range ) )
      .Object@range <- range

    if ( !missing( minimalDelay ) )
      .Object@minimalDelay <- minimalDelay

    validObject( .Object )
    return ( .Object )
  }
)

##########################################################################################################

#' Get the range of a \code{ContinuousConstraint} object.
#'
#' @name getRange
#' @param object A\code{ContinuousConstraint} object.
#' @return A numeric \code{range} giving the range of a \code{ContinuousConstraint} object.

setGeneric("getRange",
           function(object)
           {
             standardGeneric("getRange")
           }
)
setMethod("getRange",
          "ContinuousConstraint",
          function(object)
          {
            return(object@range)
          }
)

##########################################################################################################

#' Set the range of a \code{ContinuousConstraint} object.
#'
#' @name setRange<-
#' @param object A \code{ContinuousConstraint} object.
#' @param value A numeric.
#' @return The \code{ContinuousConstraint} object with the new range.


setGeneric("setRange<-",
           function(object, value)
           {
             standardGeneric("setRange<-")
           }
)
setReplaceMethod( f="setRange",
                  signature="ContinuousConstraint",
                  definition = function(object, value)
                  {
                    object@range <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################
# END Class "ContinuousConstraint"
##########################################################################################################


