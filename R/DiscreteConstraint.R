#####################################################################################################################################
#' Class "DiscreteConstraint" representing the constraints for a variable
#'
#' The class \code{DiscreteConstraint} stores constraints for a variable. Constraints are given either as a continuous range
#' (any value between a min and a max boundary is admissible) or a discrete set of values
#' (any value belonging to the set is admissible)
#'
#' @name DiscreteConstraint-class
#' @aliases DiscreteConstraint
#' @include Constraint.R
#' @docType class
#' @exportClass DiscreteConstraint
#'
#' @section Objects from the class \code{DiscreteConstraint}:
#' Objects form the class \code{DiscreteConstraint} can be created by calls of the form \code{DiscreteConstraint(...)} where
#' (...) are the parameters for the \code{DiscreteConstraint} Model.
#'
#'@section Slots for the \code{DiscreteConstraint} objects:
#' \describe{
#' \item{\code{type}:}{A character string, one of 'continuous' or 'discrete'.}
#' \item{\code{range}:}{A numeric vector with two values giving the min/max of the continuous range.}
#' \item{\code{discrete}:}{A numeric vector giving the set of possible values.}
#' }


DiscreteConstraint <- setClass(
  Class = "DiscreteConstraint",
  contains = "Constraint",
  representation = representation(
    discrete = "numeric"),
  validity = function(object)
  {
    if(length(object@discrete)==0)
      stop ("[ DiscreteConstraint : validation ] A discrete Constraint must have a discrete element containing the admissible values.")
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="DiscreteConstraint",
  definition= function (.Object, discret)
  {
    if(!missing(discret))
      .Object@discret<-discret
    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################

#' Get the set of possible values for a \code{DiscreteConstraint} object.
#'
#' @name getDiscret
#' @param object A \code{DiscreteConstraint} object.
#' @return A numeric vector \code{discret} giving the set of possible values.

setGeneric("getDiscret",
           function(object)
           {
             standardGeneric("getDiscret")
           }
)
setMethod("getDiscret",
          "DiscreteConstraint",
          function(object)
          {
            return(object@discret)
          }
)

##########################################################################################################

#' Set the possible values for a \code{DiscreteConstraint} object.
#'
#' @name setDiscret<-
#' @param object A \code{DiscreteConstraint} object.
#' @param value Value for the discrete constraint in the  \code{DiscreteConstraint} object.
#' @return The \code{DiscreteConstraint} object with the set of new values.

setGeneric("setDiscret<-",
           function(object, value)
           {
             standardGeneric("setDiscret<-")
           }
)
setReplaceMethod( f="setDiscret",
                  signature="DiscreteConstraint",
                  definition = function(object, value)
                  {
                    object@discret <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################
# END Class "DiscreteConstraint"
##########################################################################################################

