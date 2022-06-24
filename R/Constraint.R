#####################################################################################################################################
#' Class "Constraint"
#'
#' @description
#' The class \code{Constraint} stores the constraints for a variable. Constraints are given either as:
#' a continuous range, a discrete set of values, or a Design constraint.
#'
#' @name Constraint-class
#' @aliases Constraint
#' @docType class
#' @exportClass Constraint
#' @exportClass DiscreteConstraint
#' @exportClass ContinuousConstraint
#' @exportClass DesignConstraint
#'
#' @section Objects from the class \code{Constraint}:
#' Objects form the class \code{Constraint} can be created by calls of the objects from the following classes:
#' \describe{
#' \item{-}{\linkS4class{AdministrationConstraint}}
#' \item{-}{\linkS4class{ContinuousConstraint}}
#' \item{-}{\linkS4class{DesignConstraint}}
#' \item{-}{\linkS4class{DiscreteConstraint}}
#' }
#'
#####################################################################################################################################

Constraint<-setClass(Class = "Constraint",
                     representation = representation(
                     ),
                     validity = function(object)
                     {
                       return(TRUE)
                     }
)

setMethod(
  f="initialize",
  signature="Constraint",
  definition=function(.Object)
  {
    validObject(.Object)
    return(.Object)
  }
)

##########################################################################################################
# END Class "Constraint"
##########################################################################################################


