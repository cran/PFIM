#' Class "AdministrationConstraints"
#'
#' @description
#' The class \code{AdministrationConstraints} represents the constraint of an input to the system.
#' The class stores information concerning the constraints for the dosage regimen:
#' response of the model, amount of dose.
#'
#' @name AdministrationConstraints-class
#' @aliases AdministrationConstraints
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{AdministrationConstraints} can be created by calls of the form \code{AdministrationConstraints(...)} where
#' (...) are the parameters for the \code{AdministrationConstraints} objects.
#'
#' @section Slots for \code{AdministrationConstraints} objects:
#'  \describe{
#'    \item{\code{outcome}:}{A character string giving the name for the response of the model.}
#'    \item{\code{doses}:}{A numeric vector giving the amount of doses.}
#'  }

AdministrationConstraints = setClass("AdministrationConstraints",
                       representation = representation(
                         outcome = "character",
                         doses = "numeric") )
setMethod(
  f="initialize",
  signature="AdministrationConstraints",
  definition= function (.Object, outcome, doses  )
  {
    if(!missing(outcome))
    {
      .Object@outcome = outcome
    }
    if(!missing(doses))
    {
      .Object@doses = doses
    }
    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
# getOutcome
# ======================================================================================================

setMethod("getOutcome",
          "AdministrationConstraints",
          function(object)
          {
            return(object@outcome)
          }
)

# ======================================================================================================
# getDose
# ======================================================================================================

setMethod(f="getDose",
          signature="AdministrationConstraints",
          definition = function(object)
          {
            return( object@doses )
          }
)

##########################################################################################################
# END Class "AdministrationConstraints"
##########################################################################################################


