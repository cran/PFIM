#####################################################################################################################################
#' Class "AdministrationConstraint"
#'
#' @description
#' The class \code{AdministrationConstraint} represents the constraint of an input to the system.
#' The class stores information concerning the constraints for the dosage regimen :
#' response of the model, type of administration, amount of dose.
#'
#' @name AdministrationConstraint-class
#' @aliases AdministrationConstraint
#' @docType class
#' @exportClass AdministrationConstraint
#'
#' @section Objects from the class:
#' Objects form the class \code{AdministrationConstraint} can be created by calls of the form #'\code{AdministrationConstraint(...)} where (...) are the parameters for the \code{AdministrationConstraint} objects.
#'
#'@section Slots for the \code{AdministrationConstraint} objects:
#' \describe{
#' \item{\code{response}:}{A character giving the response of the model.}
#' \item{\code{Optimisability}:}{A boolean giving if a dose is optimisable or not. If not the dose is fixed.}
#' \item{\code{fixedDoses}:}{A vector giving the fixed doses.}
#' \item{\code{AllowedDoses}:}{A vector giving the allowed amount of doses.}
#' }
#'
#####################################################################################################################################

AdministrationConstraint<-setClass(
  Class = "AdministrationConstraint",
  slots = c(
    response = "character",
    Optimisability = "logical",
    fixedDoses = "vector",
    AllowedDoses = "vector"
  ),
  validity = function(object)
  {
    return(TRUE)
  })

setMethod(
  f="initialize",
  signature="AdministrationConstraint",
  definition= function (.Object, response, Optimisability, AllowedDoses, fixedDoses)
  {
    if(!missing(response))
      .Object@response<-response
    if(!missing(Optimisability))
      .Object@Optimisability<-Optimisability
    if(!missing(AllowedDoses))
      .Object@AllowedDoses<-AllowedDoses
    if(!missing(fixedDoses))
      .Object@fixedDoses<-fixedDoses
    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################

#' Define the vector of allowed amount of dose.
#'
#' @name AllowedDoses
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @param value A numeric vector giving the allowed amount of doses.
#' @return The \code{AdministrationConstraint} object with its new allowed amount of doses.

setGeneric("AllowedDoses",
           function(object,value)
           {
             standardGeneric("AllowedDoses")
           }
)
setMethod("AllowedDoses",
          "AdministrationConstraint",
          function(object,value)
          {
            object@AllowedDoses <- value
            return(object)
          }
)

##########################################################################################################

#' Set the value for the fixed doses in the administration constraints.
#'
#' @name fixedDoses
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @param value A numeric vector giving the value of the fixed dose.
#' @return The \code{AdministrationConstraint} object with its new value of the fixed dose.

setGeneric("fixedDoses",
           function(object,value)
           {
             standardGeneric("fixedDoses")
           }
)
setMethod("fixedDoses",
          "AdministrationConstraint",
          function(object,value)
          {
            object@fixedDoses <- value
            return(object)
          }
)

##########################################################################################################

#' Get the name of the response for the administration constraints.
#'
#' @name getResponseName
#' @rdname getResponseName
#' @aliases getResponseName
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @return The character string \code{response} giving the name of the response of the object \code{AdministrationConstraint} object.

setGeneric("getResponseName",
           function(object)
           {
             standardGeneric("getResponseName")
           }
)
setMethod("getResponseName",
          "AdministrationConstraint",
          function(object)
          {
            return(object@response)
          }
)

##########################################################################################################

#' Get the vector of allowed amount of dose.
#'
#' @name getAllowedDoses
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @return A vector \code{AllowedDoses} giving the allowed amount of dose.

setGeneric("getAllowedDoses",
           function(object)
           {
             standardGeneric("getAllowedDoses")
           }
)
setMethod("getAllowedDoses",
          "AdministrationConstraint",
          function(object)
          {
            return(object@AllowedDoses)
          }
)

##########################################################################################################

#' Get the boolean \code{Optimisability} for optimizable dose.
#'
#' @name getDoseOptimisability
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @return The boolean \code{Optimisability} giving FALSE for fixed dose, TRUE for an optimizable dose.

setGeneric("getDoseOptimisability",
           function(object)
           {
             standardGeneric("getDoseOptimisability")
           }
)
setMethod("getDoseOptimisability",
          "AdministrationConstraint",
          function(object)
          {
            return(object@Optimisability)
          }
)

##########################################################################################################

#' Get the vector \code{AllowedDoses} of allowed amount of dose.
#'
#' @name getNumberOfDoses
#' @param object An object \code{AdministrationConstraint} from the class \linkS4class{AdministrationConstraint}.
#' @return The numeric \code{AllowedDoses} giving the number of allowed amount of doses in the object \code{AdministrationConstraint}.

setGeneric("getNumberOfDoses",
           function(object)
           {
             standardGeneric("getNumberOfDoses")
           }
)
setMethod("getNumberOfDoses",
          "AdministrationConstraint",
          function(object)
          {
            return(length(object@AllowedDoses))
          }
)

##########################################################################################################
# END Class "AdministrationConstraint"
##########################################################################################################













