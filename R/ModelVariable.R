##################################################################################
#' Class "ModelVariable"
#' @description
#' Class "ModelVariable" represents an initial variable for ODE model.
#'
#' @name ModelVariable-class
#' @aliases ModelVariable
#' @docType class
#' @exportClass ModelVariable
#'
#' @section Objects from the class: \code{ModelVariable} objects are typically created by calls to \code{ModelVariable} and contain the following slots:
#'
#' @section Slots for \code{ModelVariable} objects:
#' \describe{
#' \item{\code{name}:}{A character string giving the name of the initial variable of an ODE model.}
#' \item{\code{value}:}{A numeric giving the value of the initial variable of an ODE model.}
#' }
##################################################################################

ModelVariable <- setClass(
  Class = "ModelVariable",
  representation = representation(
    name = "character",
    value = "numeric"
  )
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "ModelVariable",
  definition = function( .Object, name )
  {
    if(!missing(name))
      .Object@name <-name
    return(.Object)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of the initial variable for an ODE model.
#'
#' @name getNameModelVariable
#' @param object \code{ModelVariable} object.
#' @return A character string \code{name} giving the name of the initial variable for an ODE model.


setGeneric("getNameModelVariable",
           function(object)
           {
             standardGeneric("getNameModelVariable")
           }
)

setMethod("getNameModelVariable",
          "ModelVariable",
          function(object)
          {
            return(object@name)
          }
)

##########################################################################################################
# END Class "ModelVariable"
##########################################################################################################
