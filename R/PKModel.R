##################################################################################
#' Class "PKModel" representing a PK model.
#'
#' @description A class storing information concerning the PK models in the \linkS4class{LibraryOfModels}.
#'
#' @name PKModel-class
#' @aliases PKModel
#' @docType class
#' @include Model.R
#' @exportClass PKModel
#'
#' @section Objects from the class \code{PKModel}: objects are typically created by calls to \code{PKModel}.
#
#'@section Slots for the \code{PKModel} objects, that are heritated from the class \linkS4class{Model}:
#' \describe{
#' \item{\code{nameModel}:}{A character string giving the name of the model.}
#' \item{\code{descriptionModel}:}{A list of character string giving the characterisation of the model (name, administration, number of compartment)}
#' \item{\code{equationsModel}:}{A object \code{ModelEquations} giving the equations of the model.}
#' }
##################################################################################

PKModel <- setClass(
  Class = "PKModel",
  contains = "Model",
  representation = representation(),
  prototype = prototype())

# -------------------------------------------------------------------------------------------------------------------
#' Change variable in a PK Model.
#'
#' @name changeVariablePKModel
#' @param object \code{PKModel} object.
#' @return A expression giving the equations of the PK model with variable changed.

setGeneric(
  "changeVariablePKModel",
  function(object) {
    standardGeneric("changeVariablePKModel")
  })

setMethod("changeVariablePKModel",
          signature("PKModel"),
          function(object)

          {

            if(is(getEquationsModel(object),"ModelODEquations")){

              equation = getEquations(getEquationsModel(object))

              modelEquations = getEquationsModel(object)

              return(modelEquations)

            } else {

              return(getEquations(getEquationsModel(object)))}

          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a \code{PKModel} object.
#'
#' @rdname getEquations
#' @param object A \code{PKModel} object.
#' @return A list of expressions giving the equations of a \code{PKModel} object.

setMethod("getEquations",
          signature("PKModel"),
          function(object)
          {

            output = object@equationsModel

          })

########################################################################################################
# END Class "PKModel"
########################################################################################################


