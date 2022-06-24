##################################################################################
#' Class "PKPDModel" representing a PKPDModel model.
#'
#' @description A class storing information concerning the PKPDModel models in the \linkS4class{LibraryOfModels}.
#'
#' @name PKPDModel-class
#' @aliases PKPDModelModel
#' @docType class
#' @include Model.R
#' @exportClass PKPDModel
#'
#' @section Objects from the class \code{PKPDModel}: objects are typically created by calls to \code{PKPDModel}.
#
#'@section Slots for the \code{PKPDModel} objects, that are heritated from the class \linkS4class{Model}:
#' \describe{
#' \item{\code{nameModel}:}{A character string giving the name of the model.}
#' \item{\code{descriptionModel}:}{A list of character string giving the characterisation of the model (name, administration, number of compartment)}
#' \item{\code{equationsModel}:}{A object \code{ModelEquations} giving the equations of the model.}
#' }
##################################################################################

PKPDModel <- setClass(
  Class = "PKPDModel",
  contains = "Model",
  representation = representation(pkModel="PKModel", pdModel="PDModel"),
  prototype = prototype())

# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a \code{PKPDModel} object.
#'
#' @rdname getEquations
#' @param object A \code{PKPDModel} object.
#' @return A list of expressions giving the equations of a \code{PKPDModel} object.

setMethod("getEquations",
          signature("PKPDModel"),
          function(object)
          {

            # get the PK and PD models of the PKPDmodel
            pkModel = getPKModel(object)
            pdModel = getPDModel(object)

            # get the PK and PD  modelEquations of the PKPDmodel
            modelEquationsPKmodel = getEquationsModel(pkModel)
            modelEquationsPDmodel = getEquationsModel(pdModel)

            # get the equations of the PKPD model
            output = getEquationsModelPKPD( modelEquationsPKmodel, modelEquationsPDmodel )

            return(output)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get a PK model from a \code{PKPDModel} object.
#'
#' @name getPKModel
#' @param object \code{PKPDModel} object.
#' @return A \code{Model} object giving the \code{pkModel} from the PKPD model.

setGeneric(
  "getPKModel",
  function(object) {
    standardGeneric("getPKModel")
  })

setMethod(
  "getPKModel",
  signature("PKPDModel"),

  function(object){

    return(object@pkModel)

  })

# -------------------------------------------------------------------------------------------------------------------
#' Get a PD model from a \code{PKPDModel} object.
#'
#' @name getPDModel
#' @param object \code{PKPDModel} object.
#' @return A \code{Model} object giving the \code{pdModel} from the PKPD model.

setGeneric(
  "getPDModel",
  function(object) {
    standardGeneric("getPDModel")
  })

setMethod(
  "getPDModel",
  signature("PKPDModel"),

  function(object){

    return( object@pdModel)

  })

########################################################################################################
# END Class "PKPDModel"
########################################################################################################

