##################################################################################
#' Class "PDModel" representing a PD model.
#'
#' @description A class storing information concerning the PD models in the \linkS4class{LibraryOfModels}.
#'
#' @name PDModel-class
#' @aliases PDModel
#' @docType class
#' @include Model.R
#' @exportClass PDModel
#'
#' @section Objects from the class: \code{PDModel} objects are typically created by calls to \code{PDModel}.
#
#'@section Slots for the \code{PDModel} objects, that are heritated from the class \linkS4class{Model}:
#' \describe{
#' \item{\code{nameModel}:}{A character string giving the name of the model.}
#' \item{\code{descriptionModel}:}{A list of character string giving the characterisation of the model (name, administration, number of compartment)}
#' \item{\code{equationsModel}:}{A object \code{ModelEquations} giving the equations of the model.}
#' }

# ------------------------------------------------------------------------------------------------
# Class PDModels
# ------------------------------------------------------------------------------------------------

PDModel <- setClass(
  Class = "PDModel",
  contains = "Model",
  representation = representation(),
  prototype = prototype())




