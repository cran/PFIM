##################################################################################
#' Class "Model" representing a Model
#'
#'@description A class storing information concerning the models in the \code{LibraryOfModels}.
#'
#' @name Model-class
#' @aliases Model
#' @docType class
#' @include ModelEquations.R
#' @exportClass Model
#'
#' @section Objects from the Class  \code{Model}:
#' Objects form the Class \code{Model} can be created by calls of the form \code{Model(...)} where
#' (...) are the parameters for the \code{Model} object.
#'
#'@section Slots for the \code{Model} objects:
#' \describe{
#' \item{\code{nameModel}:}{A character string giving the name of the model.}
#' \item{\code{descriptionModel}:}{A list of character string giving the characterisation of the model (name, administration, number of compartment)}
#' \item{\code{equationsModel}:}{A object \code{ModelEquations} giving the equations of the model.}
#' }

Model <- setClass(
  Class = "Model",
  representation = representation(nameModel = "character",
                                  descriptionModel = "list",
                                  equationsModel = "ModelEquations"),
  prototype = prototype(nameModel = character(0),
                        descriptionModel  = list()))

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of the \code{Model} object.
#' @name getModelName
#' @param object A \code{Model} object.
#' @return A character string \code{modelName} giving the name of the \code{Model} object.

setGeneric(
  "getModelName",
  function(object) {
    standardGeneric("getModelName")
  })

setMethod(
  "getModelName",
  signature("Model"),
  function(object){
    modelName = object@nameModel
    return(modelName)
  })

# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a \code{Model} object.
#' @name getEquationsModel
#' @param object A \code{Model} object.
#' @return A list of expression \code{equationsModel} giving the equations of a \code{Model} object.

setGeneric(
  "getEquationsModel",
  function(object) {
    standardGeneric("getEquationsModel")
  })

setMethod("getEquationsModel",
          signature("Model"),
          function(object)
          {
            equationsModel = object@equationsModel
            return(equationsModel)
          })



# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a \code{Model} object after changing variable in PK model.
#' @rdname getEquations
#' @param object A \code{Model} object.
#' @return A list of expression giving the equations of a \code{Model} object after the changing variable in PK model.


          setMethod("getEquations",
          signature("Model"),
          function(object)
          {

            return(changeVariablePKModel(object))

          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the parameters of the \code{Model} object.
#' @name setParametersModel
#' @param object A \code{Model} object.
#' @param parameters The vector of character string giving the parameters names.
#' @return The \code{Model} object with the  new parameters.

setGeneric(
  "setParametersModel",
  function(object,parameters) {
    standardGeneric("setParametersModel")
  })

setMethod(
  "setParametersModel",
  signature("Model"),
  function(object,parameters){

    object@equationsModel@allParameters <- parameters
    validObject(object)
    return(object)
  })

# ########################################################################################################################
# END Class "Model"
# ########################################################################################################################

