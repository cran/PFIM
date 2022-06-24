##################################################################################
#' Class for the library of models.
#'
#' @description
#' \code{LibraryOfModels} is an S4 class that implements the library of models, consisting
#' of two libraries of PK and PD models respectively.
#'
#' The PK library includes model with different administration routes (bolus, infusion, first-order absorption),
#' different number of compartments (from 1 to 3), and different types of eliminations (linear or Michaelis-Menten).
#' The PD model library, contains direct immediate models (e.g. Emax and Imax) with various baseline models,
#' and turnover response models. The PK/PD models, on the other hand, are obtained with combination of the models
#' from the PK and PD model libraries. Throught the use of the \code{LibraryOfModels} PFIM handles both analytical and ODE models and offers the possibility to the user to define his own models.
#'
#' The library of pharmacokinetic (PK) and pharmacodynamic (PD) models is described in the vignette LibraryOfModels.
#'
#'
#' @name LibraryOfModels-class
#' @aliases LibraryOfModels
#' @docType class
#' @exportClass LibraryOfModels
#'
#' @section Objects from the class: \code{LibraryOfModels} objects are created by calls to \code{\link{LibraryOfModels}} and contain the following slots:
#' \describe{
#' \item{\code{nameLibraryOfModels}:}{A character string giving the name of the library of models.}
#' \item{\code{contentsLibraryOfModels}:}{A list of the PK, PD and PKPD models that are in the library of models.}
#'}

LibraryOfModels <- setClass(
  Class ="LibraryOfModels",
  representation = representation(nameLibraryOfModels = "character",
                                  contentsLibraryOfModels = "list"),
  prototype = prototype(nameLibraryOfModels = character(0)))

# -------------------------------------------------------------------------------------------------------------------
#' Get the content of the \code{LibraryOfModels} object.
#' @name getContentsLibraryOfModels
#' @param object A \code{LibraryOfModels} object.
#' @return A list \code{contentsLibraryOfModels} giving the two lists that respectively corresponds to the two librairies of the PK and PD models contained in the \code{\link{LibraryOfModels}}.

setGeneric(
  "getContentsLibraryOfModels",
  function(object) {
    standardGeneric("getContentsLibraryOfModels")
  })

setMethod(
  "getContentsLibraryOfModels",
  signature("LibraryOfModels"),
  function(object){
    contentsLibraryOfModels = list()
    contentsLibraryOfModels <- object@contentsLibraryOfModels
    return(contentsLibraryOfModels)
  })

# -------------------------------------------------------------------------------------------------------------------
#' Add a \code{Model} object in the \code{LibraryOfModels}.
#'
#' @name addModel
#' @param object A \code{LibraryOfModels} object.
#' @param model The model to add in the library (PK, PD or PKPD model).
#' @return The \code{LibraryOfModels} object with the loaded library of models.

setGeneric(
  "addModel",
  function(object, model) {
    standardGeneric("addModel")
  })

setMethod(
  "addModel",
  signature("LibraryOfModels"),
  function(object, model){
    object@contentsLibraryOfModels <- append(object@contentsLibraryOfModels,model)
    return(object)
  })

# -------------------------------------------------------------------------------------------------------------------
#' Get the list of all the models in the \code{LibraryOfModels} object.
#' @name getModelNameList
#' @param object A \code{LibraryOfModels} object.
#' @return The list \code{ModelNameList} of the names of the PK, PD and PKPD models in the \code{LibraryOfModels} object.

setGeneric(
  "getModelNameList",
  function(object) {
    standardGeneric("getModelNameList")
  })

setMethod(
  "getModelNameList",
  signature("LibraryOfModels"),
  function(object){

    contentsLibraryOfModels = getContentsLibraryOfModels(object)

    listOfModel = lapply(contentsLibraryOfModels, slot, name = "nameModel")
    listClassModel = lapply(contentsLibraryOfModels,class)

    indexPDModels = which(listClassModel=="PDModel")
    indexPKModels = which(listClassModel=="PKModel")
    indexPKPDModels = which(listClassModel=="PKPDModel")

    pkModelsNameList = listOfModel[indexPKModels]
    pdModelsNameList = listOfModel[indexPDModels]
    pkpdModelsNameList = listOfModel[indexPKPDModels]

    ModelNameList = list(pkModelsNameList,pdModelsNameList,pkpdModelsNameList)

    names(ModelNameList) = c("pkModel","pdModel","pkpdModel")

    return(ModelNameList)

  })

# -------------------------------------------------------------------------------------------------------------------
#' Get a model of the \code{LibraryOfModels} object.
#' @name getModel
#' @param object A \code{LibraryOfModels} object.
#' @param ... The three-dots for passing one name or two names as arguments. One name to get a PK or PD model and two names for the PK and PD models of a PKPD model.
#' @return Return a \code{Model} object giving a PK or PD model.

setGeneric(
  "getModel",
  function(object, ...) {
    standardGeneric("getModel")
  })

setMethod(
  "getModel",
  signature("LibraryOfModels"),
  function(object, ...){

    listNames = list(...)
    listNames = unlist(listNames[!unlist(lapply(listNames, is.null))])

    if (length(listNames)==1){

      nameModel = listNames[[1]]

      contentsLibraryOfModels = getContentsLibraryOfModels(object)

      listOfModel = lapply(contentsLibraryOfModels, slot, name = "nameModel")
      indexNameModel = which(listOfModel==nameModel)

      if (length(indexNameModel)==0){

        print("Model not in the Library")

        return(Model())

      }

      return(contentsLibraryOfModels[[indexNameModel]])

    } else if (length(listNames)==2){

      modelOne = getModel(object, listNames[[1]])
      modelTwo = getModel(object, listNames[[2]])

      listModel = c(modelOne,modelTwo)

      indexPkModel = which((lapply(listModel, is, class = "PKModel"))==TRUE)
      indexPdModel = which((lapply(listModel, is, class = "PDModel"))==TRUE)

      namePKModel = listNames[indexPkModel]
      namePDModel = listNames[indexPdModel]

      return(getPKPDModel(object,namePKModel,namePDModel))
    }

    else

    {
      print("Too many names given in the function getModel.")
    }

  })

# -------------------------------------------------------------------------------------------------------------------

#' Get a PKPD model of the \code{LibraryOfModels} object.
#' @name getPKPDModel
#' @param object A \code{LibraryOfModels} object.
#' @param namePKModel A character string giving the name of the PK model.
#' @param namePDModel A character string giving the name of the PD model.
#' @return Return a \code{Model} giving the PKPD model consisting of the PK and PD models named namePKModel and namePDModel respectively.

setGeneric(
  "getPKPDModel",
  function(object,namePKModel,namePDModel) {
    standardGeneric("getPKPDModel")
  })

setMethod(
  "getPKPDModel",
  signature("LibraryOfModels"),

  function(object,namePKModel,namePDModel){

    # get the contents of the library of models
    contentsLibraryOfModels = getContentsLibraryOfModels(object)

    namesModel = sapply(contentsLibraryOfModels,getModelName)

    indexpkModel = which(namePKModel == namesModel)
    indexpdModel = which(namePDModel == namesModel)

    if (length(indexpkModel)==0) {
      print("Model PK not in the Library")
      return(Model())
    }

    else if (length(indexpdModel)==0) {
      print("Model PD not in the Library")
      return(Model())
    }

    else {

      pkModel = getModel(object, namesModel[[indexpkModel]])
      pdModel = getModel(object, namesModel[[indexpdModel]])

      # define the PKPD model
      output = PKPDModel(pkModel=pkModel,pdModel=pdModel)

      # get the equations of the PK and PD models
      equationsModelPK = getEquationsModel(pkModel)
      equationsModelPD = getEquationsModel(pdModel)

      # get the parameters of the PK and PD models
      parametersPKModel = getParameters(equationsModelPK)
      parametersPDModel = getParameters(equationsModelPD)
      allParameters = c(parametersPKModel,parametersPDModel)

      # set the parameters for the PKPD model
      output = setParametersModel(output,allParameters)

    }

    return(output)

  })

##########################################################################################################
# END Class "LibraryOfModels"
##########################################################################################################

