#' Class "LibraryOfModels"
#'
#' @description The class \code{LibraryOfModels} represents the library of models.
#'
#' @name LibraryOfModels-class
#' @aliases LibraryOfModels
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{LibraryOfModels} can be created by calls of the form \code{LibraryOfModels(...)} where
#' (...) are the parameters for the \code{LibraryOfModels} objects.
#'
#'@section Slots for \code{LibraryOfModels} objects:
#' \describe{
#' \item{\code{name}:}{A string giving the name of the library of models.}
#' \item{\code{content}:}{A list giving the content of the library of model.}
#' }

LibraryOfModels = setClass(
  Class ="LibraryOfModels",
  representation = representation(name = "character", content = "list"))

setMethod(
  f="initialize",
  signature = "LibraryOfModels",
  definition = function (.Object, name, content )
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(content))
    {
      .Object@content = content
    }
    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
# getName
# ======================================================================================================

setMethod("getName",
          "LibraryOfModels",
          function(object) {
            return( object@name )
          })

# ======================================================================================================
#' Get content of a library of models.
#'
#' @name getContent
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @return A list giving the content of the library of models.
# ======================================================================================================

setGeneric(
  "getContent",
  function(object) {

    standardGeneric("getContent")
  })

setMethod("getContent",
          "LibraryOfModels",
          function(object) {
            return( object@content )
          })

# ======================================================================================================
#' Set content of a library of models.
#'
#' @name setContent
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @param content A list giving the content of the library of models.
#' @return The library of models with the updated content.
# ======================================================================================================

setGeneric(
  "setContent",
  function(object,content) {

    standardGeneric("setContent")
  })

setMethod("setContent",
          "LibraryOfModels",
          function(object,content) {
            object@content = content
            return( object )
          })

# ======================================================================================================
#' Add a model to a library of models.
#'
#' @name addModel
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @param model An object from the class \linkS4class{Model}.
#' @return The library of models with the added model.
# ======================================================================================================

setGeneric(
  "addModel",
  function(object, model) {

    standardGeneric("addModel")
  })

setMethod("addModel",
          "LibraryOfModels",
          function(object, model) {

            content = getContent( object )
            object  = setContent( object, append( content, model) )

            return(object)
          })

# ======================================================================================================
#' Add a models to a library of models.
#'
#' @name addModels
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @param models A list of object from the class \linkS4class{Model}.
#' @return The library of models with the added models.
# ======================================================================================================

setGeneric(
  "addModels",
  function(object, models) {

    standardGeneric("addModels")
  })

setMethod("addModels",
          "LibraryOfModels",
          function(object, models) {

            for ( model in models )
            {
              object = addModel( object, model )
            }
            return(object)
          })

# ======================================================================================================
#' Get the library of PK models.
#'
#' @name getLibraryPKModels
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @return A list giving the PK models.
# ======================================================================================================

setGeneric(
  "getLibraryPKModels",
  function(object) {

    standardGeneric("getLibraryPKModels")
  })

setMethod("getLibraryPKModels",
          "LibraryOfModels",
          function(object) {

            content = getContent( object )
            libraryPKModels = content$LibraryOfPKModels

            return( libraryPKModels )
          })

# ======================================================================================================
#' Get the library of PD models.
#'
#' @name getLibraryPDModels
#' @param object An object from the class \linkS4class{LibraryOfModels}.
#' @return A list giving the PD models.
# ======================================================================================================

setGeneric(
  "getLibraryPDModels",
  function(object) {

    standardGeneric( "getLibraryPDModels" )
  })

setMethod("getLibraryPDModels",
          "LibraryOfModels",
          function(object) {

            content = getContent( object )
            libraryPKModels = content$LibraryOfPDModels

            return( libraryPKModels )
          })

##########################################################################################################
# END Class "LibraryOfModels"
##########################################################################################################











