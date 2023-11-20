#' Class "LibraryOfPKPDModels"
#'
#' @description The class \code{LibraryOfPKPDModels} represents the library of PKPD models.
#' The class \code{LibraryOfPKPDModels} inherits from the class \code{LibraryOfModels}.
#'
#' @name LibraryOfPKPDModels-class
#' @aliases LibraryOfPKPDModels
#' @docType class
#' @include GenericMethods.R
#' @include LibraryOfModels.R
#' @include LibraryOfPKModels.R
#' @include LibraryOfPDModels.R
#' @include ModelParameter.R
#' @export

LibraryOfPKPDModels = setClass(
  Class ="LibraryOfPKPDModels",
  contains = "LibraryOfModels",
  representation = representation())

setMethod(f="initialize",
          signature="LibraryOfPKPDModels",
          definition=function(.Object) {

            # --------------------------
            # Library Of PK & PD models
            # --------------------------

            libraryOfPKModels = LibraryOfModels( name = "LibraryOfPKModels" )
            libraryOfPDModels = LibraryOfModels( name = "LibraryOfPDModels" )

            libraryOfPKModels = addModels( libraryOfPKModels, LibraryOfPKModels() )
            libraryOfPDModels = addModels( libraryOfPDModels, LibraryOfPDModels() )

            libraryOfModels = list( libraryOfPKModels, libraryOfPDModels )

            .Object = addModels( .Object, libraryOfModels )

            names(.Object@content) = c( getName( libraryOfPKModels ), getName( libraryOfPDModels ) )

            return(.Object)
          })

#' Get a PK model.
#'
#' @name getPKModel
#' @param object An object from the class \linkS4class{LibraryOfPKPDModels}.
#' @param PKModelName A string giving the name of the PK model.
#' @return Return a PK model.
#' @export

setGeneric(
  "getPKModel",
  function( object, PKModelName ) {

    standardGeneric("getPKModel")
  })

#' @rdname getPKModel
#' @export

setMethod("getPKModel",
          "LibraryOfPKPDModels",
          function( object, PKModelName )
          {
            libraryPKModels = getLibraryPKModels( object )

            libraryPKModelsContent = getContent( libraryPKModels )

            PKModelNames = lapply( libraryPKModelsContent, function(x) getName(x))

            if ( ( PKModelName %in% PKModelNames )==FALSE )
            {
              PKModel = list()

            }else{

              indexPKModel = which( PKModelNames %in% PKModelName )

              PKModel = libraryPKModelsContent[indexPKModel]
              PKModel = PKModel[[1]]
            }

            return( PKModel )

          })

#' Get a PD model.
#'
#' @name getPDModel
#' @param object An object from the class \linkS4class{LibraryOfPKPDModels}.
#' @param PDModelName A string giving the name of the PD model.
#' @return Return a PD model.
#' @export

setGeneric(
  "getPDModel",
  function( object, PDModelName ) {

    standardGeneric("getPDModel")
  })

#' @rdname getPDModel
#' @export

setMethod("getPDModel",
          "LibraryOfPKPDModels",
          function( object, PDModelName )
          {
            libraryPDModels = getLibraryPDModels( object )

            libraryPDModelsContent = getContent( libraryPDModels )

            PDModelNames = lapply( libraryPDModelsContent, function(x) getName(x))

            if ( ( PDModelName %in% PDModelNames )==FALSE )
            {
              PDModel = list()

            }else{

              indexPDModel = which( PDModelNames %in% PDModelName )

              PDModel = libraryPDModelsContent[indexPDModel]

              PDModel = PDModel[[1]]
            }

            return( PDModel )
          })

#' Get a PKPD model.
#'
#' @name getPKPDModel
#' @param object An object from the class \linkS4class{LibraryOfPKPDModels}.
#' @param namesModel A vector of strings giving the names of the PK and PD models.
#' @return Return a PKPD model.
#' @export

setGeneric(
  "getPKPDModel",
  function( object, namesModel ) {

    standardGeneric("getPKPDModel")
  })

#' @rdname getPKPDModel
#' @export

setMethod("getPKPDModel",
          "LibraryOfPKPDModels",
          function( object, namesModel )
          {
            PKModel = list()
            PDModel = list()

            numberOfModels = length( namesModel )

            if ( numberOfModels == 1 )
            {
              PKModel = getPKModel( object, namesModel )
              PDModel = getPDModel( object, namesModel )

            }else if ( numberOfModels ==2 )
            {
              namePKModel = namesModel[1]
              namePDModel = namesModel[2]

              if ( !is.na( namePKModel ) )
              {
                PKModel = getPKModel( object, namePKModel )
              }

              if ( !is.na( namePDModel ) )
              {
                PDModel = getPDModel( object, namePDModel )
              }
            }

            model = list( PKModel = PKModel, PDModel = PDModel )

            return( model )
          })

##########################################################################################################
# END Function "LibraryOfPKPDModels"
##########################################################################################################




