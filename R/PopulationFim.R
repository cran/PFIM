##################################################################################
##' Class "PopulationFim"
##'
##' @description
##' Class "PopulationFim" representing the population Fisher information matrix
##'
##' A class storing information regarding the population Fisher computation matrix
##' (computation method: first order linearisation (FO))
##'
##' @name PopulationFim-class
##' @aliases PopulationFim
##' @docType class
##' @include Fim.R
##' @exportClass PopulationFim
##'
#' @section Objects from the class \code{PopulationFim}:
#' Objects form the class \code{PopulationFim} can be created by calls of the form \code{PopulationFim(...)} where
#' (...) are the parameters for the \code{PopulationFim} objects.
#'
#'@section Slots for \code{PopulationFim} objects:
#'  \describe{
#'    \item{\code{mfisher}:}{A matrix giving the Fisher Information matrix.}
#'  }
##################################################################################

PopulationFim<-setClass(
  Class="PopulationFim",
  contains = "Fim",
  validity=function(object){
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="PopulationFim",
  definition= function (.Object, mfisher ){
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, mfisher)
    return (.Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Compute expected standard error data frame.
#'
#' @rdname getStatisticalModelStandardErrors
#' @param object A \code{Fim} object.
#' @param modelParameters A character string giving the model parameters.
#' @return A data frame giving the expected standard error.

setMethod(
  f ="getStatisticalModelStandardErrors",
  signature = "PopulationFim",
  definition = function (object, modelParameters)
  {

    se <- getSE(object)

    muCol <- c()
    rseCol<- c()
    i = 1
    nbParameter = 0
    for( parameter in modelParameters)
    {
      if ( isFixed( parameter ) )
        next
      muCol <- c(muCol, getMu(parameter))
      rseCol <- c(rseCol, se[ nbParameter + 1 ]/getMu(parameter) *100)
      nbParameter = nbParameter + 1
    }

    FixedEffectFrame <- data.frame(value = muCol, se = se[ 1:nbParameter ], "rse"= rseCol,  check.names=FALSE)

    se <- se[-c(1:nbParameter)]

    omegaCol<- c()
    rseCol <- c()

    for(parameter in modelParameters)
    {
      if ( isFixed( parameter ) )
        next
      if(getOmega(parameter) != 0)
      {
        omegaCol <- c(omegaCol, getOmega(parameter)^2)

        rseCol <- c(rseCol, se[( length(rseCol) + 1)]/getOmega(parameter)^2*100)

      }
    }

    RandomEffectFrame <- data.frame(value = omegaCol, se = se[1:length(omegaCol)], "rse"= rseCol, check.names=FALSE)

    return(list(fixed = FixedEffectFrame, random =RandomEffectFrame))
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the statistical model standards errors.
#'
#' @rdname showStatisticalModelStandardErrors
#' @param object An \code{IndividualFim} object.
#' @param modelParameters A \code{modelParameters.} object.
#' @return A dataframe giving the model standards errors.

setMethod("showStatisticalModelStandardErrors",
          "PopulationFim",
          function (object, modelParameters)
          {
            cat("\n Expected standard errors\n**** fixed effects \n")
            effectFrames <- getStatisticalModelStandardErrors(object, modelParameters)
            print(effectFrames$fixed)

            cat("\n**** variance of random effects \n")
            print(effectFrames$random)
          }

)

# -------------------------------------------------------------------------------------------------------------------
#' Get the type of the Fim.
#'
#' @rdname getDescription
#' @return A string giving the type of the Fim.

setMethod(
  f ="getDescription",
  signature = "PopulationFim",
  definition = function ( object )
  {
    return( "Population Fisher information matrix")
  }
)

# ------------------------------------------------------------------------
#' Finalize the Fim for one elementary design.
#'
#' @rdname FinalizeFIMForOneElementaryDesign
#' @return The Fim times size of the arm.

setMethod(
  f ="FinalizeFIMForOneElementaryDesign",
  signature = "PopulationFim",
  definition = function(object, arm)
  {
    object@mfisher <- object@mfisher * getArmSize( arm )
    return(object)
  }
)

########################################################################################################
# END Class "PopulationFim"
########################################################################################################
