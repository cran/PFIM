##################################################################################
#' Class "individualFim" representing the individual Fisher information matrix
#'
#' @description
#' A class storing information regarding the individual Fisher computation matrix.
#'
#' @name IndividualFim-class
#' @aliases IndividualFim
#' @include Fim.R
#' @docType class
#' @exportClass IndividualFim
#'
#' @section Objects from the class: IndividualFim objects are typically created by calls to \code{{fim}} or \code{{pfim}} and contain the following slots:
#'
#' \describe{
#' \item{IndividualFim}{Create a new object \code{{Fim}}}
#' }
#'

IndividualFim<-setClass(
  Class="IndividualFim",
  contains = "Fim",
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="IndividualFim",
  definition= function (.Object, mfisher )
  {
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, mfisher)
    return (.Object )
  }
)

##########################################################################################################

#' Get the SE of \code{IndividualFim} object.
#' @rdname getStatisticalModelStandardErrors
#' @param object A \code{IndividualFim} object.
#' @param modelParameters A vector of character strings giving the parameters of the model.
#' @return A list giving the fixed effects of \code{IndividualFim} object.


setMethod(
  f ="getStatisticalModelStandardErrors",
  signature = "IndividualFim",
  definition = function (object, modelParameters)
  {
    se <- getSE(object)

    muCol <- c()
    rseCol<- c()
    for( parameter in modelParameters)
    {
      muCol <- c(muCol, getMu(parameter))
      rseCol <- c(rseCol, se[(length(rseCol) + 1)]/getMu(parameter) * 100)
    }
    FixedEffectFrame <- data.frame(value = muCol, se = se[1:length(modelParameters)], "rse"= rseCol, check.names=FALSE)

    return(list(fixed = FixedEffectFrame))
  }
)

#######################################################################################

#' Show the statistical model standard errors
#'
#' @rdname showStatisticalModelStandardErrors
#' @param object An \code{IndividualFim} object.
#' @param modelParameters An object \code{modelParameters} giving the model parameters.
#' @return A dataframe giving the standard errors.

setMethod("showStatisticalModelStandardErrors",
          "IndividualFim",
          function(object, modelParameters)
          {
            cat("\n Expected standard errors\n**** fixed effects \n")
            EffectFrame <- getStatisticalModelStandardErrors(object, modelParameters)
            print(EffectFrame$fixed)
          }
)

##########################################################################################################

#' Show the Individual Fim.
#' @rdname show
#' @param object \code{IndividualFim} object.
#' @return Show the Individual Fim.
#'
setMethod(f = "show",
          signature = "IndividualFim",
          definition = function(object)
          {
            callNextMethod(object)
          }
)

##########################################################################################################

#' @rdname getDescription
#' @return Return a string giving the type of the Fim.
setMethod(
  f ="getDescription",
  signature = "IndividualFim",
  definition = function ( object )
  {
    return( "Individual Fisher information matrix")
  }
)
########################################################################################################
# END Class "IndividualFim"
########################################################################################################


