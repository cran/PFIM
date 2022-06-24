##################################################################################
##' Class "Fim" representing the Fisher information matrix,
##' a parent class used by three classes PopulationFim, IndividualFim and BayesianFim.
##'
##' @description
##' A class storing information regarding the Fisher computation matrix.
##' Type of the Fisher information: population ("PopulationFIM"), individual ("IndividualFIM") or Bayesian ("BayesianFIM").
##' The computation method for population and Bayesian matrix is first order linearisation (FO).
##  adaptive Gaussian quadrature (AGQ) or Markov Chain Monte Carlo (MCMC))
##'
##' @name Fim-class
##' @aliases Fim
##' @docType class
##' @exportClass Fim
##'
#' @section Objects from the class:
#' Objects form the class \code{Fim} can be created by calls of the form \code{Fim(...)} where
#' (...) are the parameters for the \code{Fim} objects.
#'
#'@section Slots for \code{Fim} objects:
##' \describe{
##' \item{\code{isOptimizationResult}:}{A Boolean giving TRUE for an optimization result and FALSE an evaluation result.}
##' \item{\code{mfisher}:}{A matrix of numeric giving the Fisher information.}
##' \item{\code{omega}:}{A matrix of numeric giving the variances.}
##' \item{\code{mu}:}{A matrix of numeric giving the means.}
##' \item{\code{fim_comput_method}:}{Name of the method used to approximate the population matrix : character strings, 'FO' }
##' }

Fim<-setClass(
  Class="Fim",
  representation=representation(
    isOptimizationResult = "logical",
    mfisher="matrix", # the Fisher information matrix
    omega="matrix",
    mu="vector",
    parametersIndicesMuFIM ="vector",
    parametersIndicesOmegaFIM ="vector"
  ),
  prototype=prototype(
    isOptimizationResult = FALSE
 )
  ,
  validity=function(object)
  {
    return(TRUE)
  }
)


# Initialize method
setMethod(
  f="initialize",
  signature="Fim",
  definition= function (.Object, mfisher, omega, mu, parametersIndicesMuFIM, parametersIndicesOmegaFIM)
  {
    if(!missing(mfisher))
      .Object@mfisher<-mfisher

    if(!missing(omega))
      .Object@omega<-omega

    if(!missing(mu))
      .Object@mu<-mu

    if(!missing(parametersIndicesMuFIM))
      .Object@parametersIndicesMuFIM<-parametersIndicesMuFIM

    if(!missing(parametersIndicesOmegaFIM))
      .Object@parametersIndicesOmegaFIM<-parametersIndicesOmegaFIM

    validObject(.Object)
    return (.Object )
  }
)


##########################################################################################################

#' Get the Fisher Information Matrix.
#' @name getMfisher
#' @param object A \code{Fim} object.
#' @return A matrix of numeric \code{mfisher} giving the Fisher Information Matrix.
#'
setGeneric("getMfisher",
           function(object)
           {
             standardGeneric("getMfisher")
           }
)
setMethod("getMfisher",
          "Fim",
          function(object)
          {
            return(object@mfisher)
          }
)


############################################################################################

#' FinalizeFIMForOneElementaryDesign
#'
#' @param object A \code{Fim} object.
#' @param arm A \code{Arm} object.
#' @return A matrix of numeric \code{mfisher} giving the Fisher Information Matrix.
#'
#' @rdname FinalizeFIMForOneElementaryDesign
#'
#' @docType methods

setGeneric("FinalizeFIMForOneElementaryDesign",
           function(object, arm)
           {
             standardGeneric("FinalizeFIMForOneElementaryDesign")
           }
)

setMethod(
  f ="FinalizeFIMForOneElementaryDesign",
  signature = "Fim",
  definition = function(object, arm)
  {
    return(object)
  }
)

##########################################################################################################

#' Set a matrix value for the Fisher Information Matrix.
#' @name setMfisher<-
#' @param object A \code{Fim} object.
#' @param value A matrix of numerical values.
#' @return The \code{Fim} object with the Fisher Information Matrix with the new values.

setGeneric("setMfisher<-",
           function(object, value)
           {
             standardGeneric("setMfisher<-")
           }
)
setReplaceMethod( f="setMfisher",
                  signature="Fim",
                  definition = function(object, value)
                  {
                    object@mfisher <- as(value,"matrix")
                    return(object)
                  }
)

##########################################################################################################

#' Set the Omega matrix.
#' @name setOmega
#' @param object A \code{Fim} object.
#' @param omega A matrix omega giving the new values of the variances.
#' @return The \code{Fim} object with the Omega matrix with the new values.

setGeneric( "setOmega",
            function( object, omega )
            {
              standardGeneric("setOmega")
            }
)

setMethod(
  f ="setOmega",
  signature = "Fim",
  definition = function ( object, omega )
  {
    object@omega = omega
    return( object )
  }
)


##########################################################################################################

#' Set the mu vector.
#' @name setMu
#' @param object A \code{Fim} object.
#' @param mu A vector mu of the new values of mu.
#' @return The \code{Fim} object with the mu vector with the new values.

setGeneric( "setMu",
            function( object, mu )
            {
              standardGeneric("setMu")
            }
)

setMethod(
  f ="setMu",
  signature = "Fim",
  definition = function ( object, mu )
  {
    object@mu = mu
    return( object )
  }
)

##########################################################################################################

#' Get the description of FIM.
#'
#' @param object A \code{Fim} object.
#'
#' @return A character string that tells you that is a Fisher information matrix.
#'
#' @rdname getDescription
#'
#' @docType methods


setGeneric("getDescription",
           function( object )
           {
             standardGeneric("getDescription")
           }
)

##########################################################################################################

#' Get the Determinant of a Fisher Information Matrix.
#' @name getDeterminant
#' @param object \code{Fim} object.
#' @return A numeric \code{Det} giving the determinant of a Fisher Information Matrix.

setGeneric("getDeterminant",
           function(object)
           {
             standardGeneric("getDeterminant")
           }
)

setMethod(
  f ="getDeterminant",
  signature = "Fim",
  definition = function (object)
  {
    Det<-det(object@mfisher)
    return(Det)
  }
)

##########################################################################################################

#' Get the D-criterion for a \code{Fim} object.
#' @name getDcriterion
#' @param object A \code{Fim} object.
#' @return A numeric \code{Dcriterion} giving the D-criterion of a Fisher Information Matrix.

setGeneric("getDcriterion",
           function(object)
           {
             standardGeneric("getDcriterion")
           }
)

setMethod(
  f = "getDcriterion",
  signature = "Fim",
  definition = function(object)
  {
    Dcriterion <- det(object@mfisher)^ (1/dim(object@mfisher)[1])
    return(Dcriterion)
  }
)

##########################################################################################################

#' Get the Standard Errors for a \code{Fim} object..
#' @name getSE
#' @param object A \code{Fim} object.
#' @return A vector of numerical values giving the Standard Errors from the Fisher Information Matrix.

setGeneric("getSE",
           function(object)
           {
             standardGeneric("getSE")
           }
)

setMethod(
  f ="getSE",
  signature = "Fim",
  definition = function (object)
  {
    SE<-sqrt(diag(solve(object@mfisher)))
    return(SE)
  }
)

##########################################################################################################

#' Get the correlation matrix of the Fisher Information Matrix for a \code{Fim} object.
#' @name getCorr
#' @param object A \code{Fim} object.
#' @return A matrix of numerical values \code{corr_mat} giving the correlation matrix from the Fisher Information Matrix.

setGeneric("getCorr",
           function(object)
           {
             standardGeneric("getCorr")
           }
)

setMethod(
  f ="getCorr",
  signature = "Fim",
  definition = function (object)
  {
    corr_mat<-cov2cor(solve(object@mfisher))
    return(corr_mat)
  }
)

##########################################################################################################

#' Get the eigen values of the Fisher Information Matrix for a \code{Fim} object.
#' @name getEigenValue
#' @param object A \code{Fim} object.
#' @return A vector of numerical values \code{EV} giving the eigen values of the \code{Fim} object.

setGeneric("getEigenValue",
           function(object)
           {
             standardGeneric("getEigenValue")
           }
)

setMethod(
  f ="getEigenValue",
  signature = "Fim",
  definition = function (object)
  {
    EV<-eigen(object@mfisher)$values
    return(EV)
  }
)

##########################################################################################################

#' Get the Condition Number Matrix of the Fisher Information Matrix for a \code{Fim} object..
#' @name getConditionNumberMatrix
#' @param object A \code{Fim} object.
#' @param FixedEffectParameterNumber A numerical giving the number of Fixed Effect Parameters.
#' @return A matrix \code{conditionNumbers} of numerical values giving the Condition Number Matrix the min, max and min/max for the FixedEffects and VarianceComponents.

setGeneric("getConditionNumberMatrix",
           function(object, FixedEffectParameterNumber)
           {
             standardGeneric("getConditionNumberMatrix")
           }
)

setMethod(
  f ="getConditionNumberMatrix",
  signature = "Fim",
  definition = function (object, FixedEffectParameterNumber)
  {
    EV<-eigen(object@mfisher)$values

    minFixedEffect <- min(EV[1:FixedEffectParameterNumber])
    maxFixedEffect <- max(EV[1:FixedEffectParameterNumber])

    minRandomEffect <- min(EV[(FixedEffectParameterNumber+1):length(EV)])
    maxRandomEffect <- max(EV[(FixedEffectParameterNumber+1):length(EV)])

    conditionNumbers <- matrix(c(minFixedEffect, maxFixedEffect,maxFixedEffect/minFixedEffect, minRandomEffect, maxRandomEffect, maxRandomEffect/minRandomEffect ), ncol =2)
    colnames(conditionNumbers) <- c("FixedEffects ", "VarianceComponents")
    rownames(conditionNumbers) <- c("min", "max", "max/min")
    return(conditionNumbers)
  }
)



##########################################################################################################

#' Show the Fisher Information Matrix for a \code{Fim} object and its information: Determinant, D-criterion, SE, Eigenvalues, Correlation.
#'
#' @rdname show
#' @param object A \code{Fim} object.
#' @return Print the Fisher Information Matrix and its informations: Determinant, D-criterion, SE, Eigenvalues, Correlation.

setMethod(f = "show",
          signature = "Fim",
          definition = function(object)
          {
            cat("Fisher matrix\n")
            return()
            #print( object@mfisher )
            # MF <- object@mfisher
            #
            # if( any( is.na( MF[1][1] ) ) )
            #   cat(" This design have not be evaluated.\n\n")
            # else
            # {
            #   if(object@isOptimizationResult)
            #     cat(" This is an optimization result.\n")
            #   else
            #     cat(" This is an evaluation result.\n")
            #
            #   cat( "\n ", getDescription( object ), "\n" )
            #
            #   print( getMfisher(object) )
            #
            #   tryCatch({
            #     cat("\n Determinant\n")
            #     cat( getDeterminant(object),"\n" )
            #   },error=function(e){
            #     cat("Determinant cannot be calculated.\n")
            #   })
            #
            #   tryCatch({
            #     cat("\n D-criterion\n")
            #     cat( getDcriterion(object),"\n" )
            #   },error=function(e){
            #     cat("D-criterion cannot be calculated.\n" )
            #   })
            #
            #   cat("\n SE\n")
            #   print( getSE(object) )
            #
            #   tryCatch({
            #     cat("\n EigenValue \n")
            #     eigens <- getEigenValue(object)
            #     names(eigens) <- names(getSE(object))
            #     print( eigens )
            #   },error=function(e){
            #     cat("EigenValues cannot be calculated.\n" )
            #   })
            #
            #   tryCatch({
            #     cat("\n Correlation\n")
            #     print( getCorr(object) )
            #   },error=function(e){
            #     cat("Correlations cannot be calculated.\n" )
            #   })
            #
            #   return(getSE(object))
            # }
          }
)

##########################################################################################################

# Compute expected standard error data frame
#' @name getStatisticalModelStandardErrors
#' @rdname getStatisticalModelStandardErrors
#' @aliases getStatisticalModelStandardErrors
#' @param object A \code{Fim} object.
#' @param modelParameters A character string giving the model parameters.
#' @return A data frame giving the expected standard error.

setGeneric("getStatisticalModelStandardErrors",
           function(object, modelParameters)
           {
             standardGeneric("getStatisticalModelStandardErrors")
           }
)

##########################################################################################################

#' Show expected standard error data frame.
#'
#' @name showStatisticalModelStandardErrors
#' @rdname showStatisticalModelStandardErrors
#' @aliases showStatisticalModelStandardErrors
#' @param object A \code{Fim} object.
#' @param modelParameters A character string giving the model parameters.
#' @return A data frame giving the standard error.

setGeneric("showStatisticalModelStandardErrors",
           function(object, modelParameters)
           {
             standardGeneric("showStatisticalModelStandardErrors")
           }
)

##########################################################################################################
# END Class "Fim"
##########################################################################################################


