##########################################################################################################
#' Class "BayesianFim" representing the population Fisher information matrix.
#'
#' @description
#' A class storing information regarding the population Fisher computation matrix.
#' @name BayesianFim-class
#' @docType class
#' @include IndividualFim.R
#' @section Objects from the class:
#' \code{BayesianFim} objects are typically created by calls to \code{{BayesianFim}}:
#' \describe{
#' \item{Bayesianfim:}{Create a new \code{BayesianFim}}
#' }
#'
#'@include IndividualFim.R
#' @exportClass BayesianFim
#'
##########################################################################################################

BayesianFim<-setClass(
  Class="BayesianFim",
  contains = "IndividualFim"
)

##########################################################################################################

#' Calculates the shrinkage of individual parameters from a \code{BayesianFim} object.
#'
#' @name getShrinkage
#' @param object An object \code{BayesianFim} from the class \linkS4class{BayesianFim}.
#' @return A numeric vector giving the shrinkage of individual parameters from a Bayesian matrix.

setGeneric("getShrinkage",
           function( object )
           {
             standardGeneric("getShrinkage")
           }
)

setMethod(
  f ="getShrinkage",
  signature = "BayesianFim",
  definition = function ( object )
  {
    mu = as.matrix(object@mu)
    omega = as.matrix(object@omega)
    mfisher =  as.matrix(object@mfisher)
    shrinkage <- diag( chol2inv( chol( mfisher ) )%*% chol2inv(chol( mu %*% omega %*% mu ) ) ) * 100

    return(shrinkage)
  }
)

##########################################################################################################

#' Get the description \code{BayesianFim} object.
#' @rdname getDescription
#' @return A string giving the description of the object \code{BayesianFim}.

setMethod(
  f ="getDescription",
  signature = "BayesianFim",
  definition = function ( object )
  {
    return( "Bayesian Fisher information matrix")
  }
)


###########################################################################
# End Class BayesianFIM
###########################################################################
