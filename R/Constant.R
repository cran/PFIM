#' Class "Constant"
#'
#' @description The class \code{Constant} defines the the residual error variance according
#' to the formula g(sigma_inter, sigma_slope, c_error, f(x, theta)) = sigma_inter.
#' The class \code{Constant} inherits from the class \code{ModelError}.
#'
#' @name Constant-class
#' @aliases Constant
#' @docType class
#' @include Combined1.R
#' @export
#'
#' @section Objects from the class: \code{Constant} objects are typically created by calls to \code{Constant}
#' and contain the following slots that are inherited from the class \linkS4class{ModelError}:
#'
#' \describe{
#' \item{\code{outcome}:}{A string giving the name of the outcome.}
#' \item{\code{equation}:}{An symbolic expression of the model error.}
#' \item{\code{derivatives}:}{A list containing the derivatives of the model error expression.}
#' \item{\code{sigmaInter}:}{A numeric value giving the sigma inter of the error model.}
#' \item{\code{sigmaSlope}:}{A numeric value giving the sigma slope of the error model.}
#' \item{\code{cError}:}{A numeric value giving the exponant c of the error model.}
#' }

Constant = setClass(Class = "Constant",
                     contains = "ModelError",
                     representation = representation
                     (
                       outcome = "character",
                       equation = "expression",
                       derivatives = "list",
                       sigmaInter = "numeric",
                       sigmaSlope = "numeric",
                       cError = "numeric"
                     ),
                     prototype = prototype( cError = 1,
                                            sigmaSlope = 0,
                                            equation =  parse(text = "sigmaInter")
                     ))

setMethod(
  f="initialize",
  signature="Constant",
  definition= function (.Object, outcome, equation, derivatives, sigmaInter, sigmaSlope, cError )
  {
    if(!missing(outcome))
    {
      .Object@outcome = outcome
    }
    if(!missing(equation))
    {
      .Object@equation = equation
    }
    if(!missing(derivatives))
    {
      .Object@derivatives = derivatives
    }
    if(!missing(sigmaInter))
    {
      .Object@sigmaInter = sigmaInter
    }
    if(!missing(sigmaSlope))
    {
      .Object@sigmaSlope = sigmaSlope
    }
    if( !missing( cError ) )
    {
      .Object@cError = cError
    }

    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################
# End class Constant
##########################################################################################################

















