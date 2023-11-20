#' Class "Combined1"
#'
#' @description The class \code{Combined1} defines the the residual error variance according
#' to the formula g(sigmaInter, sigmaSlope, cError, f(x, theta)) = sigmaInter + sigmaSlope*f(x,theta)).
#' The class \code{Combined1} inherits from the class \code{ModelError}.
#'
#' @name Combined1-class
#' @aliases Combined1
#' @docType class
#' @include ModelError.R
#' @export
#'
#' @section Objects from the class:
#' Combined1 objects are typically created by calls to \code{Combined1} and contain the following slots that are inherited from
#' the class \linkS4class{ModelError}:
#'
#' \describe{
#' \item{\code{outcome}:}{A string giving the name of the outcome.}
#' \item{\code{equation}:}{An symbolic expression of the model error.}
#' \item{\code{derivatives}:}{A list containing the derivatives of the model error expression.}
#' \item{\code{sigmaInter}:}{A numeric value giving the sigma inter of the error model.}
#' \item{\code{sigmaSlope}:}{A numeric value giving the sigma slope of the error model.}
#' \item{\code{cError}:}{A numeric value giving the exponant c of the error model.}
#' }

Combined1 = setClass(Class = "Combined1",
                     contains = "ModelError",
                     representation = representation
                     (
                       outcome="character",
                       equation = "expression",
                       derivatives = "list",
                       sigmaInter = "numeric",
                       sigmaSlope = "numeric",
                       cError = "numeric"
                     ),
                     prototype = prototype( cError = 1,
                                            equation =  expression(sigmaInter + sigmaSlope * evaluationOutcome )
                     ))

setMethod( f="initialize",
           signature="Combined1",
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
# End class Combined1
##########################################################################################################








