#####################################################################################################################################
#' Class "Constant"
#'
#' @description The class \code{Constant} defines the the residual error variance according
#' to the formula g(sigma_inter, sigma_slope, c_error, f(x, theta)) = sigma_inter.
#'
#' @name Constant-class
#' @aliases Constant
#' @docType class
#' @include Combined1.R
#' @exportClass Constant
#'
#' @section Objects from the class: \code{Constant} objects
#' are typically created by calls to \code{Constant} and contain the following slots
#' that are heritated from the class Combined1:
#' \describe{
#' \item{\code{.Object}:}{An object of the class \linkS4class{ModelError}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model.}
#' }
#'
#####################################################################################################################################

Constant<-setClass(
  Class="Constant",
  contains = "Combined1",
  prototype = prototype(
    c_error = 1,
    sigma_slope = 0
  ),
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="Constant",
  definition= function (.Object, sigma_inter)
  {
    # Object validation
    validObject(.Object)
    .Object = callNextMethod( .Object, sigma_inter = sigma_inter, sigma_slope = 0 )
    return (.Object )
  }
)

##########################################################################################################

#' Get the variances \code{sigma_{inter}} and \code{sigma_{slope}}.
#'
#' @rdname getSigmaNames
#' @param object A \code{Combined1} object.
#' @return The variances \code{sigma_{inter}} and \code{sigma_{slope}}.


setMethod("getSigmaNames",
          "Constant",
          function(object)
          {
            sigmaNames <- c( "\u03c3_inter" )
            return(sigmaNames)
          }
)

##########################################################################################################

#' Get the values of the variances \code{sigma_{inter}} and \code{sigma_{slope}}.
#'
#' @rdname getSigmaValues
#' @param object A \code{Combined1} object.
#' @return A vector giving the values of the variances \code{sigma_{inter}} and \code{sigma_{slope}}.


setMethod("getSigmaValues",
          "Constant",
          function(object)
          {
            return(object@sigma_inter)
          }

)

##########################################################################################################

#' Show the model errors.
#'
#' @rdname show
#' @param object \code{Combined1} object.
#' @return The model errors.

setMethod(f="show",
          signature=  "Constant",
          definition=function(object)
          {
            cat(" Error model constant (sigma_inter) = ",object@sigma_inter, "\n")
          }
)

###########################################################################
# End Class "Constant"
###########################################################################
