##################################################################################
#' Class "Proportional"
#'
#' @description The Class "Proportional" defines the the residual error variance according
#' to the formula  g(sigma_inter, sigma_slope, c_error, f(x, theta)) =  sigma_slope*f(x,theta).
#'
#' @name Proportional-class
#' @aliases Proportional
#' @docType class
#' @include Combined1.R
#' @exportClass Proportional
#'
#' @section Objects from the Class \linkS4class{Proportional}:
#' Objects are typically created by calls to \code{Proportional} and contain the following slots
#' that are heritated from the class \linkS4class{Combined1}:
#'
#' @section Slots for the \code{Proportional} objects:
#' \describe{
#' \item{\code{.Object}:}{An object of the Class \code{Proportional}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model}
#' }
##################################################################################

Proportional<-setClass(
  Class="Proportional",
  contains = "Combined1",
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="Proportional",
  definition= function (.Object, sigma_slope)
  {
    sigma_inter = 0
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, sigma_inter, sigma_slope)
    return (.Object )
  }
)

##########################################################################################################
# END Class "Proportional"
##########################################################################################################


