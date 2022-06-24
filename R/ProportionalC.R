##################################################################################
#' Class "ProportionalC"
#'
#' @description The Class "ProportionalC" defines the the residual error variance according
#' to the formula  g(sigma_inter, sigma_slope, c_error, f(x, theta)) =  sigma_slope*f(x,theta)^c_error.
#'
#' @name ProportionalC-class
#' @aliases ProportionalC
#' @docType class
#' @section Objects from the Class \linkS4class{ProportionalC}: objects
#' are typically created by calls to \code{ProportionalC} and contain the following slots
#' that are heritated from the class \linkS4class{Combined1c}:
#'
#'@section Slots for the \code{ProportionalC} objects:
#' \describe{
#' \item{\code{.Object}:}{An object of the Class \code{ProportionalC}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model}
#' \item{\code{c_error}:}{A numeric value giving the exponant c of the error model}
#' }
##'
##' @include Combined1c.R
##' @exportClass ProportionalC
##################################################################################

ProportionalC<-setClass(
  Class="ProportionalC",
  contains = "Combined1c",
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="ProportionalC",
  definition= function (.Object, sigma_slope, c_error)
  {
    sigma_inter = 0
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, sigma_inter, sigma_slope, c_error)
    return (.Object )
  }
)

##########################################################################################################
# END Class "ProportionalC"
##########################################################################################################

