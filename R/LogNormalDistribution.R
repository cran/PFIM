##################################################################################################
##' Class "LogNormalDistribution"
##'
##' @description Class \code{LogNormalDistribution} represent a Log-Normal distribution with
##' mean \code{mean_log_Gaussian} and standard deviation \code{sd_log_Gaussian}.
##'
##' @name LogNormalDistribution-class
##' @aliases LogNormalDistribution
##' @docType class
##' @include StandardDistribution.R
##' @exportClass LogNormalDistribution
##'
##' @section Objects from the Class: LogNormalDistribution objects
##' are typically created by calls to \code{\ {pfim}} and contain the following slots:
##'
##' \describe{
##' \item{\code{mean_log_Gaussian}:}{A numeric givnig the mean of the log Normal Distribution.}
##' \item{\code{sd_log_Gaussian}:}{A numeric giving the standard deviation of the log Normal Distribution.}
##' }
LogNormalDistribution <- setClass(Class = "LogNormalDistribution",
                               contains = "StandardDistribution",
                               representation = representation
                               (
                               ),
                               validity = function(object)
                               {
                                 return(TRUE)
                               }
)



setMethod(
  f="initialize",
  signature="LogNormalDistribution",
  definition= function ( .Object )
  {
    validObject(.Object)
    .Object = callNextMethod( .Object )
    return (.Object )
  }
)


#' Adjust the mean of a \code{\link{LogNormalDistribution}} object.
#'
#' @param object A \code{\link{AdjustLogNormalDistribution}}.
#' @param mu A numeric giving the mean mu.
#' @param df_total  numeric giving df_total
#'
#' @return A \code{StandardDistribution} object giving the adjusted Log Normal distribution.
#'
#' @rdname AdjustLogNormalDistribution
#'
#' @docType methods


setGeneric(
  "AdjustLogNormalDistribution",
  function(object, mu, df_total) {
    standardGeneric("AdjustLogNormalDistribution")
  })

#' @rdname AdjustLogNormalDistribution

setMethod( f = "AdjustLogNormalDistribution",
           signature = "LogNormalDistribution",
           definition = function( object, mu, df_total )
           {
             return( mu * df_total )
           }
)

##########################################################################################################
# END Class "LogNormalDistribution"
##########################################################################################################



