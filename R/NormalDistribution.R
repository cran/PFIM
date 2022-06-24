##################################################################################
##' Class "NormalDistribution"
##'
##' @description  Class \code{LogNormalDistribution} represent a Normal distribution
##'
##' @name NormalDistribution-class
##' @aliases NormalDistribution
##' @docType class
##' @include StandardDistribution.R
##' @exportClass NormalDistribution
##'
##' @section Objects from the class: \code{LogNormalDistribution} objects
##' are typically created by calls to the class \code{NormalDistribution}.


NormalDistribution <- setClass(Class = "NormalDistribution",
                               contains = "StandardDistribution",
                               representation = representation
                               (
                               ),
                               validity = function(object)
                               {
                                   return(TRUE)
                               }
)
##################################################################################

setMethod(
  f="initialize",
  signature="NormalDistribution",
  definition= function ( .Object )
    {
      validObject(.Object)
      .Object = callNextMethod( .Object )
      return (.Object )
  }
)


#' Adjust the Normal Distribution.
#'
#' @param object A \code{\link{NormalDistribution}}.
#' @param mu A numeric giving the mean mu.
#' @param df_total  numeric giving df_total
#'
#' @return A \code{StandardDistribution} object giving the adjusted Normal distribution.
#'
#' @rdname AdjustNormalDistribution
#'
#' @docType methods

setGeneric("AdjustNormalDistribution",
           function( object, mu, df_total)
           {
             standardGeneric("AdjustNormalDistribution")
           }
)

#' @rdname AdjustNormalDistribution

setMethod( f = "AdjustNormalDistribution",
          signature = "NormalDistribution",
          definition = function( object, mu, df_total )
          {
            return( df_total )
          }
)

##########################################################################################################
# END Class "NormalDistribution"
##########################################################################################################


