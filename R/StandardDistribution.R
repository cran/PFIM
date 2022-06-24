
##########################################################################################################
#' Class "StandardDistribution"
#'
#' @description
#' Class \code{StandardDistribution} represents class for standard distributions
#'
#' @include Distribution.R
#' @exportClass StandardDistribution
##########################################################################################################

StandardDistribution <- setClass(
  Class = "StandardDistribution",
  contains = "Distribution",
  representation = representation(
    "VIRTUAL"
  ),
  validity = function(object)
  {
    return(TRUE)
  }
)

setMethod(
  f="initialize",
  signature="StandardDistribution",
  definition= function (.Object )
  {
    validObject(.Object)
    .Object = callNextMethod(.Object )
    return (.Object )
  }
)

##########################################################################################################
# END Class "StandardDistribution"
##########################################################################################################
