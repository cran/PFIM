##################################################################################
#' Class "ModelParameter"
#' @description
#' Class \code{ModelParameter} represents a parameters theta
#' included in f(x,theta)
#' theta = {mu, covariance_matrix}
#' mu - parameter that acts in the individual model
#' covariance_matrix - additional parameter for the population model
#' theta_distribution - Distribution.
#'
#' @name ModelParameter-class
#' @aliases ModelParameter
#' @docType class
#' @include Distribution.R
#' @exportClass ModelParameter
#'
#' @section Objects from the class:
#' Objects form the class \code{ModelParameter} can be created by calls of the form \code{ModelParameter(...)} where
#' (...) are the parameters for the \code{ModelParameter} objects.
#'
#' @section Slots for ModelParameter objects:
#' \describe{
#' \item{\code{name}:}{A character string giving the name of the parameter.}
#' \item{\code{mu}:}{A numeric giving the value of the mean mu.}
#' \item{\code{omega}:}{A numeric giving the value of the variance.}
#' \item{\code{distribution}:}{An object of the class \code{Distribution}.}
#' \item{\code{fixed}:}{A boolean giving if the parameter is fixed or remain to be estimated.}
#' \item{\code{fixedMu}:}{A boolean giving if the mean mu is fixed or remain to be estimaed.}
#' }
##################################################################################

ModelParameter <- setClass(
  Class = "ModelParameter",
  representation = representation(
    name = "character",
    mu = "numeric",
    omega = "numeric",
    distribution = "Distribution",
    fixed = "logical",
    fixedMu = "logical",
    fixedOmega = "logical"
  ),
  prototype = prototype(
    omega = 0,
    fixedMu = F,
    fixedOmega = F,
    fixed = F
  )
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "ModelParameter",
  definition = function( .Object, name, mu, omega, distribution, fixed, fixedOmega, fixedMu )
  {
    if(!missing(name))
      .Object@name <-name
    if(!missing(mu))
      .Object@mu <-mu
    if(!missing(omega))
      .Object@omega <- omega
    if(!missing(distribution))
      .Object@distribution<-distribution
    if(!missing(fixed))
      .Object@fixed<-fixed
    if(!missing(fixedMu))
      .Object@fixedMu<-fixedMu
    if(!missing(fixedOmega))
      .Object@fixedOmega<-fixedOmega

    # for fixed parameters
    if ( omega ==0 ){
      .Object@fixedOmega <- TRUE
    }
    if ( mu ==0 ){
      .Object@fixedMu <- TRUE
    }

    validObject(.Object)
    return(.Object)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of a \code{ModelParameter} object.
#'
#' @name getNameModelParameter
#' @param object \code{ModelParameter} object.
#' @return A character string \code{name} giving the name of a \code{ModelParameter} object.

setGeneric("getNameModelParameter",
           function(object)
           {
             standardGeneric("getNameModelParameter")
           }
)


setMethod("getNameModelParameter",
          signature =  "ModelParameter",
          function(object)
          {
            return(object@name)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get \code{mu} for a \code{ModelParameter} object.
#'
#' @name getMu
#' @param object \code{ModelParameter} object.
#' @return A numeric \code{mu} giving the value of the mean \code{mu} for a \code{ModelParameter} object.

setGeneric("getMu",
           function(object)
           {
             standardGeneric("getMu")
           }
)

setMethod("getMu",
          "ModelParameter",
          function(object)
          {
            return( object@mu )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Boolean to set if a model parameters is fixed or not.
#'
#' @name isFixed
#' @param object \code{ModelParameter} object.
#' @return A boolean \code{fixed} giving TRUE if the model parameters is fixed, or FALSE is this parameters remain to be estimated.


setGeneric("isFixed",
           function(object)
           {
             standardGeneric("isFixed")
           }
)

setMethod("isFixed",
          "ModelParameter",
          function(object)
          {
            return( object@fixed )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Boolean to set if a model parameters is not fixed or not.
#'
#' @name isNotFixed
#' @param object \code{ModelParameter} object.
#' @return A boolean \code{isNotFixed} giving TRUE if the model parameters is not fixed, FALSE otherwise.

setGeneric("isNotFixed",
           function(object)
           {
             standardGeneric("isNotFixed")
           }
)

setMethod("isNotFixed",
          "ModelParameter",
          function(object)
          {
            return( !object@fixed )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Boolean to set if \code{mu} is fixed or not.
#'
#' @name isFixedMu
#' @param object \code{ModelParameter} object.
#' @return A boolean \code{isFixedMu} giving TRUE if \code{mu} is fixed, FALSE otherwise.

setGeneric("isFixedMu",
           function(object)
           {
             standardGeneric("isFixedMu")
           }
)

setMethod("isFixedMu",
          "ModelParameter",
          function(object)
          {
            return( object@fixedMu )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Boolean to set if \code{mu} is not fixed or not.
#'
#' @name isNotFixedMu
#' @param object \code{ModelParameter} object.
#' @return A boolean \code{isNotFixedMu} giving TRUE if \code{mu} is not fixed, FALSE otherwise.

setGeneric("isNotFixedMu",
           function(object)
           {
             standardGeneric("isNotFixedMu")
           }
)

setMethod("isNotFixedMu",
          "ModelParameter",
          function(object)
          {
            return( !object@fixedMu )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get \code{Omega} of a \code{ModelParameter} object.
#'
#' @name getOmega
#' @param object \code{ModelParameter} object.
#' @return A numeric \code{omega} giving the variance \code{Omega} of a \code{ModelParameter} object.

setGeneric("getOmega",
           function(object)
           {
             standardGeneric("getOmega")
           }
)

setMethod("getOmega",
          "ModelParameter",
          function(object)
          {
            return( object@omega )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the distribution of a \code{ModelParameter} object.
#'
#' @name getDistribution
#' @param object \code{ModelParameter} object.
#' @return The distribution given by \code{distribution} of a \code{ModelParameter} object.

setGeneric("getDistribution",
           function(object)
           {
             standardGeneric("getDistribution")
           }
)
setMethod("getDistribution",
          "ModelParameter",
          function(object)
          {
            return(object@distribution)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the derivates adjusted by distribution of a \code{ModelParameter} object.
#' @name getDerivatesAdjustedByDistribution
#' @param object \code{ModelParameter} object.
#' @param df_total df_total
#' @return A list of expression giving the derivates adjusted by distribution \code{distribution},
#' the mu \code{distribution} and the dftotal \code{distribution} of a \code{ModelParameter} object.

setGeneric("getDerivatesAdjustedByDistribution",
           function( object, df_total )
           {
             standardGeneric("getDerivatesAdjustedByDistribution")
           }
)
setMethod("getDerivatesAdjustedByDistribution",
          "ModelParameter",
          function( object, df_total )
          {
            distribution = getDistribution( object )

            if (class( distribution )[1] =="LogNormalDistribution" )
            {
              return( AdjustLogNormalDistribution( object@distribution, object@mu, df_total ) )
            }
            else if ( class( distribution )[1] =="NormalDistribution" )
            {
              return( AdjustNormalDistribution( object@distribution, object@mu, df_total ) )
            }
          }
)

##########################################################################################################
# END Class "ModelParameter"
##########################################################################################################
