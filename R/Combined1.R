#####################################################################################################################################
##' Class "Combined1"
##'
#' @description The class  \code{Combined1} defines the the residual error variance according
#' to the formula g(sigma_inter, sigma_slope, c_error, f(x, theta)) = sigma_inter + sigma_slope*f(x,theta)).
#'
#' @name Combined1-class
#' @aliases Combined1
#' @docType class
#' @include ModelError.R
#' @exportClass Combined1
#'
#' @section Objects from the class:
#' Combined1 objects are typically created by calls to \code{Combined1} and contain the following slots that are herited from
#' the class \linkS4class{Combined1c}:
#'
#' \describe{
#' \item{\code{.Object}:}{An object of the Class \linkS4class{Combined1}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model.}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model.}
#' }
#'
#####################################################################################################################################

Combined1<-setClass(
  Class="Combined1",
  contains = "ModelError",
  prototype = prototype(
    c_error = 1
  ),
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="Combined1",
  definition= function (.Object, sigma_inter, sigma_slope )
  {
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, sigma_inter = sigma_inter, sigma_slope = sigma_slope, c_error = 1, expression( sigma_inter + sigma_slope * f_x_i_theta ) )
    return (.Object )
  }
)

##########################################################################################################

#' Get the names of the variances.
#'
#' @rdname getSigmaNames
#' @param object An object \code{Combined1} from the class \code{Combined1}.
#' @return The character string \code{sigmaNames} giving the names of the variances.

setMethod("getSigmaNames",
          "Combined1",
          function(object)
          {
            sigmaNames <- c( )
            if(object@sigma_inter != 0)
              sigmaNames <- c( sigmaNames, "\u03c3_inter")
            if(object@sigma_slope != 0)
              sigmaNames <- c( sigmaNames, "\u03c3_slope" )
            return(sigmaNames)
          }
)

##########################################################################################################

#' Get the values of the variances \code{sigma_inter} and \code{sigma_slope}.
#'
#' @rdname getSigmaValues
#' @param object An object \code{Combined1} from the class \code{Combined1}.
#' @return A numeric vector giving the values of the variances \code{sigma_inter} and \code{sigma_slope}.

setMethod("getSigmaValues",
          "Combined1",
          function(object)
          {
            sigmaValues <- c( )
            if(object@sigma_inter != 0)
              sigmaValues <- c( sigmaValues, object@sigma_inter)
            if(object@sigma_slope != 0)
              sigmaValues <- c( sigmaValues, object@sigma_slope )
            return(sigmaValues)
          }

)

##########################################################################################################

#' Show the model errors
#'
#' @rdname show
#' @param object An object \code{Combined1} from the class \linkS4class{Combined1}
#' @return Display the model errors

setMethod(f="show",
          signature=  "Combined1",
          definition=function(object)
          {
            eq <- gsub("f_x_i_theta", "f", toString(object@equation))
            cat(" Error model combined 1 equation : ", eq, "\n")
            callNextMethod(object)

          }
)

###########################################################################
# End Class Combined1
###########################################################################
