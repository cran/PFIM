#####################################################################################################################################
#' Class "Combined2c"
#'
#' @description The class \code{Combined2c} defines the the residual error variance according
#' to the formula g(sigma_inter, sigma_slope, c_error, f(x, theta)) = sigma_inter^2 + sigma_slope^2 * f(x,theta)^(2 * c_error)

#' @name Combined2c-class
#' @aliases Combined2c
#' @docType class
#' @include ModelError.R
#' @exportClass Combined2c
#'
#' @section Objects from the class: \code{Combined2c}  objects
#' are typically created by calls to \code{Combined2c} and contain the following slots that are heritated
#' from the class \linkS4class{ModelError}:
#' \describe{
#' \item{\code{.Object}:}{An object of the class \linkS4class{ModelError}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model.}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model.}
#' \item{\code{c_error}:}{A numeric value giving the exponant c of the error model.}
#' }
#'
#####################################################################################################################################

Combined2c<-setClass(
  Class="Combined2c",
  contains = "ModelError",
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="Combined2c",
  definition= function (.Object, sigma_inter, sigma_slope, c_error )
  {
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, sigma_inter, sigma_slope, c_error, equation = expression( sigma_inter ^ 2 + sigma_slope ^ 2 * f_x_i_theta ^ ( 2 * c_error ) )  )
    return (.Object )
  }
)

##########################################################################################################

#' Get the names of the variances.
#'
#' @rdname getSigmaNames
#' @param object An object \code{Combined2c} from the class \code{Combined2c}.
#' @return The character string \code{sigmaNames} giving the names of the variances.

setMethod("getSigmaNames",
          "Combined2c",
          function(object)
          {
            sigmaNames <- c( )
            if(object@sigma_inter != 0)
              sigmaNames <- c( sigmaNames, "\u03c3_inter")
            if(object@sigma_slope != 0)
              sigmaNames <- c( sigmaNames, "\u03c3_slope" )
            if(object@c_error != 0)
              sigmaNames <- c( sigmaNames, "c_error" )
            return(sigmaNames)
          }
)

##########################################################################################################

#' Get the values of the variances \code{sigma_inter} and \code{sigma_slope}.
#'
#' @rdname getSigmaValues
#' @param object An object \code{Combined2c} from the class \code{Combined2c}.
#' @return A numeric vector giving the values of the variances \code{sigma_inter} and \code{sigma_slope}.


setMethod("getSigmaValues",
          "Combined2c",
          function(object)
          {
            sigmaValues <- c( )
            if(object@sigma_inter != 0)
              sigmaValues <- c( sigmaValues, object@sigma_inter)
            if(object@sigma_slope != 0)
              sigmaValues <- c( sigmaValues, object@sigma_slope )
            if(object@c_error != 0)
              sigmaValues <- c( sigmaValues, object@c_error )
            return(sigmaValues)
          }

)

##########################################################################################################

#' Show the model errors.
#'
#' @rdname show
#' @param object An object \code{Combined2c} from the class \code{Combined2c}.
#' @return Display the model errors.

setMethod(f="show",
          signature=  "Combined2c",
          definition=function(object)
          {
            eq <- gsub("f_x_i_theta", "f", toString(object@equation))
            cat(" Error model combined 2c equation : ",eq, "\n")
            callNextMethod(object)
          }
)

###########################################################################
# End Class Combined2c
###########################################################################

