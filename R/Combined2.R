#####################################################################################################################################
#' Class "Combined2"
#'
#' @description The class \code{{Combined2}} defines the the residual error variance according
#' to the formula g(sigma_inter, sigma_slope, c_error, f(x, theta)) = sigma_inter^2 + sigma_slope^2 * f_x_i_theta.
#'
#' @name Combined2-class
#' @aliases Combined2
#' @docType class
#' @include Combined2c.R
#' @exportClass Combined2
#'
#' @section Objects from the class:\code{{Combined2}} objects are typically created by calls to \code{{Combined2}}
#' and contain the following slots that are heritated from the class \linkS4class{Combined2c}:
#' \describe{
#' \item{\code{.Object}:}{An object of the class \linkS4class{ModelError}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model.}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model.}
#' }
#'
#####################################################################################################################################

Combined2<-setClass(
  Class="Combined2",
  contains = "Combined2c",
  validity=function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f="initialize",
  signature="Combined2",
  definition= function (.Object, sigma_inter, sigma_slope)
  {
    c_error = 1
    # Object validation
    validObject(.Object)
    .Object = callNextMethod(.Object, sigma_inter, sigma_slope, c_error, equation = expression( sigma_inter ^ 2 + sigma_slope ^ 2 * f_x_i_theta ) )
    return (.Object )
  }
)

##########################################################################################################

#' Get the names of the variances.
#'
#' @rdname getSigmaNames
#' @param object An object \code{Combined2} from the class \code{Combined2}.
#' @return The character string \code{sigmaNames} giving the names of the variances.


setMethod("getSigmaNames",
          "Combined2",
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
#' @param object An object \code{Combined2} from the class \code{Combined2}.
#' @return A numeric vector giving the values of the variances \code{sigma_inter} and \code{sigma_slope}.

setMethod("getSigmaValues",
          "Combined2",
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

#' Show the model errors.
#'
#' @rdname show
#' @param object An object \code{Combined2} from the class \code{Combined2}.
#' @return Display the model errors.

setMethod(f="show",
          signature=  "Combined2",
          definition=function(object)
          {
            eq <- gsub("f_x_i_theta", "f", toString(object@equation))
            cat(" Error model combined 2c equation : ",eq, "\n")
            callNextMethod(object)
          }
)

###########################################################################
# End Class Combined2
###########################################################################

