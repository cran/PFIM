###################################################################################################################
#' Class "ModelError" representing a Model error.
#'
#' @description
#' A class storing information concerning the model errors for the models in the \linkS4class{LibraryOfModels}.
#'
#' @name ModelError-class
#' @aliases ModelError
#' @docType class
#' @exportClass ModelError
#'
#' @section Objects from the class:
#' Objects form the class \code{ModelError} can be created by calls of the form \code{ModelError(...)} where
#' (...) are the parameters for the \code{ModelError} objects.

#'@section Slots for Administration objects:
#' \describe{
#' \item{\code{equation}:}{Expression giving the equations of the model.}
#' \item{\code{derivates}:}{Expression giving the derivatives of the model.}
#' \item{\code{sigma_inter, sigma_slope}:}{Numerics giving the parameters for the residual variance error model.}
#' \item{\code{c_error}:}{A numeric taking the values 0 or 1.
#' The ModelError is Proportional when sigma_inter = 0 and c_error = 1.
#' The ModelError isProportionalC: When sigma_inter = 0 and c_error != 1.}
#' }

# ?? derivates

ModelError <- setClass(Class = "ModelError",
                               representation = representation
                               (
                                 equation = "expression",
                                 derivates = "list",
                                 sigma_inter = "numeric",
                                 c_error = "numeric",
                                 sigma_slope = "numeric"
                               ),
                       prototype = prototype(
                          c_error = 1,
                          sigma_slope = 0
                       )
)


setMethod(
  f="initialize",
  signature="ModelError",
  definition= function (.Object, sigma_inter, sigma_slope, c_error, equation )
  {
    if(!missing(equation))
      .Object@equation <- equation
    if(!missing(sigma_inter))
      .Object@sigma_inter <- sigma_inter
    if(!missing(sigma_slope))
      .Object@sigma_slope <- sigma_slope
    if( !missing( c_error ) )
      .Object@c_error <- c_error
    for ( parameter in  all.vars( equation ) )
    {
      if ( parameter != "f_x_i_theta" )
      {### Derivate square
        squaredEquation = parse( text = paste("(", equation, " ) ^ 2" ) )
        .Object@derivates[[ parameter ]] = D( squaredEquation, parameter )
      }
    }
    validObject(.Object)
    return (.Object )
  }
)


###################################################################################################################

#' Get the number of parameters of a \code{ModelError} object.
#'
#' @name getNumberOfParameter
#' @param object A \code{ModelError} object.
#' @return A numeric giving the number of parameters.

setGeneric("getNumberOfParameter",
           function(object)
           {
             standardGeneric("getNumberOfParameter")
           }
)
setMethod("getNumberOfParameter",
        "ModelError",
        function(object)
        {
          return( sum( all.vars( object@equation ) != "f_x_i_theta" ) )
        }
)

###################################################################################################################

#' Get the DV Sigma of a \code{ModelError} object.
#' @name getDVSigma
#' @param object A \code{ModelError} object.
#' @param parameter An string giving a parameter of the model error.
#' @return A list giving the \code{derivates} Sigma for a paramerer.


setGeneric("getDVSigma",
           function(object, parameter)
           {
             standardGeneric("getDVSigma")
           }
)
setMethod("getDVSigma",
          "ModelError",
          function(object, parameter )
          {
            return( object@derivates[[ parameter ]] )
          }
)

###################################################################################################################

#' Get parameters of the error model of a \code{ModelError} object.
#' @name getErrorModelParameters
#' @param object A \code{ModelError} object.
#' @return A list of string giving the parameters of the error model.

setGeneric("getErrorModelParameters",
           function(object)
           {
             standardGeneric("getErrorModelParameters")
           }
)
setMethod("getErrorModelParameters",
          "ModelError",
          function(object)
          {
            vars <- all.vars( object@equation )
            vars <- vars[ vars[] != "f_x_i_theta" ]
            return( vars )
          }
)

###################################################################################################################

#' Get the\code{sigma_inter} of a \code{ModelError} object.
#' @name getSigmaInter
#' @param object A \code{ModelError} object.
#' @return A numeric \code{sigma_inter} giving the \code{sigma_inter}.

setGeneric("getSigmaInter",
           function(object)
           {
             standardGeneric("getSigmaInter")
           }
)
setMethod("getSigmaInter",
          "ModelError",
          function(object)
          {
            return(object@sigma_inter)
          }
)

###################################################################################################################

#' Set the value for \code{sigma_inter} of a \code{ModelError} object.
#' @name setSigmaInter<-
#' @param object An \code{ModelError} object.
#' @param value The value for \code{sigma_inter}
#' @return The \code{ModelError} object with the new value for the \code{sigma_inter}.

setGeneric("setSigmaInter<-",
           function(object, value)
           {
             standardGeneric("setSigmaInter<-")
           }
)
setReplaceMethod( f="setSigmaInter",
                  signature="ModelError",
                  definition = function(object, value)
                  {
                    object@sigma_inter <- value
                    validObject(object)
                    return(object)
                  }
)

###################################################################################################################

#' Get the \code{sigma_slope} of a \code{ModelError} object.
#' @name getSigmaSlope
#' @param object An \code{ModelError} object.
#' @return The numeric \code{sigma_slope} giving the \code{sigma_slope}.

setGeneric("getSigmaSlope",
           function(object)
           {
             standardGeneric("getSigmaSlope")
           }
)
setMethod("getSigmaSlope",
          "ModelError",
          function(object)
          {
            return(object@sigma_slope)
          }
)


###################################################################################################################

#' Set the value for \code{sigma_slope} of a \code{ModelError} object.
#' @name setSigmaSlope<-
#' @param object An \code{ModelError} object.
#' @param value The value for \code{sigma_slope}.
#' @return The \code{ModelError} object with the new value for the \code{sigma_slope}.

setGeneric("setSigmaSlope<-",
           function(object, value)
           {
             standardGeneric("setSigmaSlope<-")
           }
)
setReplaceMethod( f="setSigmaSlope",
                  signature="ModelError",
                  definition = function(object, value)
                  {
                    object@sigma_slope <- value
                    validObject(object)
                    return(object)
                  }
)

###################################################################################################################

#' Get the \code{CError} of a \code{ModelError} object.
#' @name getCError
#' @param object \code{ModelError} object.
#' @return The numeric \code{c_error} giving the CError.


setGeneric("getCError",
           function(object)
           {
             standardGeneric("getCError")
           }
)
setMethod("getCError",
          "ModelError",
          function(object)
          {
            return(object@c_error)
          }
)

###################################################################################################################

#' Set the CError of a \code{ModelError} object.
#' @name setCError<-
#' @param object An \code{ModelError} object.
#' @param value The value for CError.
#' @return The \code{ModelError} object with the new value of the CError.

setGeneric("setCError<-",
           function(object, value)
           {
             standardGeneric("setCError<-")
           }
)
setReplaceMethod( f="setCError",
                  signature="ModelError",
                  definition = function(object, value)
                  {
                    object@c_error <- value
                    validObject(object)
                    return(object)
                  }
)

#' Get the equation of a \code{ModelError} object by their names.
#' @rdname getEquation
#' @param object A \code{ModelError} object.
#' @param equationName A character string giving the names of the equations.
#' @return An expression \code{equation} giving the equation of the model error.

setGeneric("getEquation",
           function(object, equationName)
           {
             standardGeneric("getEquation")
           }
)
setMethod(f = "getEquation",
          signature = "ModelError",
          definition = function(object)
          {
            return(object@equation)
          }
)

###################################################################################################################

#' Get the names for the error sigma inter.
#' @name getSigmaNames
#' @rdname getSigmaNames
#' @aliases getSigmaNames
#' @param object \code{ModelError} object.
#' @return A character giving the names for the sigma inter of the error model.


setGeneric("getSigmaNames",
           function(object)
           {
             standardGeneric("getSigmaNames")
           }
)

###################################################################################################################

#' Get the values of the variances \code{sigma_inter} and \code{sigma_slope}.
#'
#' @name getSigmaValues
#' @rdname getSigmaValues
#' @aliases getSigmaValues
#' @param object \code{ModelError} object.
#' @return A numeric giving the values for the sigma inter of the error model.

setGeneric("getSigmaValues",
           function(object)
           {
             standardGeneric("getSigmaValues")
           }

)

###################################################################################################################

#' Show the values of \code{sigma_inter}, \code{sigma_slope}, and  \code{c_error}.
#' @rdname show
#' @param object A \code{ModelError} object.
#' @return Show the values of \code{sigma_inter}, \code{sigma_slope}, and  \code{c_error}.

setMethod(f = "show",
          signature = "ModelError",
          definition = function(object)
          {
            for(errorParameter in getErrorModelParameters(object))
            {
              if(errorParameter == "sigma_inter")
                cat("    sigma_inter = ", getSigmaInter(object),"\n")

              if(errorParameter == "sigma_slope")
                cat("    sigma_slope = ", getSigmaSlope(object),"\n")

              if( errorParameter == "c_error")
                cat("    c_error = ",  getCError(object),"\n")

            }

          }
)



##################################################################################

#' Evaluation of the model error.
#' @name g
#' @param object \code{ModelError} object.
#' @param f_x_i_theta  the nonlinear structural model \code{f_x_i_theta}.
#' @return A numeric giving the evaluation of the error model.

setGeneric("g",
           function(object, f_x_i_theta)
           {
             standardGeneric("g")
           }
)

setMethod(f="g",
          signature=  "ModelError",
          definition=function(object, f_x_i_theta)
          {
            sigma_inter = object@sigma_inter
            sigma_slope = object@sigma_slope
            c_error = object@c_error
            return( eval( object@equation ) )
          }
)

###################################################################################################################

#' Get the values for Sigma derivatives \code{DVSigma} for the \code{ModelError} object.
#' @name getSig
#' @param object \code{ModelError} object.
#' @param f_x_i_theta  the nonlinear structural model \code{f_x_i_theta}
#' @return A list indexed with the parameters giving the values for the derivatives with respect to each parameters of the model error.

setGeneric("getSig",
           function(object, f_x_i_theta)
           {
             standardGeneric("getSig")
           }
)

setMethod(f="getSig",
          signature=  "ModelError",
          definition=function(object, f_x_i_theta )
          {
            sigma_inter = object@sigma_inter
            sigma_slope = object@sigma_slope
            c_error = object@c_error

            dV = list()

            for ( parameter in getErrorModelParameters( object ) )
            {
              if (get(parameter) != 0 )
              {
                evaluation = eval( getDVSigma( object, parameter ) )

                 if ( length( evaluation ) > 1 )
                  dV[[ parameter ]] = diag( evaluation )
                else
                  dV[[ parameter ]] = rep( evaluation, length( f_x_i_theta ) )

              }

            }
             return( dV )
          }
)

##########################################################################################################
# END Class "ModelError"
##########################################################################################################

