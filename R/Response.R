##########################################################################################################
#' Class "Response"
#'
#' @description Class \code{Response} represents a structural model.
#'
#' @name Response-class
#' @aliases Response
#' @docType class
#' @include ModelError.R
#' @exportClass Response
#'
#' @section Objects from the class:
#' \code{Response} objects are typically created by calls to \code{Response} and contain the following slots
#' model_error = g(sigma_inter, sigma_slope , f(x, theta)), this part is considered in class \linkS4class{ModelError}.
#' There are different possibilities to calculate g.
#'
#' @section Slots for \code{Response} objects:
#'\describe{
#'\item{\code{name}:}{A character string giving the name for model error.}
#'\item{\code{model_error}:}{An object \code{model_error} from the Class \linkS4class{ModelError}}.
#'}
##########################################################################################################

Response <- setClass(Class = "Response",
                     representation = representation
                     (
                       name = "character",
                       model_error = "ModelError"

                     ),
                     prototype = prototype(
                       name = paste("Response_", ceiling( runif(1) * 100000 ), sep="" )
                     )
)

setMethod(
  f="initialize",
  signature="Response",
  definition= function (.Object, name, model_error)
  {
    if(!missing(name))
      .Object@name <- name

    if(!missing(model_error))
      .Object@model_error <- model_error

    validObject(.Object)
    return (.Object )
  }
)


# -------------------------------------------------------------------------------------------------------------------
#' Get the name of the response of the model.
#'
#' @name getNameResponse
#' @param object A \code{Response} object.
#' @return A character string \code{name} giving the name of the response of the model.

setGeneric("getNameResponse",
           function(object)
           {
             standardGeneric("getNameResponse")
           }
)

setMethod("getNameResponse",
          "Response",
          function(object)
          {
            return(object@name)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the model error.
#'
#' @name getModelError
#' @param object A \code{Response} object.
#' @return The object

setGeneric("getModelError",
           function(object)
           {
             standardGeneric("getModelError")
           }
)
setMethod("getModelError",
          "Response",
          function(object)
          {
            return(object@model_error)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the model error.
#'
#' @name setModelError<-
#' @param object A \code{Response} object.
#' @param value The new value for the model error.
#' @return The \code{Response} object with the new value for the model error.

setGeneric("setModelError<-",
           function(object, value)
           {
             standardGeneric("setModelError<-")
           }
)
setReplaceMethod( f="setModelError",
                  signature="Response",
                  definition = function(object, value)
                  {
                    object@model_error <- value
                    validObject(object)
                    return(object)
                  }
)


# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the Error Model Derivatives.
#'
#' @name EvaluateErrorModelDerivatives
#' @param object A \code{Response} object.
#' @param f_x_i_theta  The nonlinear structural model \code{f_x_i_theta}
#' @return A list giving the error variance \code{V_sig} and sigma derivatives \code{sigmaDerivatives} of the model error.

setGeneric("EvaluateErrorModelDerivatives",
           function(object, f_x_i_theta ) #
           {
             standardGeneric("EvaluateErrorModelDerivatives")
           }
)

setMethod(f="EvaluateErrorModelDerivatives",
          signature=  "Response",
          definition=function(object, f_x_i_theta )
          {

            I = diag( length( f_x_i_theta ) )

            V_sig = ( g( object@model_error, f_x_i_theta = f_x_i_theta ) ^ 2 ) * I
            # Build the matrix of sigmas

            sigmaDerivatives  = getSig( object@model_error, f_x_i_theta )

            return( list( errorVariance = V_sig, sigmaDerivatives = sigmaDerivatives ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the ODE Error Model Derivatives.
#'
#' @name EvaluateODEErrorModelDerivatives
#' @param object \code{Response} object.
#' @param f_x_i_theta  The nonlinear structural model \code{f_x_i_theta}
#' @return A list giving the error variance \code{V_sig} and sigma derivatives \code{sigmaDerivatives} of the model error in ODE.

setGeneric("EvaluateODEErrorModelDerivatives",
           function( object, f_x_i_theta )
           {
             standardGeneric("EvaluateODEErrorModelDerivatives")
           }
)

setMethod(f="EvaluateODEErrorModelDerivatives",
          signature=  "Response",
          definition=function(object, f_x_i_theta )
          {
            I = diag( length( f_x_i_theta ) )
            V_sig = ( g( object@model_error, f_x_i_theta = f_x_i_theta ) ^ 2 ) * I
            sigmaDerivatives  = getSig( object@model_error, f_x_i_theta )
            return( list( errorVariance = V_sig, sigmaDerivatives = sigmaDerivatives ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the Variance of a Population FIM
#'
#' @name PopulationFIMEvaluateVariance
#' @param object A \code{Response} object.
#' @param equations An object of class \code{Response} containing the name of the reponse and the equation of the model error.
#' @param model_parameters An object of class \code{ModelParameters} containing the values and the distributions of the model parameters.
#' @param administrations An object of class \code{Administration} containing the parametrization for the administration of the model.
#' @param sampling_times An object of class \code{SamplingTimes} containing the parametrization for the sampling times of the model.
#' @param df_total parameter df_total
#' @param errorVariances parameter errorVariances
#' @param sigmaDerivatives parameter sigmaDerivatives
#' @return A list giving  \code{VDist} and \code{MF_var}.

setGeneric("PopulationFIMEvaluateVariance",
           function(object, equations, model_parameters, administrations, sampling_times, df_total, errorVariances, sigmaDerivatives ) #
           {
             standardGeneric("PopulationFIMEvaluateVariance")
           }
)

setMethod(f="PopulationFIMEvaluateVariance",
          signature=  "Response",
          definition=function(object, equations, model_parameters, administrations, sampling_times, df_total, errorVariances, sigmaDerivatives )
          {
            # Number of parameter of the model
            nbParameters = 0
            # Response name
            responseName = object@name
            # Ask the statistical model for the corresponding response's equation
            equation = getEquation( equations, responseName )
            # Derivate the equations
            derivate = list()
            # Omega vector
            omega2 = c()

            # Number of parameter of the model
            nbParameters = 0
            # Response name
            responseName = object@name
            # Ask the statistical model for the corresponding response's equation
            equation = getEquation( equations, responseName )
            # Derivate the equations
            derivate = list()
            # Omega vector
            omega2 = c()

            for (parameter in model_parameters)
            {
              parameterName = getNameModelParameter( parameter )
              nbParameters = nbParameters + 1

              # Set the mean value
              omegaValue = getOmega( parameter )
              omega2 = c( omega2, omegaValue^2 )
            }

            if ( length(omega2)==1){
              Omega = omega2}
            else{
              Omega=diag(omega2)
            }

            df_total_dist =  matrix( 0.0, nrow = dim( df_total )[1], ncol = nbParameters )

            i = 1
            for (parameter in model_parameters)
            {
              df_total_dist[ , i ] = getDerivatesAdjustedByDistribution( parameter, df_total[ ,i ]  )
              i = i + 1
            }

            VDist = ( df_total_dist ) %*% Omega %*% t( df_total_dist ) + errorVariances
            VDist = as.matrix( VDist )

            ### B Block

            dV_dOmega<-list()

            i = 1
            for ( parameter in model_parameters )
            {
              parameterName = getNameModelParameter( parameter )
              dOmega = matrix(0,ncol=nbParameters,nrow=nbParameters)
              dOmega[ i, i ] <- 1

              dV_dOmega[[ parameterName ]] <- ( df_total_dist )  %*% dOmega %*% t( df_total_dist  )
              i<-i+1
            }

            dV_dLambda = c( dV_dOmega, sigmaDerivatives )
            V_mat <- c()

            for ( varParam1 in dV_dLambda )
            {
              for ( varParam2 in dV_dLambda )
              {
                V_mat <- c( V_mat, 1/2 * ( sum( diag( solve( VDist ) %*% varParam1 %*% solve( VDist ) %*% varParam2 ) ) ) )
              }
            }

            MF_var = matrix( V_mat, nrow = length( dV_dLambda ), ncol = length( dV_dLambda ) )

            return( list( V = VDist, MF_var = MF_var ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the individual FIM variance.
#'
#' @param object A \code{Response} object.
#' @param equations An object of class \code{Response} containing the name of the reponse and the equation of the model error.
#' @param model_parameters An object of class \code{ModelParameters} containing the values and the distributions of the model parameters.
#' @param administrations An object of class \code{Administration} containing the parametrization for the administration of the model.
#' @param sampling_times An object of class \code{SamplingTimes} containing the parametrization for the sampling times of the model.
#' @param df_total parameter df_total
#' @param errorVariances parameter errorVariances
#' @param sigmaDerivatives parameter sigmaDerivatives
#' @return A list giving  \code{VDist} and \code{MF_var}.

setGeneric("IndividualFIMEvaluateVariance",
           function(object, equations, model_parameters, administrations, sampling_times, df_total, errorVariances, sigmaDerivatives )
           {
             standardGeneric("IndividualFIMEvaluateVariance")
           }
)

setMethod(f="IndividualFIMEvaluateVariance",
          signature=  "Response",
          definition=function(object, equations, model_parameters, administrations, sampling_times, df_total, errorVariances, sigmaDerivatives )
          {
            # Number of parameter of the model
            nbParameters = length( model_parameters )
            # Response name
            responseName = object@name
            # Ask the statistical model for the coresponding response's equation
            equation = getEquation( equations, responseName )
            # Derivate the equations
            derivate = list()

            VDist = as.matrix(errorVariances)

            ### B Block
            dV_dLambda = c( sigmaDerivatives )
            V_mat <- c()

            for ( varParam1 in dV_dLambda )
            {
              for ( varParam2 in dV_dLambda )
              {
                V_mat <- c( V_mat, 1/2 * ( sum( diag( solve( VDist ) %*% varParam1 %*% solve( VDist ) %*% varParam2 ) ) ) )
              }
            }

            MF_var = matrix( V_mat, nrow = length( dV_dLambda ), ncol = length( dV_dLambda ) )

            return( list( V = VDist, MF_var = MF_var ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the Sigma Names.
#'
#' @rdname getSigmaNames
#' @param object A \code{Response} object.
#' @return A character string \code{sigmaNames} giving the names of the sigma.

setMethod("getSigmaNames",
          "Response",
          function(object)
          {
            sigmaNames <- getSigmaNames(object@model_error)
            sigmaNames <- paste0(sigmaNames,"_", object@name)
            return(sigmaNames)
          }
)

##########################################################################################################
# END Class "Response"
##########################################################################################################






