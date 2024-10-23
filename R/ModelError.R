###################################################################################################################
#' Class "ModelError" representing a Model error.
#'
#' @description ...
#' @name ModelError-class
#' @aliases ModelError
#' @docType class
#' @include GenericMethods.R
#' @export

ModelError = setClass(Class = "ModelError",
                      representation = representation
                      (
                        outcome="character",
                        equation = "expression",
                        derivatives = "list",
                        sigmaInter = "numeric",
                        sigmaSlope = "numeric",
                        cError = "numeric"
                      ))

#' initialize
#' @param .Object .Object
#' @param outcome outcome
#' @param equation equation
#' @param derivatives derivatives
#' @param sigmaInter sigmaInter
#' @param sigmaSlope sigmaSlope
#' @param cError cError
#' @return ModelError
#' @export
#'
setMethod( f="initialize",
           signature="ModelError",
           definition= function (.Object, outcome, equation, derivatives, sigmaInter, sigmaSlope, cError  )
           {
             if(!missing(outcome))
             {
               .Object@outcome = outcome
             }
             if(!missing(equation))
             {
               .Object@equation = equation
             }
             if(!missing(derivatives))
             {
               .Object@derivatives = derivatives
             }
             if(!missing(sigmaInter))
             {
               .Object@sigmaInter = sigmaInter
             }
             if(!missing(sigmaSlope))
             {
               .Object@sigmaSlope = sigmaSlope
             }
             if( !missing( cError ) )
             {
               .Object@cError = cError
             }
             validObject(.Object)
             return (.Object )
           })

# ======================================================================================================
# getOutcome
# ======================================================================================================

#' @rdname getOutcome
#' @export

setMethod("getOutcome",
          "ModelError",
          function( object )
          {
            return( object@outcome )
          })

#' Get the equation of a model error.
#'
#' @name getEquation
#' @param object An object from the class \linkS4class{ModelError}.
#' @return An expression giving the equation of a model error.
#' @export

setGeneric("getEquation",
           function(object)
           {
             standardGeneric("getEquation")
           })

#' @rdname getEquation
#' @export

setMethod("getEquation",
          "ModelError",
          function( object )
          {
            return( object@equation )
          })

#' Set the equation of a model error.
#'
#' @name setEquation
#' @param object An object from the class \linkS4class{ModelError}.
#' @param equation An expression giving the equation of a model error.
#' @return The model error with the updated equation.
#' @export

setGeneric("setEquation",
           function( object, equation )
           {
             standardGeneric("setEquation")
           })

#' @rdname setEquation
#' @export

setMethod("setEquation",
          "ModelError",
          function( object, equation )
          {
            object@equation = equation

            return( object )
          })

#' Get the derivatives of the model error equation.
#'
#' @name getDerivatives
#' @param object An object from the class \linkS4class{ModelError}.
#' @return The derivatives of the model error equation.
#' @export

setGeneric("getDerivatives",
           function(object)
           {
             standardGeneric("getDerivatives")
           })

#' @rdname getDerivatives
#' @export

setMethod("getDerivatives",
          "ModelError",
          function( object )
          {
            return( object@derivatives )
          })

#' Set the derivatives of the model error equation.
#'
#' @name setDerivatives
#' @param object An object from the class \linkS4class{ModelError}.
#' @param derivatives The derivatives of the model error equation.
#' @return The model error with the updated model error equation.
#' @export

setGeneric("setDerivatives",
           function( object, derivatives )
           {
             standardGeneric("setDerivatives")
           })

#' @rdname setDerivatives
#' @export

setMethod("setDerivatives",
          "ModelError",
          function( object, derivatives )
          {
            object@derivatives = derivatives

            return( object )
          })

#' Get the parameter sigma inter.
#'
#' @name getSigmaInter
#' @param object An object from the class \linkS4class{ModelError}.
#' @return A numeric giving the parameter sigma inter.
#' @export

setGeneric("getSigmaInter",
           function( object )
           {
             standardGeneric("getSigmaInter")
           })

#' @rdname getSigmaInter
#' @export

setMethod("getSigmaInter",
          "ModelError",
          function( object )
          {
            return( object@sigmaInter )
          })

#' Set the parameter sigma inter.
#'
#' @name setSigmaInter
#' @param object An object from the class \linkS4class{ModelError}.
#' @param sigmaInter A numeric giving the parameter sigma inter.
#' @return The model error with the updated sigma inter.
#' @export

setGeneric("setSigmaInter",
           function( object, sigmaInter )
           {
             standardGeneric("setSigmaInter")
           })

#' @rdname setSigmaInter
#' @export

setMethod("setSigmaInter",
          "ModelError",
          function( object, sigmaInter)
          {
            object@sigmaInter = sigmaInter

            return( object )
          })

#' Get the parameter sigma slope.
#'
#' @name getSigmaSlope
#' @param object An object from the class \linkS4class{ModelError}.
#' @return A numeric giving the parameter sigma slope.
#' @export

setGeneric("getSigmaSlope",
           function( object )
           {
             standardGeneric("getSigmaSlope")
           })

#' @rdname getSigmaSlope
#' @export

setMethod("getSigmaSlope",
          "ModelError",
          function( object )
          {
            return( object@sigmaSlope )
          })

#' Set the parameter sigma slope.
#'
#' @name setSigmaSlope
#' @param object An object from the class \linkS4class{ModelError}.
#' @param sigmaSlope A numeric giving the parameter sigma slope.
#' @return The model error with the updated sigma slope.
#' @export

setGeneric("setSigmaSlope",
           function( object, sigmaSlope )
           {
             standardGeneric("setSigmaSlope")
           })

#' @rdname setSigmaSlope
#' @export

setMethod("setSigmaSlope",
          "ModelError",
          function( object, sigmaSlope)
          {
            object@sigmaSlope = sigmaSlope

            return( object )
          })

#' Get the parameter c.
#'
#' @name getcError
#' @param object An object from the class \linkS4class{ModelError}.
#' @return A numeric giving the parameter c.
#' @export

setGeneric("getcError",
           function( object )
           {
             standardGeneric("getcError")
           })

#' @rdname getcError
#' @export

setMethod("getcError",
          "ModelError",
          function( object )
          {
            return( object@cError )
          })

#' Set the parameter c.
#'
#' @name setcError
#' @param object An object from the class \linkS4class{ModelError}.
#' @param cError A numeric giving the parameter c.
#' @return The model error with the parameter c.
#' @export

setGeneric("setcError",
           function( object, cError )
           {
             standardGeneric("setcError")
           })

#' @rdname setcError
#' @export

setMethod("setcError",
          "ModelError",
          function( object, cError)
          {
            object@cError = cError

            return( object )
          })

# ======================================================================================================
# getParameters
# ======================================================================================================

#' @rdname getParameters
#' @export

setMethod("getParameters",
          "ModelError",
          function(object) {

            sigmaInter = getSigmaInter(object)
            sigmaSlope = getSigmaSlope(object)
            cError = getcError(object)

            parameters = list( sigmaInter = sigmaInter,
                               sigmaSlope = sigmaSlope,
                               cError = cError )
            return(parameters)
          })

#' Evaluate the error model derivatives.
#'
#' @name EvaluateErrorModelDerivatives
#' @param object An object from the class \linkS4class{ModelError}.
#' @param evaluationOutcome A list giving the results of the model evaluation.
#' @return A list giving the error variance and the Sigma derivatives.
#' @export

setGeneric("EvaluateErrorModelDerivatives",
           function( object, evaluationOutcome )
           {
             standardGeneric("EvaluateErrorModelDerivatives")
           })

#' @rdname EvaluateErrorModelDerivatives
#' @export

setMethod(f="EvaluateErrorModelDerivatives",
          signature = "ModelError",
          definition = function( object, evaluationOutcome )
          {
            # ====================================================
            # model error parameter values
            # ====================================================

            sigmaInter = getSigmaInter( object )
            sigmaSlope = getSigmaSlope( object )
            cError = getcError( object )

            identityMatrix = diag( length( evaluationOutcome ) )

            # ====================================================
            # equation and derivatives
            # ====================================================

            errorVariance = list()
            sigmaDerivatives = list()

            modelErrorParameters = c( "sigmaInter", "sigmaSlope" )
            modelErrorEquation = expression( ( sigmaInter + sigmaSlope * evaluationOutcome ) ** 2 )

            for ( modelErrorParameter in modelErrorParameters )
            {
              if ( get( modelErrorParameter ) != 0 )
              {
                sigmaDerivatives[[modelErrorParameter]] = eval( D( modelErrorEquation, modelErrorParameter ) ) * identityMatrix
              }
            }

            # ====================================================
            # error variance
            # ====================================================

            errorVariance = ( sigmaInter + sigmaSlope * evaluationOutcome )**2 * identityMatrix

            return( list( errorVariance = errorVariance, sigmaDerivatives = sigmaDerivatives ) )
          })

###########################################################################################
# End class ModelError
###########################################################################################















