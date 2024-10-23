#' Class "ModelODEInfusion"
#'
#' @description The class \code{ModelODEInfusion} defines information concerning the construction of an ode model in infusion.
#' The class \code{ModelODEInfusion} inherits from the class \code{ModelInfusion}.
#'
#' @name ModelODEInfusion-class
#' @aliases ModelODEInfusion
#' @include ModelInfusion.R
#' @export

ModelODEInfusion = setClass(
  Class = "ModelODEInfusion",
  contains = "ModelInfusion")


#' @rdname getVariables
#' @export

setMethod("getVariables",
          signature("ModelInfusion"),
          function( object )
          {
            equationsDuringInfusion = getEquationsDuringInfusion( object )

            variablesNamesDerivatives = names( equationsDuringInfusion )

            variablesNames = gsub( "Deriv_", "", variablesNamesDerivatives )

            return( list( variablesNames = variablesNames, variablesNamesDerivatives = variablesNamesDerivatives ) )

          })

##########################################################################################################
# END Class ModelODEInfusion
##########################################################################################################

