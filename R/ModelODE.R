#' Class "ModelODE"
#'
#' @description The class \code{ModelODE} defines information concerning the construction of an ode model.
#' The class \code{ModelODE} inherits from the class \code{Model}.
#'
#' @name ModelODE-class
#' @aliases ModelODE
#' @docType class
#' @include Model.R
#' @export

ModelODE = setClass("ModelODE",
                    contains = "Model")

#' @rdname getVariables
#' @export

setMethod("getVariables",
          signature("ModelODE"),
          function( object )
          {
            equations = getEquations( object )

            variablesNamesDerivatives = names( equations )

            variablesNames = gsub( "Deriv_", "", variablesNamesDerivatives )

            return( list( variablesNames = variablesNames, variablesNamesDerivatives = variablesNamesDerivatives ) )

          })

###########################################################################################
# End class ModelODE
###########################################################################################

