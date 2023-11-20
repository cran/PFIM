#' Class "OptimizationAlgorithm"
#'
#' @description A class storing information concerning the optimization algorithm.
#'
#' @name OptimizationAlgorithm-class
#' @aliases OptimizationAlgorithm
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{OptimizationAlgorithm} can be created by calls of the form \code{OptimizationAlgorithm(...)} where
#' (...) are the parameters for the \code{OptimizationAlgorithm} objects.
#'
#' @section Slots for \code{Administration} objects:
#'  \describe{
#'    \item{\code{name}:}{A character string giving the name of the optimization algorithm.}
#'    \item{\code{parameters}:}{A list giving the parameters of the optimization algorithm.}
#'  }

OptimizationAlgorithm = setClass(
  Class = "OptimizationAlgorithm",
  representation = representation( name="character",
                                   parameters = "list" ) )

setMethod( f="initialize",
           signature="OptimizationAlgorithm",
           definition= function ( .Object, name, parameters )
           {
             if(!missing(name))
             {
               .Object@name = name
             }

             if(!missing(parameters))
             {
               .Object@parameters = parameters
             }

             validObject(.Object)
             return (.Object )
           }
)

#' Optimize a design.
#' @name optimize
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @param optimizerParameters A list giving the optimization parameters.
#' @param optimizationObject An object giving the optimization algorithm.
#' @return A list giving the results if the optimization.
#' @export

setGeneric("optimize",
           function(object, optimizerParameters, optimizationObject )
           {

             standardGeneric("optimize")
           })

#' Graph of the weights for the multiplicative algorithm.
#' @name plotWeights
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @param threshold A numeric giving the threshold for the optimal weights in the multiplicative algorithm.
#' @return The graphs of the  weights for the multiplicative algorithm.
#' @export

setGeneric("plotWeights",
           function(object, threshold )
           {
             standardGeneric("plotWeights")
           })

#' @rdname getFim
#' @export

setMethod(f="getFim",
          signature="OptimizationAlgorithm",
          definition = function(object)
          {
            return( object@fim )
          }
)

#' Get the optimal design.
#' @name getOptimalDesign
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @return The optimal design.
#' @export

setGeneric("getOptimalDesign",
           function(object )
           {
             standardGeneric("getOptimalDesign")
           })

#' @rdname getOptimalDesign
#' @export

setMethod(f="getOptimalDesign",
          signature="OptimizationAlgorithm",
          definition = function(object)
          {
            return( object@optimalDesign )
          }
)

#' Set the optimal design.
#' @name setOptimalDesign
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @param optimalDesign An object from the class \linkS4class{Design}.
#' @return The object with the updated optimal design.
#' @export

setGeneric("setOptimalDesign",
           function( object, optimalDesign )
           {
             standardGeneric("setOptimalDesign")
           })

#' @rdname setOptimalDesign
#' @export

setMethod(f="setOptimalDesign",
          signature="OptimizationAlgorithm",
          definition = function(object, optimalDesign)
          {
            object@optimalDesign = optimalDesign
            return( object )
          }
)

# ======================================================================================================
# getArms
# ======================================================================================================

#' @rdname getArms
#' @export

setMethod(f="getArms",
          signature="OptimizationAlgorithm",
          definition = function(object)
          {
            return(object@arms)
          }
)

# ======================================================================================================
# setArms
# ======================================================================================================

#' @rdname setArms
#' @export

setMethod(f="setArms",
          signature="OptimizationAlgorithm",
          definition = function(object,arms)
          {
            object@arms = arms
            return(object)
          }
)

#' Get the iteration with the convergence criteria.
#' @name getIterationAndCriteria
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @return A dataframe giving the iteration with the convergence criteria.
#' @export

setGeneric("getIterationAndCriteria",
           function(object )
           {
             standardGeneric("getIterationAndCriteria")
           })

#' @rdname getIterationAndCriteria
#' @export

setMethod(f="getIterationAndCriteria",
          signature="OptimizationAlgorithm",
          definition = function(object)
          {
            return( object@iterationAndCriteria )
          }
)

#' Set the iteration with the convergence criteria.
#' @name setIterationAndCriteria
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @param value A dataframe giving the iteration with the convergence criteria.
#' @return A dataframe giving the iteration with the convergence criteria.
#' @export

setGeneric("setIterationAndCriteria",
           function( object, value )
           {
             standardGeneric("setIterationAndCriteria")
           })

#' @rdname setIterationAndCriteria
#' @export

setMethod(f="setIterationAndCriteria",
          signature = "OptimizationAlgorithm",
          definition = function( object, value )
          {
            object@iterationAndCriteria = value
            return( object )
          })

#' Generate report for the optimization.
#' @name generateReportOptimization
#' @param object An object from the class \linkS4class{OptimizationAlgorithm}.
#' @param optimizationObject An object from the class \linkS4class{Optimization}.
#' @param outputPath A string giving the output path.
#' @param outputFile A string giving the name of the output file.
#' @param  plotOptions A list giving the plot options.
#' @return The report for the optimization in html.
#' @export

setGeneric("generateReportOptimization",
           function( object, optimizationObject, outputPath, outputFile, plotOptions )
           {

             standardGeneric("generateReportOptimization")
           })

##########################################################################################################
# END Class OptimizationAlgorithm
##########################################################################################################





