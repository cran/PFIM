#' Class "PFIMProject"
#'
#' @description A class storing information concerning a PFIM project.
#'
#' @name PFIMProject-class
#' @aliases PFIMProject
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{PFIMProject} can be created by calls of the form \code{PFIMProject(...)} where
#' (...) are the parameters for the \code{PFIMProject} objects.
#'
#' @section Slots for \code{PFIMProject} objects:
#'  \describe{
#'    \item{\code{name}:}{A character string giving the name of the PFIM project.}
#'    \item{\code{description}:}{A list giving the description of the PFIM project.}
#'  }
#'

PFIMProject = setClass("PFIMProject",
                       representation  =  representation(
                         name = "character",
                         description = "list" ) )
setMethod(
  f="initialize",
  signature="PFIMProject",
  definition= function (.Object, name, description )
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(description))
    {
      .Object@description = description
    }

    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
# getName
# ======================================================================================================

setMethod("getName",
          "PFIMProject",
          function(object)
          {
            return( object@name )
          })

# ======================================================================================================
#' Set the model.
#' @name setModel
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @param model An object from the class \linkS4class{Model}.
#' @return The object with the updated model.
# ======================================================================================================

setGeneric("setModel",
           function(object, model)
           {
             standardGeneric("setModel")
           })

setMethod("setModel",
          "PFIMProject",
          function(object, model )
          {
            object@model = model
            return(object)
          })

# ======================================================================================================
#' Get the model.
#' @name getModel
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return The model of the object.
# ======================================================================================================

setGeneric("getModel",
           function(object)
           {
             standardGeneric("getModel")
           })

setMethod("getModel",
          "PFIMProject",
          function(object)
          {
            return(object@model)
          })

# ======================================================================================================
#' Get the model equations.
#' @name getModelEquations
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A list giving the model equations.
# ======================================================================================================

setGeneric("getModelEquations",
           function(object)
           {
             standardGeneric("getModelEquations")
           })

setMethod("getModelEquations",
          "PFIMProject",
          function(object)
          {
            return(object@modelEquations)
          })

# ======================================================================================================
#' Get the model parameters.
#' @name getModelParameters
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A list giving the model parameters.
# ======================================================================================================

setGeneric("getModelParameters",
           function(object)
           {
             standardGeneric("getModelParameters")
           })

# ======================================================================================================
# getModelParameters
# ======================================================================================================

setMethod("getModelParameters",
          "PFIMProject",
          function(object)
          {
            return(object@modelParameters)
          })

# ======================================================================================================
# getModelError
# ======================================================================================================

setMethod("getModelError",
          "PFIMProject",
          function(object)
          {
            return(object@modelError)
          })

# ======================================================================================================
#' Get the designs.
#' @name getDesigns
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A list giving the designs of the object.
# ======================================================================================================

setGeneric("getDesigns",
           function(object)
           {
             standardGeneric("getDesigns")
           })

setMethod("getDesigns",
          "PFIMProject",
          function(object)
          {
            return(object@designs)
          })

# ======================================================================================================
# getFim
# ======================================================================================================

setMethod("getFim",
          "PFIMProject",
          function(object)
          {
            return(object@fim)
          })

# ======================================================================================================
# getOdeSolverParameters
# ======================================================================================================

setMethod("getOdeSolverParameters",
          "PFIMProject",
          function(object) {
            return( object@odeSolverParameters )
          })

# ======================================================================================================
# getOutcomes
# ======================================================================================================

setMethod("getOutcomes",
          "PFIMProject",
          function(object) {
            return( object@outcomes )
          })

# ======================================================================================================
#' Get the optimization algorithm.
#' @name getOptimizer
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A string giving the name of the optimization algorithm.
# ======================================================================================================

setGeneric("getOptimizer",
           function(object)
           {
             standardGeneric("getOptimizer")
           })

setMethod("getOptimizer",
          "PFIMProject",
          function(object) {
            return( object@optimizer )
          })

# ======================================================================================================
#' Get the optimization parameters.
#' @name getOptimizerParameters
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A list giving the optimization parameters.
# ======================================================================================================

setGeneric("getOptimizerParameters",
           function(object)
           {
             standardGeneric("getOptimizerParameters")
           })

setMethod("getOptimizerParameters",
          "PFIMProject",
          function(object) {
            return( object@optimizerParameters )
          })

# ======================================================================================================
#' run
#' @name run
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @return A list giving the results of evaluation or optimization.
# ======================================================================================================

setGeneric("run",
           function( object )
           {
             standardGeneric("run")
           }
)

# ======================================================================================================
#' Generate the tables for the report.
#' @name generateTables
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @param plotOptions A list giving the plot options.
#' @return A list giving the kable able for the report ( evaluation and optimization).
# ======================================================================================================

setGeneric(
  "generateTables",
  function( object, plotOptions ) {
    standardGeneric("generateTables")
  })

# ======================================================================================================
#' Report
#' @name Report
#' @param object An object from the class \linkS4class{PFIMProject}.
#' @param outputPath A string giving the output path.
#' @param outputFile A string giving the name of the output file.
#' @param  plotOptions A list giving the plot options.
#' @return The report in html.
# ======================================================================================================

setGeneric(
  "Report",
  function( object, outputPath, outputFile, plotOptions ) {
    standardGeneric("Report")
  })

##########################################################################################################
# END Class PFIMProject
##########################################################################################################



