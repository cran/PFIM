#' Class "ModelBolus"
#' @description ...
#' @name ModelBolus-class
#' @aliases ModelBolus
#' @docType class
#' @include Model.R
#' @export

ModelBolus = setClass("ModelBolus",
                      contains = "Model")

#' initialize
#' @param .Object .Object
#' @param name name
#' @param description description
#' @param equations equations
#' @param outcomes outcomes
#' @param parameters parameters
#' @param modelError modelError
#' @param initialConditions initialConditions
#' @param odeSolverParameters odeSolverParameters
#' @return ModelBolus
#' @export

setMethod( f="initialize",
           signature="ModelBolus",
           definition= function (.Object, name, description, equations, outcomes, parameters,
                                 modelError, initialConditions, odeSolverParameters )
           {
             if(!missing(name))
             {
               .Object@name = name
             }
             if(!missing( description ) )
             {
               .Object@description = description
             }
             if(!missing( equations ) )
             {
               .Object@equations = equations
             }
             if(!missing( outcomes ) )
             {
               .Object@outcomes = outcomes
             }
             if(!missing( parameters ) )
             {
               .Object@parameters = parameters
             }
             if(!missing( initialConditions ) )
             {
               .Object@initialConditions = initialConditions
             }
             if(!missing( modelError ) )
             {
               .Object@modelError = modelError
             }
             if(!missing( odeSolverParameters ) )
             {
               .Object@odeSolverParameters = odeSolverParameters
             }
             validObject(.Object)
             return (.Object )
           }
)

##########################################################################################################
# END Class "ModelBolus"
##########################################################################################################



