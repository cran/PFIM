#' Get the plot options for graphs responses and SI
#'
#' @name getPlotOptions
#' @param plotOptions A list giving the plots options.
#' @param outcomesNames A list giving the output names.
#' @return The list containing the plot options.
#' @export

getPlotOptions = function( plotOptions, outcomesNames )
{
  unitXAxis = c()
  unitYAxis = c()

  if ( length ( plotOptions ) !=0 )
  {
    if( length( plotOptions$unitTime ) == 0 ){
      unitXAxis = c()
    }else{
      unitXAxis = plotOptions$unitTime
    }

    if( length( plotOptions$unitOutcomes ) == 0 ){
      unitYAxis = vector("list",length( outcomesNames ) )
    }else{
      unitYAxis = plotOptions$unitOutcomes
    }
    unitYAxis = as.list( unitYAxis )
    names( unitYAxis ) = outcomesNames
  }else{
    unitXAxis = c()
    unitYAxis = c()
  }
  return( list( unitXAxis = unitXAxis,
                unitYAxis = unitYAxis ) )
}

#' Get the name of an object
#'
#' @title getName
#' @param object An object defined form a class of PFIM.
#' @return A character string \code{name} giving the name of the object.
#' @export

setGeneric(
  "getName",
  function(object) {
    standardGeneric("getName")
  })

#' Get the names of an object.
#'
#' @title getNames
#' @param object An object defined form a class of PFIM.
#' @return A vector giving the names of the object.
#' @export

setGeneric(
  "getNames",
  function(object) {
    standardGeneric("getNames")
  })

#' @rdname getNames
#' @export

setMethod(f="getNames",
          "list",
          function(object)
          {
            names =  unlist( lapply( object, function(x) getName(x) ) )
            return( names )
          }
)

#' Get the size of an object.
#'
#' @title getSize
#' @param object An object defined form a class of PFIM.
#' @return A numeric giving the size of the object.
#' @export

setGeneric(
  "getSize",
  function(object) {
    standardGeneric("getSize")
  })

#' Set the size of an object.
#'
#' @title setSize
#' @param object An object defined form a class of PFIM.
#' @param size A numeric giving the size of the object.
#' @return The object with its size updated.
#' @export

setGeneric(
  "setSize",
  function(object,size) {
    standardGeneric("setSize")
  })

#' Get the outcome of an object.
#'
#' @title getOutcome
#' @param object An object defined from a class of PFIM.
#' @return A string giving the outcome of the object.
#' @export

setGeneric(
  "getOutcome",
  function(object) {
    standardGeneric("getOutcome")
  })

#' Set the outcome of an object.
#'
#' @title setOutcome
#' @param object An object defined form a class of PFIM.
#' @param outcome A string defined the outcome.
#' @return A string giving the updated outcome of the object.
#' @export

setGeneric(
  "setOutcome",
  function(object, outcome) {
    standardGeneric("setOutcome")
  })

#' Get the fim of an of an object.
#'
#' @title getFim
#' @param object An object defined form a class of PFIM.
#' @return The \code{FIM} of the object.
#' @export

setGeneric(
  "getFim",
  function(object)
  {
    standardGeneric("getFim")
  })

#' Get the parameters for the ode solvers of an object.
#'
#' @title getOdeSolverParameters
#' @param object An object defined form a class of PFIM.
#' @return The list giving the parameters for the ode solvers.
#' @export

setGeneric(
  "getOdeSolverParameters",
  function(object) {
    standardGeneric("getOdeSolverParameters")
  })

#' Get the fixed effect of an object.
#'
#' @title getMu
#' @param object An object defined form a class of PFIM.
#' @return The object with the updated fixed effect.
#' @export

setGeneric(
  "getMu",
  function(object) {
    standardGeneric("getMu")
  })

#' Set the value of the fixed effect mu of an object.
#'
#' @name setMu
#' @param object An object defined form a class of PFIM.
#' @param value The value of the fixed effect mu.
#' @return The object with the updated fixed effect mu.
#' @export

setGeneric("setMu",
           function(object,value)
           {
             standardGeneric("setMu")
           })

#' Get the matrix omega of an object.
#'
#' @name getOmega
#' @param object An object defined form a class of PFIM.
#' @return The matrix omega of an object.
#' @export

setGeneric(
  "getOmega",
  function(object) {
    standardGeneric("getOmega")
  })

#' Set the matrix omega of an object.
#'
#' @name setOmega
#' @param object An object defined form a class of PFIM.
#' @param value The matrix omega.
#' @return The object with the updated matrix omega.
#' @export

setGeneric("setOmega",
           function(object,value)
           {
             standardGeneric("setOmega")
           })

#' Get the parameters of an object.
#'
#' @name getParameters
#' @param object An object defined form a class of PFIM.
#' @return Return the list of the parameters of the object.
#' @export

setGeneric(
  "getParameters",
  function(object) {
    standardGeneric("getParameters")
  })

#' Set the parameters of an object.
#'
#' @name setParameters
#' @param object An object defined form a class of PFIM.
#' @param parameters A list of parameters.
#' @return The object with the updated list of parameters.
#' @export

setGeneric(
  "setParameters",
  function(object,parameters) {
    standardGeneric("setParameters")
  })

#' Get the model error.
#'
#' @name getModelError
#' @param object An object defined form a class of PFIM.
#' @return The model error of the object.
#' @export

setGeneric(
  "getModelError",
  function(object) {
    standardGeneric("getModelError")
  })

#' Get the sampling of an object.
#'
#' @name getSamplings
#' @param object An object defined form a class of PFIM.
#' @return A list of the samplings of the object.
#' @export

setGeneric("getSamplings",
           function(object)
           {
             standardGeneric("getSamplings")
           }
)

#' Get the fim of an object.
#'
#' @name getFim
#' @param object An object defined form a class of PFIM.
#' @return The fim of the object.
#' @export

setGeneric("getFim",
           function(object)
           {
             standardGeneric("getFim")
           }
)

#' Set the name of an object.
#'
#' @name setName
#' @param object An object defined form a class of PFIM.
#' @param name A string giving the name of the object.
#' @return The object with the updated name.
#' @export

setGeneric(
  "setName",
  function(object, name) {
    standardGeneric("setName")
  })

#' Set the arms of an object.
#'
#' @name setArms
#' @param object An object defined form a class of PFIM.
#' @param arms A list of arms.
#' @return The object with the updated arms.
#' @export

setGeneric("setArms",
           function(object,arms)
           {
             standardGeneric("setArms")
           }
)

#' Get the arms of an object.
#'
#' @name getArms
#' @param object An object defined form a class of PFIM.
#' @return A list containing the arms of the object.
#' @export

setGeneric("getArms",
           function(object)
           {
             standardGeneric("getArms")
           }
)

#' Set the initial conditions of a ode model.
#'
#' @title setInitialConditions
#' @param object An object from the class \linkS4class{Model}.
#' @param initialConditions A list giving the initial conditions.
#' @return The model with the updated initial conditions.
#' @export

setGeneric(
  "setInitialConditions",
  function(object, initialConditions) {
    standardGeneric("setInitialConditions")
  })

#########################################################################################################
# END Class "GenericMethods"
##########################################################################################################





