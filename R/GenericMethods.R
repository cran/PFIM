# ======================================================================================================
#' Get the plot options for graphs responses and SI
#'
#' @name getPlotOptions
#' @param plotOptions A list giving the plots options.
#' @param outcomesNames A list giving the output names.
#' @return The list containing the plot options.
# ======================================================================================================

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

# ======================================================================================================
#' Get the name of an object
#'
#' @name getName
#' @param object An object defined form a class of PFIM.
#' @return A character string \code{name} giving the name of the object.
# ======================================================================================================

setGeneric(
  "getName",
  function(object) {
    standardGeneric("getName")
  })

# ======================================================================================================
#' Get the names of an object.
#'
#' @name getNames
#' @param object An object defined form a class of PFIM.
#' @return A vector giving the names of the object.
# ======================================================================================================

setGeneric(
  "getNames",
  function(object) {
    standardGeneric("getNames")
  })

setMethod(f="getNames",
          "list",
          function(object)
          {
            names =  unlist( lapply( object, function(x) getName(x) ) )
            return( names )
          }
)

# ======================================================================================================
#' Get the size of an object.
#'
#' @name getSize
#' @param object An object defined form a class of PFIM.
#' @return A numeric giving the size of the object.
# ======================================================================================================

setGeneric(
  "getSize",
  function(object) {
    standardGeneric("getSize")
  })

# ======================================================================================================
#' Set the size of an object.
#'
#' @name setSize
#' @param object An object defined form a class of PFIM.
#' @param size A numeric giving the size of the object.
#' @return The object with its size updated.
# ======================================================================================================

setGeneric(
  "setSize",
  function(object,size) {
    standardGeneric("setSize")
  })

# ======================================================================================================
#' Get the outcome of an object.
#'
#' @name getOutcome
#' @param object An object defined from a class of PFIM.
#' @return A string giving the outcome of the object.
# ======================================================================================================

setGeneric(
  "getOutcome",
  function(object) {
    standardGeneric("getOutcome")
  })

# ======================================================================================================
#' Set the outcome of an object.
#'
#' @name setOutcome
#' @param object An object defined form a class of PFIM.
#' @param outcome A string defined the outcome.
#' @return A string giving the updated outcome of the object.
# ======================================================================================================

setGeneric(
  "setOutcome",
  function(object, outcome) {
    standardGeneric("setOutcome")
  })

# ======================================================================================================
#' Get the fim of an of an object.
#'
#' @name getFim
#' @param object An object defined form a class of PFIM.
#' @return The \code{FIM} of the object.
# ======================================================================================================

setGeneric(
  "getFim",
  function(object)
  {
    standardGeneric("getFim")
  })

# ======================================================================================================
#' Get the parameters for the ode solvers of an object.
#'
#' @name getOdeSolverParameters
#' @param object An object defined form a class of PFIM.
#' @return The list giving the parameters for the ode solvers.
# ======================================================================================================

setGeneric(
  "getOdeSolverParameters",
  function(object) {
    standardGeneric("getOdeSolverParameters")
  })

# ======================================================================================================
#' Get the fixed effect of an object.
#'
#' @name getMu
#' @param object An object defined form a class of PFIM.
#' @return The object with the updated fixed effect.
# ======================================================================================================

setGeneric(
  "getMu",
  function(object) {
    standardGeneric("getMu")
  })

# ======================================================================================================
#' Set the value of the fixed effect mu of an object.
#'
#' @name setMu
#' @param object An object defined form a class of PFIM.
#' @param value The value of the fixed effect mu.
#' @return The object with the updated fixed effect mu.
# ======================================================================================================

setGeneric("setMu",
           function(object,value)
           {
             standardGeneric("setMu")
           })

# ======================================================================================================
#' Get the matrix omega of an object.
#'
#' @name getOmega
#' @param object An object defined form a class of PFIM.
#' @return The matrix omega of an object.
# ======================================================================================================

setGeneric(
  "getOmega",
  function(object) {
    standardGeneric("getOmega")
  })

# ======================================================================================================
#' Set the matrix omega of an object.
#'
#' @name setOmega
#' @param object An object defined form a class of PFIM.
#' @param value The matrix omega.
#' @return The object with the updated matrix omega.
# ======================================================================================================

setGeneric("setOmega",
           function(object,value)
           {
             standardGeneric("setOmega")
           })

# ======================================================================================================
#' Get the parameters of an object.
#'
#' @name getParameters
#' @param object An object defined form a class of PFIM.
#' @return Return the list of the parameters of the object.
# ======================================================================================================

setGeneric(
  "getParameters",
  function(object) {
    standardGeneric("getParameters")
  })

# ======================================================================================================
#' Set the parameters of an object.
#'
#' @name setParameters
#' @param object An object defined form a class of PFIM.
#' @param parameters A list of parameters.
#' @return The object with the updated list of parameters.
# ======================================================================================================

setGeneric(
  "setParameters",
  function(object,parameters) {
    standardGeneric("setParameters")
  })

# ======================================================================================================
#' Get the model error.
#'
#' @name getModelError
#' @param object An object defined form a class of PFIM.
#' @return The model error of the object.
# ======================================================================================================

setGeneric(
  "getModelError",
  function(object) {
    standardGeneric("getModelError")
  })

# ======================================================================================================
#' Get the sampling of an object.
#'
#' @name getSamplings
#' @param object An object defined form a class of PFIM.
#' @return A list of the samplings of the object.
# ======================================================================================================

# getSamplings
setGeneric("getSamplings",
           function(object)
           {
             standardGeneric("getSamplings")
           }
)

# ======================================================================================================
#' Get the fim of an object.
#'
#' @name getFim
#' @param object An object defined form a class of PFIM.
#' @return The fim of the object.
# ======================================================================================================

# getFim
setGeneric("getFim",
           function(object)
           {
             standardGeneric("getFim")
           }
)

# ======================================================================================================
#' Set the name of an object.
#'
#' @name setName
#' @param object An object defined form a class of PFIM.
#' @param name A string giving the name of the object.
#' @return The object with the updated name.
# ======================================================================================================

# setName
setGeneric(
  "setName",
  function(object, name) {
    standardGeneric("setName")
  })

# ======================================================================================================
#' Set the arms of an object.
#'
#' @name setArms
#' @param object An object defined form a class of PFIM.
#' @param arms A list of arms.
#' @return The object with the updated arms.
# ======================================================================================================

# setArms
setGeneric("setArms",
           function(object,arms)
           {
             standardGeneric("setArms")
           }
)

# ======================================================================================================
#' Get the arms of an object.
#'
#' @name getArms
#' @param object An object defined form a class of PFIM.
#' @return A list containing the arms of the object.
# ======================================================================================================

# getArms
setGeneric("getArms",
           function(object)
           {
             standardGeneric("getArms")
           }
)

##########################################################################################################
# END Class "GenericMethods"
##########################################################################################################





