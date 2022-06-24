
########################################################################################
##' Class "ModelInfusionODEquations" representing a model with infusion equations in ODE model.
##'
##' @description
##' A class giving information on the infusion equations regarding the equations of the model.
##'
##' @name ModelInfusionODEquations-class
##' @aliases ModelInfusionODEquations
##' @include ModelODEquations.R
##' @docType class
##' @exportClass ModelInfusionODEquations
##'
##' @section Objects from the class: \code{ModelInfusionODEquations} objects are typically created by calls to \code{ModelInfusionODEquations} and contain the following slots:
##'
##' \describe{
##' \item{\code{duringInfusionResponsesEquations}:}{A list containing the equations during the infusion.}
##' \item{\code{afterInfusionResponsesEquations}:}{A list containing the equations after the infusion.}
##' \item{\code{duringInfusionDerivatives}:}{A list containing the derivatives during the infusion.}
##' \item{\code{afterInfusionDerivatives}:}{A list containing the derivatives after the infusion.}
##' \item{\code{derivatives}:}{A list contaning the derivatives of the model.}
##' }
########################################################################################

ModelInfusionODEquations <- setClass(
  Class = "ModelInfusionODEquations",
  contains = "ModelODEquations",
  representation = representation(
    duringInfusionResponsesEquations = "list",
    afterInfusionResponsesEquations = "list",
    duringInfusionDerivatives = "list",
    afterInfusionDerivatives = "list" ,
    derivatives = "list"),
  validity = function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "ModelInfusionODEquations",
  definition = function(.Object, duringInfusionResponsesEquations, afterInfusionResponsesEquations,  duringInfusionDerivatives, afterInfusionDerivatives, derivatives )
  {

    .Object@duringInfusionResponsesEquations = duringInfusionResponsesEquations
    .Object@afterInfusionResponsesEquations = afterInfusionResponsesEquations
    .Object@duringInfusionDerivatives = duringInfusionDerivatives
    .Object@afterInfusionDerivatives = afterInfusionDerivatives
    .Object@derivatives = c(duringInfusionDerivatives, afterInfusionDerivatives)

    return(.Object)
  }
)

setMethod("getResponseIndice",
          "ModelInfusionODEquations",
          function(object, equationName)
          {
            responseNames <- names( unlist( object@duringInfusionResponsesEquations ) )
            i = 1
            for ( responseName in responseNames )
            {
              if( substring( responseName, nchar( responseName ) - nchar( equationName ) + 1 )  == equationName )
              {
                return( i )
              }
              i = i + 1
            }
            return( integer(0) )
          }
)


###################################################################################################################################

#' function to adjust Responses with variables values.
#'
#' @name scaleResponsesEvaluationODEInfusion
#' @param out_variable parameter out_variable.
#' @param modelParameters parameter modelParameters.
#' @param variablesNames parameter variablesNames.
#' @param responseNames parameter responseNames.
#' @param inputsModel parameter inputsModel.
#' @return A dataframe giving the evaluated responses adjusted with variables values.

scaleResponsesEvaluationODEInfusion = function( out_variable, modelParameters, variablesNames, responseNames, inputsModel){

  names(out_variable) = c( "time",variablesNames )

  namesParameter = names( modelParameters )

  for ( nameParameter in namesParameter )
  {
    assign( nameParameter, getMu( modelParameters[[nameParameter]] ) )
  }

  for ( nameVariable in variablesNames )
  {
    assign( nameVariable, out_variable[,nameVariable])
  }

  equations_after_infusion = inputsModel$modelEquations@afterInfusionResponsesEquations

  evalEquations = list()
  out_response = list()

  for ( iter in 1:length( responseNames ) )
  {
    out_response[[iter]] =  eval( equations_after_infusion[[iter]] )
  }

  out_response = as.data.frame(out_response)
  out_response = cbind( out_variable$time, out_response)
  names(out_response) = c( "time", responseNames )

  return( out_response )

}

#' Evaluate an ODE model in infusion
#'
#' @rdname EvaluateModelODEInfusion
#' @param object An \code{EvaluateModelODEInfusion} object.
#' @param samplingTimesModel A vector containing the sampling times.
#' @param cond_init_ode A vector containing the initial conditions.
#' @param inputsModel A list containing the inputs of the models.
#' @param parametersGradient A list containing the gradients of the model.
#' @param computeFIM A boolean for computing the FIM or not.
#' @return A list containing the evaluated responses and the gradients.

setGeneric("EvaluateModelODEInfusion",
           function(object, samplingTimesModel, cond_init_ode, inputsModel, parametersGradient, computeFIM)
           {
             standardGeneric("EvaluateModelODEInfusion")
           }
)

setMethod(f = "EvaluateModelODEInfusion",
          signature="ModelInfusionODEquations",
          definition= function(object, samplingTimesModel, cond_init_ode, inputsModel, parametersGradient, computeFIM)
          {
            # responses and variable names
            variablesNames = inputsModel$variablesNames
            responsePKNames = inputsModel$responsePKNames
            responsePDNames = inputsModel$responsePDNames
            responseNames = c( responsePKNames, responsePDNames )
            modelParameters = inputsModel$model_parameters
            nameModelParameters = names( modelParameters )
            variablesNames = inputsModel$variablesNames

            numberOfParameters = length( names( modelParameters ) )
            numberOfResponses = length( responsePKNames ) + length( responsePDNames )
            numberOfVariables = length(variablesNames )


            for ( nameRespPK in responsePKNames )
            {
              tau = inputsModel$tau[[nameRespPK]]

              if ( tau !=0 )
              {
                maxSamplingTimeResponse = max( inputsModel$sampleTimeResponse[[nameRespPK]] )
                n = maxSamplingTimeResponse%/%tau+1
                inputsModel$Tinf[[nameRespPK]] = rep( inputsModel$Tinf[[nameRespPK]], n )
                inputsModel$dose[[nameRespPK]] = rep( inputsModel$dose[[nameRespPK]], n )
                inputsModel$time_dose[[nameRespPK]] = seq(0,maxSamplingTimeResponse,tau)
              }
            }

            # set administration parameters
            dose = list()
            Tinf = list()
            time = list()
            time_dose = list()
            time_matrix  = list()
            index_time = list()
            dC_during_infusion = list()
            dC_after_infusion = list()

            # set parameters
            numberOfVariables = length(variablesNames)
            samplingTimesModelOde = unique(c(0,samplingTimesModel))

            # create model : ode & infusion
            model_ode_infusion <- function(samplingTimesModel,cond_init_ode,inputsModel){

              with(c(samplingTimesModel, inputsModel,cond_init_ode),{

                assign("t", samplingTimesModel)

                # equations
                dC = list()
                dC_during_infusion = inputsModel$modelEquations@duringInfusionDerivatives
                dC_after_infusion = inputsModel$modelEquations@afterInfusionDerivatives

                # initial conditions
                for ( i in 1:length( variablesNames ) )
                {
                  assign(variablesNames[i],cond_init_ode[i])
                }

                for ( nameRespPK in responsePKNames )
                {
                  dose[[nameRespPK]] = inputsModel$dose[[nameRespPK]]
                  Tinf[[nameRespPK]] = inputsModel$Tinf[[nameRespPK]]

                  assign( paste0("dose_",nameRespPK), dose[[nameRespPK]] )
                  assign( paste0("Tinf_",nameRespPK), Tinf[[nameRespPK]] )

                  time_dose[[nameRespPK]] = inputsModel$time_dose[[nameRespPK]]

                  time_matrix[[nameRespPK]] = matrix( ( c( time_dose[[nameRespPK]],
                                                           time_dose[[nameRespPK]] + Tinf[[nameRespPK]] ) ),
                                                      length( time_dose[[nameRespPK]] ), 2 )
                }

                for ( nameRespPK in responsePKNames )
                {
                  index_time[[nameRespPK]] = which( apply( time_matrix[[nameRespPK]] , 1,
                                                           findInterval, x = samplingTimesModel ) == 1)

                  if ( length( index_time[[nameRespPK]]  ) != 0 )
                  {
                    assign( paste0("dose_",nameRespPK), dose[[nameRespPK]][index_time[[nameRespPK]]] )
                    assign( paste0("Tinf_",nameRespPK), Tinf[[nameRespPK]][index_time[[nameRespPK]]] )

                    for ( iter in 1:numberOfVariables )
                    {
                      dC[[iter]] = eval( dC_during_infusion[[iter]] )
                    }
                  }
                  else
                  {
                    for ( iter in 1:numberOfVariables )
                    {
                      dC[[iter]] = eval( dC_after_infusion[[iter]] )
                    }
                  }
                }
                return(list(c(dC)))
              })
            }

            parametersOdeSolver = inputsModel$parametersOdeSolver
            atol = parametersOdeSolver$atol
            rtol = parametersOdeSolver$rtol

            out_response <- ode( cond_init_ode,
                                 samplingTimesModelOde,
                                 model_ode_infusion,
                                 inputsModel,
                                 atol=atol,rtol=rtol)

            out_response = as.data.frame( out_response )

            names(out_response) = c( "time",responseNames )

            out_response = scaleResponsesEvaluationODEInfusion( out_response, modelParameters,
                                                                variablesNames, responseNames, inputsModel )

            # for plot
            if ( computeFIM == FALSE )
            {
              for ( nameResponse in responseNames )
              {
                inputsModel$sampleTimeResponse[[nameResponse]] = samplingTimesModelOde
              }
            }

            # ----------------------------------------------------------------------------------------
            # compute gradient for sensitivity indices
            # ----------------------------------------------------------------------------------------

            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            responseGradient = list()

            numberOfshiftedParameters = dim( shiftedParameters )[2]

            for( iterResponse in 1:numberOfResponses )
            {
              resultsGrad = list()

              for ( iterShifted in 1:dim(shiftedParameters)[2] )
              {
                valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

                for( iterParameter in 1:numberOfParameters )
                {
                  nameModelParameter = nameModelParameters[[iterParameter]]
                  inputsModel[[nameModelParameter]] <- valuesParameters[iterParameter]
                }

                out <- ode( cond_init_ode,
                            samplingTimesModelOde,
                            model_ode_infusion,
                            inputsModel,
                            atol=atol,rtol=rtol )

                resultsGrad[[iterShifted]] = out[,(1+iterResponse)]
              }

              resultsGrad = do.call( cbind, resultsGrad )

              coefs = list()

              for ( i in 1 :dim( resultsGrad )[1] )
              {
                coefs[[i]] = solve( do.call( "cbind", Xcols ), resultsGrad[i,] )/frac
                coefs[[i]] = coefs[[i]][ 1 + seq_along( modelParameters ) ]
              }

              indexSamplingTimes = match( inputsModel$sampleTimeResponse[[iterResponse]],
                                          samplingTimesModelOde )

              indexSamplingTimes = sort( indexSamplingTimes )

              responseGradient[[iterResponse]] = do.call( rbind, coefs )

              # case if one parameter
              if( dim( responseGradient[[iterResponse]] )[1]==1 )
              {
                responseGradient[[iterResponse]] = responseGradient[[iterResponse]][indexSamplingTimes]
              }else{
                # case more than one parameter
                responseGradient[[iterResponse]] = responseGradient[[iterResponse]][indexSamplingTimes,]
              }
            }

            names( responseGradient ) = responseNames

            responseAllGradient = do.call( rbind, responseGradient )

            if( dim( responseAllGradient )[1]==1)
            {
              responseAllGradient = t( responseAllGradient )
            }

            return( list( evaluationResponse = out_response,
                          responseGradient = responseGradient,
                          responseAllGradient = responseAllGradient) )

          })

################################################################################################################
## End Class ModelInfusionODEquations
################################################################################################################
