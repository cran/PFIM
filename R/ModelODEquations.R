##################################################################################
#' Class "ModelODEquations" representing the equations of an ODE model
#'
#' @description
#' A class storing information concerning the equations for the ODE models in the \code{LibraryOfModels}.
#'
#' @name ModelODEquations-class
#' @aliases ModelODEquations
#' @docType class
#' @include ModelEquations.R
#' @exportClass ModelODEquations
#'
#' @section Objects from the class : \code{ModelODEquations} objects are typically created by calls to \code{ModelODEquations} and contain the following slots:
#'
#' \describe{
#' \item{\code{derivatives}:}{A list of expression giving the the derivatives of the model.}
#' }
#'
##################################################################################

ModelODEquations <- setClass(Class = "ModelODEquations",
                             contains = "ModelEquations",
                             representation = representation
                             (
                               derivatives = "list"
                             ))

setMethod(
  f="initialize",
  signature="ModelODEquations",
  definition= function (.Object, responsesEquations, derivatives )
  {
    .Object = callNextMethod(.Object, responsesEquations )
    .Object@derivatives = derivatives

    return ( .Object )
  })

# -------------------------------------------------------------------------------------------------------------------
#' Get the derivatives of a \code{ModelODEquations} object.
#'
#' @name getDerivatives
#' @param object An \code{ModelODEquations} object.
#' @return A list of expression \code{derivatives} giving the derivatives of a \code{ModelODEquations} object.

setGeneric("getDerivatives",
           function(object)
           {
             standardGeneric("getDerivatives")
           })

setMethod("getDerivatives",
          "ModelODEquations",
          function(object)
          {
            return(object@derivatives)
          })

# -------------------------------------------------------------------------------------------------------------------

#' function to adjust Responses with variables values.
#'
#' @name scaleResponsesEvaluationODE
#' @param out_variable parameter out_variable.
#' @param modelParameters parameter modelParameters.
#' @param variablesNames parameter variablesNames.
#' @param responseNames parameter responseNames.
#' @param inputsModel parameter inputsModel.
#' @return A dataframe giving the evaluated responses adjusted with variables values.

scaleResponsesEvaluationODE = function( out_variable, modelParameters, variablesNames, responseNames, inputsModel){

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

  equations =  inputsModel$modelEquations@equations
  evalEquations = list()
  out_response = list()

  for ( nameResponse in responseNames )
  {
    evalEquations[[nameResponse]] = eval( equations[[nameResponse]] )
    out_response[[nameResponse]] = evalEquations[[nameResponse]]
  }

  out_response = as.data.frame(out_response)
  out_response = cbind( out_variable$time, out_response)
  names(out_response) = c( "time", responseNames )

  return( out_response )

}

#' getEquationsModelPKPD
#' @name getEquationsModelPKPD

setGeneric(
  "getEquationsModelPKPD",
  function(modelEquationsPKmodel, modelEquationsPDmodel) {
    standardGeneric("getEquationsModelPKPD")
  })

setMethod(
  "getEquationsModelPKPD",
  signature("ModelEquations","ModelEquations"),
  function(modelEquationsPKmodel, modelEquationsPDmodel){

    equationPKmodel = modelEquationsPKmodel@equations
    equationPDmodel = modelEquationsPDmodel@equations

    # return the model equation
    output =  ModelEquations(c(equationPKmodel,equationPDmodel))

    return(output)

  })

setMethod("getEquationsModelPKPD",

          signature("ModelEquations","ModelODEquations"),

          function(modelEquationsPKmodel, modelEquationsPDmodel){

            equationDerivativesPDModel =  getDerivatives( modelEquationsPDmodel )

            # Number of PKModel Responses
            numberResponsePKPDmodelODE = length( modelEquationsPKmodel ) + length( equationDerivativesPDModel )

            # convert PK analytic to ODE
            pkModelODE = convertAnalyticToODE( modelEquationsPKmodel )

            names( pkModelODE ) =  paste0( "Deriv_","C", "1")
            names( equationDerivativesPDModel) =  paste0( "Deriv_","C", "2")

            # all modelODE equations
            allEquations = c( pkModelODE, equationDerivativesPDModel )

            # change variable names
            pkModelODE[[1]] = as.expression(do.call('substitute', list(pkModelODE[[1]][[1]], list(RespPK = quote(C1)))))

            equationDerivativesPDModel[[1]] = as.expression(do.call('substitute',
                                                                    list(equationDerivativesPDModel[[1]][[1]],
                                                                         list(RespPK = quote(C1)))))

            equationDerivativesPDModel[[1]] = as.expression(do.call('substitute',
                                                                    list(equationDerivativesPDModel[[1]][[1]],
                                                                         list(E = quote(C2)))))

            responsesPKPDModel = list()
            derivativesPKPDModel = list()

            responsesPKPDModel$RespPK = expression( C1 )
            responsesPKPDModel$RespPD = expression( C2 )

            derivativesPKPDModel$Deriv_C1 = pkModelODE$Deriv_C1
            derivativesPKPDModel$Deriv_C2 = equationDerivativesPDModel$Deriv_C2

            output = ModelODEquations( responsesPKPDModel, derivativesPKPDModel )

            return(output)

          })

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate an ODE model.
#'
#' @rdname EvaluateModelODE
#' @param object An \code{ModelODEquations} object.
#' @param samplingTimesModel A vector containing the sampling times.
#' @param cond_init_ode A vector containing the initial conditions.
#' @param inputsModel A list containing the inputs of the models.
#' @param parametersGradient A list containing the gradients of the model.
#' @param computeFIM A boolean for computing the FIM or not.
#' @return A list containing the evaluated responses and the gradients.

setGeneric("EvaluateModelODE",
           function(object, samplingTimesModel,cond_init_ode,inputsModel,parametersGradient, computeFIM)
           {
             standardGeneric("EvaluateModelODE")
           })

setMethod(f = "EvaluateModelODE",
          signature="ModelODEquations",
          definition= function(object, samplingTimesModel,cond_init_ode,inputsModel, parametersGradient, computeFIM)
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

            time_matrix  = list()
            timeDoses = list()

            for ( nameRespPK in responsePKNames )
            {
              tau = inputsModel$tau[[nameRespPK]]

              if ( tau !=0 )
              {
                maxSamplingTimeResponse = max( inputsModel$sampleTimeResponse[[nameRespPK]] )
                n = maxSamplingTimeResponse%/%tau

                inputsModel$dose[[nameRespPK]] = rep( inputsModel$dose[[nameRespPK]], n )
                inputsModel$timeDoseForOde[[nameRespPK]] = seq(0,maxSamplingTimeResponse,tau)

                timeDoses[[nameRespPK]] = inputsModel$timeDoseForOde[[nameRespPK]]
                timeDoses[[nameRespPK]] = sort(unique(c(timeDoses[[nameRespPK]])))
                n = length(timeDoses[[nameRespPK]])
                time_matrix[[nameRespPK]] = matrix(c(timeDoses[[nameRespPK]][(1:(n-1))],
                                                     timeDoses[[nameRespPK]][(2:(n))]),n-1,2)

              }else{

                # for multi doses
                timeDoses[[nameRespPK]] = inputsModel$timeDoseForOde[[nameRespPK]]
                timeDoses[[nameRespPK]] = sort(unique(c(timeDoses[[nameRespPK]])))
                n = length(timeDoses[[nameRespPK]])
                time_matrix[[nameRespPK]] = matrix(c(timeDoses[[nameRespPK]][(1:(n-1))],
                                                     timeDoses[[nameRespPK]][(2:(n))]),n-1,2)
              }
            }

            # equations
            dC_eval = list()
            dC = inputsModel$modelEquations@derivatives
            equations = inputsModel$modelEquations@equations

            # set administration parameters
            dose = list()
            time = list()
            index_time = list()

            numberOfVariables = length(variablesNames)
            samplingTimesModelOde = unique( c( 0, samplingTimesModel ) )

            # create model : ode & infusion
            model_ode <- function(samplingTimesModel,cond_init_ode,inputsModel){

              with(c(samplingTimesModel,cond_init_ode,inputsModel),{

                assign("t", samplingTimesModel)

                # initial conditions
                for ( i in 1:length( variablesNames ) )
                {
                  assign(variablesNames[i],cond_init_ode[i])
                }

                for ( nameRespPK in responsePKNames )
                {
                  dose[[nameRespPK]] = inputsModel$dose[[nameRespPK]]

                  if ( t > max(unlist(timeDoses[[nameRespPK]] ) )){
                    assign( paste0("dose_",nameRespPK), 0 )
                  }
                  else
                  {
                    index_time[[nameRespPK]] = which( apply( time_matrix[[nameRespPK]], 1,
                                                             findInterval, x = samplingTimesModel ) == 1)

                    # time update for multi doses
                    intervalTimeMatrix = time_matrix[[nameRespPK]][index_time[[nameRespPK]],]
                    t = t - intervalTimeMatrix[1]

                    assign( paste0("dose_",nameRespPK), dose[[nameRespPK]][index_time[[nameRespPK]]] )
                  }
                }

                for ( iter in 1:numberOfVariables )
                {
                  dC_eval[[iter]] = eval( dC[[iter]] )
                }
                return( list( c( dC_eval ) ) )
              })
            }

            # ----------------------------------------------------------------------------------------
            # compute response
            # ----------------------------------------------------------------------------------------

            parametersOdeSolver = inputsModel$parametersOdeSolver
            atol = parametersOdeSolver$atol
            rtol = parametersOdeSolver$rtol

            out_response <- ode( cond_init_ode,
                                 samplingTimesModelOde,
                                 model_ode,
                                 inputsModel,
                                 method="lsoda",
                                 atol=atol,
                                 rtol=rtol )

            out_response = as.data.frame( out_response )

            names( out_response ) = c( "time",responseNames )

            out_response = scaleResponsesEvaluationODE( out_response, modelParameters,
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

            numberOfshiftedParameters = length( modelParameters )

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
                            model_ode,
                            inputsModel,
                            method="lsoda",
                            atol=atol,rtol=rtol)

                resultsGrad[[iterShifted]] = out[,(1+iterResponse)]
              }

              resultsGrad = do.call( cbind, resultsGrad )

              coefs = list()

              for ( i in 1 :dim( resultsGrad)[1] )
              {
                coefs[[i]] = solve(do.call("cbind", Xcols), resultsGrad[i,])/frac
                coefs[[i]] = coefs[[i]][1 + seq_along(modelParameters)]
              }

              indexSamplingTimes = match( inputsModel$sampleTimeResponse[[iterResponse]], samplingTimesModelOde )
              indexSamplingTimes = sort( indexSamplingTimes )

              responseGradient[[iterResponse]] = do.call(rbind,coefs)

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

            responseAllGradient = do.call(rbind, responseGradient)

            if( dim( responseAllGradient )[1]==1)
            {
              responseAllGradient = t( responseAllGradient )
            }

            return( list( evaluationResponse = out_response,
                          responseGradient = responseGradient,
                          responseAllGradient = responseAllGradient) )

          })

###############################################################################################
# END CLASS "ModelODEquations"
###############################################################################################





