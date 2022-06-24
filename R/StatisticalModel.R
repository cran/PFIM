#################################################################################
#' Class "StatisticalModel"
#'
#' @description Class \code{StatisticalModel} represents a statistical model.
#'
#' @name StatisticalModel-class
#' @aliases StatisticalModel
#' @docType class
#' @include ModelEquations.R
#' @export  StatisticalModel
#' @exportClass StatisticalModel
#' @section Mathematical description of the statistical model:
#' \describe{
#' \item{-}{\deqn{y = f(x, \theta) + g*\epsilon}, this part is considered in class \linkS4class{Response}}
#' \item{-}{\deqn{f(x, \theta)}, this part is considered in class \linkS4class{Response}}
#' \item{-}{\deqn{\theta={(\mu, \omega)}}, this part is considered in class \linkS4class{ModelParameter}}
#' \item{-}{\deqn{\epsilon}, this is a slot of the object \code{NormalDistribution} with mean = 0 and covariate_matrix = I}
#' }
#'
#' @section Objects from the class: \code{StatisticalModel} objects are typically created by calls to \code{StatisticalModel} and contain the following slots:
#'
#' @section Slots for \code{StatisticalModel} objects:
#' \describe{
#' \item{\code{modelEquations}:}{An object from the class \code{ModelEquations}}
#' \item{\code{responses}:}{A list of objects of type Responses -> f(x, theta)}
#' \item{\code{correlations}:}{A list giving all the covariables.}
#' \item{\code{model_parameters}:}{A list giving all the parameters of the models.}
#' }
#################################################################################

StatisticalModel<-setClass(
  Class = "StatisticalModel",
  representation = representation(
    modelEquations = "ModelEquations",
    responses = "list", # list of object of type Response
    correlations = "list", # list of all covariables
    model_parameters = "list", # list of all parameters of the models
    computeFIM = "logical",
    parametersForSolverOde = "list",
    variables="list"),

  prototype = prototype(
    parametersForSolverOde=list( .relStep = .Machine$double.eps^(1/3),
                                 atol = 1e-6,
                                 rtol = 1e-6),
    computeFIM = TRUE),
  validity = function(object)
  {
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "StatisticalModel",
  definition = function(.Object, modelEquations, responses, correlations,
                        model_parameters, computeFIM, parametersForSolverOde, variables)
  {
    if(!missing(modelEquations))
      .Object@modelEquations <- modelEquations

    if(!missing(responses))
      .Object@responses <- responses

    if(!missing(correlations))
      .Object@correlations <- correlations

    if(!missing(model_parameters))
      .Object@model_parameters <- model_parameters

    if(!missing(computeFIM))
      .Object@computeFIM <- computeFIM

    if(!missing(parametersForSolverOde))
      .Object@parametersForSolverOde <- parametersForSolverOde

    if(!missing(variables))
      .Object@variables <- variables

    validObject(.Object)

    return(.Object)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set parameters for the ode solver
#'
#' @name setParametersOdeSolver
#' @param object A \code{StatisticalModel} object.
#' @param value A list giving the values of the parameters.
#' @return The \code{StatisticalModel} object with the new parameters for the ode solver.

setGeneric("setParametersOdeSolver",
           function(object, value)
           {
             standardGeneric("setParametersOdeSolver")
           }
)
setMethod( f="setParametersOdeSolver",
           signature="StatisticalModel",
           definition = function(object, value)
           {
             if ( length ( value ) !=0 )
             {
               if( length( value$.relStep ) !=0 ){
                 object@parametersForSolverOde$.relStep  = value$.relStep
               }
               if( length( value$atol ) !=0 ){
                 object@parametersForSolverOde$atol   = value$atol
               }
               if( length( value$rtol ) !=0 ){
                 object@parametersForSolverOde$rtol  = value$rtol
               }
             }
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get parameters for the ode solver
#'
#' @name getParametersOdeSolver
#' @param object A \code{StatisticalModel} object.
#' @return The parameters for the ode solver.

setGeneric("getParametersOdeSolver",
           function(object)
           {
             standardGeneric("getParametersOdeSolver")
           }
)
setMethod( f="getParametersOdeSolver",
           signature="StatisticalModel",
           definition = function(object)
           {

             parametersOdeSolver = list( .relStep = object@parametersForSolverOde$.relStep,
                                         atol = object@parametersForSolverOde$ato,
                                         rtol = object@parametersForSolverOde$rtol )

             return( parametersOdeSolver )
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Add a response to a statistical model.
#'
#' @name addResponse
#' @param object A \code{StatisticalModel} object.
#' @param value A character string giving the name of the  response to add.
#' @return The \code{StatisticalModel} object with the added response.

setGeneric("addResponse",
           function(object, value)
           {
             standardGeneric("addResponse")
           }
)
setMethod( f="addResponse",
           signature="StatisticalModel",
           definition = function(object, value)
           {
             object@responses[[ value@name ]] <- value
             validObject(object)
             return(object)
           }
)

# ---------------------------------------------------------------------------------------------------------------
#' Add responses to a statistical model.
#'
#' @name addResponses
#' @param object A \code{StatisticalModel} object.
#' @param listOfResponses A list of character string giving the names of the  responses to add.
#' @return The \code{StatisticalModel} object with the added responses.

setGeneric("addResponses",
           function(object, listOfResponses)
           {
             standardGeneric("addResponses")
           }
)
setMethod( f="addResponses",
           signature="StatisticalModel",
           definition = function(object, listOfResponses)
           {
             for( i in 1:length( listOfResponses ) ){

               response = listOfResponses[[i]]

               object@responses[[ response@name ]] <- response
             }
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the correlation.
#'
#' @name defineCorrelation
#' @param object A \code{StatisticalModel} object.
#' @param correlationlist ...
#' @return Return correlationlist
#
setGeneric("defineCorrelation",
           function(object, correlationlist)
           {
             standardGeneric("defineCorrelation")
           }
)

setMethod(f="defineCorrelation",
          signature=  "StatisticalModel",
          definition=function(object, correlationlist )
          {
            object@correlations = correlationlist
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Compute the residual variance thanks to the function g of the model error.
#'
#' @name CalculatedResidualVariance
#' @param objectStatisticalModel A \code{StatisticalModel} object.
#' @param objectModelError A \code{ModelError} object.
#' @param x_i variable x_i of the model error.
#' #' @return CalculatedResidualVariance

setGeneric("CalculatedResidualVariance",
           function(objectStatisticalModel, objectModelError, x_i ) #
           {
             standardGeneric("CalculatedResidualVariance")
           }
)

setMethod(f="CalculatedResidualVariance",
          signature=  "StatisticalModel",
          definition=function(objectStatisticalModel, objectModelError, x_i)
          {
            calculated_residual_variance = g(objectModelError, x_i)
            return(calculated_residual_variance)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Check the parameters in the model equations.
#'
#' @name checkParameterInEquations
#' @param object A \code{StatisticalModel} object.
#' @return The \code{ModelParameter} objects of the model parameters in the model equation.

setGeneric("checkParameterInEquations",
           function(object)
           {
             standardGeneric("checkParameterInEquations")
           }
)

setMethod(f="checkParameterInEquations",
          signature=  "StatisticalModel",
          definition=function(object)
          {
            modelParameters = getModelParameters( object )
            modelEquations = getEquationsStatisticalModel( object )

            parameterInEquation = list()

            dC = getDerivatives( modelEquations )

            allParametersinEquation = unlist(lapply( dC, function(x) all.vars( x )))

            nameParameters = names( modelParameters )

            for ( nameParameter in nameParameters )
            {
              if ( nameParameter %in% allParametersinEquation)
              {
                parameterInEquation[[nameParameter]] = "TRUE"
              }
              else
              {
                modelParameters[[nameParameter]] = NULL
              }
            }

            return( modelParameters )

          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the parameters for the evaluation of the model.
#'
#' @rdname setParametersForEvaluateModel
#' @param object A \code{StatisticalModel} object.
#' @param administrations An \code{Administration} object.
#' @param sampling_times A \code{SamplingTimes} object.
#' @param cond_init A list for the initial conditions of the \code{StatisticalModel} object.
#' @return A list containing the parameters used for the model evaluation.

setGeneric("setParametersForEvaluateModel",
           function(object, administrations, sampling_times, cond_init)
           {
             standardGeneric("setParametersForEvaluateModel")
           }
)

setMethod(f = "setParametersForEvaluateModel",
          signature="StatisticalModel",
          definition= function(object, administrations, sampling_times, cond_init)
          {
            # model parameters
            samplingTimesModel = list()
            sampleTimeResponse = list()
            inputsModel = list()

            ### get model equations
            modelEquations = getEquationsStatisticalModel( object )
            classModel = class( modelEquations )

            responses = getResponsesStatisticalModel( object )
            ### responses name and number
            responseNames = names( responses )
            numberOfResponse = length( responseNames )
            numberOfResponsePK = length( administrations)
            numberOfResponsePD = numberOfResponse - numberOfResponsePK
            responsePKNames = responseNames[1:numberOfResponsePK]
            responsePDNames = setdiff(responseNames,responsePKNames)
            inputsModel$responsePKNames = responsePKNames
            inputsModel$responsePDNames = responsePDNames
            inputsModel$responseNames = c( responsePKNames, responsePDNames )

            ### get sampling times for each responses

            for ( responseName in responseNames )
            {
              sampleTimeResponse[[responseName ]] = getSampleTime( sampling_times[[ responseName ]] )
              samplingTimesModel = append( samplingTimesModel, sampleTimeResponse[[ responseName ]] )
            }

            samplingTimesModel = sort( unique( unlist( samplingTimesModel ) ) )

            inputsModel$sampleTimeResponse = sampleTimeResponse

            ### parameters

            Tinf = list()
            dose = list()
            time_dose = list()
            tau = list()
            timeDoseForOde = c()

            for ( response in responsePKNames)
            {
              Tinf[[response]] = getTinf( administrations[[response]])
              dose[[response]] = getAmountDose( administrations[[response]])
              time_dose[[response]] = getTimeDose( administrations[[response]])
              tau[[response]] = getTau( administrations[[response]])

              if ( length( tau[[response]] )==0 )
              {
                tau[[response]] = 0
              }

              timeDoseForOde = c(timeDoseForOde,  time_dose[[response]])

              inputsModel$Tinf[[response]] = Tinf[[response]]
              inputsModel$dose[[response]] = dose[[response]]
              inputsModel$time_dose[[response]] = time_dose[[response]]
              inputsModel$tau[[response]] = tau[[response]]

              inputsModel$timeDoseForOde[[response]] =  sort(unique(c( timeDoseForOde, max(samplingTimesModel))))
            }

            ######  model parameters
            ## check case model parameter in cond_init but no inequations

            modelParameters = getModelParameters(object)

            if (classModel == "ModelODEquations")
            {
              modelParametersInEquation = checkParameterInEquations( object )
            }else
            {
              modelParametersInEquation = modelParameters
            }

            parameterInEquation = list()

            for( parameter in modelParameters )
            {
              name = getNameModelParameter( parameter )
              inputsModel[[name]] = getMu( parameter )
              assign(name,inputsModel[[name]] )
            }

            inputsModel$model_parameters = modelParameters

            # set initial conditions
            # cond_init & case for doses bolus model
            # dose in cond init and not in ode equations
            cond_init_ode = list()

            if (classModel == "ModelODEquations")
            {
              dC = getDerivatives(modelEquations)

              doseRespPKInEquation = list()
              doseRespPKInCondinit = list()

              for ( nameResponsePK in responsePKNames )
              {
                nameDose = paste0("dose_",nameResponsePK)

                doseRespPKInEquation[[nameDose]] = sapply( dC, function(x) all( grepl( nameDose, x) ) )
                doseRespPKInCondinit[[nameDose]] = vapply( cond_init, function(x) all( grepl( nameDose, x) ),logical(1) )

                for ( i in 1:length( doseRespPKInEquation[[nameDose]] ) )
                {
                  for ( j in 1:length( doseRespPKInCondinit[[nameDose]] ) )
                  {
                    if ( ( doseRespPKInEquation[[nameDose]][i] == FALSE ) & ( doseRespPKInCondinit[[nameDose]][j] == TRUE ) )
                    {
                      assign( nameDose, dose[[nameResponsePK]] )
                    }
                  }
                }
              }
            }

              for ( nameCondiInit in names( cond_init ) )
              {
                cond_init_ode[[nameCondiInit]] = eval( cond_init[[nameCondiInit]] )
              }

              cond_init_ode = c( unlist( cond_init_ode ) )

              # model equations
              inputsModel$modelEquations = modelEquations

              # variable of the model
              inputsModel$variablesNames = names( cond_init )

              # get parameters for ode solver
              parametersOdeSolver = getParametersOdeSolver( object )
              inputsModel$parametersOdeSolver = parametersOdeSolver

              return(list(samplingTimesModel = samplingTimesModel,
                          cond_init_ode = cond_init_ode,
                          inputsModel = inputsModel,
                          modelParametersInEquation = modelParametersInEquation ) )
            })

# -------------------------------------------------------------------------------------------------------------------
#' Parameters used for computing the model gradient by finite-differences.
#'
#' @rdname parametersForComputingGradient
#' @param object A \code{StatisticalModel} object.
#' @return A list containing the parameters used for computing the gradient of the model.

setGeneric("parametersForComputingGradient",
           function(object)
           {
             standardGeneric("parametersForComputingGradient")
           }
)

setMethod(f = "parametersForComputingGradient",
          signature="StatisticalModel",
          definition= function(object)
          {
            minAbsPar = 0

            parametersOdeSolver = getParametersOdeSolver( object )
            .relStep = parametersOdeSolver$.relStep
            listPars = getModelParameters( object )
            valuePars = unlist(lapply(listPars, slot, name = "mu"))
            .relStep =  pmax(abs(valuePars), minAbsPar) * .relStep

            pars <- as.numeric(valuePars)
            npar <- length(pars)
            incr <- pmax(abs(pars), minAbsPar) * .relStep
            baseInd <- diag(npar)
            frac <- c(1, incr, incr^2)
            cols <- list(0, baseInd, -baseInd)

            for ( i in seq_along(pars)[ -npar ] ) {
              cols <- c( cols, list( baseInd[ , i ] + baseInd[ , -(1:i) ] ) )
              frac <- c( frac, incr[ i ] * incr[ -(1:i) ] )
            }

            indMat <- do.call( "cbind", cols)
            shifted <- pars + incr * indMat
            indMat <- t(indMat)
            Xcols <- list(1, indMat, indMat^2)

            for ( i in seq_along(pars)[ - npar ] ) {
              Xcols <- c( Xcols, list( indMat[ , i ] * indMat[ , -(1:i) ] ) )
            }

            return( list( Xcols = Xcols, shifted = shifted, frac = frac ) )

          })

# -------------------------------------------------------------------------------------------------------------------
#' Evaluation for the model, analytic, ode, infusion
#'
#' @rdname EvaluationModel
#' @param object A \code{StatisticalModel} object.
#' @param samplingTimesModel A vector containing the sampling times of the model.
#' @param cond_init_ode A vector containing the initial conditions for an ODE model.
#' @param inputsModel A list containing the parameters used for the model evaluation.
#' @param parametersGradient  A list containing the parameters used for the evaluation of the gradient of the model
#' @param computeFIM A boolean giving TRUE id the FIM is compute, FALSE otherwise.
#' @return A list contaning a dataframe giving the results of the evaluation and a list giving the gradient of the model.

setGeneric("EvaluationModel",
           function(object, samplingTimesModel,cond_init_ode, inputsModel, parametersGradient, computeFIM )
           {
             standardGeneric("EvaluationModel")
           })

setMethod(f="EvaluationModel",
          signature=  "StatisticalModel",
          definition=function(object, samplingTimesModel,cond_init_ode, inputsModel, parametersGradient, computeFIM )
          {
            modelEquations = getEquationsStatisticalModel( object )
            classModel = class( modelEquations )

            if ( classModel == "ModelInfusionODEquations" )
            {
              evaluationModel = EvaluateModelODEInfusion( modelEquations, samplingTimesModel,cond_init_ode,
                                                          inputsModel, parametersGradient, computeFIM )

            } else if ( classModel == "ModelODEquations" ) {

              evaluationModel = EvaluateModelODE( modelEquations, samplingTimesModel,cond_init_ode,
                                                  inputsModel, parametersGradient, computeFIM )

            } else if ( classModel == "ModelEquations" )  {

              evaluationModel = EvaluateModel( modelEquations, samplingTimesModel,inputsModel, computeFIM )

            }else if ( classModel == "ModelInfusionEquations" ) {

              evaluationModel = EvaluateModelInfusion( modelEquations, samplingTimesModel,inputsModel, computeFIM )
            }

            return( evaluationModel )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate an \code{StatisticalModel} object.
#'
#' @rdname Evaluate
#' @param object A \code{StatisticalModel} object.
#' @param administrations An \code{Administration} object.
#' @param sampling_times A \code{SamplingTimes} object.
#' @param cond_init A list for the initial conditions of the \code{StatisticalModel} object.
#' @param fim  \code{FIM} object.
#' @return A \code{fim} object giving the Fisher Information Matrix of the \code{StatisticalModel} object.

setGeneric("Evaluate",
           function(object, administrations, sampling_times, cond_init, fim )
           {
             standardGeneric("Evaluate")
           })

setMethod(f="Evaluate",
          signature=  "StatisticalModel",
          definition=function(object, administrations, sampling_times, cond_init, fim )
          {

            # initial parameters
            MF_var = NA
            V_total = NA
            position = 1
            errorVariances = list()
            sigmaDerivatives = list()
            modelParameters = getModelParameters( object )
            modelEquations = getEquationsStatisticalModel( object )
            modelResponses = getResponsesStatisticalModel( object )

            # set parameters for evaluating the model
            parametersGradient = parametersForComputingGradient( object )

            parametersForEvaluateModel = setParametersForEvaluateModel( object,
                                                                        administrations,
                                                                        sampling_times, cond_init )

            samplingTimesModel = parametersForEvaluateModel$samplingTimesModel
            cond_init_ode = parametersForEvaluateModel$cond_init_ode
            inputsModel = parametersForEvaluateModel$inputsModel

            # for parameters not in equation but in the initial conditions
            modelParametersInEquation = parametersForEvaluateModel$modelParametersInEquation
            indexModelParametersInEquation = which( modelParameters %in% modelParametersInEquation )
            indexModelParametersNotInEquation = which( modelParameters %in% modelParametersInEquation == FALSE )

            # set model parameters for the evaluation
            object@model_parameters <- inputsModel$model_parameters[indexModelParametersInEquation]

            # -----------------------------------
            # evaluate model
            # -----------------------------------

            # sampling time for plot
            if ( object@computeFIM == FALSE ){

              minSamplingTimesModel = 0
              maxSamplingTimesModel = max( samplingTimesModel )

              samplingTimesModel = sort( unique( c( samplingTimesModel,
                                                    linspace( minSamplingTimesModel, maxSamplingTimesModel, 500 ) ) ) )

              evaluationModel = EvaluationModel( object,
                                                 samplingTimesModel,
                                                 cond_init_ode,
                                                 inputsModel,
                                                 parametersGradient, object@computeFIM )

              predictedResponses = evaluationModel$evaluationResponse
              responseGradient = evaluationModel$responseGradient


              # gradient of the parameters in model equations
              for ( response in modelResponses )
              {
                nameResponse = response@name

                if( length(  modelParametersInEquation ) == 1 )
                {
                  responseGradient[[nameResponse]] = responseGradient[[nameResponse]][indexModelParametersInEquation]
                }else{
                  responseGradient[[nameResponse]] = responseGradient[[nameResponse]][,indexModelParametersInEquation]
                }
              }

              return( list( fim = fim,
                            predictedResponses = predictedResponses, sensitivityIndicesModel = responseGradient ) )
            }
            else
            {
              # sampling time of the designs
              evaluationModel = EvaluationModel( object,
                                                 samplingTimesModel,
                                                 cond_init_ode,
                                                 inputsModel,
                                                 parametersGradient, object@computeFIM )

              # predicted responses and  gradients
              predictedResponses = evaluationModel$evaluationResponse
              responseGradient  = evaluationModel$responseGradient

              responseAllGradient = evaluationModel$responseAllGradient
              nTotalSamplingTimes = length( unlist( inputsModel$sampleTimeResponse ) )

              # gradient of the parameters in model equations
              for ( response in modelResponses )
              {
                nameResponse = response@name
                responseGradient[[nameResponse]] = responseAllGradient[,indexModelParametersInEquation]
              }

              responseAllGradient = responseAllGradient[,indexModelParametersInEquation]
              modelParameters = getModelParameters( object )
              modelParameters = modelParameters[indexModelParametersInEquation]
              responseAllGradient = as.matrix( responseAllGradient )

              ## gradient for parameter not in eqs but in initial conditions

              # parameter not in equations
              modelParametersNotInEquation = inputsModel$model_parameters[indexModelParametersNotInEquation]

              variablesNames = inputsModel$variablesNames
              responsePKNames = inputsModel$responsePKNames

              valueGradient = list()

              # assign values parameters not in equation & PK doses
              for( modelParameterNotInEquation in modelParametersNotInEquation )
              {
                name = getNameModelParameter( modelParameterNotInEquation )
                assign( name, getMu( modelParameterNotInEquation ) )
              }

              for ( responsePKName in responsePKNames )
              {
                assign( paste0("dose_",responsePKName ), inputsModel$dose[[responsePKName]] )
              }

              # compute and eval the gradients for variables in initial conditions
              for ( variableName in variablesNames )
              {
                equationCondInit = cond_init[[variableName]]

                gradientExpression = sapply( names( modelParametersNotInEquation ),
                                             function(x){ D( equationCondInit, x ) } )

                valueGradient = unlist( sapply( gradientExpression, function(x){eval(x)} ) )
              }

              if ( length( valueGradient ) !=0 )
              {
                gradientParameterNotInEquations = rep(0, dim( responseAllGradient )[1])
                gradientParameterNotInEquations[1] = valueGradient
                responseAllGradient = cbind( responseAllGradient, gradientParameterNotInEquations )
              }


              # evaluate FIM: individual, population, Bayesian

              object@model_parameters <- inputsModel$model_parameters
              modelParameters = getModelParameters( object )

              for ( response in modelResponses )
              {
                nameResponse = response@name
                samplingTimesResponse = getSampleTime( sampling_times[[nameResponse]] )

                predResp = predictedResponses[predictedResponses$time %in% samplingTimesResponse,nameResponse]
                errorModelDerivatives = EvaluateErrorModelDerivatives( response, predResp )
                errorVariances = append( errorVariances, bdiag( errorModelDerivatives$errorVariance ) )
                errorVariances = bdiag( errorVariances )

                for( errorModelParameter in errorModelDerivatives$sigmaDerivatives )
                {
                  emptysigmaDerivatives = matrix( 0, ncol = nTotalSamplingTimes, nrow = nTotalSamplingTimes )
                  nTime = length( inputsModel$sampleTimeResponse[[ nameResponse ]] )
                  range <- position:( position + nTime - 1 )
                  emptysigmaDerivatives[ range, range ] <- errorModelParameter
                  # Add the MF_beta matrix for the parameters for each response
                  # add the MF_var in different blocks for each response
                  sigmaDerivatives = c( sigmaDerivatives, list( emptysigmaDerivatives ) )
                }
                position <- position + nTime
              }

              # get fixed parameters
              dataFixedParameters = getFixedParameters( object )
              indexMuNoFixedParameters = dataFixedParameters$indexMuNoFixedParameters
              indexMuFixedParameters = dataFixedParameters$indexMuFixedParameters
              indexOmegaNoFixedParameters = dataFixedParameters$indexOmegaNoFixedParameters
              indexOmegaFixedParameters = dataFixedParameters$indexOmegaFixedParameters
              indexZeroOmegaParameters = dataFixedParameters$indexZeroOmegaParameters

              # names for the parameters
              namesMuNoFixedParameters = names( modelParameters[indexMuNoFixedParameters] )
              namesOmegaNoFixedParameters = names( modelParameters[indexOmegaNoFixedParameters] )

              # names for the columns/rows for the FIM
              muNames = paste0( "\u03bc_", namesMuNoFixedParameters )
              omegaNames = paste0( "\u03c9\u00B2_", namesOmegaNoFixedParameters )

              indexMuOmegaNoFixedParameters = sort( unique ( c( indexMuNoFixedParameters ) ) )

              modelParameters = getModelParameters( object )
              modelParametersNoFixedMu = modelParameters

              # vector for sigma of error model
              sigmaNames = c()
              parameterNames = c()

              # PopulationFim and IndividualFim
              for (response in modelResponses )
              {
                if ( is( fim, "PopulationFim" ) )
                {
                  MF = PopulationFIMEvaluateVariance(response,
                                                     modelEquations,
                                                     modelParametersNoFixedMu,
                                                     administrations,
                                                     sampling_times,
                                                     responseAllGradient,
                                                     errorVariances, sigmaDerivatives )

                  parameterNames <- c( muNames, omegaNames )

                }
                else
                  if ( is( fim, "IndividualFim" ) )
                  {
                    MF = IndividualFIMEvaluateVariance(response,
                                                       modelEquations,
                                                       modelParametersNoFixedMu,
                                                       administrations,
                                                       sampling_times,
                                                       responseAllGradient,
                                                       errorVariances, sigmaDerivatives )

                    parameterNames <- c( muNames )
                  }

                ### Add the MF_beta matrix for the parameters for each response
                ### add the MF_var in different blocks for each response

                if ( is.matrix( MF_var ))
                {
                  MF_var = bdiag( MF_var, MF$MF_var )
                  V_total = bdiag( V_total, MF$V )
                }
                else
                {
                  ### Create the first matrix and the first block
                  MF_var = bdiag( MF$MF_var )
                  V_total = bdiag( MF$V )
                }
                sigmaNames <- c( sigmaNames, getSigmaNames( response ) )
              }

              # compute MF_beta
              if ( length( indexMuNoFixedParameters )!=0 )
              {
                responseAllGradient = responseAllGradient[, indexMuNoFixedParameters ]
              }

              MF_beta =  t( responseAllGradient ) %*% solve( V_total ) %*% responseAllGradient

              # Bayesian FIM
              if ( is( fim, "BayesianFim" ) )
              {
                mu = c()
                omega2 = c()
                nParameter = 0

                for (parameter in modelParameters )
                {
                  omegaValue = getOmega( parameter )
                  omega2 = c( omega2, omegaValue^2 )
                  distribution = getDistribution( parameter )
                  if ( class( distribution )[1] == "LogNormalDistribution" )
                  {
                    muValue = getMu( parameter )
                    mu = c( mu, muValue )
                  }
                  else
                    if ( class( distribution )[1] == "NormalDistribution" )
                    {
                      mu = c( mu, 1 )
                    }
                  else
                    print("That distribution type is not supported for bayesian FIM")
                }

                # case: parameters fixed=FALSE & omega != 0
                indexParameters = intersect ( indexOmegaNoFixedParameters, indexMuNoFixedParameters )

                if (length(omega2)==1)
                {
                  Omega = as.matrix(omega2[indexParameters])
                  meanParametersMatrix = as.matrix( mu[indexParameters] )
                }else
                {
                  Omega = diag( omega2[indexParameters] )
                  meanParametersMatrix = diag( mu[indexParameters] )
                }

                # subsetting beta matrix: no fixed mu and no fixed omega
                colnames( MF_beta ) <- c( muNames )
                rownames( MF_beta ) <- c( muNames )
                muNames = paste0( "\u03bc_",names(modelParameters[indexParameters]))
                MF_beta = as.matrix(MF_beta[muNames,muNames ])

                # compute MF_beta
                if(  dim( MF_beta )[1] == 0 )
                {
                  identity = c(1.0)
                }else{
                  identity = diag( dim( MF_beta )[1] )
                }

                MF_beta = t( identity ) %*% MF_beta %*% identity + solve( meanParametersMatrix %*% Omega %*% meanParametersMatrix )

                # set col and row names and Fisher matrix
                colnames( MF_beta ) <- c( muNames )
                rownames( MF_beta ) <- c( muNames )

                fim <- `setMfisher<-`( fim, MF_beta )

                # parameters indices used for plot
                fim@parametersIndicesMuFIM = as.vector( indexParameters )

                # set mu and omega : no fixed no omega=0
                fim<-setOmega( fim, Omega )
                fim<-setMu( fim, meanParametersMatrix )

                return( list( fim = fim,
                              predictedResponses = predictedResponses,
                              sensitivityIndicesModel = responseGradient ) )
              }
              # case : PopulationFim & parameters with omega^2=0
              # case parameter omega == 0 fixed and mu fixed


              MF_i_total = bdiag( MF_beta, MF_var )

              if ( is( fim, "PopulationFim" ) )
              {
                if ( length( indexOmegaFixedParameters ) !=0 ){
                  MF_var = MF_var[-c(indexOmegaFixedParameters),-c(indexOmegaFixedParameters)]
                }
              }

              MF_i_total = bdiag( MF_beta, MF_var )
              parameterNames = c( parameterNames, sigmaNames )

              colnames( MF_i_total ) <- c( parameterNames )
              rownames( MF_i_total ) <- c( parameterNames )

              fim <- `setMfisher<-`( fim, MF_i_total )

              # parameters indices used for plot
              fim@parametersIndicesMuFIM = indexMuNoFixedParameters
              fim@parametersIndicesOmegaFIM = indexOmegaNoFixedParameters

              return( list( fim = fim,
                            predictedResponses = predictedResponses,
                            sensitivityIndicesModel = responseGradient ) )
            }
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the fixed and non fixed model parameters.
#'
#' @name getFixedParameters
#' @param object A \code{StatisticalModel} object.
#' @return A list that contains the name and indices of the fixed parameters.

setGeneric("getFixedParameters",
           function(object)
           {
             standardGeneric("getFixedParameters")
           }
)

setMethod("getFixedParameters",
          "StatisticalModel",
          function(object)
          {

            modelParameters = getModelParameters( object )

            # indices parameters mu and omega fixed or not
            indexMuFixedParameters = which( lapply( modelParameters, slot, "fixedMu" ) == TRUE )
            indexOmegaFixedParameters =which( lapply( modelParameters, slot, "fixedOmega" ) == TRUE )
            indexMuNoFixedParameters = which( lapply( modelParameters, slot, "fixedMu" ) == FALSE )
            indexOmegaNoFixedParameters = which( lapply( modelParameters, slot, "fixedOmega" ) == FALSE )

            indexZeroOmegaParameters = which( lapply( modelParameters, slot, "omega" ) == 0 )
            indexZeroMuParameters = which( lapply( modelParameters, slot, "mu" ) == 0 )
            indexNoZeroOmegaParameters = which( lapply( modelParameters, slot, "omega" ) != 0 )
            indexNoZeroMuParameters = which( lapply( modelParameters, slot, "mu" ) != 0 )

            indexMuFixedParameters = sort( unique( c( indexMuFixedParameters, indexZeroMuParameters ) ) )
            indexOmegaFixedParameters = sort( unique( c( indexOmegaFixedParameters, indexZeroOmegaParameters ) ) )

            return ( output = list( indexMuFixedParameters = indexMuFixedParameters,
                                    indexOmegaFixedParameters = indexOmegaFixedParameters,
                                    indexMuNoFixedParameters = indexMuNoFixedParameters,
                                    indexOmegaNoFixedParameters = indexOmegaNoFixedParameters,
                                    indexZeroOmegaParameters = indexZeroOmegaParameters,
                                    indexZeroMuParameters = indexZeroMuParameters,
                                    indexNoZeroOmegaParameters = indexNoZeroOmegaParameters,
                                    indexNoZeroMuParameters = indexNoZeroMuParameters) )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Define model equations
#'
#' @name defineModelEquations
#' @param object A \code{StatisticalModel} object.
#' @param equations An expression giving the equations of the model.
#' @return The \code{StatisticalModel} object with the equations.

setGeneric("defineModelEquations",
           function(object, equations)
           {
             standardGeneric("defineModelEquations")
           }
)

setMethod("defineModelEquations",
          "StatisticalModel",
          function(object, equations )
          {
            if ( class( equations ) %in% c( "PKModel","PKPDModel" ) ) {

              object@modelEquations =  getEquations( equations )
            }else{
              object@modelEquations =  equations
            }
            return( object )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Define a parameter of a statistical model.
#'
#' @name defineParameter
#' @param object A \code{StatisticalModel} object.
#' @param parameter An expression giving a parameter of the \code{StatisticalModel} object.
#' @return Return \code{StatisticalModel} object with new parameters.

setGeneric("defineParameter",
           function(object, parameter)
           {
             standardGeneric("defineParameter")
           }
)
setMethod("defineParameter",
          "StatisticalModel",
          function(object, parameter )
          {
            parameterName = getNameModelParameter( parameter )

            object@model_parameters[[ parameterName ]] = parameter

            object@model_parameters = object@model_parameters[ order( names( object@model_parameters ) ) ]

            return( object )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Define the parameters of a statistical model.
#'
#' @name defineParameters
#' @param object A \code{StatisticalModel} object.
#' @param listOfParameters A list of string giving the parameters of the \code{StatisticalModel} object.
#' @return Return \code{StatisticalModel} object with new parameters.

setGeneric("defineParameters",
           function(object, listOfParameters)
           {
             standardGeneric("defineParameters")
           }
)
setMethod("defineParameters",
          "StatisticalModel",
          function(object, listOfParameters )
          {
            listOfParameters = listOfParameters[order(names(listOfParameters))]

            for (i in 1:length( listOfParameters)) {

              parameter = listOfParameters[[i]]

              parameterName = getNameModelParameter( parameter )

              object@model_parameters[[ parameterName ]] = parameter
            }
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the content of a \code{StatisticalModel} object.
#'
#' @rdname show
#' @param object \code{StatisticalModel} object.
#' @return Display the responses name of the model equations,
#' the ordinary derivatives of the model equations (for an ODE model), the parameters of the model.

setMethod("show",
          signature = "StatisticalModel",
          definition = function(object)
          {
            modelEquations = getEquationsStatisticalModel(object)

            cat("*********************** Statistical model ******************************\n")

            # ------------------------------------------------------------------------
            # ModelEquations
            # ------------------------------------------------------------------------

            if( class(object@modelEquations ) %in% c("ModelEquations")){

              cat( "\n **** Model equation:","\n\n" )

              for( response in object@responses )
              {
                nameResponse = getNameResponse( response )
                equation = getEquation( object@modelEquations, nameResponse )
                cat( paste0(nameResponse ," = ", equation,"\n\n" ) )

              }

              cat( " **** Model error:","\n\n" )

              for( response in object@responses )
              {

                nameResponse = getNameResponse( response )

                cat( " Model error",nameResponse, "\n" )

                modelError = getModelError( response )

                show( modelError )

                cat( "\n" )
              }
            }

            # ------------------------------------------------------------------------
            # ModelInfusionEquations
            # ------------------------------------------------------------------------

            else  if( class(object@modelEquations ) %in% c("ModelInfusionEquations")){

              cat( "\n **** Model equation:","\n\n" )

              responses = getResponsesStatisticalModel(object)
              namesResponses = names( responses )

              for (nameResponse in namesResponses )
              {
                nameEquations = paste0( c( "DuringInfusion_","AfterInfusion_" ), nameResponse )

                for (nameEquation in nameEquations )
                {
                  equation = getEquation( object@modelEquations, nameEquation )

                  cat( paste0( paste0( nameEquation ) ," = ", equation,"\n\n" ) )
                }
              }

              cat( " **** Model error:","\n\n" )

              for( response in object@responses )
              {
                nameResponse = getNameResponse( response )

                cat( " Model error",nameResponse, "\n" )

                modelError = getModelError( response )

                show( modelError )

                cat( "\n" )
              }
            }

            # ------------------------------------------------------------------------
            # ModelODEquations
            # ------------------------------------------------------------------------

            else if(class(object@modelEquations) %in% c("ModelODEquations"))
            {
              derivatives <- getDerivatives( object@modelEquations )

              cat("\n")
              cat("**** Model responses:","\n\n" )

              for( response in object@responses)
              {
                nameResponse = getNameResponse( response )

                equation = getEquation( object@modelEquations, nameResponse )

                cat( paste0( nameResponse ," = ", equation,"\n" ) )

              }
              cat("\n")
              cat("**** Model error:","\n\n" )

              for( response in object@responses )
              {
                nameResponse = getNameResponse( response )

                cat("Model error",nameResponse, "\n" )

                modelError = getModelError( response )

                show( modelError )
                cat("\n")
              }

              cat("**** Ordinary derivatives of model equation:\n\n")

              for(i in 1:length(derivatives))
              {
                cat( names(derivatives)[i]," = ")
                print(derivatives[[i]][[1]])
              }
              cat( "\n" )
            }
            # ------------------------------------------------------------------------
            # ModelInfusionODEquations
            # ------------------------------------------------------------------------

            else if(class(modelEquations) %in% c("ModelInfusionODEquations"))
            {
              responses = getResponsesStatisticalModel(object)
              namesResponses = names( responses )

              cat("\n")

              cat("**** Model responses:","\n\n" )

              equationsDuringInfusion = modelEquations@duringInfusionResponsesEquations
              equationsAfterInfusion = modelEquations@afterInfusionResponsesEquations

              for (nameResponse in namesResponses )
              {
                nameEquationsDuringInfusion = paste0( c( "DuringInfusion_" ), nameResponse )
                nameEquationsAfterInfusion = paste0( c( "AfterInfusion_" ), nameResponse )

                cat( paste0( nameEquationsDuringInfusion ," = ", equationsDuringInfusion[[nameEquationsDuringInfusion]],"\n" ) )
                cat( paste0( nameEquationsAfterInfusion ," = ", equationsAfterInfusion[[nameEquationsAfterInfusion]],"\n" ) )
              }

              cat("\n")
              cat("**** Model error:","\n\n" )

              for( response in object@responses )
              {
                nameResponse = getNameResponse( response )

                cat("Model error",nameResponse, "\n" )

                modelError = getModelError( response )

                show( modelError )
                cat("\n")
              }

              cat("\n")
              cat("**** Ordinary derivatives of model equation:\n\n")

              derivatives = getDerivatives( modelEquations )

              namesDerivatives = names( derivatives )

              for (namesDerivative in namesDerivatives )
              {
                cat( paste0( namesDerivative ," = ", derivatives[[namesDerivative]],"\n" ) )
              }
            }

            # ------------------------------------------------------------------------
            # parameters
            # ------------------------------------------------------------------------

            nameCol <- c()
            muCol <- c()
            omegaCol <- c()
            distributionCol <- c()
            for(parameter in object@model_parameters)
            {
              nameCol <- c( nameCol, getNameModelParameter(parameter))

              muCol <- c(muCol, getMu(parameter))
              omegaCol <- c(omegaCol, getOmega(parameter))
              distributionCol <- c(distributionCol, class(getDistribution(parameter))[1])
            }

            parameterFrame <- matrix(c( as.character(muCol),
                                        as.character(omegaCol),
                                        distributionCol),ncol=3)

            colnames(parameterFrame) <- c("\u03bc", "\u03c9", "Distribution" )
            rownames(parameterFrame) <- nameCol

            cat("\n")
            cat("**** Model parameters: \n")
            print(parameterFrame)


          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the model parameters of a statistical model.
#'
#' @name getModelParameters
#' @param object \code{getModelParameters} object.
#' @return A list giving the model parameters.

setGeneric("getModelParameters",
           function(object)
           {
             standardGeneric("getModelParameters")
           }
)
setMethod("getModelParameters",
          "StatisticalModel",
          function(object)
          {
            return(object@model_parameters)
          }

)

# -------------------------------------------------------------------------------------------------------------------
#' Get the responses of a statistical model.
#'
#' @name getResponsesStatisticalModel
#' @param object A \code{getResponsesStatisticalModel} object.
#' @return A list giving the responses of a statistical model.

setGeneric("getResponsesStatisticalModel",
           function(object)
           {
             standardGeneric("getResponsesStatisticalModel")
           }
)
setMethod("getResponsesStatisticalModel",
          "StatisticalModel",
          function(object)
          {
            return(object@responses)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a statistical model.
#'
#' @name getEquationsStatisticalModel
#' @param object A \code{StatisticalModel} object.
#' @return A \code{ModelEquationsg} object giving the equations of the \code{StatisticalModel} object.

setGeneric(
  "getEquationsStatisticalModel",
  function(object) {
    standardGeneric("getEquationsStatisticalModel")
  })

setMethod("getEquationsStatisticalModel",
          signature("StatisticalModel"),
          function(object)
          {
            return( object@modelEquations)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the SE and RSE of the parameters.
#'
#' @name getErrorModelStandardErrors
#' @param object A \code{StatisticalModel} object.
#' @param fim A \code{Fim} object giving the Fisher Information Matrix.
#' @return A dataframe giving he SE and RSE of the parameters.

setGeneric("getErrorModelStandardErrors",
           function(object, fim)
           {
             standardGeneric("getErrorModelStandardErrors")
           }
)
setMethod("getErrorModelStandardErrors",
          "StatisticalModel",
          function(object, fim)
          {
            se <- getSE(fim)
            sigmaCol <- c()
            rseCol <- c()

            responses = getResponsesStatisticalModel( object )

            for(response in responses)
            {
              modelError <- getModelError(response)
              sigmaCol <- c( sigmaCol, getSigmaValues(modelError))
            }

            if (length(se) >1 ){
              if (length(se) != length(sigmaCol) ){
                se <- se[-c(1:(length(se)-length(sigmaCol)))]
              }else{
                se <- (sort(se))
              }}

            rseCol <- c(rseCol,  se/sigmaCol*100 )

            parameterFrame <- data.frame( value = sigmaCol, se = se, "rse"= rseCol, row.names=NULL, check.names=FALSE)

            return(parameterFrame)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Define a variable in a statistical model.
#'
#' @name defineVariable
#' @param .Object A \code{StatisticalModel} object.
#' @param variable A character string giving the variable to be defined.
#' @return The \code{StatisticalModel} object with the new variable.

setGeneric("defineVariable",
           function(.Object, variable )
           {
             standardGeneric("defineVariable")
           }
)

setMethod(f="defineVariable",
          signature=  "StatisticalModel",
          definition=function(.Object, variable)
          {
            variableName = variable@name
            .Object@variables[[ variableName ]] = variable
            return(.Object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Define variables in a statistical model.
#'
#' @name defineVariables
#' @param .Object A \code{StatisticalModel} object.
#' @param listOfVariables A list of character string giving the variables to be defined.
#' @return The \code{StatisticalModel} object with the new variables.

setGeneric( "defineVariables",
            function(.Object, listOfVariables ) #
            {
              standardGeneric( "defineVariables" )
            }
)

setMethod(f = "defineVariables",
          signature =  "StatisticalModel",
          definition = function(.Object, listOfVariables )
          {

            for (i in 1:length( listOfVariables)) {

              modelVariable = ModelVariable( listOfVariables[[i]] )

              variableName = modelVariable@name

              .Object@variables[[ variableName ]] = modelVariable

            }
            return(.Object)
          }
)

##########################################################################################################
# END Class "StatisticalModel"
##########################################################################################################


