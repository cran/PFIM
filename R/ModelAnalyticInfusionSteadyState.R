#' Class "ModelAnalyticInfusionSteadyState"
#'
#' @description The class \code{Model} defines information concerning the construction of an analytical model in infusion in steady state.
#' The class \code{ModelAnalyticInfusionSteadyState} inherits from the class \code{ModelAnalyticInfusion}.
#'
#' @name ModelAnalyticInfusionSteadyState-class
#' @aliases ModelAnalyticInfusionSteadyState
#' @docType class
#' @include ModelAnalyticInfusion.R
#' @export

ModelAnalyticInfusionSteadyState = setClass(
  Class = "ModelAnalyticInfusionSteadyState",
  contains = "ModelAnalyticInfusion")

setMethod(
  f="initialize",
  signature="ModelAnalyticInfusionSteadyState",
  definition= function (.Object, name, description, equations, outcomes, parameters, modelError, initialConditions, odeSolverParameters)
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(description))
    {
      .Object@description = description
    }
    if(!missing(equations))
    {
      .Object@equations = equations
    }
    if(!missing(outcomes))
    {
      .Object@outcomes = outcomes
    }
    if(!missing(parameters))
    {
      .Object@parameters = parameters
    }
    if(!missing(modelError))
    {
      .Object@modelError = modelError
    }
    if(!missing(initialConditions))
    {
      .Object@initialConditions = initialConditions
    }
    if(!missing( odeSolverParameters ) )
    {
      .Object@odeSolverParameters = odeSolverParameters
    }
    validObject(.Object)
    return (.Object )
  }
)

setMethod(
  f="initialize",
  signature="ModelAnalyticInfusionSteadyState",
  definition= function (.Object, name, description, equations, outcomes, parameters, modelError)
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(description))
    {
      .Object@description = description
    }
    if(!missing(equations))
    {
      .Object@equations = equations
    }
    if(!missing(outcomes))
    {
      .Object@outcomes = outcomes
    }
    if(!missing(parameters))
    {
      .Object@parameters = parameters
    }
    if(!missing(modelError))
    {
      .Object@modelError = modelError
    }
    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
# EvaluateModel
# ======================================================================================================

setMethod(f="EvaluateModel",
          signature =  "ModelAnalyticInfusionSteadyState",
          definition = function( object, arm )
          {
            # ===============================================
            # outcomes
            # ===============================================

            administrations = getAdministrations( arm )
            samplingTimes = getSamplingTimes( arm )

            outcomesWithAdministration = unlist( lapply( administrations, function(x) getOutcome(x) ) )
            outcomesWithNoAdministration = unlist( lapply( samplingTimes, function(x) getOutcome(x) ) )
            outcomesWithNoAdministration = outcomesWithNoAdministration[ outcomesWithNoAdministration!= outcomesWithAdministration ]

            # ===============================================
            # outcomes of the model
            # ===============================================

            outcomes = c( outcomesWithAdministration, outcomesWithNoAdministration )

            # ===============================================
            # outcomes of the evaluation
            # ===============================================

            outcomesModel = getOutcomesForEvaluation( object )
            names( outcomesModel ) = outcomes

            # ===============================================
            # convert model equations string to expression
            # ===============================================

            modelEquations = getEquations( object )
            modelEquations$duringInfusion = lapply( getEquationsDuringInfusion( object ), function(x) parse( text = x ) )
            modelEquations$afterInfusion = lapply( getEquationsAfterInfusion( object ), function(x) parse( text = x ) )

            # ===============================================
            # model parameters
            # assign parameters mu values
            # ===============================================

            parameters = getParameters( object )
            modelParametersNames = lapply( parameters, function(x) getName(x) )

            for ( parameter in parameters )
            {
              parameterMu = getMu( parameter )
              parameterName = getName( parameter )
              assign( parameterName, parameterMu )
            }

            # ======================================================
            # model sampling times = sampling times of all responses
            # ======================================================

            samplingTimesOutcome = list()

            for ( outcome in outcomes )
            {
              samplingTimesOutcome[[outcome]] = getSamplings( getSamplingTime( arm, outcome ) )
            }

            samplingTimesModel = sort( unique( c( 0, unlist( samplingTimesOutcome ) ) ) )
            colnames( samplingTimesModel ) = NULL

            # ===============================================
            # function: modelAnalyticInfusion evaluation
            # ===============================================

            evaluationOutcomes = function( object, arm, outcomesWithAdministration, outcomesWithNoAdministration,
                                           modelEquations, samplingTimesOutcome, samplingTimesModel )
            {
              # =====================================
              # parameters matrix for ode evaluation
              # =====================================

              timeMatrix = list()
              indexTime = list()
              inputsModel = list()
              Tinfs = list()
              dataForEvaluation = list()

              # =======================================
              # indicator in the dataframe for infusion
              # =======================================

              duringAndAfter = rep("After",length( samplingTimesModel ) )
              duringAndAfter[1] = "No calcul"

              # ===============================================
              #  data for evaluation
              # ===============================================

              for ( outcome in outcomesWithAdministration )
              {
                administration = getAdministration( arm, outcome )

                tau = getTau( administration )

                inputsModel$dose[[outcome]] = getDose( administration )
                inputsModel$Tinf[[outcome]] = getTinf( administration )
                inputsModel$timeDose[[outcome]] = getTimeDose( administration )

                # =======================================
                # assign tau value
                # =======================================

                assign( "tau", tau )

                if ( tau !=0 )
                {
                  maxSamplingTimeOutcome = max( samplingTimesOutcome[[outcome]] )
                  inputsModel$timeDose[[outcome]] = seq( 0, maxSamplingTimeOutcome, tau )
                  inputsModel$Tinf[[outcome]] = rep( inputsModel$Tinf[[outcome]], length( inputsModel$timeDose[[outcome]] ) )
                  inputsModel$dose[[outcome]] = rep( inputsModel$dose[[outcome]], length( inputsModel$timeDose[[outcome]] ) )
                }

                inputsModel$timeMatrix[[outcome]] = matrix( ( c( inputsModel$timeDose[[outcome]],
                                                                 inputsModel$timeDose[[outcome]] + inputsModel$Tinf[[outcome]] ) ),
                                                            length( inputsModel$timeDose[[outcome]] ), 2 )
                # =======================================
                # indices for doses and Tinf
                # =======================================

                indicesDoses = c()
                indicesDoses[1] = 1
                timeDoseInfusionEnd = c()
                timeDoseInfusion = samplingTimesModel

                vec = c( inputsModel$timeDose[[outcome]], max( sort( unique( c( samplingTimesModel, inputsModel$Tinf[[outcome]] ) ) ) ) )

                # =======================================
                # indices for doses
                # =======================================

                for ( iterk in 1:length( vec ) )
                {
                  indicesDoses[samplingTimesModel >= vec[iterk] & samplingTimesModel <= vec[iterk+1]] = iterk
                }

                # =======================================
                # during and after
                # =======================================

                iterk = 1

                for ( t in samplingTimesModel )
                {
                  assign( "t", t )

                  indexTime[[outcome]] = which( apply( inputsModel$timeMatrix[[outcome]], 1, findInterval, x = t ) == 1)

                  if ( length( indexTime[[outcome]]  ) != 0  )
                  {
                    duringAndAfter[iterk] = "duringInfusion"
                  }else{
                    duringAndAfter[iterk] = "afterInfusion"
                  }
                  iterk=iterk+1
                }

                # =======================================
                # number of does
                # =======================================

                numberOfDoses =  length( inputsModel$dose[[outcome]] )

                # =======================================
                # sampling times for dose infusion
                # =======================================

                samplingTimeDose = matrix( 0.0, length( samplingTimesModel ), numberOfDoses )

                for ( i in 1:numberOfDoses )
                {
                  samplingTimeDose[,i] = pmax( samplingTimesModel - inputsModel$timeDose[[outcome]][i], 0 )
                }

                samplingTimeDose = as.matrix( samplingTimeDose )

                dataForEvaluation[[outcome]] = data.frame( samplingTimesModel = samplingTimesModel,
                                                           duringAndAfter = duringAndAfter,
                                                           indicesDoses = indicesDoses,
                                                           outcome = outcome,
                                                           samplingTimeDose = I(samplingTimeDose) )
              }

              # ==============================================
              # evaluation outcomes with administration
              # ==============================================

              dataForEvaluation = do.call( "rbind", dataForEvaluation )
              dataForEvaluation = dataForEvaluation[ order( dataForEvaluation$samplingTimesModel ), ]
              rownames( dataForEvaluation ) = 1:dim( dataForEvaluation )[1]

              # =======================================
              # model evaluation
              # =======================================

              modelEvaluation = matrix( 0.0,length( samplingTimesModel ),length( outcomesWithAdministration ) )

              for( iterk in 1:dim( dataForEvaluation )[1] )
              {
                duringAndAfter = dataForEvaluation$duringAndAfter[iterk]
                indicesDoses = dataForEvaluation$indicesDoses[iterk]
                samplings = dataForEvaluation[iterk,5:dim( dataForEvaluation )[2]]
                outcome  = dataForEvaluation$outcome[iterk]

                # =======================================
                # evaluation infusion during
                # =======================================

                if( duringAndAfter == "duringInfusion")
                {
                  # =======================================
                  # first dose
                  # =======================================

                  if ( indicesDoses == 1)
                  {
                    assign("t",samplings[indicesDoses] )
                    assign( paste0("dose_",outcome ), inputsModel$dose[[outcome]][indicesDoses] )
                    assign( paste0("Tinf_",outcome ), inputsModel$Tinf[[outcome]][indicesDoses] )

                    for ( iter in 1:length( outcomesWithAdministration ) )
                    {
                      modelEvaluation[iterk,iter] = eval( modelEquations$duringInfusion[[iter]] )
                    }
                  }

                  # =======================================
                  # after the first dose
                  # =======================================

                  if ( indicesDoses > 1)
                  {
                    samplings =  samplings[1:indicesDoses]
                    samplingDuring= tail( samplings, 1 )
                    samplingAfter = samplings[1:(indicesDoses-1)]

                    doseDuring = inputsModel$dose[[outcome]][indicesDoses]
                    dosesAfter = inputsModel$dose[[outcome]][1:(indicesDoses-1)]

                    tinfDuring = inputsModel$Tinf[[outcome]][indicesDoses]
                    tinfAfter = inputsModel$Tinf[[outcome]][1:(indicesDoses-1)]

                    assign("t",samplingDuring )
                    assign( paste0("dose_",outcome ), doseDuring )
                    assign( paste0("Tinf_",outcome ), tinfDuring )

                    for ( iter in 1:length( outcomesWithAdministration ) )
                    {
                      modelEvaluation[iterk,iter] = eval( modelEquations$duringInfusion[[iter]] )
                    }

                    for ( i in 1:(indicesDoses-1) )
                    {
                      assign("t",samplingAfter[i] )
                      assign( paste0("dose_",outcome ), dosesAfter[i] )
                      assign( paste0("Tinf_",outcome ), tinfAfter[i] )

                      for ( iter in 1:length( outcomesWithAdministration ) )
                      {
                        modelEvaluation[iterk,iter] = modelEvaluation[iterk,iter] + c( eval( modelEquations$afterInfusion[[iter]] ) )
                      }
                    }
                  }
                }
                else if( duringAndAfter == "afterInfusion")
                {
                  # =======================================
                  # first dose
                  # =======================================

                  if ( indicesDoses == 1)
                  {
                    assign("t",samplings[indicesDoses] )
                    assign( paste0("dose_",outcome ), inputsModel$dose[[outcome]][indicesDoses] )
                    assign( paste0("Tinf_",outcome ), inputsModel$Tinf[[outcome]][indicesDoses] )

                    for ( iter in 1:length( outcomesWithAdministration ) )
                    {
                      modelEvaluation[iterk,iter]  = c( eval( modelEquations$afterInfusion[[iter]] ))
                    }
                  }

                  # =======================================
                  # after the first dose
                  # =======================================

                  if ( indicesDoses > 1)
                  {
                    samplings =  samplings[1:indicesDoses]
                    samplingDuring= tail( samplings, 1 )
                    samplingAfter = samplings[1:( indicesDoses-1 )]

                    doseDuring = inputsModel$dose[[outcome]][indicesDoses]
                    dosesAfter = inputsModel$dose[[outcome]][1:( indicesDoses-1 )]

                    tinfDuring = inputsModel$Tinf[[outcome]][indicesDoses]
                    tinfAfter = inputsModel$Tinf[[outcome]][1:( indicesDoses-1 )]

                    assign( "t",samplingDuring )
                    assign( paste0( "dose_",outcome ), doseDuring )
                    assign( paste0( "Tinf_",outcome ), tinfDuring )

                    for ( iter in 1:length( outcomesWithAdministration ) )
                    {
                      modelEvaluation[iterk,iter] = c( eval( modelEquations$afterInfusion[[iter]] ) )
                    }

                    for ( i in 1:(indicesDoses-1) )
                    {
                      assign( "t",samplingAfter[i] )
                      assign( paste0( "dose_", outcome ), dosesAfter[i] )
                      assign( paste0( "Tinf_", outcome ), tinfAfter[i] )

                      for ( iter in 1:length( outcomesWithAdministration ) )
                      {
                        modelEvaluation[iterk,iter] = modelEvaluation[iterk,iter] + c( eval( modelEquations$afterInfusion[[iter]] ) )
                      }
                    }
                  }
                }
              } # end outcome

              modelEvaluation = as.data.frame( modelEvaluation )
              colnames ( modelEvaluation ) = outcomesWithAdministration

              # ==============================================
              # evaluation Outcomes With NoAdministration
              # ==============================================

              evaluationOutcomesWithNoAdministration = list()

              duringAndAfter = dataForEvaluation$duringAndAfter

              if( length( outcomesWithNoAdministration ) != 0 )
              {
                for( outcomeWithAdministration in outcomesWithAdministration )
                {
                  assign( outcomeWithAdministration, modelEvaluation[,outcomeWithAdministration] )
                }

                for( outcomeWithNoAdministration in outcomesWithNoAdministration )
                {
                  for ( typeOfInfusion in duringAndAfter )
                  {
                    if ( typeOfInfusion ==  "duringInfusion" )
                    {
                      evaluationOutcomesWithNoAdministration[[outcomeWithNoAdministration]] = eval( modelEquations$duringInfusion[[ outcomeWithNoAdministration ]] )
                    }else if ( typeOfInfusion == "duringInfusion" )
                    {
                      evaluationOutcomesWithNoAdministration[[outcomeWithNoAdministration]] = eval( modelEquations$afterInfusion[[ outcomeWithNoAdministration ]] )
                    }
                  }
                }

                modelEvaluation = as.data.frame( do.call( "cbind", list( samplingTimesModel, modelEvaluation, evaluationOutcomesWithNoAdministration ) ) )
                colnames( modelEvaluation ) = c( "time", outcomes )

              } else if( length( outcomesWithNoAdministration ) == 0 )
              {
                modelEvaluation = as.data.frame( do.call( "cbind", list( samplingTimesModel, modelEvaluation ) ) )
                colnames( modelEvaluation ) = c( "time", outcomesWithAdministration )
              }

              return( list( modelEvaluation = modelEvaluation ) )

            } # end function evaluationOutcomes

            # ==============================================================
            # scale with the outputs in evaluation & the sampling times
            # ==============================================================

            scaleModelResponse = function( modelEvaluation, outcomes, samplingTimesOutcome, samplingTimesModel )
            {
              evaluationOutcomes = list()

              # =======================================
              # evaluate output model
              # =======================================

              for ( outcome in outcomes )
              {
                assign( outcome, modelEvaluation$modelEvaluation[, outcome] )

                modelEvaluation$modelEvaluation[,outcome ] = eval( parse( text = outcomesModel[[ outcome ]] ) )
              }

              # =========================================
              # take the sampling times for each response
              # =========================================

              for ( outcome in outcomes )
              {
                indexSamplingTimesOutcome = match( samplingTimesOutcome[[outcome]], samplingTimesModel )

                evaluationOutcomes[[ outcome ]] =  as.data.frame( cbind( samplingTimesModel[indexSamplingTimesOutcome],
                                                                         modelEvaluation$modelEvaluation[ indexSamplingTimesOutcome, outcome ] ) )

                colnames( evaluationOutcomes[[ outcome ]] ) = c( "time", outcome )
              }
              return( evaluationOutcomes )
            }

            # ===============================================
            # model evaluation
            # ===============================================

            evaluationOutcomesWithoutOutputsScaling = evaluationOutcomes( object, arm, outcomesWithAdministration, outcomesWithNoAdministration,
                                                                          modelEquations, samplingTimesOutcome, samplingTimesModel )



            evaluationOutcomesWithOutputsScaling = scaleModelResponse( evaluationOutcomesWithoutOutputsScaling,
                                                                       outcomes, samplingTimesOutcome, samplingTimesModel )

            # =================================================
            # substitute for outcomes evaluation with scaling
            # =================================================

            subsituteTmp = list()
            modelEquationsTmp = getEquations( object )

            for( outcome in outcomes )
            {
              modelEquationsTmp$duringInfusion[[outcome]] = paste0("(", modelEquationsTmp$duringInfusion[[outcome]],")")
              modelEquationsTmp$afterInfusion[[outcome]] = paste0("(", modelEquationsTmp$afterInfusion[[outcome]],")")

              subsituteTmp$duringInfusion[[outcome]] = parse( text = gsub( outcome, modelEquationsTmp$duringInfusion[[outcome]], outcomesModel[[outcome]] ) )
              subsituteTmp$afterInfusion[[outcome]] = parse( text = gsub( outcome, modelEquationsTmp$afterInfusion[[outcome]], outcomesModel[[outcome]] ) )
            }

            modelEquations = subsituteTmp
            names( modelEquations ) = names( modelEquationsTmp )

            # ===============================================
            # compute sensitivity indices
            # ===============================================

            # =========================================
            # model parameters
            # =========================================

            parameters = getParameters( object )
            numberOfParameters = getNumberOfParameters( object )

            # =========================================
            # parameters for computing gradients
            # =========================================

            parametersGradient = parametersForComputingGradient( object )
            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            outcomesGradient = list()
            resultsGrad = list()

            for ( iterShifted in 1:dim( shiftedParameters)[2] )
            {
              valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

              # =========================================
              # assign parameter values
              # =========================================

              for( iterParameter in 1:numberOfParameters )
              {
                parameterMu = valuesParameters[iterParameter]
                parameterName = getName( parameters[[iterParameter]] )
                assign( parameterName, parameterMu )
              }

              out = evaluationOutcomes( object, arm, outcomesWithAdministration, outcomesWithNoAdministration,
                                        modelEquations, samplingTimesOutcome, samplingTimesModel )

              resultsGrad[[iterShifted]] = out$modelEvaluation
            }

            for ( outcome in outcomes )
            {
              tmp = lapply(resultsGrad, "[", outcome )
              tmp = do.call( cbind,tmp )

              coefs = list()

              for( i in 1 : length( samplingTimesModel ) )
              {
                coefs[[i]] = solve( do.call("cbind", Xcols),tmp[i,] )/frac
                {
                  coefs[[i]] = coefs[[i]][1 + seq_along( parameters ) ]
                }
              }

              outcomesGradient[[outcome]] = data.frame( samplingTimesModel, do.call( "rbind",coefs ) )

              # =======================================================
              # match sampling times responses with sampling time model
              # =======================================================

              indexSamplingTimes = match( samplingTimesOutcome[[outcome]], samplingTimesModel )
              outcomesGradient[[outcome]] = outcomesGradient[[outcome]][indexSamplingTimes,]
              colnames( outcomesGradient[[outcome]] ) = c("time",modelParametersNames)
            }

            # ===============================================
            # outputs
            # ===============================================

            outcomesAllGradient = list()

            modelError = getModelError( object )

            for( outcome in outcomes )
            {
              index = which( sapply( modelError, function (x) getOutcome(x) == outcome ) )

              if ( length( index ) != 0 )
              {
                outcomesAllGradient[[outcome]] = outcomesGradient[[outcome]]
              }
            }

            outcomesAllGradient = as.data.frame( do.call( rbind, outcomesAllGradient ) )
            outcomesAllGradient = outcomesAllGradient[,-c(1)]
            rownames( outcomesAllGradient ) = NULL

            return( list( evaluationOutcomes = evaluationOutcomesWithOutputsScaling,
                          outcomesGradient = outcomesGradient,
                          outcomesAllGradient = outcomesAllGradient ) )
          })

##########################################################################################################
# END Class "ModelAnalyticInfusionSteadyState"
##########################################################################################################
