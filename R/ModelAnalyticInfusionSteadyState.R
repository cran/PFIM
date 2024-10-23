
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

#' initialize
#' @param .Object .Object
#' @param name name
#' @param description description
#' @param equations equations
#' @param outcomes outcomes
#' @param parameters parameters
#' @param modelError modelError
#' @return ModelAnalyticInfusionSteadyState
#' @export
#'
setMethod( f="initialize",
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

#' @rdname defineModelEquationsFromStringToFunction
#' @export

setMethod("defineModelEquationsFromStringToFunction",
          signature("ModelAnalyticInfusionSteadyState"),
          function( object, parametersNames, outcomesWithAdministration, outcomesWithNoAdministration )
          {
            # get all the outcomes model & for evaluation
            outcomesForEvaluation = getOutcomesForEvaluation( object )
            outcomesForEvaluationWithAdministration = outcomesForEvaluation[1:length(outcomesWithAdministration)]
            outcomesForEvaluationWithNoAdministration = outcomesForEvaluation[(1+length(outcomesWithAdministration)):length(outcomesForEvaluation)]

            # get the equations
            equationsDuringInfusionModel = getEquationsDuringInfusion( object )
            equationsAfterInfusionModel = getEquationsAfterInfusion( object )

            # arguments for the function
            doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
            TinfNames = paste( "Tinf_", outcomesWithAdministration, sep = "" )

            argsOutcomesWithAdministration = c( doseNames, TinfNames, parametersNames, "tau", "t" )

            # =========================================================
            # equations for outcomesWithAdministration
            # =========================================================

            # equations during infusion for outcomesWithAdministration
            equationsDuringInfusion = equationsDuringInfusionModel[outcomesWithAdministration]
            equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) gsub("[ \n]", "", x) )

            equationsDuringInfusionBody = list()

            for ( name in names( equationsDuringInfusion ) )
            {
              equationDuringInfusion = equationsDuringInfusion[[name]]
              equationsDuringInfusionBody = c( equationsDuringInfusionBody, sprintf( "%s = %s", name, equationDuringInfusion ) )
            }

            functionBody = paste( equationsDuringInfusionBody, collapse = "\n" )
            functionBody = sprintf("%s\nreturn(list(%s))", functionBody, paste( outcomesForEvaluationWithAdministration, collapse = ", " ) )
            functionDefinition = sprintf( "function(%s) { %s }", paste( argsOutcomesWithAdministration, collapse = ", "), functionBody )
            modelFunctionDuringInfusion = eval( parse( text = functionDefinition ) )
            argsSymbolOutcomesWithAdministration = lapply( argsOutcomesWithAdministration, as.symbol )

            modelFunctionDuringInfusionOutcomesWithAdministration = list( modelFunctionDuringInfusionOutcomesWithAdministration =
                                                                            modelFunctionDuringInfusion,
                                                                          argsOutcomesWithAdministration = argsOutcomesWithAdministration,
                                                                          argsSymbolOutcomesWithAdministration = argsSymbolOutcomesWithAdministration )

            # equations after infusion for outcomesWithAdministration

            equationsAfterInfusionBody = list()

            equationsAfterInfusion = equationsAfterInfusionModel[outcomesWithAdministration]

            for ( name in names( equationsAfterInfusion ) )
            {
              equationAfterInfusion = equationsAfterInfusion[[name]]
              equationsAfterInfusionBody = c( equationsAfterInfusionBody, sprintf( "%s = %s", name, equationAfterInfusion ) )
            }

            functionBody = paste( equationsAfterInfusionBody, collapse = "\n" )
            functionBody = sprintf("%s\nreturn(list(%s))", functionBody, paste( outcomesForEvaluationWithAdministration, collapse = ", " ) )
            functionDefinition = sprintf( "function(%s) { %s }", paste( argsOutcomesWithAdministration, collapse = ", "), functionBody )
            modelFunctionAfterInfusion = eval( parse( text = functionDefinition ) )
            argsSymbolOutcomesWithAdministration = lapply( argsOutcomesWithAdministration, as.symbol )

            modelFunctionAfterInfusionOutcomesWithAdministration = list( modelFunctionAfterInfusionOutcomesWithAdministration =
                                                                           modelFunctionAfterInfusion,
                                                                         argsOutcomesWithAdministration = argsOutcomesWithAdministration,
                                                                         argsSymbolOutcomesWithAdministration = argsSymbolOutcomesWithAdministration )

            # =========================================================
            # equations after infusion for outcomesWithNoAdministration
            # after eqs = during eqs
            # =========================================================

            equationsAfterInfusionBody = list()

            argsOutcomesWithNoAdministration = c( outcomesWithAdministration, parametersNames, "tau", "t" )

            equationsAfterInfusion = equationsAfterInfusionModel[outcomesWithNoAdministration]

            for ( name in names( equationsAfterInfusion ) )
            {
              equationAfterInfusion = equationsAfterInfusion[[name]]
              equationsAfterInfusionBody = c( equationsAfterInfusionBody, sprintf( "%s = %s", name, equationAfterInfusion ) )
            }

            functionBody = paste( equationsAfterInfusionBody, collapse = "\n" )
            functionBody = sprintf("%s\nreturn(list(%s))", functionBody, paste( outcomesForEvaluationWithNoAdministration, collapse = ", " ) )
            functionDefinition = sprintf( "function(%s) { %s }", paste( argsOutcomesWithNoAdministration, collapse = ", "), functionBody )
            modelFunctionAfterInfusion = eval( parse( text = functionDefinition ) )
            argsSymbolOutcomesWithNoAdministration = lapply( argsOutcomesWithNoAdministration, as.symbol )

            modelFunctionAfterInfusionOutcomesWithNoAdministration = list( modelFunctionAfterInfusionOutcomesWithNoAdministration =
                                                                             modelFunctionAfterInfusion,
                                                                           argsOutcomesWithNoAdministration = argsOutcomesWithNoAdministration,
                                                                           argsSymbolOutcomesWithNoAdministration =
                                                                             argsSymbolOutcomesWithNoAdministration )

            return( c( modelFunctionDuringInfusionOutcomesWithAdministration,
                       modelFunctionAfterInfusionOutcomesWithAdministration,
                       modelFunctionAfterInfusionOutcomesWithNoAdministration ) )
          })

# ======================================================================================================

#' @rdname setDataForModelEvaluation
#' @export

setMethod("setDataForModelEvaluation",
          signature("ModelAnalyticInfusionSteadyState"),
          function( object, arm )
          {
            dataForArmEvaluation = getDataForArmEvaluation( arm )

            inputsModel = list()
            data = list()
            indexTime = list()

            outcomes = dataForArmEvaluation$modelOutcomes
            outcomesWithAdministration = dataForArmEvaluation$outcomesWithAdministration
            samplingTimesModel = dataForArmEvaluation$samplingTimesModel
            samplingTimesOutcome = dataForArmEvaluation$samplingTimesOutcome

            # indicator in the dataframe for infusion
            duringAndAfter = rep("After",length( samplingTimesModel ) )
            duringAndAfter[1] = "No calcul"

            #  data for evaluation
            for ( outcome in outcomesWithAdministration )
            {
              administration = getAdministration( arm, outcome )

              tau = getTau( administration )

              inputsModel$dose[[outcome]] = getDose( administration )
              inputsModel$Tinf[[outcome]] = getTinf( administration )
              inputsModel$timeDose[[outcome]] = getTimeDose( administration )

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

              # indices for doses and Tinf
              indicesDoses = c()
              indicesDoses[1] = 1
              timeDoseInfusionEnd = c()
              timeDoseInfusion = samplingTimesModel

              vec = c( inputsModel$timeDose[[outcome]], max( sort( unique( c( samplingTimesModel,
                                                                              inputsModel$Tinf[[outcome]] ) ) ) ) )

              # indices for doses
              for (k in 1:length(vec))
              {
                indicesDoses[samplingTimesModel >= vec[k] & samplingTimesModel <=vec[k+1]] = k
              }

              # during and after
              iterk=1
              for ( t in samplingTimesModel )
              {
                assign("t", t)

                indexTime[[outcome]] = which( apply( inputsModel$timeMatrix[[outcome]], 1, findInterval, x = t ) == 1)

                if ( length( indexTime[[outcome]]  ) != 0  )
                {
                  duringAndAfter[iterk] = "duringInfusion"
                }else{
                  duringAndAfter[iterk] = "afterInfusion"
                }
                iterk=iterk+1
              }

              # number of does
              numberOfDoses =  length( inputsModel$dose[[outcome]] )

              # sampling times for dose infusion
              samplingTimeDose = matrix( 0.0, length( samplingTimesModel ), numberOfDoses )

              for ( i in 1:numberOfDoses )
              {
                samplingTimeDose[,i] = pmax( samplingTimesModel - inputsModel$timeDose[[outcome]][i], 0 )
              }

              samplingTimeDose = as.matrix( samplingTimeDose )


              data[[outcome]] = data.frame( samplingTimesModel = samplingTimesModel,
                                            duringAndAfter = duringAndAfter,
                                            indicesDoses = indicesDoses,
                                            outcome = outcome,
                                            samplingTimeDose = I(samplingTimeDose) )
            }

            data = do.call( "rbind", data )
            data = data[ order( data$samplingTimesModel ), ]
            rownames( data ) = 1:dim( data )[1]

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( inputsModel = inputsModel,
                                              tau = tau,
                                              data = data  ) )

            return( dataForModelEvaluation )
          })

# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f="EvaluateModel",
          signature =  "ModelAnalyticInfusionSteadyState",
          definition=function( object, dataForModelEvaluation, arm )
          {
            data = dataForModelEvaluation$data
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            equationFunction = dataForModelEvaluation$equationFunction
            inputsModel = dataForModelEvaluation$inputsModel
            samplingTimesOutcome = dataForModelEvaluation$samplingTimesOutcome

            outcomes = dataForModelEvaluation$modelOutcomes
            outcomesWithAdministration = dataForModelEvaluation$outcomesWithAdministration
            outcomesWithNoAdministration = dataForModelEvaluation$outcomesWithNoAdministration
            numberOfOutcomesWithAdministration = length( outcomesWithAdministration )
            numberOfOutcomesWithNoAdministration = length( outcomesWithNoAdministration )

            # equations for outcomesWithAdministration
            modelFunctionDuringInfusionOutcomesWithAdministration = equationFunction$modelFunctionDuringInfusionOutcomesWithAdministration
            modelFunctionAfterInfusionOutcomesWithAdministration = equationFunction$modelFunctionAfterInfusionOutcomesWithAdministration
            parameterNamesOutcomesWithAdministration = equationFunction$argsOutcomesWithAdministration
            parameterSymbolsOutcomesWithAdministration = equationFunction$argsSymbolOutcomesWithAdministration

            # equations for outcomesWithNoAdministration
            modelFunctionAfterInfusionOutcomesWithNoAdministration = equationFunction$modelFunctionAfterInfusionOutcomesWithNoAdministration
            parameterNamesOutcomesWithNoAdministration = equationFunction$argsOutcomesWithNoAdministration
            parameterSymbolsOutcomesWithNoAdministration = equationFunction$argsSymbolOutcomesWithNoAdministration

            # values for modelParameters
            modelParameters = getParameters( object )

            for( modelParameter in modelParameters )
            {
              modelParameterName = getName( modelParameter )
              modelParameterValue = getMu( modelParameter )
              assign( modelParameterName, modelParameterValue )
            }

            # tau value
            assign( "tau", dataForModelEvaluation$tau )

            # model evaluation
            modelEvaluation = as.data.frame( matrix( 0.0,length( samplingTimesModel ),length( outcomesWithAdministration ) ) )
            colnames ( modelEvaluation ) = outcomesWithAdministration

            for( iterTime in 1:dim( data )[1] )
            {
              duringAndAfter = data$duringAndAfter[iterTime]
              indicesDoses = data$indicesDoses[iterTime]
              samplings = data[iterTime,5:dim( data )[2]]
              outcome  = data$outcome[iterTime]

              # evaluation infusion during
              if( duringAndAfter == "duringInfusion")
              {
                # first dose
                if ( indicesDoses == 1)
                {
                  assign("t",samplings[indicesDoses] )
                  assign( paste0("dose_",outcome ), inputsModel$dose[[outcome]][indicesDoses] )
                  assign( paste0("Tinf_",outcome ), inputsModel$Tinf[[outcome]][indicesDoses] )

                  modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = unlist( do.call( modelFunctionDuringInfusionOutcomesWithAdministration,
                                                                                                    setNames( parameterSymbolsOutcomesWithAdministration,
                                                                                                              parameterNamesOutcomesWithAdministration ) ) )
                }

                # after the first dose
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

                  modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = unlist( do.call( modelFunctionDuringInfusionOutcomesWithAdministration,
                                                                                                    setNames( parameterSymbolsOutcomesWithAdministration,
                                                                                                              parameterNamesOutcomesWithAdministration ) ) )

                  for ( i in 1:(indicesDoses-1) )
                  {
                    assign("t",samplingAfter[i] )
                    assign( paste0("dose_",outcome ), dosesAfter[i] )
                    assign( paste0("Tinf_",outcome ), tinfAfter[i] )

                    output = unlist( do.call( modelFunctionAfterInfusionOutcomesWithAdministration,
                                              setNames( parameterSymbolsOutcomesWithAdministration, parameterNamesOutcomesWithAdministration ) ) )

                    modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] +
                      output[1:numberOfOutcomesWithAdministration]


                  }
                }
              }

              else if( duringAndAfter == "afterInfusion")
              {
                # first dose
                if ( indicesDoses == 1)
                {
                  assign("t",samplings[indicesDoses] )
                  assign( paste0("dose_",outcome ), inputsModel$dose[[outcome]][indicesDoses] )
                  assign( paste0("Tinf_",outcome ), inputsModel$Tinf[[outcome]][indicesDoses] )

                  modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = unlist( do.call( modelFunctionAfterInfusionOutcomesWithAdministration,
                                                                                                    setNames( parameterSymbolsOutcomesWithAdministration,
                                                                                                              parameterNamesOutcomesWithAdministration ) ) )
                }

                # after the first dose
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

                  modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = unlist( do.call( modelFunctionAfterInfusionOutcomesWithAdministration,
                                                                                                    setNames( parameterSymbolsOutcomesWithAdministration,
                                                                                                              parameterNamesOutcomesWithAdministration ) ) )

                  for ( i in 1:(indicesDoses-1) )
                  {
                    assign("t",samplingAfter[i] )
                    assign( paste0("dose_",outcome ), dosesAfter[i] )
                    assign( paste0("Tinf_",outcome ), tinfAfter[i] )

                    output = unlist( do.call( modelFunctionAfterInfusionOutcomesWithAdministration,
                                              setNames( parameterSymbolsOutcomesWithAdministration, parameterNamesOutcomesWithAdministration ) ) )

                    modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] = modelEvaluation[iterTime,1:numberOfOutcomesWithAdministration] +
                      output[1:numberOfOutcomesWithAdministration]

                  }
                }
              }
              # for responses without administration (ie response PD)
              if ( numberOfOutcomesWithNoAdministration > 0 )
              {
                assign( outcome, modelEvaluation[iterTime,outcome] )

                modelEvaluation[iterTime,(1+numberOfOutcomesWithAdministration):(2*numberOfOutcomesWithAdministration)] = unlist( do.call(
                  modelFunctionAfterInfusionOutcomesWithNoAdministration, setNames( parameterSymbolsOutcomesWithNoAdministration,
                                                                                    parameterNamesOutcomesWithNoAdministration ) ) )

                colnames( modelEvaluation ) = outcomes
              }
            } # end iter time


            evaluationOutcomes = list()

            for ( outcome in outcomes )
            {
              indexSamplingTimesOutcome = match( samplingTimesOutcome[[outcome]], samplingTimesModel )

              evaluationOutcomes[[ outcome ]] =  cbind( samplingTimesModel[indexSamplingTimesOutcome],
                                                        modelEvaluation[indexSamplingTimesOutcome, outcome ] )

              colnames( evaluationOutcomes[[ outcome ]] ) = c( "time", outcome )
            }
            return( evaluationOutcomes )
          })

# ======================================================================================================

#' @rdname EvaluateModelGradient
#' @export

setMethod(f = "EvaluateModelGradient",
          signature = "ModelAnalyticInfusionSteadyState",
          definition = function( object, dataForModelEvaluation, arm )
          {
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            modelError = dataForModelEvaluation$modelError

            inputsModel = dataForModelEvaluation$inputsModel
            atol = dataForModelEvaluation$odeSolverParameters$atol
            rtol = dataForModelEvaluation$odeSolverParameters$rtol

            shiftedParameters = dataForModelEvaluation$parametersGradient$shifted
            Xcols = dataForModelEvaluation$parametersGradient$Xcols
            Xcols = do.call( "cbind", Xcols )
            XcolsInv = as.matrix( solve( Xcols ) )
            frac = dataForModelEvaluation$parametersGradient$frac

            modelParameters = getParameters( object )
            parametersNames = map( modelParameters, ~ getName( .x ) ) %>% unlist()

            dataForArmEvaluation = getDataForArmEvaluation( arm )
            modelOutcomes = dataForArmEvaluation$modelOutcomes

            evaluationModel = map( 1:ncol( shiftedParameters ), function( iterShiftedParameters )
            {
              modelParameters = map2( modelParameters, 1:length( modelParameters ), ~ setMu(.x, shiftedParameters[.y, iterShiftedParameters] ) )
              object = setParameters( object, modelParameters )
              dataForModelEvaluation = setDataForModelEvaluation( object, arm )
              EvaluateModel( object, dataForModelEvaluation, arm )
            })

            outcomesGradient = pmap( list( modelOutcome = modelOutcomes,
                                           samplingTimesOutcomes = list( samplingTimesOutcomes ),
                                           parametersNames = list( parametersNames ) ),
                                     function( modelOutcome, parametersNames, samplingTimesOutcomes, samplingTimesModel )
                                     {
                                       evaluationGradient = evaluationModel %>%
                                         map(~ .x[[modelOutcome]][, modelOutcome]) %>%
                                         reduce( cbind )

                                       outcomesGradient = t( XcolsInv %*% t( evaluationGradient ) / frac )
                                       indexColumn = length( parametersNames )
                                       outcomesGradient =  as.data.frame( outcomesGradient[, 2:(1 + indexColumn)] )
                                       outcomesGradient = cbind( samplingTimesOutcomes[[modelOutcome]], outcomesGradient )
                                       colnames( outcomesGradient ) = c("time", parametersNames)

                                       return( outcomesGradient )
                                     }
            )

            outcomesGradient = set_names( outcomesGradient, modelOutcomes )

            outcomesAllGradient = list()

            for( modelOutcome in modelOutcomes )
            {
              index = which( sapply( modelError, function (x) getOutcome(x) == modelOutcome ) )

              if ( length( index ) != 0 )
              {
                outcomesAllGradient[[modelOutcome]] = outcomesGradient[[modelOutcome]][, 2:(1+length( modelParameters ) ) ]
              }
            }

            outcomesAllGradient = do.call( rbind, outcomesAllGradient )
            rownames( outcomesAllGradient ) = NULL

            return( list( outcomesGradient = outcomesGradient,
                          outcomesAllGradient = outcomesAllGradient ) )
          })

##########################################################################################################
# END Class "ModelAnalyticInfusionSteadyState"
##########################################################################################################
