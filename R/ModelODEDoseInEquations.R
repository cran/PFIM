

#' Class "ModelODEDoseInEquations"
#'
#' @description The class \code{ModelODEDoseInEquations} defines information concerning the construction of an ode model
#' where the dose is in the model equations. The class \code{ModelODEDoseInEquations} inherits from the class \code{ModelODE}.
#'
#' @name ModelODEDoseInEquations-class
#' @aliases ModelODEDoseInEquations
#' @docType class
#' @include ModelODE.R
#' @export

ModelODEDoseInEquations = setClass("ModelODEDoseInEquations",
                                   contains = "ModelODE")

# ======================================================================================================

#' @rdname defineModelEquationsFromStringToFunction
#' @export

setMethod("defineModelEquationsFromStringToFunction",
          signature("ModelODEDoseInEquations"),
          function( object, parametersNames, outcomesWithAdministration, outcomesWithNoAdministration )
          {
            # variables and outcomes of the model
            variables = getVariables( object )
            variablesNames = variables$variablesNames
            variablesNamesDerivatives = variables$variablesNamesDerivatives
            variablesNamesDerivatives = paste( variablesNamesDerivatives, collapse = ", ")

            # arguments for the function
            doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
            timeNames = paste( "t_", outcomesWithAdministration, sep = "" )
            functionArguments = c( doseNames, parametersNames, variablesNames, timeNames )
            functionArgumentsSymbol = map( functionArguments, ~ as.symbol(.x) )

            # create body function
            equationsBody = list()

            equations = getEquations( object )

            for ( name in names( equations ) )
            {
              equation = equations[[name]]
              equationsBody = c( equationsBody, sprintf( "%s = %s", name, equation ) )
            }

            for ( iterTimeName in seq_len( length( timeNames) ) )
            {
              equationsBody[[iterTimeName]] = str_replace( equationsBody[[iterTimeName]], "t", timeNames[iterTimeName] )
            }

            functionBody = paste( equationsBody, collapse = "\n" )
            functionBody = sprintf( paste( "%s\nreturn(list(c(", variablesNamesDerivatives, ")))", collapse = ", "), functionBody )
            functionDefinition = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", "), functionBody )
            modelFunction = eval( parse( text = functionDefinition ) )

            return( list( modelFunction = modelFunction,
                          functionArguments = functionArguments,
                          functionArgumentsSymbol = functionArgumentsSymbol ) )

          })

# ======================================================================================================

#' @rdname setDataForModelEvaluation
#' @export

setMethod("setDataForModelEvaluation",
          signature("ModelODEDoseInEquations"),
          function( object, arm )
          {
            dataForArmEvaluation = getDataForArmEvaluation( arm )

            inputsModel = list()
            initialConditions = list()
            timeMatrix = list()

            # outcomes and sampling time
            outcomesWithAdministration = dataForArmEvaluation$outcomesWithAdministration
            samplingTimesModel = dataForArmEvaluation$samplingTimesModel
            outcomesForEvaluation = dataForArmEvaluation$outcomesForEvaluation

            # function to evaluate
            equationFunction = dataForArmEvaluation$equationFunction
            modelFunction = equationFunction$modelFunction
            functionArguments = equationFunction$functionArguments
            functionArgumentsSymbols = equationFunction$functionArgumentsSymbol

            # administration parameters
            for ( outcome in outcomesWithAdministration )
            {
              # time dose, dose and tau
              administration = getAdministration( arm, outcome )
              dose = getDose( administration )
              tau = getTau( administration )
              timeDose = getTimeDose( administration )
              maxSamplingTimeOutcome = max( getSamplings( getSamplingTime( arm, outcome ) ) )
              timeDose = c( timeDose, maxSamplingTimeOutcome )

              # repeated & multi doses
              if ( tau !=0 )
              {
                timeDose = unique( seq( 0, maxSamplingTimeOutcome, tau ) )
                inputsModel$dose[[outcome]] = rep( dose, length( timeDose ) )
              }
              else
              {
                inputsModel$dose[[outcome]] = dose
              }

              # one dose
              if ( length( timeDose ) == 1 )
              {
                timeMatrix[[outcome]] =  matrix(c( timeDose, timeDose ),1,2)
              }else
              {
                # multi doses
                timeMatrix[[outcome]] = cbind( timeDose[-length(timeDose)], timeDose[-1] )
              }
            }

            # assign parameter values and initial conditions
            modelParameters = getParameters( object )

            for( modelParameter in modelParameters )
            {
              modelParameterName = getName( modelParameter )
              modelParameterValue = getMu( modelParameter )
              assign( modelParameterName, modelParameterValue )
            }

            # evaluation of the initial conditions
            initialConditions = getInitialConditions( arm )
            initialConditions = map( initialConditions, ~ eval( parse( text = .x ) ) )
            initialConditions = unlist( initialConditions )

            # function evaluation model ODE
            modelODE = function( samplingTimes, initialConditions, inputsModel )
            {
              with( c( samplingTimes, initialConditions, inputsModel ),{

                for ( outcome in outcomesWithAdministration )
                {
                  # dose
                  dose[[outcome]] = inputsModel$dose[[outcome]]

                  # time dose
                  indexTime = which( apply( timeMatrix[[outcome]], 1, findInterval, x = samplingTimes ) == 1 )
                  intervalTimeDose = timeMatrix[[outcome]][indexTime,]
                  timeDose = samplingTimes - intervalTimeDose[1]

                  # assign doses
                  if ( length( indexTime ) == 0 )
                  {
                    # unique dose
                    assign( paste0( "dose_", outcome ), dose[[outcome]][1] )
                  }else{
                    # multi dose
                    assign( paste0( "dose_", outcome ), dose[[outcome]][indexTime] )
                  }

                  # assign time dose
                  if ( timeDose >= 0 & length( intervalTimeDose ) !=0 )
                  {
                    assign( paste0( "t_", outcome ), timeDose )
                  }else{
                    assign( paste0( "t_", outcome ), samplingTimes )
                  }
                }

                # evaluation with the outcomes
                output = do.call( modelFunction, setNames( functionArgumentsSymbols, functionArguments ) )
                outcomesForEvaluation = map( outcomesForEvaluation, ~ eval( .x ) )

                return( c( output, outcomesForEvaluation ) )
              })
            }

            odeSolverParameters = getOdeSolverParameters( object )

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( initialConditions = initialConditions,
                                              samplingTimesModel = samplingTimesModel,
                                              modelODE = modelODE,
                                              inputsModel = inputsModel,
                                              odeSolverParameters = odeSolverParameters ) )

            return( dataForModelEvaluation )

          })

# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f = "EvaluateModel",
          signature = "ModelODEDoseInEquations",
          definition = function( object, dataForModelEvaluation, arm )
          {
            initialConditions = dataForModelEvaluation$initialConditions
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes
            modelODE = dataForModelEvaluation$modelODE
            inputsModel = dataForModelEvaluation$inputsModel
            atol = dataForModelEvaluation$odeSolverParameters$atol
            rtol = dataForModelEvaluation$odeSolverParameters$rtol

            modelOutcomes = dataForModelEvaluation$modelOutcomes

            modelEvaluation = ode( initialConditions,
                                   samplingTimesModel,
                                   modelODE,
                                   inputsModel,
                                   hmax = 0.0,
                                   atol = atol, rtol = rtol )

            evaluationOutcomes = list()

            for ( outcome in names( samplingTimesOutcomes ) )
            {
              indexSamplingTimesOutcome = which( modelEvaluation[,"time"] %in% samplingTimesOutcomes[[outcome]] )
              evaluationOutcomes[[ outcome ]] = modelEvaluation[indexSamplingTimesOutcome, c( "time", outcome ) ]
            }

            return( evaluationOutcomes )
          })

# ======================================================================================================

#' @rdname EvaluateModelGradient
#' @export

setMethod(f = "EvaluateModelGradient",
          signature = "ModelODEDoseInEquations",
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

# ======================================================================================================
# definePKModel
# ======================================================================================================

#' @rdname definePKModel
#' @export

setMethod("definePKModel",
          signature("ModelODEDoseInEquations"),
          function( object, outcomes )
          {
            # =======================================================
            # change names: responses, variables, doses
            # =======================================================

            # =======================================================
            # original and new outcomes
            # =======================================================

            newOutcomes = outcomes
            originalOutcomes = getOutcomes( object )

            if ( length( outcomes ) != 0 )
            {
              # =======================================================
              # variable names
              # =======================================================

              variablesNames = unlist( originalOutcomes )
              variablesNewNames = unlist( newOutcomes )

              # =======================================================
              # change equation names
              # =======================================================

              equations = getEquations( object )
              names( equations ) = paste0( "Deriv_", variablesNewNames )

              # =======================================================
              # response names old and new
              # =======================================================

              responsesNames = names( originalOutcomes )
              responsesNewNames = names( newOutcomes )

              # =======================================================
              # dose names
              # =======================================================

              doseNewName = paste0( "dose_", responsesNewNames )

              for ( iterEquation in 1:length( equations ) )
              {
                # =======================================================
                # change response names
                # =======================================================

                for( iterResponseName in 1:length( responsesNames ) )
                {
                  equations[[iterEquation]] = gsub( responsesNames[iterResponseName],
                                                    responsesNewNames[iterResponseName], equations[[iterEquation]] )
                }

                # =======================================================
                # change variable names
                # =======================================================

                for( iterVariableName in 1:length( variablesNewNames ) )
                {
                  equations[[iterEquation]] = gsub( variablesNames[iterVariableName],
                                                    variablesNewNames[iterVariableName], equations[[iterEquation]] )
                }

                # =======================================================
                # change dose names
                # =======================================================

                equations[[iterEquation]] = gsub( "dose", doseNewName[iterEquation], equations[[iterEquation]] )
              }

              object = setEquations( object, equations )
              object = setOutcomes( object, newOutcomes )

            }else{
              # =======================================================
              # change only dose name
              # =======================================================

              equations = getEquations( object )
              responseNames = names( originalOutcomes )
              doseName = paste0( "dose_", responseNames )

              for ( iterEquation in 1:length( equations ) )
              {
                equations[[iterEquation]] = gsub( "dose", doseName[iterEquation], equations[[iterEquation]] )
              }

              # =======================================================
              # set equation and outcome
              # =======================================================

              object = setOutcomes( object, originalOutcomes )
              object = setEquations( object, equations )
            }

            return( object )
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature("ModelODEDoseInEquations","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelODEDoseInEquations()

            if ( length( outcomes ) != 0 )
            {
              # =======================================================
              # original and new outcomes
              # =======================================================

              newOutcomes = outcomes
              originalOutcomesPKModel = getOutcomes( PKModel )
              originalOutcomesPDModel = getOutcomes( PDModel )

              originalOutcomesPKModel = unlist( originalOutcomesPKModel )
              originalOutcomesPDModel = unlist( originalOutcomesPDModel )

              originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

              # =======================================================
              # variable names
              # =======================================================

              variablesNames = unlist( originalOutcomes )
              variablesNewNames = unlist( newOutcomes )

              # =======================================================
              # model equation
              # =======================================================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )
              names( equations ) = paste0( "Deriv_", variablesNewNames )
              numberOfEquations = length( equations )

              # =======================================================
              # response names old and new
              # =======================================================

              responsesNames = names( originalOutcomes )
              responsesNewNames = names( newOutcomes )

              # =======================================================
              # dose names
              # =======================================================

              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # =======================================================
              # variables substitution
              # =======================================================

              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )

              # =======================================================
              # RespPK change for PD Model with PK ode Michaelis-Menten
              # =======================================================

              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:numberOfEquations )
              {
                equations[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                   list( equations[[iterEquation]][[1]], variablesNewNames ) ) )
                equations[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                   list( equations[[iterEquation]][[1]], doseNewNames ) ) )
              }

              # =======================================================
              # convert equations from expression to string
              # =======================================================

              equations = lapply( equations, function(x) x[[1]] )
              equations = lapply( equations, function(x) paste( deparse( x ), collapse = " " ) )
              equations = lapply( equations, function(x) str_replace_all( x, " ", "" ) )

              # =======================================================
              # set outcomes and equations
              # =======================================================

              model = setEquations( model, equations )
              model = setOutcomes( model, newOutcomes )

            }else{

              # =======================================================
              # outcomes
              # =======================================================

              newOutcomes = outcomes
              originalOutcomesPKModel = getOutcomes( PKModel )
              originalOutcomesPDModel = getOutcomes( PDModel )

              originalOutcomesPKModel = unlist( originalOutcomesPKModel )
              originalOutcomesPDModel = unlist( originalOutcomesPDModel )

              originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

              # =======================================================
              # response names old and new
              # =======================================================

              responsesNames = names( originalOutcomes )

              # =======================================================
              # variable names
              # =======================================================

              variablesNames = unlist( originalOutcomes )
              variablesNames = lapply( variablesNames, function(x) parse( text = x ) )

              # =======================================================
              # model equations
              # =======================================================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )
              numberOfEquations = length( equations )

              # =======================================================
              # dose names
              # =======================================================

              doseNewNames = as.list( paste0( "dose_", responsesNames ) )
              names( doseNewNames ) = rep( "dose",length( responsesNames ) )
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # =======================================================
              # RespPK change for PD Model with PK ode Michaelis-Menten
              # =======================================================

              variableSubstitutedMichaelisMenten = list()
              variableSubstitutedMichaelisMenten[[1]] = variablesNames[[1]][[1]]
              names( variableSubstitutedMichaelisMenten ) = "RespPK"

              for ( iterEquation in 1:numberOfEquations )
              {
                equations[[iterEquation]] = as.expression(do.call( 'substitute', list( equations[[iterEquation]][[1]],
                                                                                       variableSubstitutedMichaelisMenten ) ) )

                equations[[iterEquation]] = as.expression(do.call( 'substitute', list( equations[[iterEquation]][[1]],
                                                                                       doseNewNames ) ) )
              }

              # =======================================================
              # convert equations from expression to string
              # =======================================================

              equations = lapply( equations, function(x) x[[1]] )
              equations = lapply( equations, function(x) paste( deparse( x ), collapse = " " ) )
              equations = lapply( equations, function(x) str_replace_all( x, " ", "" ) )

              model = setEquations( model, equations )
              model = setOutcomes( model, originalOutcomes )
            }

            return( model )
          })

##########################################################################################################
# END Class ModelODEDoseInEquations
##########################################################################################################















