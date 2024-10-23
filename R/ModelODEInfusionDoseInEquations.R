
#' Class "ModelODEInfusionDoseInEquations"
#'
#' @description The class \code{ModelODEInfusionDoseInEquations} defines information concerning the construction of an ode model
#' in infusion where the dose is in the model equations. The class \code{ModelODEInfusionDoseInEquations} inherits from the class \code{ModelODEInfusion}.
#'
#' @name ModelODEInfusionDoseInEquations-class
#' @aliases ModelODEInfusionDoseInEquations
#' @include ModelODEInfusion.R
#' @export

ModelODEInfusionDoseInEquations = setClass("ModelODEInfusionDoseInEquations",
                                           contains = "ModelODEInfusion")

# ======================================================================================================

#' @rdname defineModelEquationsFromStringToFunction
#' @export

setMethod("defineModelEquationsFromStringToFunction",
          signature("ModelODEInfusionDoseInEquations"),
          function( object, parametersNames, outcomesWithAdministration, outcomesWithNoAdministration )
          {
            return(list(NULL))
          })

# ======================================================================================================

#' @rdname setDataForModelEvaluation
#' @export

setMethod("setDataForModelEvaluation",
          signature("ModelODEInfusionDoseInEquations"),
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
            outcomesModel = dataForArmEvaluation$modelOutcomes

            # administration parameters
            for ( outcome in outcomesWithAdministration )
            {
              # time dose, dose and tau
              administration = getAdministration( arm, outcome )
              tau = getTau( administration )

              inputsModel$dose[[outcome]] = getDose( administration )
              inputsModel$Tinf[[outcome]] = getTinf( administration )

              # repeated & multi doses
              if ( tau !=0 )
              {
                maxSamplingTimeOutcome = max( getSamplings( getSamplingTime( arm, outcome ) ) )
                inputsModel$timeDose[[outcome]] = seq( 0, maxSamplingTimeOutcome, tau )
                inputsModel$Tinf[[outcome]] = rep( inputsModel$Tinf[[outcome]], length( inputsModel$timeDose[[outcome]] ) )
                inputsModel$dose[[outcome]] = rep( inputsModel$dose[[outcome]], length( inputsModel$timeDose[[outcome]] ) )

              }else{

                inputsModel$timeDose[[outcome]] = getTimeDose( administration )
              }
              inputsModel$timeMatrix[[outcome]] = matrix( ( c( inputsModel$timeDose[[outcome]],
                                                               inputsModel$timeDose[[outcome]] + inputsModel$Tinf[[outcome]] ) ),
                                                          length( inputsModel$timeDose[[outcome]] ), 2 )
            }

            # assign parameter values and initial conditions
            modelParameters = getParameters( object )

            for( modelParameter in modelParameters )
            {
              modelParameterName = getName( modelParameter )
              modelParameterValue = getMu( modelParameter )
              assign( modelParameterName, modelParameterValue )
            }

            # names of model parameters
            parametersNames = map( modelParameters, ~getName(.x) ) %>% unlist()

            # evaluation of the initial conditions
            initialConditions = getInitialConditions( arm )
            initialConditions = map( initialConditions, ~ eval( parse( text = .x ) ) )
            initialConditions = unlist( initialConditions )

            # function evaluation model ODE infusion
            modelODEInfusion = function( samplingTimesModel, initialConditions, inputsModel ){

              with( c( samplingTimesModel, initialConditions, inputsModel ),{

                # get model equations during after
                equationsDuringInfusion = getEquationsDuringInfusion( object )
                equationsAfterInfusion = getEquationsAfterInfusion( object )

                # create model with infusion during / after
                modelEquations = c()
                modelEquationsOutcomesWithAdministration = list()
                modelEquationsOutcomesWithNoAdministration = list()

                # get variables & create names derivatives
                variables = getVariables( object )
                variablesNames = variables$variablesNames
                variablesNamesDerivatives = variables$variablesNamesDerivatives

                # indices outcome with / without administration
                indiceOutcomes = seq( 1,length( outcomesModel ) )
                indiceOutcomesWithAdministration = 1:length( outcomesWithAdministration )
                indiceOutcomesWithNoAdministration = indiceOutcomes[-c(indiceOutcomesWithAdministration)]

                # administration parameters and function arguments
                doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
                TinfNames = paste( "Tinf_", outcomesWithAdministration, sep = "" )
                functionArguments = c( doseNames, TinfNames, parametersNames, variablesNames )
                functionArgumentsSymbols = lapply( functionArguments, as.symbol )

                # equations with outcomesWithAdministration
                iterOutcome = 1

                for ( outcome in outcomesWithAdministration )
                {
                  timeMatrix = inputsModel$timeMatrix[[outcome]]
                  indexTime = which( apply( timeMatrix, 1, findInterval, x = samplingTimesModel ) == 1 )

                  dose = inputsModel$dose[[outcome]]
                  Tinf = inputsModel$Tinf[[outcome]]

                  if ( length( indexTime ) != 0 )
                  {
                    assign( paste0("dose_",outcome), dose[indexTime] )
                    assign( paste0("Tinf_",outcome), Tinf[indexTime] )

                    modelEquationsOutcomesWithAdministration[iterOutcome] = equationsDuringInfusion[iterOutcome]
                  }
                  else
                  {
                    modelEquationsOutcomesWithAdministration[iterOutcome] = equationsAfterInfusion[iterOutcome]
                  }
                  iterOutcome = iterOutcome+1
                }

                names( modelEquationsOutcomesWithAdministration ) = variablesNamesDerivatives[indiceOutcomesWithAdministration ]

                # equations with outcomesWithNoAdministration
                modelEquationsOutcomesWithNoAdministration = equationsDuringInfusion[ indiceOutcomesWithNoAdministration ]
                names( modelEquationsOutcomesWithNoAdministration ) = variablesNamesDerivatives[indiceOutcomesWithNoAdministration ]

                modelEquations = c( modelEquationsOutcomesWithAdministration, modelEquationsOutcomesWithNoAdministration )

                # create the pattern for evaluating the equations
                equationsBody = list()

                for ( name in names( modelEquations ) )
                {
                  equationsBody = c( equationsBody, sprintf( "%s = %s", name, modelEquations[[name]] ) )
                }

                # define the function with equations and outcomes
                variablesNamesDerivativesTmp = paste( variablesNamesDerivatives, collapse = ", ")
                functionBody = paste( equationsBody, collapse = "\n" )
                functionBody = sprintf( paste( "%s\nreturn(list(c(", variablesNamesDerivativesTmp, ")))", collapse = ", "), functionBody )
                functionDefinition = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", "), functionBody )
                modelFunction = eval( parse( text = functionDefinition ) )

                # model evaluation
                output = unlist( do.call( modelFunction, setNames( functionArgumentsSymbols, functionArguments ) ) )

                # model response evaluation
                outcomesForEvaluation = map( outcomesForEvaluation, ~ eval( .x ) )

                return( list( c( output ), outcomesForEvaluation ) )
              })
            }

            odeSolverParameters = getOdeSolverParameters( object )

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( initialConditions = initialConditions,
                                              samplingTimesModel = samplingTimesModel,
                                              modelODEInfusion = modelODEInfusion,
                                              inputsModel = inputsModel,
                                              odeSolverParameters = odeSolverParameters ) )

            return( dataForModelEvaluation )
          })

# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f="EvaluateModel",
          signature = "ModelODEInfusionDoseInEquations",
          definition = function( object, dataForModelEvaluation, arm )
          {
            initialConditions = unlist( dataForModelEvaluation$initialConditions )
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes
            modelODEInfusion = dataForModelEvaluation$modelODEInfusion
            inputsModel = dataForModelEvaluation$inputsModel
            atol = dataForModelEvaluation$odeSolverParameters$atol
            rtol = dataForModelEvaluation$odeSolverParameters$rtol

            modelOutcomes = dataForModelEvaluation$modelOutcomes

            modelEvaluation = ode( initialConditions,
                                   samplingTimesModel,
                                   modelODEInfusion,
                                   inputsModel,
                                   hmax = 0.0,
                                   atol = atol, rtol = rtol )

            evaluationOutcomes = list()

            for ( outcome in modelOutcomes )
            {
              indexSamplingTimesOutcome = match( samplingTimesOutcomes[[outcome]], samplingTimesModel )
              evaluationOutcomes[[ outcome ]] = modelEvaluation[indexSamplingTimesOutcome, c( "time", outcome ) ]
            }

            return( evaluationOutcomes )
          })

# ======================================================================================================

#' @rdname EvaluateModelGradient
#' @export

setMethod(f = "EvaluateModelGradient",
          signature = "ModelODEInfusionDoseInEquations",
          definition = function( object, dataForModelEvaluation, arm )
          {
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            modelError = dataForModelEvaluation$modelError

            modelODE = dataForModelEvaluation$modelODE
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
              modelParameters = map2( modelParameters,
                                      1:length( modelParameters ), ~ setMu(.x, shiftedParameters[.y, iterShiftedParameters] ) )

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
          signature( "ModelODEInfusionDoseInEquations" ),
          function( object, outcomes )
          {
            model = ModelODEInfusionDoseInEquations()

            # original and new outcomes
            newOutcomes = outcomes

            originalOutcomesPKModel = getOutcomes( object )
            originalOutcomesPKModel = unlist( originalOutcomesPKModel )
            originalOutcomes = as.list( c( originalOutcomesPKModel ) )

            if ( length( outcomes ) != 0 )
            {
              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( object )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( object )

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = names( newOutcomes )
              variablesNewNames = unlist( newOutcomes, use.names = FALSE )

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion )

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }
              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, newOutcomes )

            }else{

              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( object )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( object )

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = responseNames
              variablesNewNames = variablesNames

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              # convert equations from expression to string
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) x[[1]] )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) str_replace_all( x, " ", "" ) )

              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) x[[1]] )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) str_replace_all( x, " ", "" ) )

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, originalOutcomes )
            }

            return(model)
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature( "ModelODEInfusion", "ModelODE" ),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelODEInfusionDoseInEquations()

            # original and new outcomes
            newOutcomes = outcomes

            originalOutcomesPKModel = getOutcomes( PKModel )
            originalOutcomesPDModel = getOutcomes( PDModel )

            originalOutcomesPKModel = unlist( originalOutcomesPKModel )
            originalOutcomesPDModel = unlist( originalOutcomesPDModel )

            originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

            if ( length( outcomes ) != 0 )
            {
              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( PKModel )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( PKModel )
              PDModelEquations = getEquations( PDModel)

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = names( newOutcomes )
              variablesNewNames = unlist( newOutcomes, use.names = FALSE )

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )
              # RespPK change for PD Model with PK ode Michaelis-Menten
              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }
              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, newOutcomes )

            }else{

              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( PKModel )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( PKModel )
              PDModelEquations = getEquations( PDModel)

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = responseNames
              variablesNewNames = variablesNames

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )
              # RespPK change for PD Model with PK ode Michaelis-Menten
              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              # convert equations from expression to string
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) x[[1]] )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) str_replace_all( x, " ", "" ) )

              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) x[[1]] )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) str_replace_all( x, " ", "" ) )

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, originalOutcomes )
            }

            return(model)
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature("ModelODEInfusionDoseInEquations","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelODEInfusionDoseInEquations()

            # original and new outcomes
            newOutcomes = outcomes

            originalOutcomesPKModel = getOutcomes( PKModel )
            originalOutcomesPDModel = getOutcomes( PDModel )

            originalOutcomesPKModel = unlist( originalOutcomesPKModel )
            originalOutcomesPDModel = unlist( originalOutcomesPDModel )

            originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

            if ( length( outcomes ) != 0 )
            {
              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( PKModel )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( PKModel )
              PDModelEquations = getEquations( PDModel)

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = names( newOutcomes )
              variablesNewNames = unlist( newOutcomes, use.names = FALSE )

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )
              # RespPK change for PD Model with PK ode Michaelis-Menten
              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }
              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, newOutcomes )

            }else{

              # equations during and after infusion
              PKModelEquationsDuringInfusion = getEquationsDuringInfusion( PKModel )
              PKModelEquationsAfterInfusion = getEquationsAfterInfusion( PKModel )
              PDModelEquations = getEquations( PDModel)

              equationsDuringInfusion = c( PKModelEquationsDuringInfusion, PDModelEquations )
              equationsAfterInfusion = c( PKModelEquationsAfterInfusion, PDModelEquations )

              # responses and variables
              responseNames = names( originalOutcomes )
              variablesNames = unlist( originalOutcomes, use.names = FALSE )

              responsesNewNames = responseNames
              variablesNewNames = variablesNames

              # model equation
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) parse( text = x ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) parse( text = x ) )

              names( equationsDuringInfusion ) = paste0( "Deriv_", variablesNewNames )
              names( equationsAfterInfusion ) = paste0( "Deriv_", variablesNewNames )

              # dose names
              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # Tinf names
              tinfNewNames = as.list(paste0( "Tinf_", responsesNewNames ))
              names( tinfNewNames ) = rep( "Tinf",length(responsesNewNames))
              tinfNewNames = lapply( tinfNewNames, function(x) parse( text = x ) )
              tinfNewNames = lapply( tinfNewNames, function(x) x[[1]] )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )
              # RespPK change for PD Model with PK ode Michaelis-Menten
              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:length( equationsDuringInfusion ) )
              {
                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       variablesNewNames ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       doseNewNames[iterEquation] ) ) )

                equationsDuringInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                 list( equationsDuringInfusion[[iterEquation]][[1]],
                                                                                       tinfNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      variablesNewNames ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      doseNewNames[iterEquation] ) ) )

                equationsAfterInfusion[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                                list( equationsAfterInfusion[[iterEquation]][[1]],
                                                                                      tinfNewNames[iterEquation] ) ) )
              }

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              # convert equations from expression to string
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) x[[1]] )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsDuringInfusion = lapply( equationsDuringInfusion, function(x) str_replace_all( x, " ", "" ) )

              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) x[[1]] )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) paste( deparse( x ), collapse = " " ) )
              equationsAfterInfusion = lapply( equationsAfterInfusion, function(x) str_replace_all( x, " ", "" ) )

              # set model equations ans outcomes
              model = setEquationsDuringInfusion( model, equationsDuringInfusion )
              model = setEquationsAfterInfusion( model, equationsAfterInfusion )

              model = setOutcomes( model, originalOutcomes )
            }

            return(model)
          })

##########################################################################################################
# END Class ModelODEInfusionDoseInEquations
##########################################################################################################

