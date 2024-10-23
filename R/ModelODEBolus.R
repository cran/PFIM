#' Class "ModelODEBolus"
#'
#' @description The class \code{ModelODEBolus} defines information concerning the construction of an ode model bolus.
#' The class \code{ModelODEBolus} inherits from the class \code{ModelBolus}.
#'
#' @name ModelODEBolus-class
#' @aliases ModelODEBolus
#' @docType class
#' @include ModelBolus.R
#' @export

#' @rdname getVariables
#' @export


ModelODEBolus = setClass(
  Class = "ModelODEBolus",
  contains = "ModelBolus")

#' @rdname getVariables
#' @export
#'
setMethod("getVariables",
          signature("ModelODEBolus"),
          function( object )
          {
            equations = getEquations( object )

            variablesNamesDerivatives = names( equations )

            variablesNames = gsub( "Deriv_", "", variablesNamesDerivatives )

            return( list( variablesNames = variablesNames, variablesNamesDerivatives = variablesNamesDerivatives ) )

          })

#' @rdname defineModelEquationsFromStringToFunction
#' @export

setMethod("defineModelEquationsFromStringToFunction",
          signature("ModelODEBolus"),
          function( object, parametersNames, outcomesWithAdministration, outcomesWithNoAdministration )
          {
            # =========================================================
            # variables and outcomes of the model
            # =========================================================

            variables = getVariables( object )
            variablesNames = variables$variablesNames
            variablesNamesDerivatives = variables$variablesNamesDerivatives
            variablesNamesDerivatives = paste( variablesNamesDerivatives, collapse = ", ")

            # =========================================================
            # arguments for the function
            # =========================================================

            doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
            functionArguments = c( doseNames, parametersNames, variablesNames, "t" )
            functionArgumentsSymbol = map( functionArguments, ~as.symbol(.x) )

            # =============================
            # create body function
            # =============================

            equationsBody = list()

            equations = getEquations( object )

            for ( name in names( equations ) )
            {
              equation = equations[[name]]
              equationsBody = c( equationsBody, sprintf( "%s = %s", name, equation ) )
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
          signature("ModelODEBolus"),
          function( object, arm )
          {
            # get data for arm evaluation
            dataForArmEvaluation = getDataForArmEvaluation( arm )

            inputsModel = list()
            initialConditions = list()
            timeDose = list()

            # outcomes and sampling time
            outcomesWithAdministration = dataForArmEvaluation$outcomesWithAdministration
            samplingTimesModel = dataForArmEvaluation$samplingTimesModel
            outcomesForEvaluation = dataForArmEvaluation$outcomesForEvaluation
            names( dataForArmEvaluation$samplingTimesOutcomes ) = dataForArmEvaluation$modelOutcomes

            # function to evaluate
            equationFunction = dataForArmEvaluation$equationFunction
            modelFunction = equationFunction$modelFunction
            functionArguments = equationFunction$functionArguments
            functionArgumentsSymbols = equationFunction$functionArgumentsSymbol

            # administration and sampling times
            administrations = getAdministrations( arm )
            samplingTimes = getSamplingTimes(  arm  )
            variables = getVariables( object )
            variablesNames = variables$variablesNames

            # variables with administration
            variablesWithAdministration = unlist( lapply( administrations, function(x) getOutcome(x) ) )

            # variables without administration
            variablesWithNoAdministration = setdiff( variablesNames, variablesWithAdministration )

            # administration parameters
            for ( variable in variablesWithAdministration )
            {
              # time dose, dose and tau
              administration = getAdministration( arm, variable )
              timeDose[[variable]] = getTimeDose( administration )
              dose = getDose( administration )
              tau = getTau( administration )

              # for repeated doses
              if ( tau !=0 )
              {
                maxSamplingTimeOutcome = max( getSamplings( getSamplingTime( arm, variable ) ) )
                n = maxSamplingTimeOutcome%/%tau
                inputsModel$dose[[variable]] = rep( dose, n+1 )
                timeDose[[variable]] = sort( unique( seq( 0, maxSamplingTimeOutcome, tau ) ) )
              }
              else
              {
                # for multi doses
                inputsModel$dose[[variable]] = dose
                timeDose[[variable]] = sort( unique( c( timeDose[[variable]] ) ) )
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

            variablesNames = variables$variablesNames

            for ( variableName in variablesNames )
            {
              assign( "dose", inputsModel$dose[[variableName]][1] )

              initialConditions[[variableName]] = eval( parse( text = initialConditions[[variableName]] ) )
            }

            initialConditions = unlist( initialConditions )

            # Dose event for ode for evaluation
            doseEvent = list()

            initialConditionsVariable = getInitialConditions( arm )

            for ( variable in variablesWithAdministration )
            {
              assign( "dose",  inputsModel$dose[[variable]] )

              value = eval( parse( text = initialConditionsVariable[[variable]] ) )

              doseEvent[[variable]] = data.frame( var = variable,
                                                  time = timeDose[[variable]],
                                                  value = value ,
                                                  method = c("replace") )
            }

            # for doses at t>0
            doseEvent = do.call( rbind, doseEvent )
            doseEvent = doseEvent[ order( doseEvent$time ), ]
            doseEvent$method[ doseEvent$time > 0 ] = "add"

            # ensure that event time in times to avoid warning
            uniqueTimes = cleanEventTimes( samplingTimesModel, doseEvent$time )
            samplingTimesModel  = sort( unique( c( samplingTimesModel, uniqueTimes ) ) )

            # function evaluation model ODE
            modelEvaluation = list()

            modelODEBolus = function( samplingTimesModel, initialConditions, inputsModel )
            {
              with( c( samplingTimesModel, initialConditions, inputsModel ),{

                output = do.call( modelFunction, setNames( functionArgumentsSymbols, functionArguments  ) )

                outcomesForEvaluation = map( outcomesForEvaluation, ~ eval( .x ) )

                return( c( output, outcomesForEvaluation ) )
              })
            }

            odeSolverParameters = getOdeSolverParameters( object )

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( initialConditions = initialConditions,
                                              samplingTimesModel = samplingTimesModel,
                                              modelODEBolus = modelODEBolus,
                                              inputsModel = inputsModel,
                                              doseEvent = doseEvent,
                                              odeSolverParameters = odeSolverParameters ) )

            return( dataForModelEvaluation )

          })

#' @rdname EvaluateModel
#' @export

setMethod(f = "EvaluateModel",
          signature = "ModelODEBolus",
          definition = function( object, dataForModelEvaluation, arm  )
          {
            initialConditions = dataForModelEvaluation$initialConditions

            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes

            modelODEBolus = dataForModelEvaluation$modelODEBolus

            inputsModel = dataForModelEvaluation$inputsModel

            atol = dataForModelEvaluation$odeSolverParameters$atol
            rtol = dataForModelEvaluation$odeSolverParameters$rtol

            doseEvent = dataForModelEvaluation$doseEvent

            modelEvaluation = ode( initialConditions,
                                   samplingTimesModel,
                                   modelODEBolus,
                                   inputsModel,
                                   events = list( data = doseEvent ),
                                   hmax = 0.0,
                                   method = "lsoda",
                                   atol = atol, rtol = rtol )

            # bolus: remove dose event > 0
            timeDoseEventToRemove = doseEvent$time[doseEvent$time>0]

            if ( length( timeDoseEventToRemove ) !=0 )
            {
              modelEvaluation = as.data.frame( modelEvaluation )
              modelEvaluation = modelEvaluation[ modelEvaluation$time != timeDoseEventToRemove, ]
            }

            # evaluation for each outcomes
            evaluationOutcomes = list()

            for ( outcome in names( samplingTimesOutcomes ) )
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
          signature = "ModelODEBolus",
          definition = function( object, dataForModelEvaluation, arm )
          {
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes
            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            modelError = dataForModelEvaluation$modelError

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

                                       outcomesGradient = as.data.frame( outcomesGradient[, 2:(1 + indexColumn)] )

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
                outcomesAllGradient[[modelOutcome]] = as.data.frame( outcomesGradient[[modelOutcome]][, parametersNames ] )
                names(  outcomesAllGradient[[modelOutcome]] ) = parametersNames
              }
            }

            outcomesAllGradient = do.call( rbind, outcomesAllGradient )
            rownames( outcomesAllGradient ) = NULL

            return( list( outcomesGradient = outcomesGradient,
                          outcomesAllGradient = outcomesAllGradient ) )
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature("ModelODEBolus","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelODEBolus()
            if ( length( outcomes ) != 0 )
            {
              # ====================================================
              # original and new outcomes
              # ====================================================

              newOutcomes = outcomes
              originalOutcomesPKModel = getOutcomes( PKModel )
              originalOutcomesPDModel = getOutcomes( PDModel )
              originalOutcomesPKModel = unlist( originalOutcomesPKModel )
              originalOutcomesPDModel = unlist( originalOutcomesPDModel )
              originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

              # ====================================================
              # variable names
              # ====================================================

              variablesNames = unlist( originalOutcomes )
              variablesNewNames = unlist( newOutcomes )

              # ====================================================
              # model equation
              # ====================================================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )
              names( equations ) = paste0( "Deriv_", variablesNewNames )
              numberOfEquations = length( equations )

              # ====================================================
              # response names old and new
              # ====================================================

              responsesNames = names( originalOutcomes )
              responsesNewNames = names( newOutcomes )

              # ====================================================
              # dose names
              # ====================================================

              doseNewNames = as.list(paste0( "dose_", responsesNewNames ))
              names( doseNewNames ) = rep( "dose",length(responsesNewNames))
              doseNewNames = lapply( doseNewNames, function(x) parse( text = x ) )
              doseNewNames = lapply( doseNewNames, function(x) x[[1]] )

              # ====================================================
              # variables substitution
              # ====================================================

              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )

              # =======================================================
              # RespPK change for PD Model with PK ode Michaelis-Menten
              # =======================================================

              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )
              for ( iterEquation in 1:numberOfEquations )
              {
                equations[[iterEquation]] = as.expression(do.call( 'substitute', list( equations[[iterEquation]][[1]], variablesNewNames ) ) )
                equations[[iterEquation]] = as.expression(do.call( 'substitute', list( equations[[iterEquation]][[1]], doseNewNames ) ) )
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
# END Class ModelODEBolus
##########################################################################################################


