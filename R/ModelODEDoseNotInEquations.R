#' Class "ModelODEDoseNotInEquations"
#' @description ...
#' @name ModelODEDoseNotInEquations-class
#' @aliases ModelODEDoseNotInEquations
#' @docType class
#' @include ModelODE.R
#' @export

ModelODEDoseNotInEquations = setClass("ModelODEDoseNotInEquations", contains = "ModelODE")

#' @rdname defineModelEquationsFromStringToFunction
#' @export

setMethod("defineModelEquationsFromStringToFunction",
          signature("ModelODEDoseNotInEquations"),
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

            functionArguments = c( parametersNames, variablesNames, "t" )
            functionArgumentsSymbol = lapply( functionArguments, as.symbol )

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
          signature("ModelODEDoseNotInEquations"),
          function( object, arm )
          {
            # get data for arm evaluation
            dataForArmEvaluation = getDataForArmEvaluation( arm )

            inputsModel = list()
            initialConditions = list()
            timeDose = list()

            # outcomes and sampling time
            samplingTimesModel = dataForArmEvaluation$samplingTimesModel
            outcomesForEvaluation = dataForArmEvaluation$outcomesForEvaluation
            samplingTimesOutcomes = dataForArmEvaluation$samplingTimesOutcomes
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

            # parameters matrix for ode evaluation
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
                maxSamplingTimeVariable = max ( unlist( samplingTimesOutcomes ) )
                timeDose[[variable]] = sort( unique( seq( 0, maxSamplingTimeVariable, tau ) ) )
                inputsModel$dose[[variable]] =  rep( dose, length( timeDose[[variable]] ) )

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

            # variable with administration initial value
            for ( variable in variablesWithAdministration )
            {
              assign( variable, inputsModel$dose[[variable]][1] )
            }

            # variablesWithAdministrationToAdd or the case for PK model ie cmpt dose = the only variable
            variablesWithAdministrationToAdd =
            variablesWithAdministration[ which( !( variablesWithAdministration %in% names( initialConditions ) ) ) ]

            if ( length( variablesWithAdministrationToAdd ) != 0 )
            {
              initialConditionsTmp = c( variablesWithAdministrationToAdd, initialConditions )

              initialConditions = map( initialConditionsTmp, ~ eval( parse( text = .x ) ) ) %>% unlist()

              names( initialConditions ) = c( variablesWithAdministration, variablesWithNoAdministration )
            }else{
              initialConditions = map( initialConditions, ~ eval( parse( text = .x ) ) ) %>% unlist()
            }

            # Dose event
            doseEvent = list()

            for ( variable in variablesWithAdministration )
            {
              doseEvent[[variable]] = data.frame( var = variable,
                                                  time = timeDose[[variable]],
                                                  value = inputsModel$dose[[variable]],
                                                  method = c("add") )
            }

            doseEvent = do.call( rbind, doseEvent )
            doseEvent = doseEvent[ order( doseEvent$time ), ]

            # ensure that event time in times to avoid warning
            uniqueTimes = cleanEventTimes( doseEvent$time, samplingTimesModel )
            samplingTimesModel = sort( unique( c( samplingTimesModel, doseEvent$time ) ) )

            # create and solve ode model
            modelODEDoseAsCmpt = function( samplingTimesModel, initialConditions, parameters )
            {
              with( as.list( c(  samplingTimesModel, initialConditions, parameters ) ),{

                output = do.call( modelFunction, setNames( functionArgumentsSymbols, functionArguments  ) )

                outcomesForEvaluation = map( outcomesForEvaluation, ~ eval( .x ) )

                return( c( output, outcomesForEvaluation ) )
              })
            }

            odeSolverParameters = getOdeSolverParameters( object )

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( initialConditions = initialConditions,
                                              samplingTimesModel = samplingTimesModel,
                                              samplingTimesOutcomes = samplingTimesOutcomes,
                                              modelODEDoseAsCmpt = modelODEDoseAsCmpt,
                                              inputsModel = inputsModel,
                                              odeSolverParameters = odeSolverParameters,
                                              doseEvent = doseEvent ) )

            return( dataForModelEvaluation )

          })

# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f = "EvaluateModel",
          signature = "ModelODEDoseNotInEquations",
          definition = function( object, dataForModelEvaluation, arm  )
          {
            initialConditions = dataForModelEvaluation$initialConditions

            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            samplingTimesOutcomes = dataForModelEvaluation$samplingTimesOutcomes

            modelODEDoseAsCmpt = dataForModelEvaluation$modelODEDoseAsCmpt

            inputsModel = dataForModelEvaluation$inputsModel

            atol = dataForModelEvaluation$odeSolverParameters$atol
            rtol = dataForModelEvaluation$odeSolverParameters$rtol

            doseEvent = dataForModelEvaluation$doseEvent

            modelEvaluation = ode( initialConditions,
                                   samplingTimesModel,
                                   modelODEDoseAsCmpt,
                                   inputsModel,
                                   events = list( data = doseEvent ),
                                   hmax = 0,
                                   method = "lsode",
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
          signature = "ModelODEDoseNotInEquations",
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
#'
setMethod("definePKModel",
          signature("ModelODE"),
          function( object, outcomes )
          {
            # -------------------------------------------
            # change names: responses, variables, doses
            # -------------------------------------------

            # original and new outcomes
            newOutcomes = outcomes
            originalOutcomes = getOutcomes( object )

            if ( length( outcomes ) != 0 )
            {
              # variable names
              variablesNames = unlist( originalOutcomes )
              variablesNewNames = unlist( newOutcomes )

              # change equation names
              equations = getEquations( object )
              names( equations ) = paste0( "Deriv_", variablesNewNames )

              # response names old and new
              responsesNames = names( originalOutcomes )
              responsesNewNames = names( newOutcomes )

              for ( iterEquation in 1:length( equations ) )
              {
                # change response names
                for( iterResponseName in 1:length( responsesNames ) )
                {
                  equations[[iterEquation]] = gsub( responsesNames[iterResponseName],
                                                    responsesNewNames[iterResponseName], equations[[iterEquation]] )
                }

                # change variable names
                for( iterVariableName in 1:length( variablesNewNames ) )
                {
                  equations[[iterEquation]] = gsub( variablesNames[iterVariableName],
                                                    variablesNewNames[iterVariableName], equations[[iterEquation]] )
                }

              }
              object = setEquations( object, equations )
              object = setOutcomes( object, newOutcomes )

            }else{

              # change only dose name
              equations = getEquations( object )
              responseNames = names( originalOutcomes )

              # set equation and outcome
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
#'
setMethod("definePKPDModel",
          signature("ModelODEDoseNotInEquations","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelODEDoseNotInEquations()

            if ( length( outcomes ) != 0 )
            {
              # original and new outcomes
              newOutcomes = outcomes
              originalOutcomesPKModel = getOutcomes( PKModel )
              originalOutcomesPDModel = getOutcomes( PDModel )

              originalOutcomesPKModel = unlist( originalOutcomesPKModel )
              originalOutcomesPDModel = unlist( originalOutcomesPDModel )

              originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

              # variable names
              variablesNames = unlist( originalOutcomes )
              variablesNewNames = unlist( newOutcomes )

              # model equation
              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )
              names( equations ) = paste0( "Deriv_", variablesNewNames )
              numberOfEquations = length( equations )

              # response names old and new
              responsesNames = names( originalOutcomes )
              responsesNewNames = names( newOutcomes )

              # variables substitution
              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )
              variablesNewNames = lapply( variablesNewNames, function(x) x[[1]] )

              # RespPK change for PD Model with PK ode Michaelis-Menten
              variablesNewNames = append( variablesNewNames, variablesNewNames[[1]] )
              names( variablesNewNames ) = c( variablesNames, "RespPK" )

              for ( iterEquation in 1:numberOfEquations )
              {
                equations[[iterEquation]] = as.expression(do.call( 'substitute',
                                                                   list( equations[[iterEquation]][[1]], variablesNewNames ) ) )
              }

              # convert equations from expression to string
              equations = lapply( equations, function(x) x[[1]] )
              equations = lapply( equations, function(x) paste( deparse( x ), collapse = " " ) )
              equations = lapply( equations, function(x) str_replace_all( x, " ", "" ) )

              # set outcomes and equations
              model = setEquations( model, equations )
              model = setOutcomes( model, newOutcomes )

            }else{

              # outcomes
              newOutcomes = outcomes
              originalOutcomesPKModel = getOutcomes( PKModel )
              originalOutcomesPDModel = getOutcomes( PDModel )

              originalOutcomesPKModel = unlist( originalOutcomesPKModel )
              originalOutcomesPDModel = unlist( originalOutcomesPDModel )

              originalOutcomes = as.list( c( originalOutcomesPKModel, originalOutcomesPDModel ) )

              # response names old and new
              responsesNames = names( originalOutcomes )

              # variable names
              variablesNames = unlist( originalOutcomes )
              variablesNames = lapply( variablesNames, function(x) parse( text = x ) )

              # model equations
              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )
              numberOfEquations = length( equations )

              # RespPK change for PD Model with PK ode Michaelis-Menten
              variableSubstitutedMichaelisMenten = list()
              variableSubstitutedMichaelisMenten[[1]] = variablesNames[[1]][[1]]
              names( variableSubstitutedMichaelisMenten ) = "RespPK"

              for ( iterEquation in 1:numberOfEquations )
              {
                equations[[iterEquation]] = as.expression(do.call( 'substitute', list( equations[[iterEquation]][[1]],
                                                                                       variableSubstitutedMichaelisMenten ) ) )
              }

              # convert equations from expression to string
              equations = lapply( equations, function(x) x[[1]] )
              equations = lapply( equations, function(x) paste( deparse( x ), collapse = " " ) )
              equations = lapply( equations, function(x) str_replace_all( x, " ", "" ) )

              model = setEquations( model, equations )
              model = setOutcomes( model, originalOutcomes )
            }

            return( model )
          })

##########################################################################################################
# END Class ModelODEDoseNotInEquations
##########################################################################################################

