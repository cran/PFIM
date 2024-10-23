#' Class "ModelAnalyticSteadyState"
#'
#' @description The class \code{ModelAnalyticSteadyState} defines information concerning the construction of an analytical model steady state.
#' The class \code{ModelAnalyticSteadyState} inherits from the class \code{ModelAnalytic}.
#'
#' @name ModelAnalyticSteadyState-class
#' @aliases ModelAnalyticSteadyState
#' @docType class
#' @include ModelAnalytic.R
#' @export

ModelAnalyticSteadyState = setClass("ModelAnalyticSteadyState",
                                    contains = "ModelAnalytic",
                                    prototype = prototype(
                                      initialConditions = list(NULL),
                                      odeSolverParameters = list(NULL)))

#' initialize
#' @param .Object .Object
#' @param name name
#' @param description description
#' @param equations equations
#' @param outcomes outcomes
#' @param parameters parameters
#' @param modelError modelError
#' @return ModelAnalyticSteadyState
#' @export
#'
setMethod( f="initialize",
           signature="ModelAnalyticSteadyState",
           definition= function (.Object, name, description, equations, outcomes, parameters, modelError )
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
          signature("ModelAnalyticSteadyState"),
          function( object, parametersNames, outcomesWithAdministration, outcomesWithNoAdministration )
          {
            # function parameters
            doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
            outcomes = getOutcomesForEvaluation( object )
            equations = getEquations( object )

            # index for equations
            indexEquationsWithAdmin = which( names( equations ) %in% outcomesWithAdministration )
            indexEquationsWithoutAdmin = which( names( equations ) %in% outcomesWithNoAdministration )

            # ==================================================
            # equations for responses with administration ie PK
            # ==================================================

            equationsBody = list()

            for ( name in names( equations )[indexEquationsWithAdmin] )
            {
              equation = equations[[name]]
              equationsBody = c( equationsBody, sprintf( "%s = %s", name, equation ) )
            }

            functionBody = paste( equationsBody, collapse = "\n" )
            functionBody = sprintf("%s\nreturn(list(%s))", functionBody, paste( outcomesWithAdministration, collapse = ", " ) )
            argsWithAdmin = c( doseNames, parametersNames, "tau", "t" )
            functionDefinition = sprintf( "function(%s) { %s }", paste( argsWithAdmin, collapse = ", "), functionBody )
            modelFunctionWithAdmin = eval( parse( text = functionDefinition ) )
            argsSymbolWithAdmin = lapply( argsWithAdmin, as.symbol )

            # ====================================================
            # equations for responses without administration ie PD
            # ====================================================

            equationsBody = list()

            for ( name in names( equations )[indexEquationsWithoutAdmin] )
            {
              equation = equations[[name]]
              equationsBody = c( equationsBody, sprintf( "%s = %s", name, equation ) )
            }

            functionBody = paste( equationsBody, collapse = "\n" )
            functionBody = sprintf("%s\nreturn(list(%s))", functionBody, paste( outcomesWithNoAdministration, collapse = ", " ) )
            argsWithoutAdmin = c( outcomesWithAdministration, parametersNames, "t" )
            functionDefinition = sprintf( "function(%s) { %s }", paste( argsWithoutAdmin, collapse = ", "), functionBody )
            modelFunctionWithoutAdmin = eval( parse( text = functionDefinition ) )
            argsSymbolWithoutAdmin = lapply( argsWithoutAdmin, as.symbol )

            return( modelFunction = list( modelFunctionWithAdmin = list( modelFunctionWithAdmin = modelFunctionWithAdmin,
                                                                         argsSymbolWithAdmin = argsSymbolWithAdmin,
                                                                         argsWithAdmin = argsWithAdmin ),

                                          modelFunctionWithoutAdmin = list( modelFunctionWithoutAdmin = modelFunctionWithoutAdmin,
                                                                            argsSymbolWithoutAdmin = argsSymbolWithoutAdmin,
                                                                            argsWithoutAdmin = argsWithoutAdmin ) ) )

          })

# ======================================================================================================

#' @rdname setDataForModelEvaluation
#' @export

setMethod("setDataForModelEvaluation",
          signature("ModelAnalyticSteadyState"),
          function( object, arm )
          {
            dataForArmEvaluation = getDataForArmEvaluation( arm )

            inputsModel = list()
            initialConditions = list()
            timeMatrix = list()

            # outcomes and sampling time
            outcomesWithAdministration = dataForArmEvaluation$outcomesWithAdministration
            samplingTimesModel = dataForArmEvaluation$samplingTimesModel
            samplingTimesOutcome = dataForArmEvaluation$samplingTimesOutcomes

            # administration parameters
            data = list()

            for ( outcome in outcomesWithAdministration )
            {
              # max samplingTimes
              maxSamplingTimeOutcome = max( samplingTimesOutcome[[outcome]] )

              # time dose, dose and tau
              administration = getAdministration( arm, outcome )

              dose = getDose( administration )
              tau = getTau( administration )

              inputsModel[[outcome]]$timeDose = getTimeDose( administration )
              inputsModel[[outcome]]$timeDose = inputsModel[[outcome]]$timeDose

              # for repeated doses
              if ( tau !=0 )
              {
                inputsModel[[outcome]]$timeDose = unique( seq( 0, maxSamplingTimeOutcome, tau ) )
                inputsModel[[outcome]]$dose = rep( dose, length( inputsModel[[outcome]]$timeDose  ) )

              }else{

                # for multi doses
                inputsModel[[outcome]]$dose = dose
                inputsModel[[outcome]]$timeDose = sort( unique( c( inputsModel[[outcome]]$timeDose ) ) )
              }

              timeDose = inputsModel[[outcome]]$timeDose

              timeMatrixEvaluationTmp = matrix( samplingTimesModel, length( samplingTimesModel ),length( timeDose ) )

              indicesDoses = c()
              doseResponse = c()

              for ( i in 1:dim(timeMatrixEvaluationTmp)[1] )
              {
                for ( j in 1 :length( timeDose  ) )
                {
                  if( timeMatrixEvaluationTmp[i,j] - timeDose[j] > 0 )
                  {
                    timeMatrixEvaluationTmp[i,j] = timeMatrixEvaluationTmp[i,j] - timeDose[j]
                  }
                }
                indicesDoses[i] = length( unique(timeMatrixEvaluationTmp[i,] ) )
              }

              data = rbind( data, data.frame( timeMatrixEvaluationTmp,
                                              indicesDoses = indicesDoses,
                                              doseName = paste0("dose_",outcome  ),
                                              outcome = outcome ) )
            }

            dataForModelEvaluation = c( dataForArmEvaluation,

                                        list( inputsModel = inputsModel,
                                              tau = tau,
                                              data = data ) )
            return( dataForModelEvaluation )
          })


# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f = "EvaluateModel",
          signature = "ModelAnalyticSteadyState",
          definition = function( object, dataForModelEvaluation, arm )
          {
            data = dataForModelEvaluation$data
            inputsModel = dataForModelEvaluation$inputsModel

            samplingTimesModel = dataForModelEvaluation$samplingTimesModel
            samplingTimesOutcome = dataForModelEvaluation$samplingTimesOutcome

            outcomes = dataForModelEvaluation$modelOutcomes
            outcomesWithAdministration = dataForModelEvaluation$outcomesWithAdministration
            outcomesWithNoAdministration = dataForModelEvaluation$outcomesWithNoAdministration
            numberOfOutcomesWithAdministration = length( outcomesWithAdministration )
            numberOfOutcomesWithNoAdministration = length( outcomesWithNoAdministration )

            modelEvaluation = dataForModelEvaluation$modelEvaluation
            equationFunction = dataForModelEvaluation$equationFunction

            # for equations with admin
            modelFunctionWithAdmin = equationFunction$modelFunctionWithAdmin$modelFunctionWithAdmin
            argsSymbolWithAdmin = equationFunction$modelFunctionWithAdmin$argsSymbolWithAdmin
            argsWithAdmin = equationFunction$modelFunctionWithAdmin$argsWithAdmin

            # for equations without admin
            modelFunctionWithoutAdmin = equationFunction$modelFunctionWithoutAdmin$modelFunctionWithoutAdmin
            argsSymbolWithoutAdmin = equationFunction$modelFunctionWithoutAdmin$argsSymbolWithoutAdmin
            argsWithoutAdmin = equationFunction$modelFunctionWithoutAdmin$argsWithoutAdmi

            modelParameters = getParameters( object )

            for( modelParameter in modelParameters )
            {
              modelParameterName = getName( modelParameter )
              modelParameterValue = getMu( modelParameter )
              assign( modelParameterName, modelParameterValue )
            }

            # evaluation outcomes

            # value for tau
            assign( "tau", dataForModelEvaluation$tau )

            modelEvaluation = matrix( 0.0, length( samplingTimesModel ),
                                      numberOfOutcomesWithAdministration+numberOfOutcomesWithNoAdministration )

            for ( iterTime in seq_along( samplingTimesModel ) )
            {
              # select outcome
              outcome = data$outcome[iterTime]

              # dose
              indicesDoses = data$indicesDoses[iterTime]
              doseName = data$doseName[iterTime]

              dose = inputsModel[[outcome]]$dose
              doses = dose[1:indicesDoses]

              # time doses
              time = unlist( data[iterTime,1:indicesDoses] )

              outputOutcomeWithAdmin = 0

              for ( indiceDose in seq_len( indicesDoses ) )
              {
                t = time[indiceDose]

                assign( doseName, doses[indiceDose] )

                # evaluation outcomes with administration
                output = unlist( do.call( modelFunctionWithAdmin, setNames( argsSymbolWithAdmin, argsWithAdmin ) ) )
                outputOutcomeWithAdmin = outputOutcomeWithAdmin + output[1:numberOfOutcomesWithAdministration]
                modelEvaluation[iterTime,] = c( outputOutcomeWithAdmin )

                # evaluation outcomes with administration
                if ( numberOfOutcomesWithNoAdministration !=0 )
                {
                  assign( outcomesWithAdministration, outputOutcomeWithAdmin )
                  output = unlist( do.call( modelFunctionWithoutAdmin, setNames( argsSymbolWithoutAdmin, argsWithoutAdmin ) ) )
                  outputOutcomeWithNoAdmin = output[1:numberOfOutcomesWithAdministration]
                  modelEvaluation[iterTime,] = c( outputOutcomeWithAdmin, outputOutcomeWithNoAdmin )
                }
              }
            }

            modelEvaluation = as.data.frame( modelEvaluation )

            colnames( modelEvaluation ) = outcomes

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
# EvaluateModel
# ======================================================================================================

#' @rdname EvaluateModelGradient
#' @export

setMethod(f = "EvaluateModelGradient",
          signature = "ModelAnalyticSteadyState",
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
          signature("ModelAnalyticSteadyState"),
          definition = function( object, outcomes )
          {
            # ====================================================
            # outcomes from the library of models and new outcomes
            # ====================================================

            originalOutcomes = getOutcomes( object )
            newOutcomes = outcomes

            # ====================================================
            # with newOutcomes
            # ====================================================

            if ( length( newOutcomes ) != 0 )
            {
              # ====================================================
              # change equation names
              # ====================================================

              equations = getEquations( object )
              names( equations ) = newOutcomes

              # ====================================================
              # response names old and new
              # ====================================================

              responseNames = unlist( originalOutcomes )
              responseNewNames = unlist( newOutcomes )

              # ====================================================
              # new doses names
              # ====================================================

              doseNewName = paste0( "dose_", responseNewNames )

              for ( equationName in names( equations ) )
              {
                # ====================================================
                # change variable names
                # ====================================================

                for( iterResponseName in 1:length( responseNames ) )
                {
                  equations[[equationName]] = gsub( responseNames[iterResponseName],
                                                    responseNewNames[iterResponseName], equations[[equationName]] )
                }

                # ====================================================
                # change dose names
                # ====================================================

                for( iterDoseNewName in 1:length( doseNewName ) )
                {
                  equations[[equationName]] = gsub( "dose", doseNewName[iterDoseNewName], equations[[equationName]] )
                }
              }
              # ====================================================
              # set equations and outcomes
              # ====================================================

              object = setOutcomes( object, newOutcomes )
              object = setEquations( object, equations )

            }else{
              # ====================================================
              # change only dose name
              # ====================================================

              equations = getEquations( object )
              responseNames = unlist( originalOutcomes )
              doseName = paste0( "dose_", responseNames )

              for ( equationName in names( equations ) )
              {
                # ====================================================
                # change dose names
                # ====================================================

                for( iterDoseName in 1:length( doseName ) )
                {
                  equations[[equationName]] = gsub( "dose", doseName[iterDoseName], equations[[equationName]] )
                }
              }

              # ====================================================
              # set equation and outcome
              # ====================================================

              outcomes = getOutcomes( object )
              names( outcomes ) = outcomes
              object = setOutcomes( object, outcomes )
              object = setEquations( object, equations )
            }

            return( object)
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature("ModelAnalyticSteadyState","ModelAnalyticSteadyState"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelAnalytic()

            if ( length( outcomes ) != 0 )
            {
              # ====================================================
              # original and new outcomes
              # ====================================================

              newOutcomes = outcomes

              outcomesPK = getOutcomes( PKModel )
              outcomesPD = getOutcomes( PDModel )
              originalOutcomes = c( outcomesPK, outcomesPD )

              # ====================================================
              # set the equations
              # ====================================================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )

              equations = c( PKModelEquations, PDModelEquations )

              model = setEquations( model, equations )

              # ====================================================
              # change equation names
              # ====================================================

              names( equations ) = newOutcomes

              # ====================================================
              # response names old and new
              # ====================================================

              responsesNames = unlist( originalOutcomes )
              responsesNewNames = unlist( newOutcomes )

              # ====================================================
              # new doses names
              # ====================================================

              doseNewName = paste0( "dose_", responsesNewNames )

              for ( iterEquation in 1:length( equations ) )
              {
                # ====================================================
                # change response names
                # ====================================================

                for( iterResponseName in 1:length( responsesNames ) )
                {
                  equations[[iterEquation]] = gsub( responsesNames[iterResponseName],
                                                    responsesNewNames[iterResponseName], equations[[iterEquation]] )
                }

                # ====================================================
                # change dose names
                # ====================================================

                equations[[iterEquation]] = gsub( "dose", doseNewName[iterEquation], equations[[iterEquation]] )
              }

              # ====================================================
              # set equations and outcomes
              # ====================================================

              model = setOutcomes( model, newOutcomes )
              model = setEquations( model, equations )

            }else{

              # ====================================================
              # change only dose name
              # ====================================================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )

              equations = c( PKModelEquations, PDModelEquations )

              dosesName = paste0( "dose_",  names( PKModelEquations ) )

              for ( iterEquation in 1:length( equations ) )
              {
                equations[[iterEquation]] = gsub( "dose", dosesName[iterEquation], equations[[iterEquation]] )
              }

              # ====================================================
              # set equation and outcome
              # ====================================================

              outcomesPK = getOutcomes( PKModel )
              outcomesPD = getOutcomes( PDModel )
              originalOutcomes = c( outcomesPK, outcomesPD )

              model = setOutcomes( model, originalOutcomes )
              model = setEquations( model, equations )
            }

            return( model )
          })

# ======================================================================================================
# definePKPDModel
# ======================================================================================================

#' @rdname definePKPDModel
#' @export

setMethod("definePKPDModel",
          signature("ModelAnalyticSteadyState","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            # ====================================================
            # create model
            # ====================================================

            model = ModelODEDoseInEquations()

            # ====================================================
            # convert PK model analytic to model ODE
            # ====================================================

            PKModelEquations = convertPKModelAnalyticToPKModelODE( PKModel )
            PDModelEquations = getEquations( PDModel )

            # ====================================================
            # outcomes from the library of models
            # ====================================================

            originalOutcomes = list( "RespPK" = "C1", "RespPD" = "E" )

            if ( length( outcomes ) != 0 )
            {
              # ====================================================
              # response and variable name
              # ====================================================

              variablesNames = unlist( originalOutcomes )
              responsesNames = names( originalOutcomes )

              responsesNewNames = names( outcomes )
              variablesNewNames = unlist( outcomes, use.names = FALSE )

              # ====================================================
              # dose new name # PK and PKPD
              # ====================================================

              doseNewName = paste0( "dose_", responsesNewNames[1] )

              # ====================================================
              # change variable for RespPK to C1
              # ====================================================

              PKModelEquations = gsub( "RespPK", "C1", PKModelEquations )
              PDModelEquations = gsub( "RespPK", "C1", PDModelEquations )

              # ====================================================
              # change equation names
              # ====================================================

              equations = c( PKModelEquations, PDModelEquations )
              names( equations ) = paste0( "Deriv_", variablesNewNames )
              equations = lapply( equations, function(x) parse( text = x ) )

              numberOfEquations = length( equations )

              # ====================================================
              # variable substitution
              # ====================================================

              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )

              variableSubstituted = list()

              for( iterVariableName in 1:length( variablesNewNames ) )
              {
                variableSubstituted[[iterVariableName]] = variablesNewNames[[iterVariableName]][[1]]
              }

              names( variableSubstituted ) = variablesNames

              # ====================================================
              # dose substitution
              # ====================================================

              doseSubstituted = list()
              doseSubstituted[[1]] = parse( text = doseNewName )[[1]]
              names( doseSubstituted ) = "dose"

              # ====================================================
              # all substitutions
              # ====================================================

              allSubstitutions = c( variableSubstituted, doseSubstituted )

              # ====================================================
              # substitution in equations
              # ====================================================

              for ( equationName in names( equations ) )
              {
                equations[[equationName]] = do.call( 'substitute',
                                                     list( equations[[equationName]][[1]], allSubstitutions ) )
              }

              # ====================================================
              # set the equations
              # ====================================================

              equations = lapply( equations, function(x) paste( deparse( x ), collapse=" " ) )
              model = setEquations( model, equations )

              # ====================================================
              # set the outcomes
              # ====================================================

              model = setOutcomes( model, outcomes )

            }else{

              # ====================================================
              # By default: RespPK to C1
              # ====================================================

              variablesNewNames =  list("RespPK" = quote(C1))
              responsesNames = names( originalOutcomes )

              # ====================================================
              # model equations
              # ====================================================

              names( PKModelEquations ) = "Deriv_C1"
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )

              # ====================================================
              # variable substitution
              # ====================================================

              variableSubstituted = variablesNewNames

              # ====================================================
              # dose name
              # ====================================================

              doseSubstituted = list()
              doseNames = paste0( "dose_", responsesNames[1] )
              doseSubstituted[[1]] = parse( text = doseNames )[[1]]
              names( doseSubstituted ) = "dose"

              # ====================================================
              # all substitutions
              # ====================================================

              allSubstitutions = c( variableSubstituted, doseSubstituted )

              for ( equationName in names( equations ) )
              {
                equations[[equationName]] = do.call( 'substitute',
                                                     list( equations[[equationName]][[1]], allSubstitutions ) )
              }

              # ====================================================
              # set the equations
              # ====================================================

              equations = lapply( equations, function(x) paste( deparse( x ), collapse=" " ) )
              model = setEquations( model, equations )

              # ====================================================
              # set the outcomes
              # ====================================================

              outcomesPD = getOutcomes( PDModel )
              outcomes =  c( c( "RespPK" = "C1"), outcomesPD )
              model = setOutcomes( model, outcomes )
            }

            return( model )
          })


# ======================================================================================================
# convertPKModelAnalyticToPKModelODE
# ======================================================================================================

#' @rdname convertPKModelAnalyticToPKModelODE
#' @export

setMethod("convertPKModelAnalyticToPKModelODE",
          signature("ModelAnalyticSteadyState"),
          function( object )
          {
            # ====================================================
            # name, equations and  outcome
            # ====================================================

            modelName = getName( object )
            equations = getEquations( object )
            outcomes = getOutcomes( object )

            output = list()

            # ====================================================
            # change analytic equation to ode equation
            # ====================================================

            for ( equation in equations )
            {
              equation = parse( text = equation )

              equationPKsubstitute = equation[[1]]
              dtEquationPKsubstitute = D( equationPKsubstitute, "t" )

              equationPKsubstitute = paste( deparse( equationPKsubstitute ), collapse = "" )
              dtEquationPKsubstitute = paste( deparse( dtEquationPKsubstitute ), collapse = "" )

              if ( grepl( "Cl", modelName ) == TRUE )
              {
                equation = paste0( dtEquationPKsubstitute,"+(Cl/V)*", equationPKsubstitute, collapse = "" )
                equation = paste0( equation, "- (Cl/V)*RespPK", collapse = "" )
              }else{
                equation = paste0( dtEquationPKsubstitute,"+k*", equationPKsubstitute, collapse = "" )
                equation = paste0( equation, "- k*RespPK", collapse = "" )
              }
              equation = paste( Simplify( equation ) )
              equation = gsub( " ","", equation )
            }

            equation = as.list(equation)
            names( equation ) = outcomes

            return( equation )
          })

##########################################################################################################
# END Class "ModelAnalyticSteadyState"
##########################################################################################################





