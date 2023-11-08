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
# EvaluateModel
# ======================================================================================================

setMethod(f="EvaluateModel",
          signature="ModelODEDoseInEquations",
          definition = function( object, arm )
          {
            # ===============================================
            # outcomes and variables
            # ===============================================

            modelOutcomes = getOutcomesForEvaluation( object )
            outcomes = names( modelOutcomes )

            initialConditions = getInitialConditions( arm )
            initialConditions = unlist( initialConditions )

            variables = names( initialConditions )

            numberOfVariables = length( initialConditions )
            numberOfOutcomes = length( modelOutcomes )

            # ===============================================
            # convert model equations string to expression
            # ===============================================

            modelEquations = getEquations( object )
            modelEquations = lapply( modelEquations, function(x) parse( text = x ) )

            # ===============================================
            # sampling times
            # ===============================================

            # ======================================================
            # model sampling times = sampling times of all responses
            # ======================================================

            samplingTimesOutcome = list()

            for ( outcome in outcomes )
            {
              samplingTimesOutcome[[outcome]] = getSamplings( getSamplingTime( arm, outcome ) )
            }

            samplingTimesModel = sort( unique( c( 0.0, unlist( samplingTimesOutcome ) ) ) )
            colnames( samplingTimesModel ) = NULL

            # ===============================================
            # model parameters
            # ===============================================

            inputsModel = list()

            parameters = getParameters( object )

            modelParametersNames = lapply( parameters, function(x) getName(x) )

            numberOfParameters = getNumberOfParameters( object )

            # ===================================================
            # assign parameter values
            # ===================================================

            for ( parameter in parameters )
            {
              parameterMu = getMu( parameter )
              parameterName = getName( parameter )
              assign( parameterName, parameterMu )
            }

            # ===============================================
            # set initial conditions
            # ===============================================

            for ( i in 1:length( initialConditions ) )
            {
              initialConditions[i] = eval( parse( text = initialConditions[i] ) )
            }

            initialConditions = as.numeric( initialConditions )

            names( initialConditions ) = variables

            # ===============================================
            # outcome with administration
            # ===============================================

            outcomesWithAdministration = c()

            for ( outcome in outcomes )
            {
              administrationsTmp = getAdministration( arm, outcome )

              if ( length( administrationsTmp ) !=0 )
              {
                outcomesWithAdministration = c( outcomesWithAdministration, outcome )
              }
            }

            # ===============================================
            # time matrix for ODE
            # ===============================================

            # ===================================================
            # parameters matrix for ode evaluation
            # ===================================================

            timeMatrix = list()
            timeDose = list()
            indexTime = list()

            for ( outcome in outcomesWithAdministration )
            {
              # ===================================================
              # max samplingTimes
              # ===================================================

              maxSamplingTimeOutcome = max( getSamplings( getSamplingTime( arm, outcome ) ) )

              # ===================================================
              # time dose, dose and tau
              # ===================================================

              administration = getAdministration( arm, outcome )
              timeDose[[outcome]] = getTimeDose( administration )
              timeDose[[outcome]] = c( timeDose[[outcome]], maxSamplingTimeOutcome )
              dose = getDose( administration )
              tau = getTau( administration )

              # ===================================================
              # for repeated doses
              # ===================================================

              if ( tau !=0 )
              {
                n = maxSamplingTimeOutcome%/%tau
                inputsModel$dose[[outcome]] = rep( dose, n+1 )
                timeDose[[outcome]] = sort( unique( seq( 0, maxSamplingTimeOutcome, tau ) ) )

                n = length(timeDose[[outcome]])
                timeMatrix[[outcome]] = matrix( c( timeDose[[outcome]][(1:(n-1))], timeDose[[outcome]][(2:(n))]),n-1,2 )
              }
              else
              {
                # ===================================================
                # for multi doses
                # ===================================================

                inputsModel$dose[[outcome]] = dose
                timeDose[[outcome]] = sort( unique( c( timeDose[[outcome]] ) ) )

                n = length(timeDose[[outcome]])
                timeMatrix[[outcome]] = matrix(c(timeDose[[outcome]][(1:(n-1))], timeDose[[outcome]][(2:(n))]),n-1,2)
              }
            }

            # ===============================================
            # function: evaluation model ODE
            # ===============================================

            modelEvaluation = list()
            indexTime = list()

            modelODE = function( samplingTimes, initialConditions, inputsModel )
            {
              with( c( samplingTimes, initialConditions, inputsModel ),{

                assign("t", samplingTimes)

                # ===================================================
                # initial conditions
                # ===================================================

                for ( i in 1:length( variables ) )
                {
                  assign( variables[i], eval( parse( text = initialConditions[i] ) ) )
                }

                for ( outcome in outcomesWithAdministration )
                {
                  dose[[outcome]] = inputsModel$dose[[outcome]]

                  if ( t > max( unlist( timeDose[[outcome]] ) ) )
                  {
                    assign( paste0("dose_",outcome), 0 )
                  }
                  else
                  {
                    indexTime[[outcome]] = which( apply( timeMatrix[[outcome]], 1, findInterval, x = samplingTimes ) == 1)

                    # ===================================================
                    # time update for multi doses
                    # ===================================================

                    intervalTimeMatrix = timeMatrix[[outcome]][indexTime[[outcome]],]
                    t = t - intervalTimeMatrix[1]
                    assign( paste0("dose_",outcome), dose[[outcome]][indexTime[[outcome]]] )
                  }
                }

                for ( iter in 1:length( modelEquations ) )
                {
                  modelEvaluation[[iter]] = eval( modelEquations[[iter]] )
                }

                return( list( c( modelEvaluation ) ) )
              })
            }

            # ===============================================
            # function: scale evaluation model ODE
            # with outcomes evaluation
            # ===============================================

            scaleModelResponse = function( samplingTimesOutcome, samplingTimesModel, outcomes, modelEvaluation, variables )
            {
              evaluationOutcomes = list()

              for ( variable in variables )
              {
                assign( variable, modelEvaluation[, variable] )
              }

              for ( outcome in outcomes )
              {
                # ===================================================
                # scale the model outcomes
                # ===================================================

                evaluationOutcomes[[outcome]] = eval( parse( text = modelOutcomes[[outcome]] ) )

                indexSamplingTimes = match( samplingTimesOutcome[[outcome]] , samplingTimesModel )

                evaluationOutcomes[[outcome]] = as.data.frame( cbind( samplingTimesModel[indexSamplingTimes],
                                                                      evaluationOutcomes[[outcome]][indexSamplingTimes] ) )

                names( evaluationOutcomes[[outcome]] ) = c( "time", outcome )
              }

              return( evaluationOutcomes )
            }

            # ===============================================
            # model evaluation
            # ===============================================

            # ===================================================
            # parameters atol and rtol for ode solver
            # ===================================================

            odeSolverParameters = getOdeSolverParameters( object )

            atol = odeSolverParameters$atol
            rtol = odeSolverParameters$rtol

            out = ode( initialConditions,
                       samplingTimesModel,
                       modelODE,
                       inputsModel,
                       hmax = 0.0,
                       atol = atol, rtol = rtol,
                       method = "lsodar" )

            evaluationOutcomes = scaleModelResponse( samplingTimesOutcome, samplingTimesModel, outcomes, out, variables )

            # =================================================
            # substitute for outcomes evaluation with scaling
            # =================================================

            subsituteTmp = list()

            modelEquationsTmp = getEquations( object )
            modelEquationsNames = names( modelEquations )
            modelEquationsTmpNames = str_remove(names(modelEquationsTmp), "Deriv_")
            names( modelEquationsTmp ) = modelEquationsTmpNames

            for( outcome in outcomes )
            {
              variableOutcome = modelOutcomes[[outcome]]

              for ( variable in variables )
              {
                modelEquationsTmp[[variable]] = paste0("(", modelEquationsTmp[[variable]],")")

                variableOutcome = parse( text = gsub( variable, modelEquationsTmp[[variable]], variableOutcome ) )
                variableOutcome = Simplify( variableOutcome )
              }
              subsituteTmp[[outcome]] = variableOutcome
            }

            modelEquations = subsituteTmp
            names( modelEquations ) = modelEquationsNames

            # ===============================================
            # compute sensitivity indices
            # ===============================================

            # ===================================================
            # model parameters
            # ===================================================

            parameters = getParameters( object )

            # ===================================================
            # parameters for computing gradients
            # ===================================================

            parametersGradient = parametersForComputingGradient( object )
            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            outcomesGradient = list()

            for( outcome in outcomes )
            {
              resultsGrad = list()

              for ( iterShifted in 1:dim( shiftedParameters)[2] )
              {
                valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

                # ===================================================
                # assign parameter values
                # ===================================================

                for( iterParameter in 1:numberOfParameters )
                {
                  parameterMu = valuesParameters[iterParameter]
                  parameterName = getName( parameters[[iterParameter]] )
                  assign( parameterName, parameterMu )
                }

                out = ode( initialConditions,
                           samplingTimesModel,
                           modelODE,
                           inputsModel,
                           hmax = 0.0,
                           atol = atol, rtol = rtol,
                           method = "lsodar" )

                colnames( out ) = c("time",outcomes)

                resultsGrad[[iterShifted]] = out[,outcome]
              }

              resultsGrad = do.call( cbind, resultsGrad )

              coefs = list()

              for ( i in 1 :dim( resultsGrad)[1] )
              {
                coefs[[i]] = solve( do.call("cbind", Xcols), resultsGrad[i,])/frac
                coefs[[i]] = coefs[[i]][1 + seq_along( parameters )]
              }

              # =======================================================
              # match sampling times responses with sampling time model
              # =======================================================

              indexSamplingTimes = match( samplingTimesOutcome[[outcome]] , samplingTimesModel )

              outcomesGradient[[outcome]] = do.call(rbind,coefs)

              # =======================================================
              # case if one parameter
              # =======================================================

              if( dim( outcomesGradient[[outcome]] )[1]==1 )
              {
                outcomesGradient[[outcome]] = outcomesGradient[[outcome]][indexSamplingTimes]
              }else{
                # =======================================================
                # case more than one parameter
                # =======================================================

                outcomesGradient[[outcome]] = outcomesGradient[[outcome]][indexSamplingTimes,]
              }

              outcomesGradient[[outcome]] = as.data.frame( cbind( samplingTimesModel[indexSamplingTimes], outcomesGradient[[outcome]] ) )

              colnames( outcomesGradient[[outcome]] ) = c( "time", modelParametersNames )
            }

            names( outcomesGradient ) = outcomes

            # =======================================================
            # outcomesAllGradient
            # select with model error
            # =======================================================

            outcomesAllGradient = list()

            modelError = getModelError( object )

            for ( outcome in outcomes )
            {
              index = which( sapply( modelError, function (x) getOutcome(x) == outcome ) )

              if ( length( index ) != 0 )
              {
                outcomesAllGradient[[outcome]] = outcomesGradient[[outcome]]
              }
            }

            outcomesAllGradient = as.data.frame( do.call( rbind, outcomesAllGradient ) )
            rownames( outcomesAllGradient ) = NULL

            return( list( evaluationOutcomes = evaluationOutcomes,
                          outcomesGradient = outcomesGradient,
                          outcomesAllGradient = outcomesAllGradient ) )
          })

# ======================================================================================================
# definePKModel
# ======================================================================================================

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















