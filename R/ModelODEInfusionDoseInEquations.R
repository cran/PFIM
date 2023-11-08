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
# EvaluateModel
# ======================================================================================================

setMethod(f="EvaluateModel",
          signature="ModelODEInfusionDoseInEquations",
          definition = function( object, arm )
          {
            # ===============================================
            # model parameters
            # ===============================================

            parameters = getParameters( object )

            modelParametersNames = lapply( parameters, function(x) getName(x) )
            numberOfParameters = getNumberOfParameters( object )

            # assign parameter values
            for ( parameter in parameters )
            {
              parameterMu = getMu( parameter )
              parameterName = getName( parameter )
              assign( parameterName, parameterMu )
            }

            # ===============================================
            # outcomes and variables and initial conditions
            # ===============================================

            modelOutcomes = getOutcomesForEvaluation( object )
            outcomes = names( modelOutcomes )

            initialConditions = getInitialConditions( arm )

            for ( i in 1:length( initialConditions ) )
            {
              initialConditions[i] = eval( parse( text = initialConditions[i] ) )
            }

            initialConditions = unlist( initialConditions )
            variables = names( initialConditions )

            numberOfVariables = length( initialConditions )
            numberOfOutcomes = length( modelOutcomes )

            # ===============================================
            # convert model equations string to expression
            # ===============================================

            modelEquations = getEquations( object )
            modelEquations$duringInfusion = lapply( modelEquations$duringInfusion, function(x) parse( text = x ) )
            modelEquations$afterInfusion = lapply( modelEquations$afterInfusion, function(x) parse( text = x ) )

            # ===============================================
            # sampling times
            # ===============================================

            # model sampling times = sampling times of all responses
            samplingTimesOutcome = list()

            for ( outcome in outcomes )
            {
              samplingTimesOutcome[[outcome]] = getSamplings( getSamplingTime( arm, outcome ) )
            }

            samplingTimesModel = sort( unique( c( 0, unlist( samplingTimesOutcome ) ) ) )
            colnames( samplingTimesModel ) = NULL

            # ===============================================
            # outcome with administration
            # ===============================================

            outcomesWithAdministration = c()
            outcomesWithNoAdministration = c()
            for ( outcome in outcomes )
            {
              administrationsTmp = getAdministration( arm, outcome )

              if ( length( administrationsTmp ) !=0 )
              {
                outcomesWithAdministration = c( outcomesWithAdministration, outcome )
              } else
              {
                outcomesWithNoAdministration = c( outcomesWithNoAdministration, outcome )

              }
            }

            # ===============================================
            # time matrix
            # ===============================================

            # parameters matrix for ode evaluation
            timeMatrix = list()
            timeDose = list()
            indexTime = list()
            inputsModel = list()

            for ( outcome in outcomesWithAdministration )
            {
              administration = getAdministration( arm, outcome )
              tau = getTau( administration )

              inputsModel$dose[[outcome]] = getDose( administration )
              inputsModel$Tinf[[outcome]] = getTinf( administration )

              if ( tau !=0 )
              {
                maxSamplingTimeOutcome = max( samplingTimesOutcome[[outcome]] )
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

            # ===============================================
            # function: modelODEInfusion evaluation
            # ===============================================

            indexTime = list()
            modelEvaluation = list()

            ## set initial conditions
            for ( variable in variables )
            {
              assign( variable[i], initialConditions[i] )
            }

            modelODEInfusion = function( samplingTimesModel, initialConditions, inputsModel ){

              with( c( samplingTimesModel, initialConditions, inputsModel ),{

                assign("t", samplingTimesModel)

                for ( outcome in outcomesWithAdministration )
                {
                  for ( outcome in outcomesWithAdministration )
                  {
                    dose = inputsModel$dose[[outcome]]
                    Tinf = inputsModel$Tinf[[outcome]]
                    assign( paste0("dose_",outcome), dose )
                    assign( paste0("Tinf_",outcome), Tinf )
                  }

                  timeMatrix = inputsModel$timeMatrix[[outcome]]
                  indexTime[[outcome]] = which( apply( timeMatrix, 1, findInterval, x = samplingTimesModel ) == 1 )

                  if ( length( indexTime[[outcome]]  ) != 0 )
                  {
                    assign( paste0("dose_",outcome), dose[indexTime[[outcome]]] )
                    assign( paste0("Tinf_",outcome), Tinf[indexTime[[outcome]]] )

                    for ( iter in 1:numberOfVariables )
                    {
                      modelEvaluation[[iter]] = eval( modelEquations$duringInfusion[[iter]] )
                    }
                  }
                  else
                  {
                    for ( iter in 1:numberOfVariables )
                    {
                      modelEvaluation[[iter]] = eval( modelEquations$afterInfusion[[iter]] )
                    }
                  }
                }
                return( list( c( modelEvaluation ) ) )
              })
            }

            # ===============================================
            # evaluate the model
            # ===============================================

            # parameters atol and rtol for ode solver
            odeSolverParameters = getOdeSolverParameters( object )

            atol = odeSolverParameters$atol
            rtol = odeSolverParameters$rtol

            out = ode( initialConditions,
                       samplingTimesModel,
                       modelODEInfusion,
                       inputsModel,
                       atol = atol, rtol = rtol,
                       method = "lsoda")

            # ===============================================
            # model outcomes evaluation
            # ===============================================

            indexSamplingTimes = list()
            samplingTimesOutcomes = list()
            evaluationOutcomes = list()

            evaluationOutcomes = list()

            for ( variable in variables )
            {
              assign( variable, out[, variable] )
            }

            for ( outcome in outcomes )
            {
              # scale the model outcomes
              evaluationOutcomes[[outcome]] = eval( parse( text = modelOutcomes[[outcome]] ) )

              indexSamplingTimes = match( samplingTimesOutcome[[outcome]] , samplingTimesModel )

              evaluationOutcomes[[outcome]] = as.data.frame( cbind( samplingTimesModel[indexSamplingTimes], evaluationOutcomes[[outcome]][indexSamplingTimes] ) )

              names( evaluationOutcomes[[outcome]] ) = c( "time", outcome )
            }

            # ===============================================
            # compute sensitivity indices
            # ===============================================

            # model parameters
            parameters = getParameters( object )

            # parameters for computing gradients
            parametersGradient = parametersForComputingGradient( object )
            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            # parameters atol and rtol for ode solver
            odeSolverParameters = getOdeSolverParameters( object )
            atol = odeSolverParameters$atol
            rtol = odeSolverParameters$rtol

            outcomesGradient = list()

            for( outcome in outcomes )
            {
              resultsGrad = list()

              for ( iterShifted in 1:dim( shiftedParameters)[2] )
              {
                valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

                # assign parameter values
                for( iterParameter in 1:numberOfParameters )
                {
                  parameterMu = valuesParameters[iterParameter]
                  parameterName = getName( parameters[[iterParameter]] )
                  assign( parameterName, parameterMu )
                }

                out = ode( initialConditions,
                           samplingTimesModel,
                           modelODEInfusion,
                           inputsModel,
                           atol = atol, rtol = rtol,
                           method = "lsoda" )

                # response evaluation
                for ( variable in variables )
                {
                  assign( variable, out[, variable ])
                }

                resultsGrad[[iterShifted]] = eval( parse( text = modelOutcomes[[outcome]] ) )
              }

              resultsGrad = do.call( cbind, resultsGrad )

              coefs = list()

              for ( i in 1 :dim( resultsGrad)[1] )
              {
                coefs[[i]] = solve( do.call("cbind", Xcols), resultsGrad[i,])/frac
                coefs[[i]] = coefs[[i]][1 + seq_along(parameters)]
              }

              # match sampling times responses with sampling time model
              indexSamplingTimes = match( samplingTimesOutcome[[outcome]] , samplingTimesModel )

              outcomesGradient[[outcome]] = do.call(rbind,coefs)

              # case if one parameter
              if( dim( outcomesGradient[[outcome]] )[1]==1 )
              {
                outcomesGradient[[outcome]] = outcomesGradient[[outcome]][indexSamplingTimes]
              }else{
                # case more than one parameter
                outcomesGradient[[outcome]] = outcomesGradient[[outcome]][indexSamplingTimes,]
              }

              outcomesGradient[[outcome]] = as.data.frame( cbind( samplingTimesModel[indexSamplingTimes], outcomesGradient[[outcome]] ) )
              colnames( outcomesGradient[[outcome]] ) = c( "time", modelParametersNames )
            }

            names( outcomesGradient ) = outcomes

            # -----------------------------------------------
            # outcomesAllGradient
            # select with model error
            # -----------------------------------------------

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
            rownames( outcomesAllGradient ) = NULL

            return( list( evaluationOutcomes = evaluationOutcomes,
                          outcomesGradient = outcomesGradient,
                          outcomesAllGradient = outcomesAllGradient ) )
          })

# ======================================================================================================
# definePKModel
# ======================================================================================================

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

