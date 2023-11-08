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

ModelODEBolus = setClass(
  Class = "ModelODEBolus",
  contains = "ModelBolus")

# ======================================================================================================
# EvaluateModel
# ======================================================================================================

setMethod(f="EvaluateModel",
          signature="ModelODEBolus",
          definition = function( object, arm )
          {
            # ============================================================
            # model parameters
            # ============================================================

            inputsModel = list()
            parameters = getParameters( object )
            modelParametersNames = lapply( parameters, function(x) getName(x) )
            numberOfParameters = getNumberOfParameters( object )

            # ====================================================
            # assign parameter values
            # ====================================================

            for ( parameter in parameters )
            {
              parameterMu = getMu( parameter )
              parameterName = getName( parameter )
              assign( parameterName, parameterMu )
            }

            # ============================================================
            # outcomes and variables
            # ============================================================

            initialConditions = getInitialConditions( arm )
            modelOutcomes = getOutcomesForEvaluation( object )
            outcomes = names( modelOutcomes )

            variables = names( initialConditions )
            numberOfVariables = length( initialConditions )
            numberOfOutcomes = length( modelOutcomes )

            # ============================================================
            # convert model equations string to expression
            # ============================================================

            modelEquations = list()
            modelEquations = getEquations( object )
            equationNames = names( modelEquations )

            for ( equationName in equationNames )
            {
              modelEquations[[equationName]] = parse( text = modelEquations[[equationName]] )
            }

            # ============================================================
            # sampling times
            # ============================================================

            # =======================================================
            # model sampling times = sampling times of all responses
            # =======================================================

            samplingTimesVariable = list()

            for ( variable in variables )
            {
              samplingTimesVariable[[variable]] = getSamplings( getSamplingTime( arm, variable ) )
            }

            samplingTimesModel = sort( unique( c( 0, unlist( samplingTimesVariable ) ) ) )
            colnames( samplingTimesModel ) = NULL

            # ============================================================
            # Administration
            # ============================================================

            variablesWithAdministration = c()
            variablesWithNoAdministration = c()

            for ( variable in variables )
            {
              administrationsTmp = getAdministration( arm, variable )

              if ( length( administrationsTmp ) !=0 )
              {
                variablesWithAdministration = c( variablesWithAdministration, variable )
              }else{
                variablesWithNoAdministration = c( variablesWithNoAdministration, variable )
              }
            }

            # ============================================================
            # time matrix
            # ============================================================

            # ====================================================
            # parameters matrix for ode evaluation
            # ====================================================

            timeDose = list()
            indexTime = list()
            doseInInitialConditions = list()

            for ( variable in variablesWithAdministration )
            {
              # ====================================================
              # max samplingTimes
              # ====================================================

              maxSamplingTimeVariable = max( getSamplings( getSamplingTime( arm, variable ) ) )

              # ====================================================
              # time dose, dose and tau
              # ====================================================

              administration = getAdministration( arm, variable )
              timeDose[[variable]] = getTimeDose( administration )
              dose = getDose( administration )

              # ====================================================
              # set tau
              # ====================================================

              tau = getTau( administration )

              # ====================================================
              # for repeated doses
              # ====================================================

              if ( tau !=0 )
              {
                timeDose[[variable]] = sort( unique( seq( 0, maxSamplingTimeVariable, tau ) ) )
                inputsModel$dose[[variable]] = rep( dose,length(timeDose[[variable]] ) )
              }else{
                # ====================================================
                # for multi doses
                # ====================================================

                inputsModel$dose[[variable]] = dose
                timeDose[[variable]] = sort( unique( c( timeDose[[variable]] ) ) )
              }
            }

            # ============================================================
            # initial conditions
            # ============================================================

            for ( variable in variables )
            {
              assign( "dose",  inputsModel$dose[[variable]][1])
              initialConditions[[variable]] = eval( parse( text = initialConditions[[variable]] ) )
            }

            initialConditions = unlist( initialConditions )

            # ============================================================
            # Dose event for ode
            # ============================================================

            doseEvent = list()
            dosesBolus = list()

            # ====================================================
            # select doses for bolus
            # ====================================================

            for ( variable in variablesWithAdministration )
            {
              tmp = inputsModel$dose[[variable]][ which( timeDose[[variable]] > 0 ) ]
              dosesBolus[[variable]] = c( initialConditions[[variable]], tmp )
            }

            # ====================================================
            # create doseEvent
            # ====================================================

            for ( variable in variablesWithAdministration )
            {
              doseEvent[[variable]] = data.frame( var = variable,
                                                  time = timeDose[[variable]],
                                                  value = dosesBolus[[variable]],
                                                  method = c("replace") )
            }

            doseEvent = do.call( rbind, doseEvent )
            doseEvent = doseEvent[ order( doseEvent$time ), ]

            # ====================================================
            # for doses at t>0
            # ====================================================

            doseEvent$method[ doseEvent$time > 0 ] = "add"

            # ====================================================
            # ensure that event time in times to avoid warning
            # ====================================================

            uniqueTimes = cleanEventTimes( doseEvent$time, samplingTimesModel )
            samplingTimesModel = sort( unique( c( samplingTimesModel, uniqueTimes ) ) )

            # ===============================================
            # function: evaluation model ODE
            # ===============================================

            modelEvaluation = list()

            modelODE = function( samplingTimesModel, initialConditions, parameters )
            {
              with(as.list(c(initialConditions)),{
                for ( i in 1:numberOfVariables )
                {
                  modelEvaluation[[i]] = eval( modelEquations[[i]] )
                }
                return( list( c( modelEvaluation ) ) )
              })
            }

            # ===============================================
            # model evaluation
            # ===============================================

            # ====================================================
            # parameters atol and rtol for ode solver
            # ====================================================

            odeSolverParameters = getOdeSolverParameters( object )

            atol = odeSolverParameters$atol
            rtol = odeSolverParameters$rtol

            out = ode( initialConditions,
                       samplingTimesModel,
                       modelODE,
                       atol = atol, rtol = rtol,
                       method = "lsoda",
                       events = list( data = doseEvent ) )

            # ===================================================
            # scale evaluation model ODE with outcomes evaluation
            # take the sampling times
            # ===================================================

            evaluationOutcomes = list()

            for ( variable in variables )
            {
              assign( variable, out[,variable] )
            }

            iterVariableName = 1

            for ( outcome in outcomes )
            {
              variable = variables[iterVariableName]

              evaluationOutcomes[[outcome]] = eval( parse( text = modelOutcomes[[outcome]]) )

              indexSamplingTimes = which( samplingTimesModel %in%  samplingTimesVariable[[variable]] )

              evaluationOutcomes[[outcome]] = as.data.frame( cbind( samplingTimesModel[indexSamplingTimes], evaluationOutcomes[[outcome]][indexSamplingTimes] ) )

              colnames( evaluationOutcomes[[outcome]] ) = c( "time", outcomes[iterVariableName] )

              iterVariableName = iterVariableName + 1
            }

            # =================================================
            # substitute for outcomes evaluation with scaling
            # =================================================

            subsituteTmp = list()

            modelEquationsTmp = getEquations( object )
            modelEquationsNames = names( modelEquations )
            modelEquationsTmpNames = str_remove( names( modelEquationsTmp ), "Deriv_" )
            names( modelEquationsTmp ) = modelEquationsTmpNames

            for( outcome in outcomes )
            {
              variableOutcome = modelOutcomes[[outcome]]

              for ( variable in variables )
              {
                modelEquationsTmp[[variable]] = paste0("(", modelEquationsTmp[[variable]],")")
                variableOutcome = gsub( variable, modelEquationsTmp[[variable]], variableOutcome )
              }
              subsituteTmp[[outcome]] = parse( text = variableOutcome )
            }

            # ====================================================
            # rename equations
            # ====================================================

            names( subsituteTmp ) = paste0( "Deriv_", c( variables ) )

            for ( name in  names( subsituteTmp ) )
            {
              modelEquations[[name]] = subsituteTmp[[name]]
            }

            # ===============================================
            # compute gradient
            # ===============================================

            parameters = getParameters( object )

            parametersGradient = parametersForComputingGradient( object )
            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            coefs = list()
            outcomesGradient = list()

            iterTime = 1

            for ( samplingTime in  samplingTimesModel )
            {
              resultsGrad = list()

              assign( "t", samplingTime )

              samplings = c( 0, t )

              for ( iterShifted in 1:dim( shiftedParameters )[2] )
              {
                valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

                # ==============================
                # assign parameter values
                # ==============================

                for( iterParameter in 1:numberOfParameters )
                {
                  parameterMu = valuesParameters[iterParameter]
                  parameterName = getName( parameters[[iterParameter]] )
                  assign( parameterName, parameterMu )
                }

                # ==============================
                # initial conditions
                # ==============================

                initialConditions = getInitialConditions( arm )

                for ( variable in variables )
                {
                  assign( "dose",  inputsModel$dose[[variable]][1] )

                  initialConditions[[variable]] = eval( parse( text = initialConditions[[variable]] ) )
                }

                initialConditions = unlist( initialConditions )

                # ==============================
                # doseEvent
                # ==============================

                for( variableWithAdministration in variablesWithAdministration )
                {
                  doseEvent$value[ ( doseEvent$var == variableWithAdministration ) & ( doseEvent$time == 0 ) ] = initialConditions[variableWithAdministration]
                }

                # ==============================
                # solve ode
                # ==============================

                out = ode( initialConditions,
                           samplings,
                           modelODE,
                           method = "lsodar",
                           atol = atol, rtol = rtol, hmax = 0.0,
                           events = list( data = doseEvent ) )

                resultsGrad[[iterShifted]] = out
              }

              resultsGrad = as.data.frame( do.call( "rbind", resultsGrad ) )

              # ====================================================
              # case samplings = c( 0, t=0 )
              # ====================================================

              if ( t == 0 )
              {
                indexTime = seq( 2, dim(resultsGrad)[1], 2 )
                resultsGrad = resultsGrad[ indexTime,]
              }else{
                resultsGrad = resultsGrad[ resultsGrad$time == max( samplings ),]
              }

              for ( variable in variables )
              {
                tmp = resultsGrad[,variable]
                coefs[[variable]][[iterTime]] = solve( do.call("cbind", Xcols), resultsGrad[,variable])/frac
                coefs[[variable]][[iterTime]] = coefs[[variable]][[iterTime]][1 + seq_along( parameters )]
              }

              iterTime = iterTime + 1
            }

            for( variable in variables )
            {
              indexSamplingTimes = match( samplingTimesVariable[[variable]], samplingTimesModel )

              outcomesGradient[[variable]] = data.frame( samplingTimesModel[indexSamplingTimes], do.call("rbind", coefs[[variable]][indexSamplingTimes] ) )

              colnames( outcomesGradient[[variable]] ) = c( "time", modelParametersNames )
            }

            names( outcomesGradient ) = outcomes

            # ====================================================
            # outcomesAllGradient
            # select with model error
            # ====================================================

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
# definePKPDModel
# ======================================================================================================

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


