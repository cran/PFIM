#' Class "ModelAnalytic"
#'
#' @description The class \code{Model} defines information concerning the construction of an analytical model.
#' The class \code{ModelAnalytic} inherits from the class \code{Model}.
#'
#' @name ModelAnalytic-class
#' @aliases ModelAnalytic
#' @include Model.R
#' @include ModelODE.R
#' @export

ModelAnalytic = setClass("ModelAnalytic",
                         contains = "Model",
                         prototype = prototype(
                           initialConditions = list(NULL),
                           odeSolverParameters = list(NULL)))

setMethod( f="initialize",
           signature="ModelAnalytic",
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
# EvaluateModel
# ======================================================================================================

#' @rdname EvaluateModel
#' @export

setMethod(f = "EvaluateModel",
          signature = "ModelAnalytic",
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

            modelEquations = list()
            modelEquations = getEquations( object )
            equationNames = names( modelEquations )

            modelEquations = lapply( modelEquations, function(x) parse( text = x ) )

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

            samplingTimesModel = sort( unique( c( 0, unlist( samplingTimesOutcome ) ) ) )
            colnames( samplingTimesModel ) = NULL

            # ===============================================
            # Administration
            # ===============================================

            outcomesWithAdministration = c()
            outcomesWithNoAdministration = c()

            for ( outcome in outcomes )
            {
              administrationsTmp = getAdministration( arm, outcome )

              if ( length( administrationsTmp ) !=0 )
              {
                outcomesWithAdministration = c( outcomesWithAdministration, outcome )
              }else{
                outcomesWithNoAdministration = c( outcomesWithNoAdministration, outcome )
              }
            }

            # ===============================================
            # time matrix for outcomesWithAdministration
            # ===============================================

            inputsModel = list()
            timeMatrixEvaluation = list()

            # ===============================================
            # parameters matrix for evaluation
            # ===============================================

            for ( outcome in outcomesWithAdministration )
            {
              # ==================
              # max samplingTimes
              # ==================

              maxSamplingTimeOutcome = max( getSamplings( getSamplingTime( arm, outcome ) ) )

              # ========================
              # time dose, dose and tau
              # ========================

              administration = getAdministration( arm, outcome )
              inputsModel[[outcome]]$timeDose = getTimeDose( administration )
              inputsModel[[outcome]]$timeDose = inputsModel[[outcome]]$timeDose

              dose = getDose( administration )
              tau = getTau( administration )

              # ========================
              # for repeated doses
              # ========================

              if ( tau !=0 )
              {
                n = maxSamplingTimeOutcome%/%tau
                inputsModel[[outcome]]$dose = rep( dose, n+1 )
                inputsModel[[outcome]]$timeDose = sort( unique( seq( 0, maxSamplingTimeOutcome, tau ) ) )

              }else{

                # ========================
                # for multi doses
                # ========================

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

              timeMatrixEvaluation[[outcome]] = data.frame( timeMatrixEvaluationTmp, indicesDoses, outcome  )
            }

            timeMatrixEvaluation = do.call( rbind, timeMatrixEvaluation )
            timeMatrixEvaluation = timeMatrixEvaluation[order(timeMatrixEvaluation[,1],decreasing=FALSE),]
            #remove NA values
            timeMatrixEvaluation = timeMatrixEvaluation[complete.cases(timeMatrixEvaluation), ]

            # =========================================================
            # function: evaluation outcomesWithAdministration
            # =========================================================

            evaluationOutcomes = function( inputsModel, samplingTimesModel, outcomesWithAdministration, outcomesWithNoAdministration,
                                           modelEquations, timeMatrixEvaluation )
            {
              evaluationOutcomes  = matrix( 0.0, length( samplingTimesModel ), length( outcomesWithAdministration ) )
              indicesDoses = c()

              for ( outcome in outcomesWithAdministration )
              {
                timeDose = inputsModel[[outcome]]$timeDose
                dose = inputsModel[[outcome]]$dose

                for ( j in 1:dim(timeMatrixEvaluation)[1]  )
                {
                  # ========================
                  # outcomeName
                  # ========================

                  outcomeName = paste0( "dose_", timeMatrixEvaluation$outcome[j] )

                  # ========================
                  # doses and time doses
                  # ========================

                  indicesDoses = timeMatrixEvaluation$indicesDoses[j]
                  doses = dose[1:indicesDoses]
                  time = sort( unique( unlist( c( timeMatrixEvaluation[j,1:indicesDoses] ) ) ), decreasing = TRUE )

                  # ========================
                  # model evaluation
                  # ========================

                  for ( iter in 1:length( outcomesWithAdministration ) )
                  {
                    evaluationOutcomes[j,iter] = 0

                    for ( i in 1:indicesDoses )
                    {
                      assign( "t", time[i] )
                      assign( outcomeName, doses[i] )
                      evaluationOutcomes[j,iter]  = evaluationOutcomes[j,iter] + eval( modelEquations[[iter]] )
                    }
                  }
                }
              }

              evaluationOutcomes = as.data.frame( evaluationOutcomes )
              colnames ( evaluationOutcomes ) = outcomesWithAdministration

              # =========================================================
              # function: evaluation outcomesWithNoAdministration
              # =========================================================

              evaluationOutcomesWithNoAdministration = list()

              if( length( outcomesWithNoAdministration ) != 0 )
              {
                for( outcomeWithAdministration in outcomesWithAdministration )
                {
                  assign( outcomeWithAdministration, evaluationOutcomes[, outcomeWithAdministration])
                }

                for( outcomeWithNoAdministration in outcomesWithNoAdministration )
                {
                  evaluationOutcomesWithNoAdministration[[outcomeWithNoAdministration]] = eval( modelEquations[[ outcomeWithNoAdministration ]] )
                }

                modelEvaluation = as.data.frame( do.call( "cbind", list( samplingTimesModel, evaluationOutcomes,
                                                                         evaluationOutcomesWithNoAdministration ) ) )
                colnames( modelEvaluation ) = c( "time", outcomes )

              }else if( length( outcomesWithNoAdministration ) == 0 )
              {
                modelEvaluation = as.data.frame( do.call( "cbind", list( samplingTimesModel, evaluationOutcomes ) ) )
                colnames( modelEvaluation ) = c( "time", outcomesWithAdministration )
              }

              return( list( modelEvaluation = modelEvaluation ) )

            } # end function evaluationOutcomes

            # =============================================================
            # function: scale the model outcomes & take the sampling times
            # =============================================================

            scaleModelResponse = function( modelEvaluation, outcomes, samplingTimesOutcome, samplingTimesModel )
            {
              evaluationOutcomes = list()

              for ( outcome in outcomes )
              {
                indexSamplingTimesOutcome = match( samplingTimesOutcome[[outcome]] , samplingTimesModel )

                assign( outcome, modelEvaluation$modelEvaluation[, outcome])

                modelEvaluation$modelEvaluation[,outcome ] = eval( parse( text = outcomesModel[[ outcome ]]))

                evaluationOutcomes[[ outcome ]] =  as.data.frame( cbind( samplingTimesModel[indexSamplingTimesOutcome],
                                                                         modelEvaluation$modelEvaluation[indexSamplingTimesOutcome, outcome ] ) )

                colnames( evaluationOutcomes[[ outcome ]] ) = c( "time", outcome )
              }
              return( evaluationOutcomes )
            }

            # ===============================================
            # model evaluation
            # ===============================================

            evaluationOutcomesWithoutOutputsScaling = evaluationOutcomes( inputsModel, samplingTimesModel,
                                                                          outcomesWithAdministration, outcomesWithNoAdministration,
                                                                          modelEquations, timeMatrixEvaluation )

            evaluationOutcomesWithOutputsScaling = scaleModelResponse( evaluationOutcomesWithoutOutputsScaling, outcomes,
                                                                       samplingTimesOutcome, samplingTimesModel  )

            # =================================================
            # substitute for outcomes evaluation with scaling
            # =================================================

            subsituteTmp = list()
            modelEquationsTmp = getEquations( object )

            for( outcome in outcomes )
            {
              modelEquationsTmp[[outcome]] = paste0( "(", modelEquationsTmp[[outcome]], ")" )
              subsituteTmp[[outcome]] = parse( text = gsub( outcome, modelEquationsTmp[[outcome]], outcomesModel[[outcome]] ) )
            }

            modelEquations = subsituteTmp
            names( modelEquations ) = names( modelEquationsTmp )

            # ===============================================
            # compute sensitivity indices
            # ===============================================

            # ========================
            # model parameters
            # ========================

            parameters = getParameters( object )
            numberOfParameters = getNumberOfParameters( object )

            # ==================================
            # parameters for computing gradients
            # ==================================

            parametersGradient = parametersForComputingGradient( object )
            shiftedParameters = parametersGradient$shifted
            Xcols = parametersGradient$Xcols
            frac = parametersGradient$frac

            outcomesGradient = list()
            resultsGrad = list()

            for ( iterShifted in 1:dim( shiftedParameters)[2] )
            {
              valuesParameters = shiftedParameters[1:numberOfParameters,iterShifted]

              # ==================================
              # assign parameter values
              # ==================================

              for( iterParameter in 1:numberOfParameters )
              {
                parameterMu = valuesParameters[iterParameter]
                parameterName = getName( parameters[[iterParameter]] )
                assign( parameterName, parameterMu )
              }

              out = evaluationOutcomes( inputsModel, samplingTimesModel,
                                        outcomesWithAdministration, outcomesWithNoAdministration, modelEquations, timeMatrixEvaluation )

              resultsGrad[[iterShifted]] = out$modelEvaluation
            }

            for ( outcome in outcomes )
            {
              tmp = lapply(resultsGrad, "[", outcome )
              tmp = do.call( cbind,tmp )

              coefs = list()

              for( i in 1 : length( samplingTimesModel ) )
              {
                coefs[[i]]  = solve( do.call("cbind", Xcols),tmp[i,])/frac
                coefs[[i]] = coefs[[i]][1 + seq_along( parameters )]
              }

              outcomesGradient[[outcome]] = data.frame( samplingTimesModel, do.call("rbind",coefs) )

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

# ======================================================================================================
# definePKModel
# ======================================================================================================

#' @rdname definePKModel
#' @export

setMethod("definePKModel",
          signature("ModelAnalytic"),
          definition = function( object, outcomes )
          {
            # ====================================================
            # outcomes from the library of models and new outcomes
            # ====================================================

            originalOutcomes = getOutcomes( object )
            newOutcomes = outcomes

            # ==========================
            # with newOutcomes
            # ==========================

            if ( length( newOutcomes ) != 0 )
            {
              # ==========================
              # change equation names
              # ==========================

              equations = getEquations( object )
              names( equations ) = newOutcomes

              # ==========================
              # response names old and new
              # ==========================

              responseNames = unlist( originalOutcomes )
              responseNewNames = unlist( newOutcomes )

              # ==========================
              # new doses names
              # ==========================

              doseNewName = paste0( "dose_", responseNewNames )

              for ( equationName in names( equations ) )
              {
                # ==========================
                # change variable names
                # ==========================

                for( iterResponseName in 1:length( responseNames ) )
                {
                  equations[[equationName]] = gsub( responseNames[iterResponseName],
                                                    responseNewNames[iterResponseName], equations[[equationName]] )
                }

                # ==========================
                # change dose names
                # ==========================

                for( iterDoseNewName in 1:length( doseNewName ) )
                {
                  equations[[equationName]] = gsub( "dose", doseNewName[iterDoseNewName], equations[[equationName]] )
                }
              }

              # ==========================
              # set equations and outcomes
              # ==========================

              object = setOutcomes( object, newOutcomes )
              object = setEquations( object, equations )

            }else{

              # ==========================
              # change only dose name
              # ==========================

              equations = getEquations( object )
              responseNames = unlist( originalOutcomes )
              doseName = paste0( "dose_", responseNames )

              for ( equationName in names( equations ) )
              {
                # ==========================
                # change dose names
                # ==========================

                for( iterDoseName in 1:length( doseName ) )
                {
                  equations[[equationName]] = gsub( "dose", doseName[iterDoseName], equations[[equationName]] )
                }
              }

              # ==========================
              # set equation and outcome
              # ==========================

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
          signature("ModelAnalytic","ModelAnalytic"),
          function( PKModel, PDModel, outcomes )
          {
            model = ModelAnalytic()

            if ( length( outcomes ) != 0 )
            {
              # ==========================
              # original and new outcomes
              # ==========================

              newOutcomes = outcomes

              outcomesPK = getOutcomes( PKModel )
              outcomesPD = getOutcomes( PDModel )
              originalOutcomes = c( outcomesPK, outcomesPD )

              # ==========================
              # set the equations
              # ==========================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )

              equations = c( PKModelEquations, PDModelEquations )

              model = setEquations( model, equations )

              # ==========================
              # change equation names
              # ==========================

              names( equations ) = newOutcomes

              # ==========================
              # response names old and new
              # ==========================

              responsesNames = unlist( originalOutcomes )
              responsesNewNames = unlist( newOutcomes )

              # ==========================
              # new doses names
              # ==========================

              doseNewName = paste0( "dose_", responsesNewNames )

              for ( iterEquation in 1:length( equations ) )
              {
                # ==========================
                # change response names
                # ==========================

                for( iterResponseName in 1:length( responsesNames ) )
                {
                  equations[[iterEquation]] = gsub( responsesNames[iterResponseName],
                                                    responsesNewNames[iterResponseName], equations[[iterEquation]] )
                }

                # ==========================
                # change dose names
                # ==========================

                equations[[iterEquation]] = gsub( "dose", doseNewName[iterEquation], equations[[iterEquation]] )
              }

              # ==========================
              # set equations and outcomes
              # ==========================

              model = setOutcomes( model, newOutcomes )
              model = setEquations( model, equations )

            }else{

              # ==========================
              # change only dose name
              # ==========================

              PKModelEquations = getEquations( PKModel )
              PDModelEquations = getEquations( PDModel )

              equations = c( PKModelEquations, PDModelEquations )

              dosesName = paste0( "dose_",  names( PKModelEquations ) )

              for ( iterEquation in 1:length( equations ) )
              {
                equations[[iterEquation]] = gsub( "dose", dosesName[iterEquation], equations[[iterEquation]] )
              }

              # ==========================
              # set equation and outcome
              # ==========================

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
          signature("ModelAnalytic","ModelODE"),
          function( PKModel, PDModel, outcomes )
          {
            # ==========================
            # create model
            # ==========================

            model = ModelODEDoseInEquations()

            # ======================================
            # convert PK model analytic to model ODE
            # ======================================

            PKModelEquations = convertPKModelAnalyticToPKModelODE( PKModel )
            PDModelEquations = getEquations( PDModel )

            # ======================================
            # outcomes from the library of models
            # ======================================

            originalOutcomes = list( "RespPK" = "C1", "RespPD" = "E" )

            if ( length( outcomes ) != 0 )
            {
              # ======================================
              # response and variable names
              # ======================================

              variablesNames = unlist( originalOutcomes )
              responsesNames = names( originalOutcomes )

              responsesNewNames = names( outcomes )
              variablesNewNames = unlist( outcomes, use.names = FALSE )

              # ======================================
              # dose new name # PK and PKPD
              # ======================================

              doseNewName = paste0( "dose_", responsesNewNames[1] )

              # ======================================
              # change variable for RespPK to C1
              # ======================================

              PKModelEquations = gsub( "RespPK", "C1", PKModelEquations )
              PDModelEquations = gsub( "RespPK", "C1", PDModelEquations )

              # ======================================
              # change equation names
              # ======================================

              equations = c( PKModelEquations, PDModelEquations )
              names( equations ) = paste0( "Deriv_", variablesNewNames )
              equations = lapply( equations, function(x) parse( text = x ) )

              numberOfEquations = length( equations )

              # ======================================
              # variable substitution
              # ======================================

              variablesNewNames = lapply( variablesNewNames, function(x) parse( text = x ) )

              variableSubstituted = list()

              for( iterVariableName in 1:length( variablesNewNames ) )
              {
                variableSubstituted[[iterVariableName]] = variablesNewNames[[iterVariableName]][[1]]
              }

              names( variableSubstituted ) = variablesNames

              # ======================================
              # dose substitution
              # ======================================

              doseSubstituted = list()
              doseSubstituted[[1]] = parse( text = doseNewName )[[1]]
              names( doseSubstituted ) = "dose"

              # ======================================
              # all substitutions
              # ======================================

              allSubstitutions = c( variableSubstituted, doseSubstituted )

              # ======================================
              # substitution in equations
              # ======================================

              for ( equationName in names( equations ) )
              {
                equations[[equationName]] = do.call( 'substitute',
                                                     list( equations[[equationName]][[1]], allSubstitutions ) )
              }

              # ======================================
              # set the equations
              # ======================================

              equations = lapply( equations, function(x) paste( deparse( x ), collapse=" " ) )
              model = setEquations( model, equations )

              # ======================================
              # set the outcomes
              # ======================================

              model = setOutcomes( model, outcomes )

            }else{

              # ======================================
              # By default: RespPK to C1
              # ======================================

              variablesNewNames =  list("RespPK" = quote(C1))
              responsesNames = names( originalOutcomes )

              # ======================================
              # model equations
              # ======================================

              names( PKModelEquations ) = "Deriv_C1"
              equations = c( PKModelEquations, PDModelEquations )
              equations = lapply( equations, function(x) parse( text = x ) )

              # ======================================
              # variable substitution
              # ======================================

              variableSubstituted = variablesNewNames

              # ======================================
              # dose name
              # ======================================

              doseSubstituted = list()
              doseNames = paste0( "dose_", responsesNames[1] )
              doseSubstituted[[1]] = parse( text = doseNames )[[1]]
              names( doseSubstituted ) = "dose"

              # ======================================
              # all substitutions
              # ======================================

              allSubstitutions = c( variableSubstituted, doseSubstituted )

              for ( equationName in names( equations ) )
              {
                equations[[equationName]] = do.call( 'substitute',
                                                     list( equations[[equationName]][[1]], allSubstitutions ) )
              }

              # ======================================
              # set the equations
              # ======================================

              equations = lapply( equations, function(x) paste( deparse( x ), collapse=" " ) )
              model = setEquations( model, equations )

              # ======================================
              # set the outcomes
              # ======================================

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
          signature("ModelAnalytic"),
          function( object )
          {
            # ======================================
            # name, equations and  outcome
            # ======================================

            modelName = getName( object )
            equations = getEquations( object )
            outcomes = getOutcomes( object )

            output = list()

            # =========================================
            # change analytic equation to ode equation
            # =========================================

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
# END Class "ModelAnalytic"
##########################################################################################################

