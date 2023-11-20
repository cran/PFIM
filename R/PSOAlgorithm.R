#' Class "PSOAlgorithm"
#'
#' @description
#' The class "PSOAlgorithm" implements the PSO algorithm.
#'
#' @name PSOAlgorithm-class
#' @aliases PSOAlgorithm
#' @docType class
#' @include OptimizationAlgorithm.R
#' @include Design.R
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class \code{PSOAlgorithm}:
#' Objects form the class \code{PSOAlgorithm} can be created by calls of the form \code{PSOAlgorithm(...)} where
#' (...) are the parameters for the \code{PSOAlgorithm} objects.

#'@section Slots for \code{PSOAlgorithm} objects:
#'  \describe{
#'    \item{\code{maxIteration}:}{A numeric giving the maximum of iterations.}
#'    \item{\code{populationSize}:}{A numeric giving the population size.}
#'    \item{\code{seed}:}{A numeric giving the seed.}
#'    \item{\code{personalLearningCoefficient}:}{A numeric giving the personal learning coefficient.}
#'    \item{\code{globalLearningCoefficient}:}{A numeric giving the global learning coefficient.}
#'    \item{\code{showProcess}:}{A boolean to show or not the process.}
#'    \item{\code{optimalDesign}:}{A \code{Design} object giving the optimal design.}
#'    \item{\code{iterationAndCriteria}:}{A list giving the optimal criteria at each iteration.}
#'  }

PSOAlgorithm = setClass(
  Class = "PSOAlgorithm",
  contains = "OptimizationAlgorithm",
  representation = representation(
    maxIteration = "numeric",
    populationSize = "numeric",
    seed = "numeric",
    personalLearningCoefficient = "numeric",
    globalLearningCoefficient = "numeric",
    showProcess = "logical",
    optimalDesign = "Design",
    iterationAndCriteria = "list" ),
  prototype = prototype(
    showProcess = F ) )

setMethod( f = "initialize",
           signature = "PSOAlgorithm",
           definition = function ( .Object,
                                   maxIteration,
                                   populationSize,
                                   personalLearningCoefficient,
                                   globalLearningCoefficient,
                                   seed,
                                   showProcess,
                                   optimalDesign,
                                   iterationAndCriteria )
           {
             # ===============================
             # values by default
             # ===============================

             .Object@maxIteration = 200
             .Object@populationSize = 200
             .Object@personalLearningCoefficient = 1.4962
             .Object@globalLearningCoefficient = 1.4962
             .Object@seed = -1

             .Object@showProcess = TRUE

             if ( !missing( maxIteration ) )
             {
               .Object@maxIteration = maxIteration
             }
             if ( !missing( populationSize ) )
             {
               .Object@populationSize = populationSize
             }
             if ( !missing( personalLearningCoefficient ) )
             {
               .Object@personalLearningCoefficient = personalLearningCoefficient
             }
             if ( !missing( globalLearningCoefficient ) )
             {
               .Object@globalLearningCoefficient = globalLearningCoefficient
             }
             if ( !missing( seed ) )
             {
               .Object@seed = seed
             }
             if ( !missing( showProcess ) )
             {
               .Object@showProcess = showProcess
             }
             if ( !missing( optimalDesign ) )
             {
               .Object@optimalDesign = optimalDesign
             }
             if ( !missing( iterationAndCriteria ) )
             {
               .Object@iterationAndCriteria = iterationAndCriteria
             }

             validObject( .Object )
             return ( .Object )
           })

# ======================================================================================================
# setParameters
# ======================================================================================================

#' @rdname setParameters
#' @export
#'
setMethod("setParameters",
          "PSOAlgorithm",
          function( object, parameters ) {
            object@maxIteration = parameters$maxIteration
            object@name = "PSOAlgorithm"
            object@populationSize = parameters$populationSize
            object@personalLearningCoefficient = parameters$personalLearningCoefficient
            object@globalLearningCoefficient = parameters$globalLearningCoefficient
            object@showProcess = parameters$showProcess
            return( object )
          })

# ======================================================================================================
# optimize
# ======================================================================================================

#' @rdname optimize
#' @export
#'
setMethod(f = "optimize",
          signature = "PSOAlgorithm",
          definition = function(  object, optimizationObject  )
          {
            # ===================================================
            # get PSO parameters
            # ===================================================

            optimizationParameters = getOptimizerParameters( optimizationObject )
            populationSize = optimizationParameters$populationSize
            maxIteration = optimizationParameters$maxIteration
            personalLearningCoefficient = optimizationParameters$personalLearningCoefficient
            globalLearningCoefficient = optimizationParameters$globalLearningCoefficient
            showProcess = optimizationParameters$showProcess

            # ===================================================
            # get parameters for the evaluation
            # ===================================================

            modelParameters = getModelParameters( optimizationObject )
            modelEquations = getModelEquations( optimizationObject )
            modelError = getModelError( optimizationObject )
            outcomesForEvaluation = getOutcomes( optimizationObject )
            designs = getDesigns( optimizationObject )
            odeSolverParameters = getOdeSolverParameters( optimizationObject )

            # ===================================================
            # get design
            # ===================================================

            design = designs[[1]]
            optimalDesign = design

            # ===================================================
            # fim
            # ===================================================

            fimOptimization = getFim( optimizationObject )

            if ( is( fimOptimization, "PopulationFim" ) )
            {
              fimEvaluation = "population"
            }
            else if ( is( fimOptimization, "IndividualFim" ) )
            {
              fimEvaluation = "individual"
            }
            else if ( is( fimOptimization, "BayesianFim" ) )
            {
              fimEvaluation = "Bayesian"
            }

            # ==============================================
            # check validity of the samplingTimesConstraints
            # ==============================================

            checkValiditySamplingConstraint( design )

            # ===============================================
            # in case of partial sampling constraints
            # set the new arms with the sampling constraints
            # ===============================================

            design = setSamplingConstraintForOptimization( design )
            arms = getArms( design )

            # ===============================================
            # get arm outcomes
            # ===============================================

            outcomes = list()

            for ( arm in arms )
            {
              armName = getName( arm )
              outcomes[[armName]] = unlist( lapply( getSamplingTimesConstraints( arm ), function( x ) getOutcome( x ) ) )
            }

            # ===============================================
            # get arm outcomes
            # ===================================================
            # generate the initial population for the PSO
            # ===================================================

            cost = list()
            bestCost = rep(Inf, populationSize)
            globalBestCost = Inf

            globalBestPosition = list()
            bestPosition = list()
            velocity = list()
            position = list()
            results = data.frame()

            for ( iterPop in 1:populationSize )
            {
              # ===============================
              # generate initial population
              # ===============================

              for ( arm in arms )
              {
                armName = getName( arm )

                # ===============================
                # sampling constraints
                # ===============================

                samplingTimesArms = list()

                samplingTimesConstraints = getSamplingTimesConstraints( arm )

                for ( samplingTimesConstraint in samplingTimesConstraints )
                {
                  samplingsFromSamplingConstraint = generateSamplingsFromSamplingConstraints( samplingTimesConstraint )

                  outcome = names( samplingsFromSamplingConstraint )

                  # ======================================
                  # set initial position
                  # ======================================

                  position[[ armName ]][[ outcome ]][[ iterPop ]] = samplingsFromSamplingConstraint[[outcome]]

                  # ======================================
                  # Set the best position
                  # ======================================

                  bestPosition[[ armName ]][[ outcome ]][[ iterPop ]] = position[[ armName ]][[ outcome ]][[ iterPop ]]

                  # ======================================
                  # Set initial velocity
                  # ======================================

                  velocity[[ armName ]][[ outcome ]][[ iterPop ]] = 0.0

                  # ======================================
                  # Set sampling times
                  # ======================================

                  samplings = position[[ armName ]][[ outcome ]][[ iterPop ]]
                  samplingTimes = getSamplingTime( arm, outcome )
                  samplingTimes = setSamplings( samplingTimes, samplings )
                  arm = setSamplingTime( arm, samplingTimes )
                }
                design = setArm( design, arm)
              }

              # ===================================================
              # Evaluation
              # ===================================================

              # ===================================================
              # set arms
              # ===================================================

              evaluationFIM = Evaluation( name = "",
                                          modelEquations = modelEquations,
                                          modelParameters = modelParameters,
                                          modelError = modelError,
                                          outcomes = outcomesForEvaluation,
                                          designs = list( design ),
                                          fim = fimEvaluation,
                                          odeSolverParameters = odeSolverParameters )

              evaluationFIM =  run( evaluationFIM )

              # ===================================================
              # get criteria
              # ===================================================

              designs = getDesigns( evaluationFIM )
              fim = getFim( designs[[1]] )
              Dcriterion = getDcriterion( fim )
              cost[[ iterPop ]] = 1/getDcriterion( fim )

              # ===================================================
              # update Personal Best
              # ===================================================

              # ===================================================
              # update bestPosition
              # ===================================================

              for ( arm in arms )
              {
                armName = getName( arm )

                for ( outcome in outcomes[[armName]] )
                {
                  bestPosition[[ armName ]][[ outcome ]][[ iterPop ]] = position[[ armName ]][[ outcome ]][[ iterPop ]]
                }
              }

              # ===================================================
              # update bestCost
              # ===================================================

              bestCost[[ iterPop ]] = cost[[ iterPop ]]

              indexMinBestCost = which.min( unlist( bestCost ) )

              # ===================================================
              # update Global Best
              # ===================================================

              if ( bestCost[[ indexMinBestCost ]] < globalBestCost )
              {
                for ( arm in arms )
                {
                  armName = getName( arm )

                  for ( outcome in outcomes[[armName]] )
                  {
                    globalBestPosition[[ armName ]][[ outcome ]] = bestPosition[[ armName ]][[ outcome ]][[ indexMinBestCost ]]
                  }
                }
                globalBestCost = bestCost[[ indexMinBestCost ]]
              }
            } # end iterPop

            # ===================================================
            # Run the PSO
            # ===================================================

            for ( iteration in 1:maxIteration )
            {
              # ===================================================
              # show process
              # ===================================================

              if ( showProcess == TRUE )
              {
                print( paste0( "iter = ", iteration ) )
              }

              for ( iterPop in 1:populationSize )
              {
                # ===================================================
                # update Velocity
                # ===================================================

                # ===================================================
                # constrictionFactor
                # ===================================================

                phi1 = personalLearningCoefficient
                phi2 = globalLearningCoefficient
                phi = phi1 + phi2
                kappa = 1
                constrictionFactor = 2*kappa / abs( 2 - phi - sqrt( phi * ( phi - 4 ) ) )

                for ( arm in arms )
                {
                  armName = getName( arm )

                  for ( outcome in outcomes[[armName]] )
                  {
                    n = length( globalBestPosition[[ armName ]][[ outcome ]] )

                    velocity[[ armName ]][[ outcome ]][[ iterPop ]] =
                      constrictionFactor *
                      ( velocity[[ armName ]][[ outcome ]][[ iterPop ]] +
                          phi1 * runif(1,0,1) * ( bestPosition[[ armName ]][[ outcome ]][[ iterPop ]] - position[[ armName ]][[ outcome ]][[ iterPop ]] ) +
                          phi2 * runif(1,0,1) * ( globalBestPosition[[ armName ]][[ outcome ]] - position[[ armName ]][[ outcome ]][[ iterPop ]] ) )
                  }
                }

                # ===================================================
                # update Position
                # ===================================================

                for ( arm in arms )
                {
                  armName = getName( arm )

                  for ( outcome in outcomes[[armName]] )
                  {
                    position[[ armName ]][[ outcome ]][[ iterPop ]] =
                      position[[ armName ]][[ outcome ]][[ iterPop ]] + velocity[[ armName ]][[ outcome ]][[ iterPop ]]

                    position[[ armName ]][[ outcome ]][[ iterPop ]] =
                      sort( position[[ armName ]][[ outcome ]][[ iterPop ]] )
                  }
                }

                # ===================================================
                # apply position limits
                # ===================================================

                for ( arm in arms )
                {
                  armName = getName( arm )

                  for ( outcome in outcomes[[armName]] )
                  {
                    samplingConstraints = getSamplingTimeConstraint( arm, outcome )

                    samplingsWindows = getSamplingsWindows( samplingConstraints )

                    positionLength = length( position[[ armName ]][[ outcome ]][[ iterPop ]] )

                    for( j in 1:positionLength )
                    {
                      distance = list()

                      k = 1
                      for ( samplingsWindow in samplingsWindows )
                      {
                        tmp = lapply( samplingsWindow, function(x) abs( x-position[[ armName ]][[ outcome ]][[ iterPop ]][ j ] ) )
                        distance[[k]] = unlist( tmp )
                        k=k+1
                      }

                      indDistanceMin = which.min( lapply( distance, function(x) min(x) ) )

                      maxSamplings = max( samplingsWindows[[indDistanceMin]])
                      minSamplings = min( samplingsWindows[[indDistanceMin]])

                      position[[ armName ]][[ outcome ]][[ iterPop ]][ j ] = min( maxSamplings, position[[ armName ]][[ outcome ]][[ iterPop ]][ j ] )
                      position[[ armName ]][[ outcome ]][[ iterPop ]][ j ] = max( minSamplings, position[[ armName ]][[ outcome ]][[ iterPop ]][ j ] )
                    }
                    position[[ armName ]][[ outcome ]][[ iterPop ]] = sort(  position[[ armName ]][[ outcome ]][[ iterPop ]] )
                  }
                }


                # ===================================================
                # constraints
                # ===================================================

                samplingTimeConstraintsForContinuousOptimization = list()

                for ( arm in arms )
                {
                  armName = getName( arm )

                  samplingTimesConstraints = getSamplingTimesConstraints( arm )

                  # =======================================================
                  # check the constraints on the samplings times
                  # =======================================================

                  for ( outcome in outcomes[[armName]] )
                  {
                    newSamplings = position[[ armName ]][[ outcome ]][[ iterPop ]]

                    samplingTimesConstraints = getSamplingTimeConstraint( arm, outcome )

                    samplingTimeConstraintsForContinuousOptimization[[ armName ]][[ outcome ]] =
                      checkSamplingTimeConstraintsForContinuousOptimization( samplingTimesConstraints, arm, newSamplings, outcome )

                  }
                }

                samplingTimeConstraintsForContinuousOptimization = unlist( samplingTimeConstraintsForContinuousOptimization )

                if( all( samplingTimeConstraintsForContinuousOptimization ) == TRUE )
                {
                  # ===================================================
                  # evaluation
                  # ===================================================

                  # ===================================================
                  # set new sampling times
                  # ===================================================

                  for ( arm in arms )
                  {
                    armName = getName( arm )

                    for ( outcome in outcomes[[armName]] )
                    {
                      samplings = position[[ armName ]][[ outcome ]][[ iterPop ]]
                      samplingTimes = getSamplingTime( arm, outcome )
                      samplingTimes = setSamplings( samplingTimes, samplings )
                      arm = setSamplingTime( arm, samplingTimes )
                    }

                    design = setArm( design, arm )
                  }

                  # ===================================================
                  # set and evaluate new design with the constraints
                  # ===================================================

                  evaluationFIM = Evaluation( name = "",
                                              modelEquations = modelEquations,
                                              modelParameters = modelParameters,
                                              modelError = modelError,
                                              outcomes = outcomesForEvaluation,
                                              designs = list( design ),
                                              fim = fimEvaluation,
                                              odeSolverParameters = odeSolverParameters )

                  evaluationFIM =  run( evaluationFIM )


                  # ===================================================
                  # get criteria
                  # ===================================================

                  designs = getDesigns( evaluationFIM )
                  newDesign = designs[[1]]

                  fim = getFim( newDesign )

                  cost[[ iterPop ]] = 1/getDcriterion( fim )

                  if (is.nan( cost[[ iterPop ]]))
                  {
                    cost[[ iterPop ]] = Inf
                  }

                  if (is.nan( bestCost[[ iterPop ]]))
                  {
                    bestCost[[ iterPop ]] = Inf
                  }

                  # ===================================================
                  # Update Personal Best
                  # ===================================================

                  if ( cost[[ iterPop ]] < bestCost[[ iterPop ]] )
                  {
                    for ( arm in arms )
                    {
                      armName = getName( arm )

                      for ( outcome in outcomes[[armName]] )
                      {
                        bestPosition[[ armName ]][[ outcome ]][[ iterPop ]] = position[[ armName ]][[ outcome ]][[ iterPop ]]
                      }
                      bestCost[[ iterPop ]] = cost[[ iterPop ]]
                    }

                    indexMinBestCost = which.min( unlist( bestCost ) )

                    if ( bestCost[[ indexMinBestCost ]] < globalBestCost )
                    {
                      for ( arm in arms )
                      {
                        armName = getName( arm )

                        samplingTimesArms = list()

                        for ( outcome in outcomes[[armName]] )
                        {
                          globalBestPosition[[ armName ]][[ outcome ]] = bestPosition[[ armName ]][[ outcome ]][[ indexMinBestCost ]]
                        }
                      }

                      globalBestCost = bestCost[[ indexMinBestCost ]]

                      # ===================================================
                      # update design
                      # ===================================================

                      optimalDesign = design
                    }
                  }
                }

              } # end iterPop

              results = rbind( c( iteration , globalBestCost ), results )

              if ( showProcess == TRUE )
              {
                print( paste0( "globalBestCost = ", 1/globalBestCost ) )
              }
            } # end maxIteration

            # ===================================================
            # Outputs
            # ===================================================

            # ===================================================
            # set Iteration & Criteria
            # ===================================================

            colnames( results ) = c("Iteration","Criterion")
            results = results[rev(rownames(results)),]
            object = setIterationAndCriteria( object, results )

            # ===================================================
            # set optimal design
            # ===================================================

            object = setOptimalDesign( object, optimalDesign )

            return( object )

          })

# ======================================================================================================
# show
# ======================================================================================================

#' @title show
#' @rdname show
#' @param object object
#' @export

setMethod(f="show",
          signature = "PSOAlgorithm",
          definition = function( object )
          {

            cat( " ************************************************* ")
            cat("\n")
            cat( " Criterion ")
            cat("\n")
            cat( " ************************************************* ")

            cat("\n")

            iterationAndCriteria = getIterationAndCriteria( object )
            rownames(iterationAndCriteria )=NULL

            # ===================================================
            # keep only criterion change
            # ===================================================

            iterationAndCriteriaUnique = iterationAndCriteria[!duplicated(iterationAndCriteria$Criterion),]

            iterationAndCriteriaUnique = rbind(iterationAndCriteriaUnique,
                                               iterationAndCriteria[max(iterationAndCriteria$Iteration),])
            print( iterationAndCriteriaUnique )

          })

# ======================================================================================================
# generateReportOptimization
# ======================================================================================================

#' @rdname generateReportOptimization
#' @export
#'
setMethod( "generateReportOptimization",
           signature = "PSOAlgorithm",
           definition = function( object, optimizationObject, outputPath, outputFile, plotOptions )
           {
             # ===================================================
             # projectName and outputs tables
             # ===================================================

             projectName = getName( optimizationObject )

             evaluationFIMResults = getEvaluationFIMResults( optimizationObject )
             fimType = is( getFim( evaluationFIMResults ) )[1]

             evaluationFIMIntialDesignResults = getEvaluationInitialDesignResults( optimizationObject )

             tablesEvaluationFIMIntialDesignResults = generateTables( evaluationFIMIntialDesignResults, plotOptions )

             tablesOptimizationObject = generateTables( optimizationObject, plotOptions )

             # ===================================================
             # markdown template
             # ===================================================

             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "template_PSOAlgorithm.rmd" )

             rmarkdown::render( input = nameInputFile,
                                output_file = outputFile,
                                output_dir = outputPath,
                                params = list(
                                  object = "object",
                                  plotOptions = "plotOptions",
                                  projectName = "projectName",
                                  fimType = "fimType",
                                  tablesEvaluationFIMIntialDesignResults = "tablesEvaluationFIMIntialDesignResults",
                                  tablesOptimizationObject = "tablesOptimizationObject" ) )

           })

##############################################################################
# END Class PSOAlgorithm
##############################################################################








