#' Class "PGBOAlgorithm"
#'
#' @description
#' The class "PGBOAlgorithm" implements the PGBO algorithm: Population Genetics Based Optimizer,
#' developed by Hervé Le Nagard [1].
#'
#' @references [1] Rebecca Bauer, France Mentré, Halima Kaddouri, Jacques Le Bras, Hervé Le Nagard,
#' Benefits of a new Metropolis-Hasting based algorithm, in non-linear regression for estimation of ex vivo antimalarial sensitivity in patients infected with two strains,
#' Computers in Biology and Medicine, Volume 55, 2014, Pages 16-25, ISSN 0010-4825
#'
#' @name PGBOAlgorithm-class
#' @aliases PGBOAlgorithm
#'
#' @docType class
#' @include OptimizationAlgorithm.R
#' @include Design.R
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the Class \code{PGBOAlgorithm}:
#' Objects form the Class \code{PGBOAlgorithm} can be created by calls of the form \code{PGBOAlgorithm(...)} where
#' (...) are the parameters for the \code{PGBOAlgorithm} objects.

#'@section Slots for \code{PGBOAlgorithm} objects:
#'  \describe{
#'    \item{\code{N}:}{A numeric giving the population size.}
#'    \item{\code{muteEffect}:}{A numeric giving the mutation effect.}
#'    \item{\code{maxIteration}:}{A numeric giving the maximum number of iterations.}
#'    \item{\code{seed}:}{A numeric giving the seed.}
#'    \item{\code{showProcess}:}{A boolean to show or not the process.}
#'    \item{\code{optimalDesign}:}{A \code{Design} object giving the optimal design.}
#'    \item{\code{iterationAndCriteria}:}{A list giving the optimal criteria at each iteration.}
#'  }

PGBOAlgorithm = setClass(
  Class = "PGBOAlgorithm",
  contains = "OptimizationAlgorithm",
  representation = representation(
    N = "numeric",
    muteEffect = "numeric",
    maxIteration = "numeric",
    purgeIteration = "numeric",
    seed = "numeric",
    showProcess = "logical",
    optimalDesign = "Design",
    iterationAndCriteria = "list"
  ),
  prototype = prototype(
    showProcess = F
  )
)

setMethod( f="initialize",
           signature="PGBOAlgorithm",
           definition= function ( .Object,
                                  N,
                                  muteEffect,
                                  maxIteration,
                                  purgeIteration,
                                  seed,
                                  showProcess,
                                  optimalDesign,
                                  iterationAndCriteria )
           {
             # ===================
             # values by default
             # ===================

             .Object@N = 50
             .Object@muteEffect = 0.2
             .Object@maxIteration = 10e4
             .Object@purgeIteration = 1000
             .Object@seed = -1

             if ( !missing( N ) )
             {
               .Object@N = N
             }

             if ( !missing( muteEffect ) )
             {
               .Object@muteEffect = muteEffect
             }
             if ( !missing( maxIteration ) )
             {
               .Object@maxIteration = maxIteration
             }

             if ( !missing( purgeIteration ) )
             {
               .Object@purgeIteration = purgeIteration
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
           }
)

# ======================================================================================================
# setParameters
# ======================================================================================================

#' @rdname setParameters
#' @export

setMethod("setParameters",
          "PGBOAlgorithm",
          function( object, parameters ) {
            object@N = parameters$N
            object@name = "PGBOAlgorithm"
            object@muteEffect = parameters$muteEffect
            object@maxIteration = parameters$maxIteration
            object@purgeIteration = parameters$purgeIteration
            object@showProcess = parameters$showProcess
            return( object )
          })

# ======================================================================================================
# optimize
# ======================================================================================================

#' @rdname optimize
#' @export

setMethod(f = "optimize",
          signature = "PGBOAlgorithm",
          definition = function( object, optimizationObject )
          {
            if( object@seed != -1 )
            {
              set.seed( object@seed )
            }

            results = list()

            # ==============================
            # designs
            # ==============================

            designs = getDesigns( optimizationObject )
            design = designs[[1]]

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

            # ==========================================
            # set size for checking the constraints
            # ==========================================

            numberOfArms = length( arms )
            numberOutcomesArmsInConstraints = sum( lengths( outcomes ) )

            # ==============================
            # initialize best design
            # ==============================

            designA = design
            best = design

            # ==============================
            # get pgbo parameters
            # ==============================

            optimizationParameters = getOptimizerParameters( optimizationObject )
            N = optimizationParameters$N
            muteEffect = optimizationParameters$muteEffect
            maxIteration = optimizationParameters$maxIteration
            purgeIteration = optimizationParameters$purgeIteration
            showProcess = optimizationParameters$showProcess

            # ==================================
            # get parameters for the evaluation
            # ==================================

            modelEquations = getModelEquations( optimizationObject )
            modelParameters = getModelParameters( optimizationObject )
            modelError = getModelError( optimizationObject )
            outcomesForEvaluation = getOutcomes( optimizationObject )
            designs = getDesigns( optimizationObject )
            odeSolverParameters = getOdeSolverParameters( optimizationObject )

            # =====================
            # fim
            # =====================

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

            # =============================================================
            # Generate the initial solution
            # =============================================================

            samplingInitialSolution = list()

            for ( arm in arms )
            {
              armName = getName( arm )

              # ===============================
              # Sampling constraints
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

                samplingInitialSolution[[ armName ]][[ outcome ]] = samplingsFromSamplingConstraint[[outcome]]

                samplings = samplingInitialSolution[[ armName ]][[ outcome ]]
                samplingTimes = getSamplingTime( arm, outcome )
                samplingTimes = setSamplings( samplingTimes, samplings )
                arm = setSamplingTime( arm, samplingTimes )
              }
              design = setArm( design, arm )
            }

            evaluationFIM = Evaluation( name = "",
                                        modelEquations = modelEquations,
                                        modelParameters = modelParameters,
                                        modelError = modelError,
                                        outcomes = outcomesForEvaluation,
                                        designs = list( design ),
                                        fim = fimEvaluation,
                                        odeSolverParameters = odeSolverParameters )

            evaluationFIM =  run( evaluationFIM )

            # ==========================================
            # get criteria
            # ==========================================

            designs = getDesigns( evaluationFIM )
            fim = getFim( designs[[1]] )
            Dcriterion = getDcriterion( fim )
            d = 1/Dcriterion

            if ( is.finite(d) == FALSE )
            {
              d = 10e6
            }

            # ==========================================
            # set the PGBO parameters
            # ==========================================

            fitBase = 0.03
            theta = -log10( fitBase ) / d
            fitA = 10**( -theta * d )
            fitBest = fitA

            # ==========================================
            # Run PGBO
            # ==========================================

            arms = getArms( designA )

            samplingTimesArms = list()

            for ( iteration in 1:maxIteration )
            {
              # ==========================================
              # Boolean checking constraints in for while
              # ==========================================

              foundSamplingWithConstraints = FALSE

              while ( foundSamplingWithConstraints == FALSE )
              {
                # ==========================================
                # select arm
                # ==========================================

                indexArm = sample( numberOfArms, 1 )
                arm = arms[[indexArm]]
                armName = getName( arm )

                # ==========================================
                # sampling constraints
                # ==========================================

                samplingTimesConstraints = getSamplingTimesConstraints( arm )

                # ==========================================
                # select outcome
                # get samplings
                # ==========================================

                numberOfOutcome = length( outcomes[[armName]] )
                indexOutcome = sample( numberOfOutcome, 1 )
                outcome = outcomes[[armName]][indexOutcome]

                samplingTimes = getSamplingTimes( arm )
                samplingTimes = getSamplingTime( arm, outcome )
                samplings = getSamplings( samplingTimes )

                # ==========================================
                # sampling time mutation
                # ==========================================

                indexSamplings = sample( numberOfOutcome, 1 )

                if ( runif( 1 ) < 0.8 )
                {
                  samplings[indexSamplings] = samplings[indexSamplings] + rcauchy(1)*muteEffect
                }else{
                  samplings = samplings + rnorm( length( samplings ) )*muteEffect
                }

                samplings = sort( samplings )

                # ==========================================
                # check sampling time constraints
                # ==========================================

                samplingTimesConstraints = getSamplingTimeConstraint( arm, outcome )

                samplingTimeConstraintsForContinuousOptimization =
                  checkSamplingTimeConstraintsForContinuousOptimization( samplingTimesConstraints, arm, samplings, outcome )

                if( all( unlist( samplingTimeConstraintsForContinuousOptimization ) ) == TRUE )
                {
                  # ===========================================
                  # check constraints and set the new sampling
                  # ===========================================

                  samplingTime = getSamplingTime( arm, outcome )
                  samplingTime = setSamplings( samplingTime, samplings )
                  samplingTimesArms[[armName]][[outcome]] = samplingTime
                }

                # ==============================================================================
                # check if all constraints TRUE and number of constraints = nb Arm * nb Response
                # ==============================================================================

                if ( length( unlist( samplingTimesArms ) ) == numberOutcomesArmsInConstraints )
                {
                  foundSamplingWithConstraints = TRUE
                }
              } # end while

              # ==========================================
              # set new sampling times
              # ==========================================

              for ( arm in arms )
              {
                armName = getName( arm )

                for ( outcome in outcomes[[armName]]  )
                {
                  arm = setSamplingTime( arm, samplingTimesArms[[armName]][[outcome]] )
                }
                designA = setArm( designA, arm )
              }

              # ==========================================
              # Evaluation
              # ==========================================

              designB = designA

              evaluationFIM = Evaluation( name = "",
                                          modelEquations = modelEquations,
                                          modelParameters = modelParameters,
                                          modelError = modelError,
                                          outcomes = outcomesForEvaluation,
                                          designs = list( designB ),
                                          fim = fimEvaluation,
                                          odeSolverParameters = odeSolverParameters )

              evaluationFIM =  run( evaluationFIM )

              # ==========================================
              # get criteria
              # ==========================================

              designs = getDesigns( evaluationFIM )

              fim = getFim( designs[[1]] )

              d = 1/getDcriterion( fim )

              if ( is.finite(d) == FALSE )
              {
                d = 10e6
              }

              fitB = 10**( -theta * d )

              # ==========================================
              # Update
              # ==========================================

              if ( !is.nan(fitB) && fitB > 0 )
              {
                if ( fitA == fitB )
                {
                  proba = 1/N
                }
                else
                {
                  f = fitA/fitB
                  proba = 1 - f**2
                  proba = proba / (1 - f**( 2*N ) )

                  if ( is.nan(proba) )
                  {
                    proba = 0
                  }
                }

                if (runif(1) < proba)
                {
                  fitA = fitB
                  designA = designB

                  if ( fitBest < 1/d )
                  {
                    fitBest = 1/d
                    best = designA

                    if ( showProcess == TRUE )
                    {
                      # ==========================================
                      # iteration and Dcriteria
                      # ==========================================

                      print( paste0('Iteration = ',iteration))
                      print( paste0('Criterion = ',1/d))
                    }
                  }
                }
              }

              # ==========================================
              # Purge
              # ==========================================

              if ( iteration%%purgeIteration == 0 )
              {
                d =  - log10( fitA ) / theta
                theta = -log10( fitBase ) / d
                fitA = fitBase
              }

              results[[iteration]] = c( iteration , 1/d )

            }# end iteration

            # ==========================================
            # Outputs
            # ==========================================

            # ==========================================
            # set the fim for the optimal design
            # ==========================================

            evaluationFIM = Evaluation( name = "",
                                        modelEquations = modelEquations,
                                        modelParameters = modelParameters,
                                        modelError = modelError,
                                        outcomes = outcomesForEvaluation,
                                        designs = list( best ),
                                        fim = fimEvaluation,
                                        odeSolverParameters = odeSolverParameters )

            evaluationFIM =  run( evaluationFIM )

            designs = getDesigns( evaluationFIM )
            fim = getFim( designs[[1]] )

            best = setFim( best, fim )

            # ==========================================
            # Iteration & Criteria
            # ==========================================

            results = as.data.frame( do.call( rbind, results ) )
            colnames( results ) = c("Iteration","Criterion")

            object = setIterationAndCriteria( object, results )

            # ==========================================
            # set optimal design
            # ==========================================

            object = setOptimalDesign( object, best )

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
          signature = "PGBOAlgorithm",
          definition = function( object )
          {
            cat("\n\n")
            cat( " ************************************************* ")
            cat("\n")
            cat( " Criterion ")
            cat("\n")
            cat( " ************************************************* ")

            cat("\n")

            iterationAndCriteria = getIterationAndCriteria( object )

            rownames( iterationAndCriteria )=NULL

            print( iterationAndCriteria )
          })

# ======================================================================================================
# generateReportOptimization
# ======================================================================================================

#' @rdname generateReportOptimization
#' @export

setMethod( "generateReportOptimization",
           signature = "PGBOAlgorithm",
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

             # ========================================
             # markdown template
             # ========================================

             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "template_PGBOAlgorithm.rmd" )

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
# END Class PGBOAlgorithm
##############################################################################

