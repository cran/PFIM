#' Class "Optimization"
#'
#' @description A class storing information concerning the design optimization.
#'
#' @name Optimization-class
#' @aliases Optimization
#' @docType class
#' @include PFIMProject.R
#' @include Model.R
#' @include Fim.R
#' @include GenericMethods.R
#' @include OptimizationAlgorithm.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Optimization} can be created by calls of the form \code{Optimization(...)} where
#' (...) are the parameters for the \code{Optimization} objects.
#'
#' @section Slots for \code{Administration} objects:
#'  \describe{
#'    \item{\code{name}:}{A character string giving the name of the optimization process.}
#'    \item{\code{model}:}{A object of class \code{Model} giving the model.}
#'    \item{\code{modelEquations}:}{A list giving the model equations.}
#'    \item{\code{modelParameters}:}{A list giving the model parameters.}
#'    \item{\code{modelError}:}{A list giving the model error.}
#'    \item{\code{optimizer}:}{A object of class \code{OptimizationAlgorithm} giving the optimization algorithm.}
#'    \item{\code{optimizerParameters}:}{A list giving the parameters of the optimization algorithm.}
#'    \item{\code{outcomes}:}{A list giving the outcomes of the model.}
#'    \item{\code{designs}:}{A list giving the designs to be optimized.}
#'    \item{\code{fim}:}{A object of class \code{FIM} giving the Fisher information matrix.}
#'    \item{\code{odeSolverParameters}:}{A list giving the parameters for the ode solver.}
#'    \item{\code{optimizationResults}:}{A object of class \code{OptimizationAlgorithm} giving the results of the optimization.}
#'    \item{\code{evaluationFIMResults}:}{A object of class \code{Evaluation} giving the results of the evaluation of the optimal design.}
#'    \item{\code{evaluationInitialDesignResults}:}{A object of class \code{Evaluation} giving the results of the evaluation of the initial design.}
#'  }

Optimization = setClass(
  Class = "Optimization",
  contains = "PFIMProject",
  representation = representation(
    name = "character",
    model = "Model",
    modelEquations = "list",
    modelParameters ="list",
    modelError = "list",
    optimizer = "OptimizationAlgorithm",
    optimizerParameters = "list",
    outcomes = "list",
    designs = "list",
    fim = "Fim",
    odeSolverParameters = "list",
    optimizationResults = "OptimizationAlgorithm",
    evaluationFIMResults = "Evaluation",
    evaluationInitialDesignResults = "Evaluation" ),

  prototype = prototype( odeSolverParameters = list( atol = 1e-6, rtol = 1e-6 ) ) )

setMethod(
  f="initialize",
  signature="Optimization",
  definition=function(.Object, name, model, modelEquations, modelParameters, modelError, optimizer,
                      optimizerParameters, outcomes, designs, fim, odeSolverParameters, optimizationResults, evaluationFIMResults, evaluationInitialDesignResults )
  {
    if(!missing(name))
    {
      .Object@name = name
    }

    if(!missing(model))
    {
      .Object@model = model
    }

    if(!missing(modelEquations))
    {
      .Object@modelEquations = modelEquations
    }

    if(!missing(modelParameters))
    {
      .Object@modelParameters = modelParameters
    }

    if(!missing(modelError))
    {
      .Object@modelError = modelError
    }

    if(!missing(optimizer))
    {
      if ( optimizer == "MultiplicativeAlgorithm")
      {
        .Object@optimizer = MultiplicativeAlgorithm()

      } else  if ( optimizer == "SimplexAlgorithm")
      {
        .Object@optimizer = SimplexAlgorithm()

      } else  if ( optimizer == "PSOAlgorithm")
      {
        .Object@optimizer = PSOAlgorithm()

      } else  if ( optimizer == "PGBOAlgorithm")
      {
        .Object@optimizer = PGBOAlgorithm()

      }else  if ( optimizer == "FedorovWynnAlgorithm")
      {
        .Object@optimizer = FedorovWynnAlgorithm()
      }
    }

    if(!missing(optimizerParameters))
    {
      .Object@optimizerParameters = optimizerParameters
    }

    if(!missing(outcomes))
    {
      .Object@outcomes = outcomes
    }

    if(!missing(designs))
    {
      .Object@designs = designs
    }

    if(!missing(fim))
    {
      if ( fim == "population")
      {
        .Object@fim = PopulationFim()
      }
      else if ( fim == "individual")
      {
        .Object@fim = IndividualFim()
      }
      else if ( fim == "Bayesian")
      {
        .Object@fim = BayesianFim()
      }
    }

    if(!missing(odeSolverParameters))
    {
      .Object@odeSolverParameters = odeSolverParameters
    }

    # set the names of the designs
    names(.Object@designs)= getNames( designs )

    if(!missing(optimizationResults))
    {
      .Object@optimizationResults = optimizationResults
    }

    if(!missing(evaluationFIMResults))
    {
      .Object@evaluationFIMResults = evaluationFIMResults
    }

    if(!missing(evaluationInitialDesignResults))
    {
      .Object@evaluationInitialDesignResults = evaluationInitialDesignResults
    }

    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
#' Set the designs.
#' @name setDesigns
#' @param object An object from the class \linkS4class{Optimization}.
#' @param designs A list of objects from the class \linkS4class{Design}.
#' @return The object with the new designs.
# ======================================================================================================

setGeneric("setDesigns",
           function( object, designs )
           {
             standardGeneric("setDesigns")
           })

setMethod(f="setDesigns",
          signature="Optimization",
          definition = function( object, designs )
          {
            object@designs = designs

            return( object )
          })


# ======================================================================================================
#' Get the proportion of subjects.
#' @name getProportionsOfSubjects
#' @param object An object from the class \linkS4class{Optimization}.
#' @return A vector giving the proportion of subjects.
# ======================================================================================================

setGeneric("getProportionsOfSubjects",
           function( object )
           {
             standardGeneric("getProportionsOfSubjects")
           })

setMethod(f="getProportionsOfSubjects",
          signature="Optimization",
          definition = function( object )
          {
            optimizerParameters = getOptimizerParameters( object )

            return( optimizerParameters$proportionsOfSubjects )
          })

# ======================================================================================================
#' Get the optimization results.
#' @name getOptimizationResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @return An object from the class \linkS4class{OptimizationAlgorithm} giving the optimization results.
# ======================================================================================================

setGeneric("getOptimizationResults",
           function( object )
           {
             standardGeneric("getOptimizationResults")
           })

setMethod(f="getOptimizationResults",
          signature="Optimization",
          definition = function( object )
          {
            return( object@optimizationResults )
          })

# ======================================================================================================
#' Set the optimization results.
#' @name setOptimizationResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @param value An object from the class \linkS4class{OptimizationAlgorithm} giving the optimization results.
#' @return The object with the updated object from the class \linkS4class{OptimizationAlgorithm}.
# ======================================================================================================

setGeneric("setOptimizationResults",
           function( object, value )
           {
             standardGeneric("setOptimizationResults")
           })

setMethod(f="setOptimizationResults",
          signature="Optimization",
          definition = function( object, value )
          {
            object@optimizationResults = value
            return( object )
          })

# ======================================================================================================
#' Get the results of the evaluation.
#' @name getEvaluationFIMResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @return An object from the class \linkS4class{Evaluation} giving the evaluation results for the optimal design.
# ======================================================================================================

setGeneric("getEvaluationFIMResults",
           function( object )
           {
             standardGeneric("getEvaluationFIMResults")
           })

setMethod(f="getEvaluationFIMResults",
          signature="Optimization",
          definition = function( object )
          {
            return( object@evaluationFIMResults )
          })

# ======================================================================================================
#' Set the evaluation results.
#' @name setEvaluationFIMResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @param value An object from the class \linkS4class{Evaluation} giving the evaluation results.
#' @return The object with the updated object from the class \linkS4class{Evaluation}.
# ======================================================================================================

setGeneric("setEvaluationFIMResults",
           function( object, value )
           {
             standardGeneric("setEvaluationFIMResults")
           })

setMethod(f="setEvaluationFIMResults",
          signature="Optimization",
          definition = function( object, value )
          {
            object@evaluationFIMResults = value
            return( object )
          })

# ======================================================================================================
#' Set the evaluation results of the initial design.
#' @name setEvaluationInitialDesignResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @param value An object from the class \linkS4class{Evaluation} giving the evaluation results of the initial design.
#' @return The object with the updated object from the class \linkS4class{Evaluation}.
# ======================================================================================================

setGeneric("setEvaluationInitialDesignResults",
           function( object, value )
           {
             standardGeneric("setEvaluationInitialDesignResults")
           })

setMethod(f="setEvaluationInitialDesignResults",
          signature="Optimization",
          definition = function( object, value )
          {
            object@evaluationInitialDesignResults = value
            return( object )
          })

# ======================================================================================================
#' Get the evaluation results of the initial design.
#' @name getEvaluationInitialDesignResults
#' @param object An object from the class \linkS4class{Optimization}.
#' @return The object from the class \linkS4class{Evaluation} giving the results of the evaluation of the initial design.
# ======================================================================================================

setGeneric("getEvaluationInitialDesignResults",
           function( object )
           {
             standardGeneric("getEvaluationInitialDesignResults")
           })

setMethod(f="getEvaluationInitialDesignResults",
          signature="Optimization",
          definition = function( object )
          {
            return( object@evaluationInitialDesignResults )
          })

# ======================================================================================================
#' Get the elementary protocols.
#' @name getElementaryProtocols
#' @param object An object from the class \linkS4class{Optimization}.
#' @param fims A list of object from the class \linkS4class{Fim}.
#' @return A list containing the results of the evaluation of the elementary protocols giving
#' the numberOfTimes, nbOfDimensions, totalCost, samplingTimes and the fisherMatrices
# ======================================================================================================

setGeneric("getElementaryProtocols",
           function( object, fims )
           {
             standardGeneric("getElementaryProtocols")
           })

setMethod(f="getElementaryProtocols",
          signature="Optimization",
          definition = function( object, fims )
          {
            # =======================================
            # design ,arm and fims
            # =======================================

            designs = getDesigns( object )
            design = designs[[1]]

            arms = getArms( design )
            arm = arms[[1]]

            samplingTimes = getSamplingTimes( arm )
            outcomes = unlist( lapply( samplingTimes, function(x) getOutcome(x) ) )

            fisherMatrices = fims$listFims
            fisherMatricesArms = fims$listArms

            # =======================================
            # samplings by outcomes
            # =======================================

            samplings = list()

            for ( outcome in outcomes )
            {
              samplingTime = lapply( fisherMatricesArms, function(x) getSamplingTime(x, outcome) )
              samplings[[outcome]] = lapply( samplingTime, function(x) getSamplings(x) )
              samplings[[outcome]] = do.call( rbind, samplings[[outcome]] )
            }

            combinedTimes = do.call(cbind, samplings)

            # =======================================
            # total cost
            # =======================================

            optimizerParameters = getOptimizerParameters( object )
            initialSamplings = optimizerParameters$elementaryProtocols
            totalNumberOfIndividuals = optimizerParameters$numberOfSubjects
            totalCost = sum( lengths( initialSamplings ) * totalNumberOfIndividuals )

            # ====================================================================================
            # reshape the fims
            # in initFedo.C : Fisher matrices = vector of lower element fisher matrix + diagonal
            # nota bene: initFedo.C implemented by Sylvie Retout in 2007 (see doc for references)
            # elements = [(1,1) ,(2,1:2),(3,1:3),etc ..]
            # number of elements = n*(n+1)/2 ; n = dim Fisher matrix
            # ====================================================================================

            dimFim = dim(fisherMatrices[[1]])[[1]]
            dimVectorTriangularInfWithDiagFisherMatrices = dimFim*(dimFim+1)/2
            fisherMatrices = lapply( fisherMatrices, function( x ) x[ rev( lower.tri( t( x ), diag=TRUE ) ) ] )
            fisherMatrices = matrix( unlist( fisherMatrices ), ncol = dimVectorTriangularInfWithDiagFisherMatrices, byrow = TRUE )

            # =======================================
            # elementaryProtocols
            # =======================================

            elementaryProtocolsFW = list()
            elementaryProtocolsFW$numberOfprotocols = dim( combinedTimes )[1]
            elementaryProtocolsFW$numberOfTimes = dim( combinedTimes )[2]
            elementaryProtocolsFW$nbOfDimensions = dimFim
            elementaryProtocolsFW$totalCost = totalCost
            elementaryProtocolsFW$samplingTimes = combinedTimes
            elementaryProtocolsFW$fisherMatrices = fisherMatrices
            return( elementaryProtocolsFW )

          })

# ======================================================================================================
#' Generate the fim from the constraints
#' @name generateFimsFromConstraints
#' @param object An object from the class \linkS4class{Optimization}.
#' @param fims A list of object from the class \linkS4class{Fim}.
#' @return A list giving the arms with their fims.
# ======================================================================================================

setGeneric("generateFimsFromConstraints",
           function( object, fims )
           {
             standardGeneric("generateFimsFromConstraints")
           })

setMethod(f="generateFimsFromConstraints",
          signature="Optimization",
          definition = function( object )
          {
            modelEquations = getModelEquations( object )
            modelParameters = getModelParameters( object )
            modelError = getModelError( object )
            outcomesForEvaluation = getOutcomes( object )
            designs = getDesigns( object )
            fim = getFim( object )
            odeSolverParameters = getOdeSolverParameters( object )
            fimEvaluation = setFimTypeToString( fim )

            # =======================================
            # generate the sampling times
            # =======================================

            samplingTimesCombinations = list()
            numberOfSamplings = list()
            indexAllCombinaisonsSamplings = list()
            numberOfFims = 0
            doses = list()

            designs = getDesigns( object )

            for ( design in designs )
            {
              designName = getName( design )

              arms = getArms( design )

              for ( arm in arms )
              {
                armName = getName( arm )

                samplingTimesConstraints = getSamplingTimesConstraints( arm )
                administrationConstraints = getAdministrationsConstraints( arm )

                outcomes = unlist( lapply( samplingTimesConstraints, function(x) getOutcome( x ) ) )
                doses[[armName]] = getDose( administrationConstraints[[1]] )

                for ( indiceDose in 1:length( doses[[armName]] ) )
                {
                  for ( outcome in outcomes )
                  {
                    samplingTimeConstraint = getSamplingTimeConstraint( arm, outcome )

                    samplings = getSamplings( samplingTimeConstraint )
                    fixedTimes = getFixedTimes( samplingTimeConstraint )
                    numberOfsamplingsOptimisable = getNumberOfsamplingsOptimisable( samplingTimeConstraint )

                    combinations = t( combn( samplings, numberOfsamplingsOptimisable ) )

                    n = length( samplings )

                    if ( length( fixedTimes ) != 0 )
                    {
                      p = length( fixedTimes )
                    }else
                    {
                      p = 0
                    }

                    numberOfSamplings[[outcome]] = 1:dim(combn(n-p,numberOfsamplingsOptimisable-p))[2]

                    samplingTimesCombinationsTmp = list()

                    if ( length( fixedTimes ) != 0 )
                    {
                      k = 1

                      for ( i in 1:dim( combinations )[1] )
                      {
                        if (all( fixedTimes %in% combinations[i,]) == TRUE )
                        {
                          samplingTimesCombinationsTmp[[k]] = combinations[i,]
                          k = k+1
                        }
                      }
                    } else if ( length( fixedTimes ) == 0 )
                    {
                      for ( i in 1:dim( combinations )[1] )
                      {
                        samplingTimesCombinationsTmp[[i]] = combinations[i,]
                      }
                    }

                    samplingTimesCombinations[[designName]][[armName]][[outcome]] = do.call( rbind, samplingTimesCombinationsTmp )
                  }

                  indexAllCombinaisonsSamplings[[designName]][[armName]] = as.data.frame( do.call( expand.grid, numberOfSamplings ) )
                  numberOfFims = numberOfFims + dim(  indexAllCombinaisonsSamplings[[designName]][[armName]] )[1]
                  colnames( indexAllCombinaisonsSamplings[[designName]][[armName]] ) = outcomes
                }
              }
            }

            # =======================================
            # create list of arms with constraints
            # =======================================

            listArms = list()
            listFims = list()

            for ( design in designs )
            {
              designName = getName( design )

              arms = getArms( design )

              iter = 1

              print(" Generate Fims ")

              for ( arm in arms )
              {
                armName = getName( arm )

                samplingTimesConstraints = getSamplingTimesConstraints( arm )

                outcomes = unlist( lapply( samplingTimesConstraints, function(x) getOutcome( x ) ) )

                administrations = getAdministrations( arm )

                # ============
                # set doses
                # ============

                for( dose in doses[[armName]] )
                {
                  administration = setDose( administrations[[1]],  dose )
                  arm = setAdministrations( arm, list( administration ) )

                  # ===================
                  # set sampling times
                  # ===================

                  for ( i in 1:dim( indexAllCombinaisonsSamplings[[designName]][[armName]] )[1] )
                  {
                    for ( outcome in outcomes )
                    {
                      indexSamplingTimes = indexAllCombinaisonsSamplings[[designName]][[armName]][,outcome][i]

                      samplingTimes = SamplingTimes( outcome,
                                                     samplings = samplingTimesCombinations[[designName]][[armName]][[outcome]][indexSamplingTimes,] )

                      arm = setSamplingTime( arm, samplingTimes )
                    }

                    design = setArm( design, arm )

                    outcomesForEvaluation = getOutcomes( object )

                    evaluationFIM = Evaluation( name = "",
                                                modelEquations = modelEquations,
                                                modelParameters = modelParameters,
                                                modelError = modelError,
                                                outcomes = outcomesForEvaluation,
                                                designs = list( design ),
                                                fim = fimEvaluation,
                                                odeSolverParameters = odeSolverParameters )

                    evaluationFIM =  run( evaluationFIM )

                    designs = getDesigns( evaluationFIM )

                    fim = getFim( designs[[1]] )

                    fisherMatrix = getFisherMatrix( fim )

                    listArms[[iter]] = arm
                    listFims[[iter]] = fisherMatrix

                    print( paste0( c( iter,"/", numberOfFims ),collapse="" ) )

                    iter = iter + 1
                  }
                }
              }
            }
            return( list( listArms = listArms, listFims = listFims ) )
          })

# ======================================================================================================
# run
# ======================================================================================================

setMethod(f = "run",
          signature = "Optimization",
          definition = function( object )
          {
            # ===============================================================================
            # evaluate initial design (for comparison with the optimal design )
            # ===============================================================================

            modelEquations = getModelEquations( object )
            modelParameters = getModelParameters( object )
            modelError = getModelError( object )
            outcomes = getOutcomes( object )
            designs = getDesigns( object )

            # ===========================================
            # get and set the fim
            # ===========================================

            fim = getFim( object )
            fimEvaluation = setFimTypeToString( fim )

            odeSolverParameters = getOdeSolverParameters( object )

            # ===========================================================
            # evaluate the initial design and set its evaluation results
            # ===========================================================

            evaluationFIMInitialDesign = Evaluation( name = "",
                                                     modelEquations = modelEquations,
                                                     modelParameters = modelParameters,
                                                     modelError = modelError,
                                                     outcomes = outcomes,
                                                     designs = designs,
                                                     fim = fimEvaluation,
                                                     odeSolverParameters = odeSolverParameters )

            evaluationFIMInitialDesignResults = run( evaluationFIMInitialDesign )

            object = setEvaluationInitialDesignResults( object, evaluationFIMInitialDesignResults )

            # ===========================================
            # set parameters of the optimizer
            # ===========================================

            optimizationAlgo = getOptimizer( object )
            optimizerParameters = getOptimizerParameters( object )
            optimizationAlgo = setParameters( optimizationAlgo, optimizerParameters )

            # ===========================================
            # set the outcomes design
            # ===========================================

            model = getModel( object )
            model = setOutcomes( model, outcomes )
            object = setModel( object, model )

            # ===========================================
            # design optimization
            # ===========================================

            optimizationResults = optimize( optimizationAlgo, optimizerParameters, object )

            # ===========================================
            # evaluate the fim for the optimal design
            # ===========================================

            optimalDesign = getOptimalDesign( optimizationResults )

            # ===========================================
            # Evaluation parameters
            # ===========================================

            modelEquations = getModelEquations( object )
            modelParameters = getModelParameters( object )
            modelError = getModelError( object )
            outcomes = getOutcomes( object )

            # ===========================================
            # get and set the fim
            # ===========================================

            fim = getFim( object )
            fimEvaluation = setFimTypeToString( fim )

            odeSolverParameters = getOdeSolverParameters( object )

            # ===========================================
            # evaluate the optimal design
            # ===========================================

            evaluationFIM = Evaluation( name = "",
                                        modelEquations = modelEquations,
                                        modelParameters = modelParameters,
                                        modelError = modelError,
                                        outcomes = outcomes,
                                        designs = list( optimalDesign ),
                                        fim = fimEvaluation,
                                        odeSolverParameters = odeSolverParameters )

            evaluationFIMResults = run( evaluationFIM )

            # ===========================================
            # set the optimization and evaluation results
            # ===========================================

            designs = getDesigns( evaluationFIMResults )
            optimizationResults = setOptimalDesign( optimizationResults, designs[[1]] )

            object = setOptimizationResults( object, optimizationResults )
            object = setEvaluationFIMResults( object, evaluationFIMResults )

            return( object )
          })

# ======================================================================================================
# show
# ======================================================================================================

setMethod(f="show",
          signature = "Optimization",
          definition = function( object )
          {
            optimizationResults = getOptimizationResults( object )
            evaluationFIMResults = getEvaluationFIMResults( object )

            optimalDesign = getOptimalDesign( optimizationResults )

            show( optimizationResults )

            cat("\n")

            cat( " ************************************************** ")
            cat("\n")
            cat( " Optimal Design ")
            cat("\n")
            cat( " ************************************************** ")

            cat("\n\n")

            show( optimalDesign )

            cat("\n")

            show( evaluationFIMResults )

          })

# ======================================================================================================
# plotWeights
# ======================================================================================================

setMethod(f="plotWeights",
          signature = "Optimization",
          definition = function( object, threshold )
          {
            optimizationResults = getOptimizationResults( object )

            plotWeights( optimizationResults, threshold )

          })

# ======================================================================================================
# generateTables Optimization
# ======================================================================================================

setMethod(f="generateTables",
          signature("Optimization"),
          function( object, plotOptions )
          {
            # ===========================================
            # get model and model error
            # ===========================================

            evaluationInitialDesign = getEvaluationInitialDesignResults( object )
            evaluationFIMResults = getEvaluationFIMResults( object )

            model = getModel( evaluationInitialDesign )

            # ===========================================
            # get design
            # ===========================================

            designs = getDesigns( evaluationInitialDesign )
            designNames = getNames( designs )
            designName = designNames[[1]]
            initialDesign = designs[[designName]]

            designs = getDesigns( evaluationFIMResults )
            designNames = getNames( designs )
            designName = designNames[[1]]
            optimalDesign = designs[[designName]]

            # ===========================================
            # get fim
            # ===========================================

            fim = getFim( optimalDesign )

            # ===========================================
            # tables for model equations
            # ===========================================

            modelEquations = getEquations( model )
            modelOutcomes = getOutcomes( evaluationInitialDesign )

            tablesModelEquations = list( outcomes = modelOutcomes, equations = modelEquations )

            # ===========================================
            # tables for model error
            # tables for model parameters
            # ===========================================

            tablesModelParameters = reportTablesModelParameters( model )
            tablesModelError = reportTablesModelError( model )

            # ===========================================
            # tables for administration
            # ===========================================

            tablesAdministration = reportTablesAdministration( initialDesign  )

            # ===========================================
            # tables for sampling constraints
            # ===========================================

            tablesSamplingConstraints = reportTablesSamplingConstraints( initialDesign  )

            # ===========================================
            # tables for design
            # ===========================================

            tablesDesign = reportTablesDesign( optimalDesign  )

            # ===========================================
            # tables for FIM
            # ===========================================

            tablesFIM = reportTablesFIM( fim, evaluationFIMResults )

            # ===========================================
            # tables for plot design, SI, SE and RSE
            # ===========================================

            tablesPlot = reportTablesPlot( evaluationFIMResults, plotOptions )

            # ===========================================
            # tables for report
            # ===========================================

            reportTables = list( tablesModelEquations = tablesModelEquations,
                                 tablesModelError = tablesModelError,
                                 tablesModelParameters = tablesModelParameters,
                                 tablesDesign = tablesDesign,
                                 tablesAdministration = tablesAdministration,
                                 tablesSamplingConstraints = tablesSamplingConstraints,
                                 tablesFIM = tablesFIM,
                                 tablesPlot = tablesPlot )

            return( reportTables )

          })
# ======================================================================================================
# Report
# ======================================================================================================

setMethod(f="Report",
          signature("Optimization"),
          function( object, outputPath, outputFile, plotOptions )
          {
            # ===========================================
            # set parameters of the optimizer
            # ===========================================

            optimizationAlgo = getOptimizer( object )
            optimizationResults = generateReportOptimization( optimizationAlgo, object,  outputPath, outputFile, plotOptions )
          })



##########################################################################################################
# END Class Optimization
##########################################################################################################



