#' @description The class \code{Optimization} implements the Optimization.
#' @title Optimization
#' @inheritParams PFIMProject
#' @param optimisationDesign A list giving the evaluation of initial and optimal design.
#' @param optimisationAlgorithmOutputs A list giving the outputs of the optimization process.
#' @include PFIMProject.R
#' @export

Optimization = new_class("Optimization",
                         package = "PFIM",
                         parent = PFIMProject,
                         properties = list(
                           optimisationDesign = new_property(class_list, default = list()),
                           optimisationAlgorithmOutputs = new_property(class_list, default = list())
                         ))

defineOptimizationAlgorithm = new_generic( "defineOptimizationAlgorithm", c( "optimization" ) )
generateFimsFromConstraints = new_generic( "generateFimsFromConstraints", c( "optimization" ) )
plotWeights = new_generic( "plotWeights", c( "optimization" ) )
plotFrequencies = new_generic( "plotFrequencies", c( "optimization" ) )
optimizeDesign = new_generic( "optimizeDesign", c("optimizationObject", "optimizationAlgorithm" ) )
constraintsTableForReport = new_generic( "constraintsTableForReport", c( "optimizationAlgorithm" ) )

#' Generate FIMs from constraints
#' @name generateFimsFromConstraints
#' @param optimization An \code{Optimization} object.
#' @return A list containing FIMs from constraints.
#' @export

method( generateFimsFromConstraints, Optimization ) = function( optimization ) {

  listArms = list()
  listFimsAlgoFW = list()
  listFimsAlgoMult = list()

  samplingsForFedorovWynnAlgo = list()

  # set the evaluation parameters from the optimization object
  evaluation = Evaluation( name = "",
                           modelEquations = prop( optimization, "modelEquations" ),
                           modelParameters = prop( optimization, "modelParameters" ),
                           modelError = prop( optimization, "modelError" ),
                           designs =  prop( optimization, "designs" ),
                           fimType = prop( optimization, "fimType" ),
                           outputs = prop( optimization, "outputs" ),
                           odeSolverParameters = prop( optimization, "odeSolverParameters" ) )

  # Extract the designs from the optimization object
  designs = prop( optimization, "designs" )
  designNames = map( designs, ~ prop( .x, "name" ) )

  # All combinations for administration times constraints
  dosesForFIMs = map( designs, ~ generateDosesCombination( .x ) ) %>% set_names( designNames )

  # All combinations for sampling times constraints
  samplingsForFIMs = map( designs, ~ generateSamplingTimesCombination( .x ) ) %>% set_names( designNames )

  # evaluate the FIMs
  # for each dose combination associate each sampling time combination
  for ( design in designs )
  {
    numberOfFims = 1

    designName = prop( design, "name" )
    arms = prop( design, "arms" )

    dosesForFIMs = dosesForFIMs[[designName]]
    numberOfDoses = dosesForFIMs$numberOfDoses

    for ( iterDose in seq( numberOfDoses ) )
    {
      # assign the new doses for each arm
      arms = map( arms, function( arm )
      {
        armName = prop( arm, "name")
        administrations = prop( arm, "administrations" )
        administrations = map( administrations, function( administration )
        {
          outcome = prop( administration, "outcome")
          prop( administration, "dose") = dosesForFIMs[[armName]][[outcome]][iterDose]
          return(administration )
        })
        prop( arm, "administrations" ) = administrations
        return( arm )
      })

      # Create combination indices for the sampling times
      combinationIndicesSamplingTimes = expand.grid( map( samplingsForFIMs[[designName]], ~ seq_along(.x) ) )
      numberOfCombinationIndicesSamplingTimes = dim( combinationIndicesSamplingTimes )[1]

      for ( iterCombinationIndicesSamplingTimes in 1:numberOfCombinationIndicesSamplingTimes )
      {
        # Update arms with new sampling times for the current combination
        armsWithNewSamplingTimes = map(arms, function(arm) {
          armName = prop( arm, 'name' )
          indexSamplingsForFims = combinationIndicesSamplingTimes[iterCombinationIndicesSamplingTimes, armName]

          samplingsForFedorovWynnAlgoTmp = pluck( samplingsForFIMs, designName, armName, indexSamplingsForFims ) %>%
            map(~ prop( .x, "samplings" ) ) %>%
            flatten_dbl()

          prop( arm, "samplingTimes" ) = pluck( samplingsForFIMs, designName, armName, indexSamplingsForFims )
          list( arm = arm,  samplingsForFedorovWynnAlgoTmp = samplingsForFedorovWynnAlgoTmp )
        })

        armsWithNewSamplingTimes = pluck( armsWithNewSamplingTimes, 1 )

        # samplings for FW method
        samplingsForFedorovWynnAlgo[[designName]][[numberOfFims]] = armsWithNewSamplingTimes$samplingsForFedorovWynnAlgoTmp

        # Update the design with the new arms
        prop( design, "arms" ) = list( armsWithNewSamplingTimes$arm )
        prop( evaluation, "designs" ) = list( design )

        # design evaluation and get the FIM
        evaluationFIM = run( evaluation )
        fim = getFim( evaluationFIM )
        fisherMatrix = fim$fisherMatrix

        # reshape the fims
        # in initFedo.C : Fisher matrices = vector of lower element fisher matrix + diagonal
        # elements = [(1,1) ,(2,1:2),(3,1:3),etc ..]
        # number of elements = n*(n+1)/2 ; n = dim Fisher matrix
        dimFim = pluck( dim(fisherMatrix), 1 )
        dimVectorTriangularInfWithDiagFisherMatrices = dimFim*(dimFim+1)/2
        fisherMatrixForAlgoFW = fisherMatrix[ rev( lower.tri( t( fisherMatrix ), diag = TRUE ) ) ]
        fisherMatrixForAlgoFW = matrix( fisherMatrixForAlgoFW , ncol = dimVectorTriangularInfWithDiagFisherMatrices, byrow = TRUE )

        listArms[[designName]][[numberOfFims]] = armsWithNewSamplingTimes
        listFimsAlgoFW[[designName]][[numberOfFims]] = fisherMatrixForAlgoFW
        listFimsAlgoMult[[designName]][[numberOfFims]] = fisherMatrix
        # Print the iteration for progress tracking
        print( paste0( "Evaluation of the FIMs: ",  numberOfFims, "/", numberOfCombinationIndicesSamplingTimes*numberOfDoses ) )
        numberOfFims = numberOfFims + 1
      } # end samplingTimes
    } #end doses
  } # end designs
  return( list( listArms = listArms, dimFim = dimFim, listFimsAlgoFW = listFimsAlgoFW ,
                listFimsAlgoMult = listFimsAlgoMult, samplingsForFedorovWynnAlgo = samplingsForFedorovWynnAlgo ) ) }

#' Define optimization algorithm
#' @name defineOptimizationAlgorithm
#' @param optimization An \code{Optimization} object.
#' @return An optimization algorithm.
#' @export
#
method( defineOptimizationAlgorithm, Optimization ) = function( optimization )
{
  optimizerParameters = prop( optimization, "optimizer")

  optimizationAlgorithm = switch( optimizerParameters,
                                  "MultiplicativeAlgorithm" = MultiplicativeAlgorithm(),
                                  "FedorovWynnAlgorithm" = FedorovWynnAlgorithm(),
                                  "PSOAlgorithm" = PSOAlgorithm(),
                                  "PGBOAlgorithm" = PGBOAlgorithm(),
                                  "SimplexAlgorithm" = SimplexAlgorithm(),
                                  optimizationAlgorithm )

  return( optimizationAlgorithm )
}

#' Run optimization
#' @name run
#' @param optimization An \code{Optimization} object.
#' @return The optimization design results.
#' @export

method( run, Optimization ) = function( pfimproject )
{
  # set the optimization parameters
  optimizationAlgorithm = defineOptimizationAlgorithm( pfimproject )

  # define the type of the Fim
  prop( pfimproject, "fim" ) = defineFim( pfimproject )

  # define model equations
  modelFromLibraryOfModel = prop( pfimproject, "modelFromLibrary" )
  if ( length( modelFromLibraryOfModel ) != 0 ) { prop( pfimproject, "modelEquations" ) = defineModelEquationsFromLibraryOfModel( pfimproject ) }

  # Run the optimization process
  optimizationDesign = optimizeDesign( pfimproject, optimizationAlgorithm  )

  return( optimizationDesign )
}

#' Show optimization results
#' @name show
#' @param optimization An \code{Optimization} object.
#' @return Prints results to console.
#' @export

method( show, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  evaluationInitialDesign = optimisationDesign$evaluationInitialDesign

  # initial design
  designs = prop( evaluationInitialDesign, "designs" )
  designsNames = map_chr( designs, "name" )
  arms = map( designs, ~ prop( .x, "arms" ))
  armsData = flatten(map( pluck( arms,1), getArmData ) )
  initialDesignData = map_df( armsData, ~ as.data.frame( .x, stringsAsFactors = FALSE ) )
  colnames( initialDesignData ) = c( "Arms name" , "Number of subjects", "Outcome", "Dose","Sampling times" )

  # optimal design
  designs = prop( evaluationOptimalDesign, "designs" )
  designsNames = map_chr( designs, "name" )
  arms = map( designs, ~ prop( .x, "arms" ))
  armsData = flatten(map( pluck( arms,1), getArmData ) )
  optimalDesignData = map_df( armsData, ~ as.data.frame( .x, stringsAsFactors = FALSE ) )
  colnames( optimalDesignData ) = c( "Arms name" , "Number of subjects", "Outcome", "Dose","Sampling times" )

  # Fisher matrix and SE
  fim = prop( evaluationInitialDesign, "fim" )
  fimInitialDesign = setEvaluationFim( fim, evaluationInitialDesign )
  fim = prop( evaluationOptimalDesign, "fim" )
  fimOptimalDesign = setEvaluationFim( fim, evaluationOptimalDesign )

  cat("\n===================================== \n")
  cat("  Initial design \n" )
  cat("===================================== \n\n")
  print( initialDesignData )
  showFIM( fimInitialDesign )
  cat("\n===================================== \n")
  cat("  Optimal design \n" )
  cat("===================================== \n\n")
  print( optimalDesignData )
  showFIM( fimOptimalDesign )
}

#' getFisherMatrix: display the Fisher matrix components
#' @name getFisherMatrix
#' @param evaluation An object \code{Evaluation} giving the evaluation to be run.
#' @return The matrices fisherMatrix, fixedEffects, varianceEffects.
#' @export

method( getFisherMatrix, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  fisherMatrix = prop( fim, "fisherMatrix" )
  fixedEffects = prop( fim, "fixedEffects" )
  varianceEffects = prop( fim, "varianceEffects" )
  return( list( fisherMatrix = fisherMatrix, fixedEffects = fixedEffects, varianceEffects = varianceEffects ) )
}

#' getSE: get the SE
#' @name getSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The SE of the parameters.
#' @export

method( getSE, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  SEAndRSE = prop( fim, "SEAndRSE" )
  SE = SEAndRSE$SE
  return( SE )
}

#' getRSE: get the RSE
#' @name getRSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The RSE of the parameters.
#' @export

method( getRSE, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  SEAndRSE = prop( fim, "SEAndRSE" )
  RSE = SEAndRSE$RSE
  return( RSE )
}

#' getShrinkage: get the shrinkage
#' @name getShrinkage
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The shrinkage of the FIM.
#' @export

method( getShrinkage, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  shrinkage = prop( fim, "shrinkage" )
  return( shrinkage )
}

#' getDeterminant: get the determinant
#' @name getDeterminant
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The determinant of the FIM.
#' @export

method( getDeterminant, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fisherMatrix = getFisherMatrix( evaluationOptimalDesign )
  return( det( fisherMatrix$fisherMatrix ) )
}

#' getCorrelationMatrix : get the correlation matrix
#' @name getCorrelationMatrix
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The correlation matrix
#' @export

method( getCorrelationMatrix, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fisherMatrix = getFisherMatrix( evaluationOptimalDesign )
  fisherMatrix = fisherMatrix$fisherMatrix
  return( cor( fisherMatrix ) )
}

#' getDcriterion : get the Dcriterion
#' @name getDcriterion
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The Dcriterion of the FIM.
#' @export

method( getDcriterion, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  return( Dcriterion( fim ) )
}

#' Plot sensitivity indices.
#' @name plotSensitivityIndices
#' @param optimization An \code{Optimization} object.
#' @return Graph of sensitivity indices.
#' @export

method( plotSensitivityIndices, Optimization ) = function( pfimproject )
{
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  plotSensitivityIndicesOptimalDesign = plotSensitivityIndices( evaluationOptimalDesign )
  return( plotSensitivityIndicesOptimalDesign )
}

#' Plot standard errors
#' @name plotSE
#' @param optimization An \code{Optimization} object.
#' @return Graph of standard errors
#' @export

method( plotSE, Optimization ) = function( pfimproject )
{
  # set the FIM and plot SE
  fim = prop( pfimproject, "fim" )
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  plotSE = plotSEFIM( fim, evaluationOptimalDesign )
  return( plotSE )
}

#' Plot relative standard errors
#' @name plotRSE
#' @param optimization An \code{Optimization} object.
#' @return Graph of relative standard errors
#' @export

method( plotRSE, Optimization ) = function( pfimproject )
{
  # set the FIM and plot RSE
  fim = prop( pfimproject, "fim" )
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  plotRSE = plotRSEFIM( fim, evaluationOptimalDesign )
  return( plotRSE )
}

#' Plot weights for the multiplicative algorithm
#' @name plotWeights
#' @param optimization An \code{Optimization} object.
#' @return Plot of weights
#' @export

method( plotWeights, Optimization ) = function( optimization )
{
  optimisationAlgorithmOutputs = prop( optimization, "optimisationAlgorithmOutputs" )
  optimizationAlgorithm = optimisationAlgorithmOutputs$optimizationAlgorithm
  plotWeightsMultiplicativeAlgorithm( optimization, optimizationAlgorithm )
}

#' Plot frequencies for the FedorovWynn algorithm
#' @name plotFrequencies
#' @param optimization An \code{Optimization} object.
#' @return Graph of the optimal frequencies.
#' @export

method( plotFrequencies, Optimization ) = function( optimization )
{
  optimisationAlgorithmOutputs = prop( optimization, "optimisationAlgorithmOutputs" )
  optimizationAlgorithm = optimisationAlgorithmOutputs$optimizationAlgorithm
  plotFrequenciesFedorovWynnAlgorithm( optimization, optimizationAlgorithm )
}

#' Generate optimization report
#' @name Report
#' @param optimization An \code{Optimization} object.
#' @param outputPath Output path for the report.
#' @param outputFile Output file name.
#' @param plotOptions Plot options.
#' @return Generated report.
#' @export

method( Report, Optimization ) = function( pfimproject, outputPath, outputFile, plotOptions  )
{
  # projectName
  projectName = prop( pfimproject, "name" )

  # slots optimization
  optimisationDesign = prop( pfimproject, "optimisationDesign" )
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  evaluationInitialDesign = optimisationDesign$evaluationInitialDesign

  # outputs
  evaluationOutputs = prop( pfimproject , "outputs" )

  # model
  model = defineModelType( evaluationInitialDesign  )
  modelEquations = prop( model, "modelEquations" )

  # model error table
  modelError = prop( evaluationInitialDesign, "modelError" )
  modelError = map( modelError, getModelErrorData )
  modelErrorData = map_df( modelError, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( modelErrorData ) = c( "Output", "Type", "$\\sigma_{slope}$", "$\\sigma_{inter}$" )

  modelErrorTable = kbl( modelErrorData, align = c( "c","c","c","c" ) ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # model parameters table
  modelParameters = prop( evaluationInitialDesign, "modelParameters" )
  modelParameters = map( modelParameters, getModelParametersData )
  modelParametersData = map_df( modelParameters, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( modelParametersData ) = c("Parameter","$\\mu$","$\\omega$","Distribution", paste0("$\\mu$"," fixed"), paste0("$\\omega$"," fixed"))

  modelParametersTable = kbl( modelParametersData, align = c( "l","l","l","c","c","c" ) ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # arms
  designs = prop( evaluationInitialDesign, "designs" )
  designsNames = map_chr( designs, "name" )
  arms = map( designs, ~ prop( .x, "arms" ))
  armsData = flatten( map( pluck(arms,1), getArmData ) )

  # administration table
  administration = flatten( map( pluck(arms, 1), armAdministration ) )
  administrationData = map_df( administration, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( administrationData ) = c( "Design name","Arms name" , "Number of subject ", "Outcome", "Dose","Time dose", "$\\tau$", "$T_{inf}$" )

  administrationTable = kbl( administrationData, align = c( "l","l","l","c","c","c","c" ) ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # initial design
  initialDesignData = map_df( armsData, ~ as.data.frame( .x, stringsAsFactors = FALSE ) )
  colnames( initialDesignData ) = c( "Arms name" , "Number of subjects", "Outcome", "Dose","Sampling times" )

  initialDesignTable = kbl( initialDesignData, align = c( "l","c","c","c") ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # Fisher matrix and SE
  fim = prop( evaluationInitialDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationInitialDesign )
  fimInitialDesignTable = tablesForReport( fim, evaluationInitialDesign )

  # design constraints
  optimisationAlgorithmOutputs = prop( pfimproject, "optimisationAlgorithmOutputs" )
  optimizationAlgorithm = optimisationAlgorithmOutputs$optimizationAlgorithm
  constraintsTableForReport = constraintsTableForReport( optimizationAlgorithm, arms )

  # optimal design table
  designs = prop( evaluationOptimalDesign, "designs" )
  designsNames = map_chr( designs, "name" )
  arms = map( designs, ~ prop( .x, "arms" ) )
  armsData = flatten(map( pluck( arms,1), getArmData ) )
  optimalDesignData = map_df(armsData, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( optimalDesignData ) = c( "Arms name" , "Number of subjects", "Outcome", "Dose","Sampling times" )

  optimalDesignTable = kbl( optimalDesignData, align = c( "l","c","c","c","c","c") ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # Fisher matrix and SE
  fim = prop( evaluationOptimalDesign, "fim" )
  fim = setEvaluationFim( fim, evaluationOptimalDesign )
  fimOptimalTable = tablesForReport( fim, evaluationOptimalDesign )

  # plotsEvaluation & plotSensitivityIndices
  plotsEvaluation = plotEvaluation( evaluationOptimalDesign, plotOptions )
  plotSensitivityIndices = plotSensitivityIndices( evaluationOptimalDesign, plotOptions )
  plotSE = plotSE( evaluationOptimalDesign )
  plotRSE = plotRSE( evaluationOptimalDesign )

  # tablesForReport
  tablesForReport = list( evaluationOutputs = evaluationOutputs,
                          modelEquations = modelEquations,
                          modelErrorTable = modelErrorTable,
                          modelParametersTable = modelParametersTable,
                          administrationTable = administrationTable,
                          initialDesignTable = initialDesignTable,
                          constraintsTableForReport = constraintsTableForReport,
                          fimInitialDesignTable = fimInitialDesignTable,
                          optimalDesignTable = optimalDesignTable,
                          fimOptimalTable = fimOptimalTable,
                          plotsEvaluation = plotsEvaluation,
                          plotSensitivityIndices = plotSensitivityIndices,
                          plotSE = plotSE,
                          plotRSE = plotRSE,
                          fim = fim,
                          pfimproject = pfimproject,
                          projectName = projectName )

  generateReportOptimization( fim, optimizationAlgorithm, tablesForReport )
}






