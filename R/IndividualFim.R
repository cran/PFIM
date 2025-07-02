#' @description
#' The class \code{IndividualFim} represents and stores information for the IndividualFim.
#' @title IndividualFim
#' @inheritParams Fim
#' @include Fim.R
#' @export

IndividualFim = new_class( "IndividualFim", package = "PFIM", parent = Fim )

#' evaluateFim: evaluation of the Fim
#' @name evaluateFim
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param model An object \code{Model} giving the model.
#' @param arm An object \code{Arm} giving the arm.
#' @return The object \code{IndividualFim} with the fisherMatrix and the shrinkage.
#' @export

method( evaluateFim, list( IndividualFim, Model, Arm ) ) = function( fim, model, arm ) {

  # variance for the FIM
  evaluateVarianceFIM = evaluateVarianceFIM( fim, model, arm )
  V = evaluateVarianceFIM$V
  MFVar = evaluateVarianceFIM$MFVar
  chol2invV = chol2inv(chol(V))

  # fixed mu and fixed omega
  parameters = prop( model, "modelParameters")
  parameterNamesNoFixedMu = parameters %>% keep( ~ prop( .x, "fixedMu" ) == FALSE ) %>% map_chr("name")

  # components of the Fim: MFbeta & MFVar
  if ( length( parameterNamesNoFixedMu ) !=0 ) {
    gradients = prop( arm, "evaluationGradients" ) %>% reduce( rbind ) %>% { .[, parameterNamesNoFixedMu] } %>% as.matrix() }

  MFbeta = t( gradients ) %*% chol2invV %*% gradients

  # Fisher matrix
  prop( fim, "fisherMatrix" ) = as.matrix( bdiag( MFbeta, MFVar ) )

  return( fim )
}

#' evaluateVarianceFIM: evaluate the variance
#' @name evaluateVarianceFIM
#' @param arm A object of class \code{Arm} giving the arm.
#' @param model A object of class \code{Model} giving the model.
#' @param fim A object of class \code{IndividualFim} giving the Fim.
#' @return The matrices MFbeta and V.
#' @export

method( evaluateVarianceFIM, list( IndividualFim, Model, Arm ) ) = function( fim, model, arm ) {

  parameters = prop( model, "modelParameters")
  parameterNames = map_chr( prop( model, "modelParameters"), ~ prop( .x, "name" ) )

  # responses gradient
  gradient = prop( arm, "evaluationGradients" ) %>% reduce( rbind ) %>% { .[, parameterNames] }

  # evaluation variance
  evaluationVariance = prop( arm, "evaluationVariance" )
  V = evaluationVariance$errorVariance
  sigmaDerivatives = evaluationVariance$sigmaDerivatives

  chol2invV = chol2inv(chol(V))
  n = length(sigmaDerivatives)

  pairs = expand.grid(i = seq_along(sigmaDerivatives), j = seq_along(sigmaDerivatives))

  MFVar = map2_dbl(pairs$i, pairs$j, ~ 0.5 * sum(diag(chol2invV %*% sigmaDerivatives[[.x]] %*% chol2invV %*% sigmaDerivatives[[.y]]))) %>%
    matrix(nrow = n) %>% bdiag()

  V = bdiag(V)

  return( list( MFVar = MFVar, V = V ) )
}

#' setOptimalArms: set the optimal arms of an optimization algorithm.
#' @name setOptimalArms
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the optimization algorithm.
#' @return The list optimalArms.
#' @export

method( setOptimalArms, list( IndividualFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

  # get the parameters of the MultiplicativeAlgorithm
  multiplicativeAlgorithmOutputs = prop( optimizationAlgorithm, "multiplicativeAlgorithmOutputs" )

  # get the inputs arms and FIMs for the MultiplicativeAlgorithm
  armFims = multiplicativeAlgorithmOutputs$armFims
  multiplicativeAlgorithmOutput = multiplicativeAlgorithmOutputs$multiplicativeAlgorithmOutput

  # get the parameters of the MultiplicativeAlgorithm
  weightThreshold = multiplicativeAlgorithmOutputs$weightThreshold

  # weights: threshold with user threshold and normalization sum = 1
  weights = multiplicativeAlgorithmOutput[["weights"]]
  weightsIndex = which( weights > weightThreshold )

  armList = list()

  for( weightIndex in weightsIndex )
  {
    arm = pluck( armFims[[weightIndex]], 1 )
    prop( arm, "size" ) = 1
    prop( arm, "name" ) = paste0( "Arm", weightIndex )
    armList = append( armList, arm )
  }

  # sort by decreasing order
  sizes = map_dbl( armList, "size" )
  orderIndices = rev( order( sizes ) )
  optimalArms = armList[orderIndices]

  return( optimalArms )
}

#' setOptimalArms: set the optimal arms of an optimization algorithm.
#' @name setOptimalArms
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the optimization algorithm.
#' @return The list optimalArms.
#' @export

method( setOptimalArms, list( IndividualFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

  # get the parameters of the FedorovWynnAlgorithm
  FedorovWynnAlgorithmOutputs = prop( optimizationAlgorithm, "FedorovWynnAlgorithmOutputs" )

  optimalFrequencies = FedorovWynnAlgorithmOutputs$optimalFrequencies
  numberOfIndividuals = FedorovWynnAlgorithmOutputs$numberOfIndividuals
  listArms = FedorovWynnAlgorithmOutputs$listArms

  optimalArms = imap( listArms, ~ {
    prop(.x$arm, "name") = paste0("Arm", .y)
    prop(.x$arm, "size") = 1
    .x
  })

  return( optimalArms )
}

#' setEvaluationFim: set the Fim results.
#' @name setEvaluationFim
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The object \code{IndividualFim} with its fisherMatrix, fixedEffects, shrinkage, condNumberFixedEffects, SEAndRSE.
#' @export

method( setEvaluationFim, IndividualFim ) = function( fim, evaluation ) {

  # get parameters names and model error
  parameters = prop( evaluation, "modelParameters" )
  parametersNames = map_chr( parameters, ~ prop( .x, "name" ) )
  modelError = prop( evaluation, "modelError" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc_", omega = "\u03c9\u00B2_", sigma = "\u03c3" )

  # define the name for the columns and rows for mu, omega and sigma
  columnNamesMu = parameters %>%
    keep( ~ prop( .x, "fixedMu" ) == FALSE) %>%
    keep( ~ .x@distribution@mu != 0 ) %>%
    map_chr( "name" ) %>%
    map_chr(~ paste0( greeksLetterForCOnsole['mu'], .x ) )

  columnNamesSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE )
      sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )

    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE )
      sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )

    return( sigma )
  }) %>% unlist()  %>% unname()

  # get mu values
  muValues = parameters %>% keep( ~ prop( .x, "fixedMu" ) == FALSE ) %>% map_dbl( ~ pluck( .x, "distribution", "mu" ) )

  # get sigma values
  sigmaValues = map( modelError, ~ {
    values = list()
    if ( prop( .x, "sigmaInter" ) !=0 && prop( .x, "sigmaInterFixed" ) == FALSE )
    {
      values$sigmaInter = prop( .x, "sigmaInter" )
    }
    if ( prop( .x, "sigmaSlope" ) !=0 && prop( .x, "sigmaSlopeFixed" ) == FALSE )
    {
      values$sigmaSlope =  prop( .x, "sigmaSlope" )
    }
    return( values )
  }) %>% unlist()

  # get fisherMatrix
  fisherMatrix = prop( fim, "fisherMatrix")
  colnames( fisherMatrix ) = c( columnNamesMu, columnNamesSigma )
  rownames( fisherMatrix ) = c( columnNamesMu, columnNamesSigma )

  # fixed effects and variance effects
  fixedEffects = fisherMatrix[ columnNamesMu, columnNamesMu ]
  varianceEffects = fisherMatrix[ columnNamesSigma, columnNamesSigma ]

  # compute SE ans RSE
  SE = sqrt( diag( chol2inv( chol( fisherMatrix ) ) ) )

  parametersValues = c( muValues, sigmaValues )
  RSE = SE / parametersValues * 100

  SEAndRSE = data.frame( "parametersValues" = parametersValues, "SE" = SE, "RSE" = RSE )
  rownames( SEAndRSE ) = rownames( fisherMatrix )
  SE = data.frame( "parametersValues" = parametersValues, "SE" = SE )
  RSE = data.frame( "parametersValues" = parametersValues, "RSE" = RSE )

  prop( fim, "fisherMatrix" ) = fisherMatrix
  prop( fim, "fixedEffects" ) = fixedEffects
  prop( fim, "varianceEffects" ) = varianceEffects
  prop( fim, "condNumberFixedEffects" ) = cond(fixedEffects)
  prop( fim, "condNumberVarianceEffects" ) = cond(varianceEffects)
  prop( fim, "SEAndRSE" ) = list( SE = SE, RSE = RSE, SEAndRSE = SEAndRSE )

  return( fim )
}

#' showFIM: show the Fim in the R console.
#' @name showFIM
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @return The fisherMatrix, fixedEffects, Determinant, condition numbers and D-criterion, Shrinkage and Parameters estimation
#' @export

method( showFIM, IndividualFim ) = function( fim ) {

  SEAndRSE = prop( fim, "SEAndRSE" )
  fisherMatrix = prop( fim, "fisherMatrix")
  fixedEffects =prop( fim, "fixedEffects")
  varianceEffects = prop( fim, "varianceEffects")
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )
  condNumberVarianceEffects = prop( fim, "condNumberVarianceEffects" )

  RSE = SEAndRSE$SE
  RSE = SEAndRSE$RSE
  SEAndRSE = SEAndRSE$SEAndRSE
  Dcriterion = Dcriterion( fim )

  cat("\n*************************************** \n")
  cat(" Individual Fisher Matrix \n" )
  cat("*************************************** \n\n")
  print( fisherMatrix )
  cat("\n*************************************** \n")
  cat(" Fixed effects \n" )
  cat("*************************************** \n\n")
  print( fixedEffects )
  cat("\n*************************************** \n")
  cat(" Variance components \n" )
  cat("*************************************** \n\n")
  print( varianceEffects )
  cat("\n*********************************************** \n")
  cat(" Determinant, condition numbers and D-criterion \n" )
  cat("*********************************************** \n\n")
  cat( c( "Determinant:", as.numeric(det(fisherMatrix)) ), "\n")
  cat( c( "D-criterion:", as.numeric(Dcriterion) ), "\n")
  cat( c("Conditional number of the fixed effects:", as.numeric(condNumberFixedEffects) , "\n") )
  cat( c("Conditional number of the variance effects:", as.numeric(condNumberVarianceEffects) , "\n") )
  cat("\n*************************************** \n")
  cat(" Parameters estimation \n" )
  cat("*************************************** \n\n")
  print( SEAndRSE )
}

#' plotSEFIM: barplot for the SE
#' @name plotSEFIM
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the SE.
#' @export

method( plotSEFIM, list( IndividualFim, PFIMProject ) ) = function( fim, evaluation ) {

  # get parameter names and model error
  parameters = prop( evaluation, "modelParameters" )
  modelError = prop( evaluation, "modelError" )

  # get SEAndRSE
  fim = prop( evaluation, "fim" )
  fim = setEvaluationFim( fim, evaluation )
  standardErrors = prop( fim, "SEAndRSE" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc", omega = "\u03c9\u00B2", sigma = "\u03c3" )

  parametersMu =  parameters %>%
    keep( ~ prop( .x, "fixedMu" ) == FALSE) %>%
    keep( ~ .x@distribution@mu != 0 ) %>%
    map_chr( "name" )

  parametersSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )
  columnNamesSigma = parametersSigma %>% map_chr( ~  greeksLetterForCOnsole['sigma']  )

  # data for plot
  data = data.frame( Parameter = c( parametersMu,  parametersSigma ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     SE = standardErrors$SE,
                     cat = paste0( "SE ", c(columnNamesMu, columnNamesSigma ) ) )

  colnames( data ) = c("Parameter", "Value", "parametersValues", "SE", "cat")

  # bar plot of the plot SE
  plotSE = ggplot( data, aes( x = Parameter, y = SE ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    facet_wrap( ~factor( cat, levels =  paste0( "SE ", c( greeksLetterForCOnsole['mu'],  greeksLetterForCOnsole['omega'], greeksLetterForCOnsole["sigma"] ) ) ), scales = "free_x" ) +
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust = 0.5),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
          strip.text.x = element_text(size=16))
  return( plotSE )
}

#' plotRSEFIM: barplot for the RSE
#' @name plotRSEFIM
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the RSE.
#' @export

method( plotRSEFIM, list( IndividualFim, PFIMProject ) ) = function( fim, evaluation ) {

  # get parameter names and model error
  parameters = prop( evaluation, "modelParameters" )
  modelError = prop( evaluation, "modelError" )

  # get SEAndRSE
  fim = prop( evaluation, "fim" )
  fim = setEvaluationFim( fim, evaluation )
  standardErrors = prop( fim, "SEAndRSE" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc", omega = "\u03c9\u00B2", sigma = "\u03c3" )

  parametersMu =  parameters %>%
    keep( ~ prop( .x, "fixedMu" ) == FALSE) %>%
    keep( ~ .x@distribution@mu != 0 ) %>%
    map_chr( "name" )

  parametersSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )
  columnNamesSigma = parametersSigma %>% map_chr( ~  greeksLetterForCOnsole['sigma']  )

  # data for plot
  data = data.frame( Parameter = c( parametersMu, parametersSigma ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     RSE = standardErrors$RSE,
                     cat = paste0( "RSE ", c(columnNamesMu, columnNamesSigma ) ) )

  colnames( data ) = c("Parameter", "Value", "parametersValues", "RSE", "cat")

  # bar plot of the plot SE
  plotRSE = ggplot( data, aes( x = Parameter, y = RSE ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    facet_wrap( ~factor( cat, levels =  paste0( "RSE ", c( greeksLetterForCOnsole['mu'],  greeksLetterForCOnsole["sigma"] ) ) ), scales = "free_x" ) +
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust = 0.5),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
          strip.text.x = element_text(size=16))

  return( plotRSE )
}

#' tablesForReport: generate the table for the report.
#' @name tablesForReport
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return fixedEffectsTable, FIMCriteriaTable, SEAndRSETable.
#' @export

method( tablesForReport, list( IndividualFim, PFIMProject ) ) = function( fim, evaluation ) {

  SEAndRSE = prop( fim, "SEAndRSE" )
  fisherMatrix = prop( fim, "fisherMatrix")
  fixedEffects = prop( fim, "fixedEffects")
  varianceEffects = prop( fim, "varianceEffects")
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )
  condNumberVarianceEffects = prop( fim, "condNumberVarianceEffects" )
  Dcriterion = Dcriterion( fim )
  determinant = det( fisherMatrix )
  SEAndRSE = SEAndRSE$SEAndRSE
  parameters = prop( evaluation, "modelParameters" )
  modelError = prop( evaluation, "modelError" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "$\\mu_{", omega = "$\\omega^2_{", sigma = "${\\sigma_" )

  # define the name for the columns and rows for mu, omega and sigma
  columnNamesMu = parameters %>%
    keep( ~ prop( .x, "fixedMu" ) == FALSE) %>%
    keep( ~  prop( prop(.x,"distribution"), "mu" ) != 0 ) %>%
    map_chr( "name" ) %>%
    map_chr(~ paste0( greeksLetterForCOnsole['mu'], .x,"}$" ) )

  columnNamesSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE )
      sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "{inter}}_{", prop( .x ,"output" ),"}$" ) )

    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE )
      sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "{slope}}_{", prop( .x ,"output" ),"}$" ) )

    return( sigma )
  }) %>% unlist() %>% unname()

  fixedEffects = as.matrix( fixedEffects )
  colnames( fixedEffects ) = columnNamesMu
  rownames( fixedEffects ) = columnNamesMu
  colnames( varianceEffects ) = c( columnNamesSigma )
  rownames( varianceEffects ) = c( columnNamesSigma )

  fixedEffectsTable = fixedEffects %>% kbl() %>% kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )
  varianceEffectsTable = varianceEffects %>% kbl() %>% kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  FIMCriteria = data.frame( Determinant = determinant, Dcriterion = Dcriterion,  condNumberFixedEffects = condNumberFixedEffects, condNumberVarianceEffects = condNumberVarianceEffects )

  FIMCriteriaTable = kbl( FIMCriteria,
                          col.names = c("", "", "Fixed effects", "Variance effects"),
                          align = c("c", "c", "c", "c"),
                          format = "html" ) %>%
    add_header_above(c( "Determinant" = 1, "D-criterion" = 1, "Condition number" = 2 )) %>%
    kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  # SEAndRSE table
  SEAndRSE = data.frame( c( columnNamesMu, columnNamesSigma ), round(SEAndRSE,3) )
  row.names( SEAndRSE ) = NULL

  SEAndRSETable = kbl( SEAndRSE,
                       col.names = c("Parameters", "Parameter values", "SE", "RSE (%)"),
                       align = c("c", "c", "c", "c") ) %>% kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  fimTables = list( fixedEffectsTable = fixedEffectsTable, varianceEffectsTable = varianceEffectsTable, FIMCriteriaTable = FIMCriteriaTable, SEAndRSETable =  SEAndRSETable)

  return( fimTables )
}

#' generateReportEvaluation: generate the report for the model evaluation.
#' @name generateReportEvaluation
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report for the model evaluation.
#' @export

method( generateReportEvaluation, IndividualFim ) = function( fim, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "EvaluationIndividualFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath,
                     params = list(
                       #plotOptions = "plotOptions", #projectName = "projectName",
                       tablesForReport = "tablesForReport" ) )

}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the MultiplicativeAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list(IndividualFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationMultiplicativeAlgorithmIndividualFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )

}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the FedorovWynnAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( IndividualFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationFedorovWynnAlgorithmIndividualFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{SimplexAlgorithm} giving the SimplexAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( IndividualFim, SimplexAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationSimplexAlgorithmIndividualFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{PSOAlgorithm} giving the PSOAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( IndividualFim, PSOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPSOAlgorithmIndividualFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )

}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{IndividualFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{PGBOAlgorithm} giving the PGBOAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( IndividualFim, PGBOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPGBOAlgorithmIndividualFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )

}
