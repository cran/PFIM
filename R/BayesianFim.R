#' @description
#' The class \code{BayesianFim} represents and stores information for the Bayesian Fim.
#' @title BayesianFim
#' @param fisherMatrix A matrix giving the numerical values of the Fim.
#' @param shrinkage A vector giving the shrinkage values.
#' @param fixedEffects A matrix giving the numerical values of the fixedEffects of the Fim.
#' @param varianceEffects A matrix giving the numerical values of varianceEffects of the Fim.
#' @param SEAndRSE A data frame giving the value of the SE and RSE.
#' @param condNumberFixedEffects The conditional number of the fixedEffects of the Fim.
#' @param condNumberVarianceEffects The conditional number of the varianceEffects of the Fim.
#' @include Fim.R
#' @include MultiplicativeAlgorithm.R
#' @include FedorovWynnAlgorithm.R
#' @export

BayesianFim = new_class( "BayesianFim", package = "PFIM", parent = Fim,

                         properties = list(
                           fisherMatrix = new_property(class_double, default = 0.0),
                           fixedEffects = new_property(class_double, default = 0.0),
                           varianceEffects = new_property(class_double, default = 0.0),
                           SEAndRSE = new_property(class_list, default = list()),
                           condNumberFixedEffects = new_property(class_double, default = 0.0),
                           condNumberVarianceEffects = new_property(class_double, default = 0.0),
                           shrinkage = new_property(class_double, default = 0.0)
                         ))

#' evaluateFim: evaluation of the Fim
#' @name evaluateFim
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param model An object \code{Model} giving the model.
#' @param arm An object \code{Arm} giving the arm.
#' @return The object  \code{Fim} with the fisherMatrix and the shrinkage.
#' @export

method( evaluateFim, list( BayesianFim, Model, Arm ) ) = function( fim, model, arm ) {

  parameters = prop( model, "modelParameters" )
  gradient = prop( arm, "evaluationGradients" ) %>% reduce( rbind ) %>% as.matrix()

  # variance for the FIM
  evaluateVarianceFIM = evaluateVarianceFIM( fim, model, arm )
  V = evaluateVarianceFIM$V

  # matrix MFBeta
  MFbeta = crossprod( gradient, chol2inv( chol( V ) ) ) %*% gradient

  # set mu for distribution Normal & LogNormal
  mu = map_dbl( parameters, ~ { if ( is( prop( .x, "distribution" ), "PFIM::Normal" ) ) 1 else .x@distribution@mu } ) %>%
    { if(length(.) == 1) as.numeric(.) else diag(.) }

  omega = parameters %>% map_dbl( ~ pluck( .x, "distribution", "omega" ) ) %>% { if(length(.) == 1) as.numeric(.) else diag(.^2) }

  # fixed parameters
  indexFixedMu = which( map_lgl( parameters, ~ .x@distribution@mu == 0 || prop(.x, "fixedMu") == TRUE ) )
  indexFixeddOmega = which( map_lgl( parameters, ~ .x@distribution@omega == 0 || prop(.x, "fixedOmega") == TRUE ) )
  indexFixed = unique( c( indexFixedMu, indexFixeddOmega ) )

  if ( length( indexFixed ) != 0 ) {
    mu = mu[ -c(indexFixed), -c(indexFixed) ]
    omega = omega[ -c(indexFixed), -c(indexFixed) ]
    MFbeta = MFbeta[ -c(indexFixed), -c(indexFixed) ] }

  MFbeta = t( mu ) %*% MFbeta %*% mu + solve( mu %*% omega %*% mu )
  prop( fim, "fisherMatrix" ) = as.matrix( MFbeta )

  # shrinkage parameters
  prop( fim, "shrinkage" ) = diag( chol2inv( chol( MFbeta ) ) %*% chol2inv( chol( mu %*% omega %*% mu ) ) ) * 100 %>% as.vector()

  return( fim )
}

#' evaluateVarianceFIM: evaluate the variance
#' @name evaluateVarianceFIM
#' @param arm A object of class \code{Arm} giving the arm.
#' @param model A object of class \code{Model} giving the model.
#' @param fim A object of class \code{BayesianFim} giving the Fim.
#' @return The matrices MFbeta and V.
#' @export

method( evaluateVarianceFIM, list( BayesianFim, Model, Arm ) ) = function( fim, model, arm ) {

  parameters = prop( model, "modelParameters")
  parameterNames = map_chr( prop( model, "modelParameters"), ~ prop( .x, "name" ) )

  # responses gradient
  gradient = prop( arm, "evaluationGradients" ) %>% reduce( rbind ) %>% { .[, parameterNames, drop = FALSE ] } %>% as.matrix()

  # evaluation variance & MFBeta
  evaluationVariance = prop( arm, "evaluationVariance" )
  V = bdiag( evaluationVariance$errorVariance )
  MFbeta = crossprod( gradient, chol2inv( chol(V) ) ) %*% gradient

  return( list( MFbeta = MFbeta, V = V ) )
}

#' setOptimalArms: set the optimal arms of an optimization algorithm.
#' @name setOptimalArms
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the optimization algorithm.
#' @return The optimal arms.
#' @export

method( setOptimalArms, list( BayesianFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

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
#' @param fim An object \code{Fim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the optimization algorithm.
#' @return The optimal arms.
#' @export

method( setOptimalArms, list( BayesianFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

  # get the parameters
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
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The object \code{Fim} with its fisherMatrix, fixedEffects, shrinkage, condNumberFixedEffects, SEAndRSE.
#' @export

method( setEvaluationFim, BayesianFim ) = function( fim, evaluation ) {

  # get parameters names and model error
  parameters = prop( evaluation, "modelParameters" )
  parametersNames = map_chr( parameters, ~ prop( .x, "name" ) )
  modelError = prop( evaluation, "modelError" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc_", omega = "\u03c9\u00B2_", sigma = "\u03c3" )

  # define the name for the columns and rows for mu, omega and sigma
  columnNamesMu = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE) %>%       # Remove fixed omega parameters
    keep(~ pluck(.x, "distribution", "omega") != 0) %>%              # Keep non-zero omega
    map_chr("name") %>%                                 # Extract names
    map_chr(~ paste0(greeksLetterForCOnsole['mu'], .x)) # Add mu symbol

  # Get mu values
  muValues = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE ||  pluck(.x, "distribution", "omega") == 0) %>%
    map_dbl(~ pluck(.x, "distribution", "mu"))          # Extract mu values

  # get fisherMatrix
  fisherMatrix = prop( fim, "fisherMatrix")
  colnames( fisherMatrix ) = c( columnNamesMu )
  rownames( fisherMatrix ) = c( columnNamesMu )

  # fixed effects and variance effects
  fixedEffects = fisherMatrix[ columnNamesMu, columnNamesMu ]

  # shrinkage
  shrinkage = prop( fim, "shrinkage")

  # compute SE ans RSE
  SE = sqrt( diag( chol2inv( chol( fisherMatrix ) ) ) )
  RSE = SE / muValues * 100
  SEAndRSE = data.frame( "parametersValues" = muValues, "SE" = SE, "RSE" = RSE )
  rownames( SEAndRSE ) = rownames( fisherMatrix )
  SE = data.frame( "parametersValues" = muValues, "SE" = SE )
  RSE = data.frame( "parametersValues" = muValues, "RSE" = RSE )

  prop( fim, "fisherMatrix" ) = fisherMatrix
  prop( fim, "fixedEffects" ) = fixedEffects
  prop( fim, "shrinkage" ) = t(matrix( shrinkage, nrow = 1, dimnames = list( "Shrinkage", columnNamesMu ) ))
  prop( fim, "condNumberFixedEffects" ) = cond(fixedEffects)
  prop( fim, "SEAndRSE" ) = list( SE = SE, RSE = RSE, SEAndRSE = SEAndRSE )

  return( fim )
}

#' showFIM: show the Fim in the R console.
#' @name showFIM
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @return The fisherMatrix, fixedEffects, Determinant, condition numbers and D-criterion, Shrinkage and Parameters estimation
#' @export

method( showFIM, BayesianFim ) = function( fim ) {

  SEAndRSE = prop( fim, "SEAndRSE" )
  fisherMatrix = prop( fim, "fisherMatrix")
  fixedEffects = prop( fim, "fixedEffects")
  shrinkage = prop( fim, "shrinkage")
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )
  RSE = SEAndRSE$SE
  RSE = SEAndRSE$RSE
  SEAndRSE = SEAndRSE$SEAndRSE
  Dcriterion = Dcriterion( fim )

  cat("\n*************************************** \n")
  cat(" Bayesian Fisher Matrix \n" )
  cat("*************************************** \n\n")
  print( fisherMatrix )
  cat("\n*************************************** \n")
  cat(" Fixed effects \n" )
  cat("*************************************** \n\n")
  print( fixedEffects )
  cat("\n*********************************************** \n")
  cat(" Determinant, condition numbers and D-criterion \n" )
  cat("*********************************************** \n\n")
  cat( c( "Determinant:", as.numeric(det(fisherMatrix)) ), "\n")
  cat( c( "D-criterion:", as.numeric(Dcriterion) ), "\n")
  cat( c("Conditional number of the fixed effects:", as.numeric(condNumberFixedEffects) , "\n") )
  cat("\n*************************************** \n")
  cat(" Shrinkage \n" )
  cat("*************************************** \n\n")
  print( shrinkage )
  cat("\n*************************************** \n")
  cat(" Parameters estimation \n" )
  cat("*************************************** \n\n")
  print( SEAndRSE )
}

#' plotSEFIM: barplot for the SE
#' @name plotSEFIM
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the SE.
#' @export

method( plotSEFIM, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation ) {

  # get parameter names and model error
  parameters = prop( evaluation, "modelParameters" )
  modelError = prop( evaluation, "modelError" )

  # get SEAndRSE
  fim = prop( evaluation, "fim" )
  fim = setEvaluationFim( fim, evaluation )
  standardErrors = prop( fim, "SEAndRSE" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc", omega = "\u03c9\u00B2", sigma = "\u03c3" )

  parametersMu = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE) %>%       # Remove fixed omega parameters
    keep(~ pluck(.x, "distribution", "omega") != 0) %>%              # Keep non-zero omega
    map_chr("name")                               # Extract names

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )

  # data for plot
  data = data.frame( Parameter = c( parametersMu ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     SE = standardErrors$SE,
                     cat = paste0( "SE ", c(columnNamesMu ) ) )

  colnames( data ) = c("Parameter", "Value", "parametersValues", "SE", "cat")

  # bar plot of the plot SE
  plotSE = ggplot( data, aes( x = Parameter, y = SE ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    facet_wrap( ~factor( cat, levels =  paste0( "SE ", c( greeksLetterForCOnsole['mu'] ) ) ), scales = "free_x" ) +
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
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the RSE.
#' @export

method( plotRSEFIM, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation ) {

  # get parameter names and model error
  parameters = prop( evaluation, "modelParameters" )

  # get SEAndRSE
  fim = prop( evaluation, "fim" )
  fim = setEvaluationFim( fim, evaluation )
  standardErrors = prop( fim, "SEAndRSE" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc" )

  parametersMu = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE) %>%       # Remove fixed omega parameters
    keep(~ pluck(.x, "distribution", "omega") != 0) %>%              # Keep non-zero omega
    map_chr("name")                               # Extract names
  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )

  # data for plot
  data = data.frame( Parameter = c( parametersMu ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     RSE = standardErrors$RSE,
                     cat = paste0( "RSE ", c(columnNamesMu ) ) )

  colnames( data ) = c("Parameter", "Value", "parametersValues", "RSE", "cat")

  # bar plot of the plot SE
  plotRSE = ggplot( data, aes( x = Parameter, y = RSE ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    facet_wrap( ~factor( cat, levels =  paste0( "RSE ", c( greeksLetterForCOnsole['mu'] ) ) ), scales = "free_x" ) +
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust = 0.5),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
          strip.text.x = element_text(size=16))

  return( plotRSE )
}

#' plotShrinkage: plot the shrinkage values.
#' @name plotShrinkage
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the shrinkage.
#' @export

method( plotShrinkage, list( BayesianFim, PFIMProject ) ) = function(  fim, evaluation ) {

  # get parameter names and model error
  parameters = prop( evaluation, "modelParameters" )

  # get shrinkage
  shrinkage = prop( fim, "shrinkage" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "\u03bc", omega = "\u03c9\u00B2", sigma = "\u03c3" )

  parametersMu = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE) %>%       # Remove fixed omega parameters
    keep(~ pluck(.x, "distribution", "omega") != 0) %>%              # Keep non-zero omega
    map_chr("name")                               # Extract names

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )

  # data for plot
  data = data.frame( Parameter = c( parametersMu ), shrinkage = shrinkage )
  colnames( data ) = c("Parameter", "Shrinkage")

  # bar plot of the plot Sh
  plotSh = ggplot( data, aes( x = Parameter, y = Shrinkage ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust = 0.5),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
          strip.text.x = element_text(size=16))

  return( plotSh )
}

#' tablesForReport: generate the table for the report.
#' @name tablesForReport
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return fixedEffectsTable, FIMCriteriaTable, SEAndRSETable.
#' @export

method( tablesForReport, list( BayesianFim, PFIMProject ) ) = function( fim, evaluation ) {

  SEAndRSE = prop( fim, "SEAndRSE" )
  fisherMatrix = prop( fim, "fisherMatrix")
  fixedEffects = prop( fim, "fixedEffects")
  shrinkage = prop( fim, "shrinkage" )
  condNumberFixedEffects = prop( fim, "condNumberFixedEffects" )
  Dcriterion = Dcriterion( fim )
  determinant = det( fisherMatrix )
  SEAndRSE = SEAndRSE$SEAndRSE
  parameters = prop( evaluation, "modelParameters" )
  modelError = prop( evaluation, "modelError" )

  # Greek letter for column names
  greeksLetterForCOnsole = c( mu = "$\\mu_{", omega = "$\\omega^2_{", sigma = "${\\sigma_" )

  # define the name for the columns and rows for mu, omega and sigma
  columnNamesMu = parameters %>%
    discard(~ prop(.x, "fixedMu") == TRUE) %>%          # Remove fixed mu parameters
    keep(~ pluck(.x, "distribution", "mu") != 0) %>%                 # Keep non-zero mu
    discard(~ prop(.x, "fixedOmega") == TRUE) %>%       # Remove fixed omega parameters
    keep(~ pluck(.x, "distribution", "omega") != 0) %>%              # Keep non-zero omega
    map_chr("name") %>%                                 # Extract names
    map_chr(~ paste0(greeksLetterForCOnsole['mu'], .x,"}$")) # Add mu symbol

  fixedEffects = as.matrix( fixedEffects )
  colnames( fixedEffects ) = columnNamesMu
  rownames( fixedEffects ) = columnNamesMu

  fixedEffectsTable = fixedEffects %>% kbl() %>% kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  FIMCriteria = data.frame( Determinant = determinant, Dcriterion = Dcriterion, FixedEffects = condNumberFixedEffects )

  FIMCriteriaTable = kbl( FIMCriteria,
                          col.names = c("", "", "Fixed effects"),
                          align = c("c", "c", "c", "c"),
                          format = "html" ) %>%
    add_header_above(c( "Determinant" = 1, "D-criterion" = 1, "Condition number" = 1 )) %>%
    kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  # SEAndRSE table
  SEAndRSE = data.frame( c( columnNamesMu ), round(SEAndRSE,3), shrinkage )
  row.names( SEAndRSE ) = NULL

  SEAndRSETable = kbl(
    SEAndRSE,
    col.names = c("Parameters", "Parameter values", "SE", "RSE (%)", "Shrinkage"),
    align = c("c", "c", "c", "c") ) %>%
    kable_styling( bootstrap_options = c("hover"), full_width = FALSE, position = "center", font_size = 13 )

  fimTables = list( fixedEffectsTable = fixedEffectsTable, FIMCriteriaTable = FIMCriteriaTable, SEAndRSETable =  SEAndRSETable)

  return( fimTables )
}

#' generateReportEvaluation: generate the report for the model evaluation.
#' @name generateReportEvaluation
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report for the design evaluation.
#' @export

method( generateReportEvaluation, BayesianFim ) = function( fim, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "EvaluationBayesianFim.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath,
                     params = list( #plotOptions = "plotOptions",
                       #projectName = "projectName",
                       tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the MultiplicativeAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report for the design optimization.
#' @export

method( generateReportOptimization, list(BayesianFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationMultiplicativeAlgorithmBayesianFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{BayesianFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the FedorovWynnAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report for the design optimization.
#' @export

method( generateReportOptimization, list( BayesianFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationFedorovWynnAlgorithmBayesianFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

method( generateReportOptimization, list( BayesianFim, SimplexAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationSimplexAlgorithmBayesianFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

method( generateReportOptimization, list( BayesianFim, PSOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPSOAlgorithmBayesianFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

method( generateReportOptimization, list( BayesianFim, PGBOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPGBOAlgorithmBayesianFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )
}

