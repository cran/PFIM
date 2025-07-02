#' @description
#' The class \code{PopulationFim} represents and stores information for the PopulationFim.
#' @title PopulationFim
#' @inheritParams Fim
#' @include Fim.R
#' @export

PopulationFim = new_class( "PopulationFim", package = "PFIM", parent = Fim )

#' evaluateFim: evaluation of the Fim
#' @name evaluateFim
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param model An object \code{Model} giving the model.
#' @param arm An object \code{Arm} giving the arm.
#' @return The object \code{IndividualFim} with the fisherMatrix and the shrinkage.
#' @export

method( evaluateFim, list( PopulationFim, Model, Arm ) ) = function( fim, model, arm ) {

  # variance for the FIM
  evaluateVarianceFIM = evaluateVarianceFIM( fim, model, arm )
  V = evaluateVarianceFIM$V
  MFVar = evaluateVarianceFIM$MFVar

  # fixed mu and fixed omega
  parameters = prop( model, "modelParameters")
  indexParameterNamesFixedMu = which( map_lgl( parameters, ~ prop( .x, "fixedMu" ) == TRUE ) )
  indexParameterNamesFixedOmega = which( map_lgl( parameters, ~ prop( .x, "fixedOmega" ) == TRUE ) )

  # omega = 0 ie fixed omega
  indicesOmegaZero = imap(parameters, ~ if (.x@distribution@omega == 0) .y) %>% compact() %>% unlist()
  indexParameterNamesFixedOmega = unique(c(indexParameterNamesFixedOmega, indicesOmegaZero ))

  indicesMuZero = imap(parameters, ~ if (.x@distribution@mu == 0) .y) %>% compact() %>% unlist()
  indexParameterNamesFixedMu = unique(c(indexParameterNamesFixedMu, indicesMuZero ))

  evaluationGradients =  prop( arm, "evaluationGradients" ) %>% reduce( rbind ) %>% as.matrix()

  # components of the Fim
  if ( length( indexParameterNamesFixedMu ) != 0 )
  {
    evaluationGradients = prop( arm, "evaluationGradients" ) %>%
      reduce( rbind ) %>%
      { .[, -c( indexParameterNamesFixedMu ) ] } %>%
      as.matrix()
  }

  MFbeta = t( evaluationGradients ) %*% chol2inv( chol( V ) ) %*% evaluationGradients

  if ( length( indexParameterNamesFixedOmega ) != 0 )
  {
    MFVar = MFVar[ -c( indexParameterNamesFixedOmega ), -c( indexParameterNamesFixedOmega ) ]
  }

  # Fisher matrix & adjust Fisher matrix with the number of individuals
  MFbeta = MFbeta * prop( arm, "size" )
  MFVar = MFVar * prop( arm, "size" )

  prop( fim, "fisherMatrix" ) = as.matrix( bdiag( MFbeta, MFVar ) )

  return( fim )
}

#' computeVMat
#' @name computeVMat
#' @param varParam1 varParam1
#' @param varParam2 varParam2
#' @param invCholV invCholV
#' @return VMat
#' @export

computeVMat = function( varParam1, varParam2, invCholV )
{
  1/2 * sum( diag( invCholV %*% varParam1 %*% invCholV %*% varParam2 ) )
}

#' evaluateVarianceFIM: evaluate the variance
#' @name evaluateVarianceFIM
#' @param arm A object of class \code{Arm} giving the arm.
#' @param model A object of class \code{Model} giving the model.
#' @param fim A object of class \code{PopulationFim} giving the Fim.
#' @return The matrices MFVar and V.
#' @export

method( evaluateVarianceFIM, list( PopulationFim, Model, Arm ) ) = function( fim, model, arm ) {

  parameters = prop( model, "modelParameters")
  parameterNames = map_chr( parameters, ~ prop( .x, "name" ) )

  evaluationVariance = prop( arm, "evaluationVariance")
  errorVariance = evaluationVariance$errorVariance
  sigmaDerivatives = evaluationVariance$sigmaDerivatives

  # matrix Omega
  omega = parameters %>% map_dbl( ~ pluck( .x, "distribution", "omega" ) ) %>% { (.^2) }

  # responses gradient adjusted
  gradient = prop( arm, "evaluationGradients" ) %>% reduce( rbind )

  adjustedGradient = map( parameters, function( parameter ) {
    distribution = prop( parameter, "distribution" )
    parameterName = prop( parameter, "name" )
    adjustGradient( distribution, gradient[,parameterName] )
  }) %>% reduce( rbind ) %>% t(.)

  # V matrix
  if ( length(omega)[1] == 1)
  { # cas omega is one scale
    adjustedGradient = matrix(adjustedGradient)
    V = omega * adjustedGradient %*% t( adjustedGradient ) + errorVariance
  }else
  {
    V = adjustedGradient %*% diag(omega) %*% t( adjustedGradient ) + errorVariance
  }

  # B Block
  dVdOmega = map( parameterNames, ~{
    iter = which( parameterNames == .x )
    dOmega = matrix( 0, ncol = length( parameters ), nrow = length( parameters ) )
    dOmega[iter, iter] = 1
    adjustedGradient %*% dOmega %*% t( adjustedGradient )
  }) %>% setNames( parameterNames )

  # V matrix
  dVdLambda = c( dVdOmega, sigmaDerivatives )
  invCholV = chol2inv( chol( V ) )
  VMat = outer( dVdLambda, dVdLambda, Vectorize( function(x, y) computeVMat( x, y, invCholV ) ) )
  MFVar = matrix( VMat, nrow = length( dVdLambda ), ncol = length( dVdLambda ) )

  return( list( MFVar = MFVar, V = V ) )
}

#' setOptimalArms: set the optimal arms of an optimization algorithm.
#' @name setOptimalArms
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the optimization algorithm.
#' @return The list optimalArms.
#' @export

method( setOptimalArms, list( PopulationFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

  # get the parameters of the MultiplicativeAlgorithm
  multiplicativeAlgorithmOutputs = prop( optimizationAlgorithm, "multiplicativeAlgorithmOutputs" )

  # get the inputs arms and FIMs for the MultiplicativeAlgorithm
  armFims = multiplicativeAlgorithmOutputs$armFims
  multiplicativeAlgorithmOutput = multiplicativeAlgorithmOutputs$multiplicativeAlgorithmOutput
  numberOfArms = multiplicativeAlgorithmOutputs$numberOfArms

  # get the parameters of the MultiplicativeAlgorithm
  weights = multiplicativeAlgorithmOutputs$optimalWeights
  weightsIndex = multiplicativeAlgorithmOutputs$weightsIndex

  #number of individual per group
  numberOfIndividualPerGroupTmp = numberOfArms*weights
  numberOfIndividualPerGroup = numberOfIndividualPerGroupTmp / sum( numberOfIndividualPerGroupTmp )*numberOfArms

  armList = list()

  for( index in weightsIndex )
  {
    arm = pluck( armFims[[index]], 1 )
    prop( arm, "size" ) = numberOfIndividualPerGroup[ index == weightsIndex ]
    prop( arm, "name" ) = paste0( "Arm", index )
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
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the optimization algorithm.
#' @return The list optimalArms.
#' @export

method( setOptimalArms, list( PopulationFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm ) {

  # get the parameters of the FedorovWynnAlgorithm
  FedorovWynnAlgorithmOutputs = prop( optimizationAlgorithm, "FedorovWynnAlgorithmOutputs" )

  numberOfIndividuals = FedorovWynnAlgorithmOutputs$numberOfIndividuals
  listArms = FedorovWynnAlgorithmOutputs$listArms

  optimalArms = imap(listArms, function(listArm, iter) {
    prop( listArm$arm, "size" ) = numberOfIndividuals[iter]
    prop( listArm$arm, "name" ) = paste0( "Arm", iter )
    return(listArm)
  })

  return( optimalArms )
}

#' setEvaluationFim: set the Fim results.
#' @name setEvaluationFim
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The object \code{PopulationFim} with its fisherMatrix, fixedEffects, shrinkage, condNumberFixedEffects, SEAndRSE.
#' @export

method( setEvaluationFim, PopulationFim ) = function( fim, evaluation ) {

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

  columnNamesOmega = parameters %>%
    keep( ~ prop( .x, "fixedOmega" ) == FALSE ) %>%
    keep( ~ .x@distribution@omega != 0 ) %>%
    map_chr( "name" ) %>%
    map_chr( ~ paste0( greeksLetterForCOnsole['omega'], .x ) )

  columnNamesSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  # get mu values
  muValues = parameters %>% keep( ~ prop( .x, "fixedMu" ) == FALSE ) %>%
    keep( ~ .x@distribution@mu != 0 ) %>%
    map_dbl( ~ pluck( .x, "distribution", "mu" ) )

  # get omega values
  omegaValues = parameters %>%
    keep( ~ prop( .x, "fixedOmega" ) == FALSE ) %>%
    keep( ~ .x@distribution@omega != 0 ) %>%
    map_dbl( ~ pluck( .x, "distribution", "omega" ) ) %>% { (.^2) }

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
  colnames( fisherMatrix ) = c( columnNamesMu, columnNamesOmega, columnNamesSigma )
  rownames( fisherMatrix ) = c( columnNamesMu, columnNamesOmega, columnNamesSigma )

  # fixed effects and variance effects
  fixedEffects = fisherMatrix[ columnNamesMu, columnNamesMu ]
  varianceEffects = fisherMatrix[ c( columnNamesOmega, columnNamesSigma ), c( columnNamesOmega, columnNamesSigma ) ]

  # compute SE ans RSE
  SE = sqrt( diag( chol2inv( chol( fisherMatrix ) ) ) )

  parametersValues = c( muValues, omegaValues, sigmaValues )
  RSE = SE / parametersValues * 100

  SEAndRSE = data.frame( "parametersValues" = parametersValues, "SE" = SE, "RSE" = RSE )
  SE = data.frame( "parametersValues" = parametersValues, "SE" = SE )
  RSE = data.frame( "parametersValues" = parametersValues, "RSE" = RSE )

  rownames( SEAndRSE ) = rownames( fisherMatrix )
  rownames( SE ) = rownames( fisherMatrix )
  rownames( RSE ) = rownames( fisherMatrix )

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

method( showFIM, PopulationFim ) = function( fim ) {

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

  determinant = det( fisherMatrix )

  cat("\n*************************************** \n")
  cat(" Population Fisher Matrix \n" )
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
  cat("\n********************************************* \n")
  cat(" Determinant, condition numbers and D-criterion  \n" )
  cat("*********************************************** \n\n")
  cat( c( "Determinant:", as.numeric(determinant) ), "\n")
  cat( c( "D-criterion:", as.numeric(Dcriterion) ), "\n")
  cat( c("Conditional number of the fixed effects:", as.numeric(condNumberFixedEffects) , "\n") )
  cat( c("Conditional number of the random effects:", as.numeric(condNumberVarianceEffects) , "\n") )
  cat("\n*************************************** \n")
  cat(" Parameters estimation \n" )
  cat("*************************************** \n\n")
  print( SEAndRSE )
}

#' plotSEFIM: barplot for the SE
#' @name plotSEFIM
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the SE.
#' @export

method( plotSEFIM, list( PopulationFim, PFIMProject ) ) = function( fim, evaluation ) {

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

  parametersOmega = parameters %>%
    keep( ~ prop( .x, "fixedOmega" ) == FALSE ) %>%
    keep( ~ .x@distribution@omega != 0 ) %>%
    map_chr( "name" )

  parametersSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )
  columnNamesOmega = parametersOmega %>% map_chr( ~  greeksLetterForCOnsole['omega']  )
  columnNamesSigma = parametersSigma %>% map_chr( ~  greeksLetterForCOnsole['sigma']  )

  # data for plot
  data = data.frame( Parameter = c( parametersMu, parametersOmega, parametersSigma ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     SE = standardErrors$SE,
                     cat = paste0( "SE ", c(columnNamesMu, columnNamesOmega, columnNamesSigma ) ) )

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
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return The bar plot of the RSE.
#' @export

method( plotRSEFIM, list( PopulationFim, PFIMProject ) ) = function( fim, evaluation ) {

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

  parametersOmega = parameters %>%
    keep( ~ prop( .x, "fixedOmega" ) == FALSE ) %>%
    keep( ~ .x@distribution@omega != 0 ) %>%
    map_chr( "name" )

  parametersSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_inter_", prop( .x ,"output" ) ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "_slope_", prop( .x ,"output" ) ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  columnNamesMu = parametersMu %>% map_chr(~  greeksLetterForCOnsole['mu'] )
  columnNamesOmega = parametersOmega %>% map_chr( ~  greeksLetterForCOnsole['omega']  )
  columnNamesSigma = parametersSigma %>% map_chr( ~  greeksLetterForCOnsole['sigma']  )

  # data for plot
  data = data.frame( Parameter = c( parametersMu, parametersOmega, parametersSigma ),
                     Value = standardErrors$SEAndRSE$parametersValues,
                     RSE = standardErrors$RSE,
                     cat = paste0( "RSE ", c(columnNamesMu, columnNamesOmega, columnNamesSigma ) ) )

  colnames( data ) = c("Parameter", "Value", "parametersValues", "RSE", "cat")

  # bar plot of the plot SE
  plotRSE = ggplot( data, aes( x = Parameter, y = RSE ) ) +
    geom_bar( stat = "identity", position = "dodge", show.legend = FALSE ) +
    facet_wrap( ~factor( cat, levels =  paste0( "RSE ", c( greeksLetterForCOnsole['mu'],  greeksLetterForCOnsole['omega'], greeksLetterForCOnsole["sigma"] ) ) ), scales = "free_x" ) +
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
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param evaluation An object \code{Evaluation} giving the evaluation of the model.
#' @return fixedEffectsTable, FIMCriteriaTable, SEAndRSETable.
#' @export

method( tablesForReport, list( PopulationFim, PFIMProject ) ) = function( fim, evaluation ) {

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

  columnNamesOmega = parameters %>%
    keep( ~ prop( .x, "fixedOmega" ) == FALSE ) %>%
    keep( ~  prop( prop(.x,"distribution"), "omega" ) != 0 ) %>%
    map_chr( "name" ) %>%
    map_chr( ~ paste0( greeksLetterForCOnsole['omega'], .x ,"}$" ) )

  columnNamesSigma = map( modelError, ~{
    sigma = character()
    if ( prop( .x, "sigmaInter" ) != 0 && prop( .x, "sigmaInterFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "{inter}}_{", prop( .x ,"output" ),"}$" ) )
    if ( prop( .x, "sigmaSlope" ) != 0 && prop( .x, "sigmaSlopeFixed" ) == FALSE ) sigma = c( sigma, paste0( greeksLetterForCOnsole["sigma"], "{slope}}_{", prop( .x ,"output" ),"}$" ) )
    return( sigma )
  }) %>% unlist()  %>% unname()

  fixedEffects = as.matrix( fixedEffects )
  colnames( fixedEffects ) = columnNamesMu
  rownames( fixedEffects ) = columnNamesMu
  colnames( varianceEffects ) = c( columnNamesOmega, columnNamesSigma )
  rownames( varianceEffects ) = c( columnNamesOmega, columnNamesSigma )

  fixedEffectsTable = fixedEffects %>%
    kbl() %>%
    kable_styling(
      bootstrap_options = c("hover"),
      full_width = FALSE,
      position = "center",
      font_size = 13 )

  varianceEffectsTable = varianceEffects %>%
    kbl() %>%
    kable_styling(
      bootstrap_options = c("hover"),
      full_width = FALSE,
      position = "center",
      font_size = 13 )

  FIMCriteria = data.frame( Determinant = determinant, Dcriterion = Dcriterion, FixedEffects = condNumberFixedEffects, VarianceEffects = condNumberVarianceEffects )

  FIMCriteriaTable = kbl(
    FIMCriteria,
    col.names = c("", "", "Fixed effects", "Variance effects"),
    align = c("c", "c", "c", "c"),
    format = "html" ) %>%
    add_header_above(c(
      "Determinant" = 1,
      "D-criterion" = 1,
      "Condition number" = 2
    )) %>%
    kable_styling(
      bootstrap_options = c("hover"),
      full_width = FALSE,
      position = "center",
      font_size = 13 )

  # SEAndRSE table
  SEAndRSE = data.frame( c( columnNamesMu, columnNamesOmega, columnNamesSigma ), round(SEAndRSE,3) )
  row.names( SEAndRSE ) = NULL

  SEAndRSETable = kbl(
    SEAndRSE,
    col.names = c("Parameters", "Parameter values", "SE", "RSE (%)"),
    align = c("c", "c", "c", "c") ) %>%
    kable_styling(
      bootstrap_options = c("hover"),
      full_width = FALSE,
      position = "center",
      font_size = 13 )

  fimTables = list( fixedEffectsTable = fixedEffectsTable, varianceEffectsTable = varianceEffectsTable, FIMCriteriaTable = FIMCriteriaTable, SEAndRSETable =  SEAndRSETable)

  return( fimTables )
}

#' generateReportEvaluation: generate the report for the model evaluation.
#' @name generateReportEvaluation
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report for the model evaluation.
#' @export

method( generateReportEvaluation, PopulationFim ) = function( fim, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "EvaluationPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list( tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{MultiplicativeAlgorithm} giving the MultiplicativeAlgorithm.
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( PopulationFim, MultiplicativeAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationMultiplicativeAlgorithmPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list( tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{FedorovWynnAlgorithm} giving the FedorovWynnAlgorithm
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( PopulationFim, FedorovWynnAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationFedorovWynnAlgorithmPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list( tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{SimplexAlgorithm} giving the SimplexAlgorithm
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( PopulationFim, SimplexAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationSimplexAlgorithmPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list(
    #plotOptions = "plotOptions", #projectName = "projectName",
    tablesForReport = "tablesForReport" ) )

}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{PSOAlgorithm} giving the PSOAlgorithm
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( PopulationFim, PSOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPSOAlgorithmPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list( tablesForReport = "tablesForReport" ) )
}

#' generateReportOptimization: generate the report for the design optimization.
#' @name generateReportOptimization
#' @param fim An object \code{PopulationFim} giving the Fim.
#' @param optimizationAlgorithm An object \code{PGBOAlgorithm} giving the PGBOAlgorithm
#' @param tablesForReport The output list giving by the method tablesForReport.
#' @return The html report.
#' @export

method( generateReportOptimization, list( PopulationFim, PGBOAlgorithm ) ) = function( fim, optimizationAlgorithm, tablesForReport ) {

  path = system.file(package = "PFIM")
  path = paste0( path, "/rmarkdown/templates/skeleton/" )
  nameInputFile = paste0( path, "OptimizationPGBOAlgorithmPopulationFIM.rmd" )

  rmarkdown::render( input = nameInputFile, output_file = outputFile, output_dir = outputPath, params = list( tablesForReport = "tablesForReport" ) )
}
