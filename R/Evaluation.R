#' @description
#' The class \code{Evaluation} represents and stores information for the evaluation of a design
#' @title Evaluation
#' @inheritParams PFIMProject
#' @param evaluationDesign A list giving the evaluation of the design.
#' @include PFIMProject.R
#' @export

Evaluation = new_class("Evaluation", package = "PFIM", parent = PFIMProject,
                       properties = list(
                         evaluationDesign = new_property(class_list, default = list()),
                         name = new_property(class_character, default = character(0)),
                         modelParameters = new_property(class_list, default = list()),
                         modelEquations = new_property(class_list, default = list()),
                         modelFromLibrary  = new_property(class_list, default = list()),
                         modelError = new_property(class_list, default = list()),
                         designs = new_property(class_list, default = list()),
                         outputs = new_property(class_list, default = list()),
                         fimType = new_property(class_character, default = character(0)),
                         odeSolverParameters =  new_property(class_list, default = list())
                       ),
                       constructor = function(evaluationDesign = list(),
                                              name = character(0),
                                              modelParameters = list(),
                                              modelEquations = list(),
                                              modelFromLibrary = list(),
                                              modelError = list(),
                                              designs = list(),
                                              outputs = list(),
                                              fimType = character(0),
                                              odeSolverParameters = list() ) {
                         new_object(
                           .parent = PFIMProject(),
                           evaluationDesign = evaluationDesign,
                           name = name,
                           modelParameters = modelParameters,
                           modelEquations = modelEquations,
                           modelFromLibrary = modelFromLibrary,
                           modelError = modelError,
                           designs = designs,
                           outputs = outputs,
                           fimType = fimType,
                           odeSolverParameters = odeSolverParameters
                         )
                       })

getFim = new_generic( "getFim", c( "evaluation" ) )

#' getListLastName: routine to get the names of last element of a nested list.
#' @name getListLastName
#' @param list The list to be used.
#' @return The names of last element.
#' @export

# names of last element nested list
getListLastName = function( list ) {
  if ( is.list( list ) ) {
    result = map( list, getListLastName )
    result = unlist( result, recursive = FALSE )
    if ( length( result ) == 0 ) {
      return( names( list ) )
    } else {
      return( result )
    }
  }
}

#' run: run the evaluation of a design.
#' @name run
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The object \code{Evaluation} giving the design evaluation.
#' @export

# run the model evaluation
method( run, Evaluation ) = function( pfimproject )
{
  # define the model equations if model from library of model
  modelFromLibraryOfModel = prop( pfimproject, "modelFromLibrary" )

  if ( length( modelFromLibraryOfModel ) != 0 ) {
    prop( pfimproject, "modelEquations" ) = defineModelEquationsFromLibraryOfModel( pfimproject ) }

  # define the class of the model
  model = defineModelType( pfimproject )
  # define the parameters for computing the gradients
  model = finiteDifferenceHessian( model )
  # define the function to evaluate from the model
  model = defineModelWrapper( model, pfimproject )
  # define the type of the Fim
  fim = defineFim( pfimproject )

  # evaluate the designs
  designs = prop( pfimproject, "designs" )
  evaluationDesign = map( designs, ~ evaluateDesign( .x, model, fim ) )
  prop( pfimproject, "evaluationDesign" ) = evaluationDesign

  # results of the evaluation
  designNumber = 1
  evaluationDesign = pluck( evaluationDesign, designNumber )
  prop( pfimproject, "fim" ) = prop( evaluationDesign, "fim" )

  return( pfimproject )
}

#' getFim: get the Fisher matrix.
#' @name getFim
#' @param evaluation An object \code{Evaluation} giving the evaluation to be run.
#' @return The matrices fisherMatrix, fixedEffects, varianceEffects.
#' @export

method( getFim, Evaluation ) = function( evaluation )
{
  fim = prop( evaluation, "fim" )
  fisherMatrix = prop( fim, "fisherMatrix" )
  fixedEffects = prop( fim, "fixedEffects" )
  varianceEffects = prop( fim, "varianceEffects" )

  return( list( fisherMatrix = fisherMatrix, fixedEffects = fixedEffects, varianceEffects = varianceEffects ) )
}

#' getFisherMatrix: display the Fisher matrix components
#' @name getFisherMatrix
#' @param evaluation An object \code{Evaluation} giving the evaluation to be run.
#' @return The matrices fisherMatrix, fixedEffects, varianceEffects.
#' @export

method( getFisherMatrix, Evaluation ) = function( pfimproject )
{
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  fisherMatrix = prop( fim, "fisherMatrix" )
  fixedEffects = prop( fim, "fixedEffects" )
  varianceEffects = prop( fim, "varianceEffects" )

  return( list( fisherMatrix = fisherMatrix, fixedEffects = fixedEffects, varianceEffects = varianceEffects ) )
}

#' show: show the evaluation in the R console.
#' @name show
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The show of the evaluation of the design.
#' @export

method( show, Evaluation ) = function( pfimproject )
{
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  showFIM( fim )
}

#' plotEvaluation: plots for the evaluation of the model responses.
#' @name plotEvaluation
#' @param pfimproject A object \code{PFIMProject}.
#' @param plotOptions A list giving the plot options.
#' @return All the plots for the evaluation of the model responses.
#' @export

method( plotEvaluation, Evaluation ) = function( pfimproject, plotOptions )
{
  designs = prop( pfimproject, "designs" )
  model = defineModelType( pfimproject ) %>% finiteDifferenceHessian() %>% defineModelWrapper( pfimproject )
  fim = defineFim( pfimproject )
  design = pluck( designs, 1 )
  designName = prop( design, "name" )
  arms = prop( design, "arms" )
  # generate and print all plots
  allPlots = map( arms, ~ processArmEvaluationResults( .x, model, fim, designName, plotOptions ) )
  allPlots = setNames( list( allPlots %>% map( ~ .x[[designName]] ) %>% flatten() ), designName )
  return( allPlots )
}

#' plotSensitivityIndices:  plots for the evaluation of the gradient of the model responses.
#' @name plotSensitivityIndices
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @param plotOptions A list giving the plot options.
#' @return All the plots for the evaluation of the gradient of the model responses.
#' @export

method( plotSensitivityIndices, Evaluation ) = function( pfimproject, plotOptions )
{
  designs = prop( pfimproject, "designs" )
  model = defineModelType( pfimproject ) %>% finiteDifferenceHessian() %>% defineModelWrapper( pfimproject )
  fim = defineFim( pfimproject )
  design = pluck( designs, 1 )
  designName = prop( design, "name" )
  arms = prop( design, "arms" )

  # generate and print all plots
  allPlots = map( arms, ~ processArmEvaluationSI( .x, model, fim, designName, plotOptions ) )
  allPlots = setNames( list( allPlots %>% map( ~ .x[[designName]] ) %>% flatten() ), designName )
  return( allPlots )
}

#' plotSE: bar plot of the SE.
#' @name plotSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The bar plot of the SE.
#' @export

# plot SE  from evaluation
method( plotSE, Evaluation ) = function( pfimproject )
{
  # set the FIM and plot SE
  fim = prop( pfimproject, "fim" )
  plotSE = plotSEFIM( fim, pfimproject )
  return( plotSE )
}

#' plotRSE: bar plot of the RSE.
#' @name plotRSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The bar plot of the RSE.
#' @export

method( plotRSE, Evaluation ) = function( pfimproject )
{
  # set the FIM and plot RSE
  fim = prop( pfimproject, "fim" )
  plotRSE = plotRSEFIM( fim, pfimproject )
  return( plotRSE )
}

#' getSE: get the SE
#' @name getSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The SE.
#' @export

method( getSE, Evaluation ) = function( pfimproject )
{
  # set the FIM and plot SE
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  SEAndRSE = prop( fim, "SEAndRSE" )
  SE = SEAndRSE$SE
  return( SE )
}

#' getRSE: get the RSE
#' @name getRSE
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The RSE
#' @export

method( getRSE, Evaluation ) = function( pfimproject )
{
  # set the FIM and plot SE
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  SEAndRSE = prop( fim, "SEAndRSE" )
  RSE = SEAndRSE$RSE
  return( RSE )
}

#' getShrinkage: get the shrinkage
#' @name getShrinkage
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The shrinkage
#' @export

method( getShrinkage, Evaluation ) = function( pfimproject )
{
  # set the FIM and plot SE
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  shrinkage = prop( fim, "shrinkage" )
  return( shrinkage )
}

#' getDeterminant: get the determinant
#' @name getDeterminant
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The determinant
#' @export

method( getDeterminant, Evaluation ) = function( pfimproject )
{
  fisherMatrix = getFisherMatrix( pfimproject )
  return( det( fisherMatrix$fisherMatrix ) )
}

#' getDcriterion : get the Dcriterion
#' @name getDcriterion
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The Dcriterion
#' @export

method( getDcriterion, Evaluation ) = function( pfimproject )
{
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  return( Dcriterion( fim ) )
}

#' getCorrelationMatrix : get the correlation matrix
#' @name getCorrelationMatrix
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation.
#' @return The Dcriterion
#' @export

method( getCorrelationMatrix, Evaluation ) = function( pfimproject )
{
  fisherMatrix = getFisherMatrix( pfimproject )
  fisherMatrix = fisherMatrix$fisherMatrix

  return( cor( fisherMatrix ) )
}

#' Report: generate the report.
#' @name Report
#' @param pfimproject A object \code{PFIMProject} giving the Evaluation or Optimization.
#' @param outputPath A string giving the path where the output are saved.
#' @param outputFile A string giving the name of the output file.
#' @param plotOptions A list giving the plot options.
#' @return The html report of the design evaluation or optimization.
#' @export

method( Report, Evaluation ) = function( pfimproject, outputPath, outputFile, plotOptions  )
{
  # projectName
  projectName = prop( pfimproject, "name" )

  # outputs
  evaluationOutputs = prop( pfimproject , "outputs" )

  # model
  model = defineModelType( pfimproject  )
  modelEquations = prop( model, "modelEquations" )

  # model error table
  modelError = prop( pfimproject, "modelError" )
  modelError = map( modelError, getModelErrorData )
  modelErrorData = map_df( modelError, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( modelErrorData ) = c( "Output", "Type", "$\\sigma_{slope}$", "$\\sigma_{inter}$" )

  modelErrorTable = kbl( modelErrorData, align = c( "c","c","c","c" ) ) %>%
    kable_styling( bootstrap_options = c(  "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # model parameters table
  modelParameters = prop( pfimproject, "modelParameters" )
  modelParameters = map( modelParameters, getModelParametersData )
  modelParametersData = map_df( modelParameters, ~ as.data.frame(.x, stringsAsFactors = FALSE ) )
  colnames( modelParametersData ) = c("Parameter","$\\mu$","$\\omega$","Distribution", paste0("$\\mu$"," fixed"), paste0("$\\omega$"," fixed"))

  modelParametersTable = kbl( modelParametersData, align = c( "l","l","l","c","c","c" ) ) %>%
    kable_styling( bootstrap_options = c( "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # arms
  designs = prop( pfimproject, "designs" )
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
    kable_styling( bootstrap_options = c( "hover" ), full_width = FALSE, position = "center", font_size = 13 )

  # Fisher matrix and SE
  fim = prop( pfimproject, "fim" )
  fim = setEvaluationFim( fim, pfimproject )
  fimInitialDesignTable = tablesForReport( fim, pfimproject )

  # plotsEvaluation & plotSensitivityIndices
  plotsEvaluation = plotEvaluation( pfimproject, plotOptions )
  plotSensitivityIndices = plotSensitivityIndices( pfimproject, plotOptions )
  plotSE = plotSE( pfimproject )
  plotRSE = plotRSE( pfimproject )

  # tablesForReport
  tablesForReport = list(
    evaluationOutputs = evaluationOutputs,
    modelEquations = modelEquations,
    modelErrorTable = modelErrorTable,
    modelParametersTable = modelParametersTable,
    administrationTable = administrationTable,
    initialDesignTable = initialDesignTable,
    fimInitialDesignTable = fimInitialDesignTable,
    plotsEvaluation = plotsEvaluation,
    plotSensitivityIndices = plotSensitivityIndices,
    plotSE = plotSE,
    plotRSE = plotRSE,
    fim = fim,
    pfimproject = pfimproject,
    projectName = projectName )

  # generate the report
  generateReportEvaluation( fim, tablesForReport )
}










