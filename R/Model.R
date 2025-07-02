#' @description The class \code{Model} represents and stores information for a model.
#' @title Model
#' @param name Character vector specifying the model name
#' @param modelParameters List of model parameters
#' @param samplings Numeric vector of sampling times
#' @param modelEquations List containing the model equations
#' @param wrapper Function wrapper for the model (default: function () NULL)
#' @param outputFormula List of output formulas
#' @param outputNames Character vector of output names
#' @param variableNames Character vector of variable names
#' @param outcomesWithAdministration Character vector of outcomes with administration
#' @param outcomesWithNoAdministration Character vector of outcomes without administration
#' @param modelError List defining the error model
#' @param odeSolverParameters List of ODE solver parameters
#' @param parametersForComputingGradient List of parameters for gradient computation
#' @param initialConditions Numeric vector of initial conditions
#' @param functionArguments Character vector of function arguments
#' @param functionArgumentsSymbol List of function argument symbols
#' @export

Model = new_class("Model", package = "PFIM",

                  properties = list(
                    name = new_property(class_character, default = character(0)),
                    modelParameters = new_property(class_list, default = list()),
                    samplings = new_property(class_numeric, default = numeric(0)),
                    modelEquations = new_property(class_list, default = list()),
                    wrapper = new_property(class_function, default = NULL ),
                    outputFormula = new_property(class_list, default = list()),
                    outputNames = new_property(class_character, default = character(0)),
                    variableNames = new_property(class_character, default = character(0)),
                    outcomesWithAdministration = new_property(class_character, default = character(0)),
                    outcomesWithNoAdministration = new_property(class_character, default = character(0)),
                    modelError = new_property(class_list, default = list()),
                    odeSolverParameters = new_property(class_list, default = list()),
                    parametersForComputingGradient = new_property(class_list, default = list()),
                    initialConditions = new_property(class_double, default = numeric(0)),
                    functionArguments = new_property(class_character, default = character(0)),
                    functionArgumentsSymbol = new_property(class_list, default = list())
                  ) )

defineModelWrapper = new_generic( "defineModelWrapper", c( "model" ) )
defineModelAdministration = new_generic( "defineModelAdministration", c( "model" ) )
evaluateModel = new_generic( "evaluateModel", c( "model" ) )
evaluateModelGradient = new_generic( "evaluateModelGradient", c( "model" ) )
evaluateModelVariance = new_generic( "evaluateModelVariance", c( "model"  ) )
evaluateInitialConditions = new_generic( "evaluateInitialConditions", c( "model"  ) )
finiteDifferenceHessian = new_generic( "finiteDifferenceHessian", c( "model" ) )
definePKModel = new_generic( "definePKModel", c( "pkModel", "pfimproject") )
definePKPDModel = new_generic( "definePKPDModel", c("pkModel", "pdModel", "pfimproject"))

#' finiteDifferenceHessian: compute the Hessian
#' @name finiteDifferenceHessian
#' @param model A object \code{Model} giving the model.
#' @return The model with the slots parametersForComputingGradient with XcolsInv, shifted, frac.
#' @export

method( finiteDifferenceHessian, Model ) = function( model ) {

  pars = map( prop( model, "modelParameters" ), ~ {
    distribution = prop( .x, "distribution")
    mu = prop( distribution, "mu" )
  })%>% unlist() %>% as.numeric()
  minAbsPar = 0
  .relStep = .Machine$double.eps^(1/3)
  npar = length(pars)
  incr = pmax(abs(pars), minAbsPar) * .relStep
  baseInd = diag(npar)
  frac = c(1, incr, incr^2)
  cols = list(0, baseInd, -baseInd)

  for ( i in seq_along(pars)[ -npar ] ) {
    cols = c( cols, list( baseInd[ , i ] + baseInd[ , -(1:i) ] ) )
    frac = c( frac, incr[ i ] * incr[ -(1:i) ] ) }

  indMat = do.call( "cbind", cols )
  shifted = pars + incr * indMat
  indMat = t( indMat )
  Xcols = list( 1, indMat, indMat^2 )

  for ( i in seq_along(pars)[ - npar ] ) {
    Xcols = c( Xcols, list( indMat[ , i ] * indMat[ , -(1:i) ] ) ) }

  XcolsInv = solve( do.call( "cbind", Xcols ) )
  prop( model, 'parametersForComputingGradient' ) = list( XcolsInv = XcolsInv, shifted = shifted, frac = frac )
  return( model )
}

#' evaluateModelVariance: evaluate the variance of the model
#' @name evaluateModelVariance
#' @param model A object \code{Model} giving the model.
#' @param arm A object \code{Arm} giving the arm
#' @return A list giving errorVariance  and sigmaDerivatives.
#' @export

method( evaluateModelVariance, Model ) = function( model, arm ) {

  modelErrors = prop( model, "modelError")
  evaluationModel = prop( arm, "evaluationModel")
  samplings = prop( model, "samplings" )
  outputNames = prop( model, "outputNames")

  # model derivatives
  evaluateErrorModelDerivatives = map( modelErrors, ~ {
    outcome = prop( .x, "output" )
    if ( outcome %in% outputNames ) {
      evaluateErrorModelDerivatives( .x, evaluationModel[[outcome]][,outcome] )
    } }) %>% compact() %>% set_names( outputNames )

  # compute error variance
  errorVariance = map( evaluateErrorModelDerivatives, ~{ bdiag( .x$errorVariance ) } ) %>% bdiag()

  # sigmaDerivatives
  totalNumberOfSamplings = map_int( evaluationModel, ~length( .x[["time"]] ) ) %>% sum()

  sigmaDerivatives = list()
  iter = 1
  for ( outputName in outputNames )
  {
    samplings = evaluationModel[[outputName]][["time"]]
    for( evaluateErrorModelDerivative in evaluateErrorModelDerivatives[[outputName]]$sigmaDerivatives )
    {
      sigmaDerivativesMatrix = matrix( 0, ncol = totalNumberOfSamplings, nrow = totalNumberOfSamplings )

      range = iter:( iter + length( samplings ) - 1 )
      sigmaDerivativesMatrix[ range, range ] = evaluateErrorModelDerivative
      sigmaDerivatives = c( sigmaDerivatives, list( sigmaDerivativesMatrix ) )
    }
    iter = iter + length( samplings )
  }
  return( list( errorVariance = errorVariance, sigmaDerivatives = sigmaDerivatives ) )
}

#' evaluateModelGradient: evaluate the gradient of the model
#' @name evaluateModelGradient
#' @param model An object \code{Model} that defines the model.
#' @param arm A object \code{Arm} giving the arm
#' @return A data frame that contains the gradient of the model.
#' @export

method( evaluateModelGradient, Model ) = function( model, arm ) {

  # parameters
  parameters = prop( model, "modelParameters" )
  parameterNames = map( parameters, ~ prop( .x, "name") ) %>% unlist()
  outputNames = prop( model, "outputNames")

  # parameters for computing the gradients
  parametersForComputingGradient = prop( model, "parametersForComputingGradient" )
  XcolsInv = parametersForComputingGradient$XcolsInv
  shiftedParameters = parametersForComputingGradient$shifted
  frac = parametersForComputingGradient$frac

  # evaluation for gradients computing
  evaluationModel = map( seq( ncol( shiftedParameters ) ), function( iter ) {
    parameters = map2( parameters, shiftedParameters[,iter], function( parameter, newMu ) {
      distribution = prop( parameter, "distribution" )
      prop( distribution, "mu" ) = newMu
      prop( parameter, "distribution" ) = distribution
      return( parameter )
    })
    #  evaluate model with updated parameter
    prop( model, "modelParameters" ) = parameters
    model = defineModelAdministration( model, arm )
    evaluateModel( model, arm )
  })

  # evaluate the gradients
  evaluationGradients = map( outputNames, function( outputName ) {
    output = map( evaluationModel, function( evaluation ) {
      evaluation[[outputName]][, 2] }) %>% as.data.frame() %>% t(.)
  })

  gradients = map( evaluationGradients, function( evaluationGradient ) {
    gradients = XcolsInv %*% evaluationGradient / frac
    gradients = t( gradients )[, 2:( 1 + length( parameters ) ) ]
    gradients = as.data.frame( matrix( gradients, ncol = length( parameters ) ) )
    colnames( gradients ) = parameterNames
    return( gradients)
  } ) %>% setNames( outputNames )

  return( gradients )
}

#' replaceVariablesLibraryOfModels: replace variable in the LibraryOfModels
#' @name replaceVariablesLibraryOfModels
#' @param text the text
#' @param old old string
#' @param new new string
#' @return text with new string
#' @export

replaceVariablesLibraryOfModels = function(text, old, new) {
  protected_terms = c("dose_", "Tinf_", "Emax")
  if(any(str_detect(old, fixed(protected_terms)))) {
    return(text)
  }
  protected_pattern = paste0("(?<!", paste0(protected_terms, collapse = "|"), ")")
  if(old == "RespPK") {
    str_replace_all(text, paste0(protected_pattern, old, "\\b"), new)
  } else {
    str_replace_all(text, regex(paste0("\\b", old, "\\b")), new)
  }
}

