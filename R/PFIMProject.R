#' @description The class \code{PFIMProject} implements the PFIM project.
#' @title PFIMProject
#' @param name A string giving the name of the design evaluation.
#' @param modelEquations A list giving the model equations.
#' @param modelFromLibrary A list giving the model equations from the library of model.
#' @param modelParameters A list giving the model parameters.
#' @param modelError A list giving the model error.
#' @param optimizer A string giving the name of the optimization algorithm being used.
#' @param optimizerParameters A list giving the parameters of the optimization algorithm.
#' @param outputs A list giving the model outputs.
#' @param designs A list giving the designs to be evaluated.
#' @param fimType A string giving the type of Fim being evaluated.
#' @param fim A object \code{Fim} giving the Fim.
#' @param odeSolverParameters A list giving the atol and rtol parameters for the ode solver.
#' @include Fim.R
#' @export

PFIMProject = new_class("PFIMProject", package = "PFIM",
                        properties = list(
                          name = new_property(class_character, default = character(0)),
                          modelEquations = new_property(class_list, default = list()),
                          modelFromLibrary = new_property(class_list, default = list()),
                          modelParameters = new_property(class_list, default = list()),
                          modelError = new_property(class_list, default = list()),
                          optimizer = new_property(class_character, default = character(0)),
                          optimizerParameters = new_property(class_list, default = list()),
                          outputs = new_property(class_list, default = list()),
                          designs = new_property(class_list, default = list()),
                          fimType = new_property(class_character, default = character(0)),
                          fim = new_property(Fim, default = NULL),
                          odeSolverParameters = new_property(class_list, default = list())
                        ))

run = new_generic( "run", "pfimproject" )
defineFim = new_generic( "defineFim", c( "pfimproject" ) )
plotEvaluation = new_generic( "plotEvaluation", c( "pfimproject" ) )
plotSensitivityIndices = new_generic( "plotSensitivityIndices", c( "pfimproject" ) )
plotSE = new_generic( "plotSE", c( "pfimproject" ) )
plotRSE = new_generic( "plotRSE", c( "pfimproject" ) )
show = new_generic( "show", c( "pfimproject" ) )
Report = new_generic( "Report", c( "pfimproject" ) )

getFisherMatrix = new_generic( "getFisherMatrix", c( "pfimproject" ) )
getSE = new_generic( "getSE", c( "pfimproject" ) )
getRSE = new_generic( "getRSE", c( "pfimproject" ) )
getShrinkage = new_generic( "getShrinkage", c( "pfimproject" ) )
getDeterminant = new_generic( "getDeterminant", c( "pfimproject" ) )
getDcriterion = new_generic( "getDcriterion", c( "pfimproject" ) )
getCorrelationMatrix = new_generic( "getCorrelationMatrix", c( "pfimproject" ) )

defineModelType = new_generic( "defineModelType", c( "pfimproject" ) )
defineModelEquationsFromLibraryOfModel = new_generic( "defineModelEquationsFromLibraryOfModel", c( "pfimproject" ) )

#' define the type of Fisher information matrix: population, individual or Bayesian
#' @name defineFim
#' @param pfimproject An object \code{PFIMProject}.
#' @return An object \code{Fim}.
#' @export

method( defineFim, PFIMProject ) = function( pfimproject )
{
  fimType = prop( pfimproject, "fimType" )
  fim = switch( fimType,
                "population" = PopulationFim(),
                "individual" = IndividualFim(),
                "Bayesian" = BayesianFim(),
                fimType )
  return( fim )
}

#' defineModelType: define the class of the model to be evaluated.
#' @name defineModelType
#' @param pfimproject An object \code{PFIMProject} giving the evaluation to be run.
#' @return An object \code{Model} giving the model to be evaluated with its modelParameters, odeSolverParameters, modelError, modelEquations.
#' @export

method( defineModelType, PFIMProject ) = function( pfimproject )
{
  isModelODE = FALSE
  isModelInfusion = FALSE
  isDoseInEquation = FALSE
  isDoseInInitialConditions = FALSE

  equations = prop( pfimproject, "modelEquations" )
  parameters = prop( pfimproject, "modelParameters")

  #  check if model ode
  isModelODE = getListLastName( equations ) %>% str_detect("Deriv_") %>% any()

  # check if model analytic
  isModelAnalytic = !( getListLastName( equations ) %>% str_detect("Deriv_") %>% all() )

  # check if model steady state
  isTauInEquations = equations %>% map_lgl( ~ if ( is.list(.x) ) {
    any( map_lgl( .x, ~ str_detect( .x, "tau" ) ) )
  } else {
    str_detect( .x, "tau" )
  }) %>% any()

  # check if mode infusion
  isModelInfusion = equations %>% map_lgl( ~ if ( is.list(.x) ) {
    any( map_lgl( .x, ~ str_detect( .x, "Tinf_") ) )
  } else {
    str_detect( .x, "Tinf_" )
  }) %>% any()

  # check if dose in equations
  isDoseInEquation = equations %>% map_lgl( ~ if ( is.list(.x) ) {
    any( map_lgl( .x, ~ str_detect( .x, "dose_" ) ) )
  } else {
    str_detect( .x, "dose_" )
  }) %>% any()

  # check if dose in initial conditions
  initialConditions = pfimproject %>%
    pluck( "designs" ) %>%
    map(~ {
      arms = pluck( .x, "arms" )
      names( arms ) = map_chr( arms, ~ prop( .x, "name" ) )
      map( arms, ~ pluck( .x, "initialConditions" ) )
    }) %>%
    unlist()

  isDoseInInitialConditions = any( map_lgl( initialConditions, ~ str_detect( .x, "dose_" ) ) )

  # define the class of the model
  if ( isModelODE ){
    # ode model dose defined in equations
    if ( isDoseInEquation )
    {
      model = ModelODEDoseInEquations()
    }
    # ode model dose as cmpt
    if ( !isDoseInEquation )
    {
      model = ModelODEDoseNotInEquations()
    }
    # ode model & infusion & dose defined in equations
    if ( isModelInfusion & isDoseInEquation )
    {
      model = ModelODEInfusionDoseInEquation()
    }
    # ode model bolus & dose defined in initial conditions
    if ( isDoseInInitialConditions )
    {
      model = ModelODEBolus()
    }
  }

  if ( isModelAnalytic ){
    if ( !isModelInfusion & !isTauInEquations )
    {
      # model analytic
      model = ModelAnalytic()
    }
    if ( !isModelInfusion & isTauInEquations )
    {
      # ModelAnalyticSteadyState
      model = ModelAnalyticSteadyState()
    }
    if ( isModelInfusion & isDoseInEquation )
    {
      # model analytic with infusion
      model = ModelAnalyticInfusion()
    }
    if ( isModelInfusion & isDoseInEquation & isTauInEquations )
    {
      # model analytic with infusion
      model = ModelAnalyticInfusionSteadyState()
    }
  }

  # model parameters order by names
  prop( model, "modelParameters") = parameters
  prop( model, "odeSolverParameters") = prop( pfimproject, "odeSolverParameters" )
  prop( model, "modelError") = prop( pfimproject, "modelError" )
  prop( model, "modelEquations") = equations

  return( model )
}

#' defineModelEquationsFromLibraryOfModel: define the model equations giving the models in the library of models.
#' @name defineModelEquationsFromLibraryOfModel
#' @param pfimproject An object \code{PFIMProject} giving the evaluation to be run.
#' @return A list giving the model equations.
#' @export

method( defineModelEquationsFromLibraryOfModel, PFIMProject ) = function( pfimproject )
{
  equations = prop( pfimproject, "modelFromLibrary" )
  outputs = prop( pfimproject, "outputs" )

  pkModelName = equations[["PKModel"]]
  pdModelName = equations[["PDModel"]]

  pkModels = prop( LibraryOfPKModels, "models")
  prop( pfimproject, "modelEquations" ) = pkModels[[equations[["PKModel"]]]]

  pkModel = defineModelType( pfimproject )

  # pkpd model
  if ( !is.null( pdModelName ) ) {
    pdModels = prop( LibraryOfPDModels, "models")
    prop( pfimproject, "modelEquations" ) = pdModels[[equations[["PDModel"]]]]
    pdModel = defineModelType( pfimproject )
    pkpdModelEquations = definePKPDModel( pkModel, pdModel, pfimproject )
  }else{
    pkpdModelEquations = definePKModel( pkModel, pfimproject )
  }
  return( pkpdModelEquations )
}







