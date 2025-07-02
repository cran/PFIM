#' @description The class \code{ModelODEDoseNotInEquations} is used to defined a ModelODEDoseNotInEquations
#' @title ModelODEDoseNotInEquations
#' @inheritParams ModelODE
#' @param modelODEDoseInEquations An object \code{modelODEDoseInEquations}.
#' @param solverInputs A list giving the solver inputs.
#' @include ModelODE.R
#' @export

ModelODEDoseInEquations = new_class( "ModelODEDoseInEquations",
                                     package = "PFIM",
                                     parent = ModelODE,

                                     properties = list(
                                       modelODEDoseInEquations = new_property(class_function, default = NULL ),
                                       solverInputs = new_property(class_list, default = list())))

#' defineModelWrapper: define the model wrapper for the ode solver
#' @name defineModelWrapper
#' @param model An object of class \code{ModelODEDoseInEquations} that defines the model.
#' @param evaluation An object of class Evaluation that defines the evaluation
#' @return The model with the updated slots.

method( defineModelWrapper, ModelODEDoseInEquations ) = function( model, evaluation ) {

  # names of the equations and the variables
  equations = prop( evaluation, "modelEquations" )
  variableNames = str_remove( names( equations ), "Deriv_" )
  variableNamesDerivatives = paste( names( equations ), collapse = ", " )

  # outcomes with administration
  outcomesWithAdministration = evaluation %>%
    pluck( "designs" ) %>%
    map( ~ pluck( .x, "arms" ) ) %>%
    unlist() %>%
    map( ~ pluck( .x, "administrations" ) ) %>%
    unlist()%>%
    map( ~ pluck( .x, "outcome" ) ) %>%
    unlist()

  # arguments for the function
  parameters = prop( evaluation, "modelParameters" )
  parameterNames = map_chr( parameters, "name" )
  doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
  timeNames = paste( "t_", outcomesWithAdministration, sep = "" )

  functionArguments = c( doseNames, parameterNames, variableNames, timeNames )
  functionArguments = unique( functionArguments )
  functionArgumentsSymbol = map( functionArguments, ~ as.symbol(.x) )

  # dans les equations remplace t par t_outcomeName
  equations = map( equations, ~ {
    for ( outcomeWithAdministration in outcomesWithAdministration ) {
      if ( str_detect(.x, outcomeWithAdministration ) ) {
        .x = str_replace_all( .x, "\\bt\\b", paste0("t_", outcomeWithAdministration ) )
      }
    }
    return(.x)
  })

  # create the body function
  equationsBody = map_chr( names( equations ), ~ sprintf( "%s = %s", .x, equations[[.x]] ) )
  functionBody = paste( equationsBody, collapse = "\n" )
  functionBody = sprintf( paste( "%s\nreturn(list(c(", variableNamesDerivatives, ")))", collapse = ", " ), functionBody )
  functionDefinition = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", " ), functionBody )

  # define the model
  outputs = prop( evaluation, "outputs")
  prop( model, "outputFormula") = outputs
  prop( model, "outputNames") = names( outputs )
  prop( model, "outcomesWithAdministration") = outcomesWithAdministration
  prop( model, "wrapper" ) = eval( parse( text = functionDefinition ) )
  prop( model, "functionArguments" ) = functionArguments
  prop( model, "functionArgumentsSymbol" ) = functionArgumentsSymbol

  return( model )
}

#' defineModelAdministration: define the administration
#' @name defineModelAdministration
#' @param model An object of class \code{ModelODEDoseInEquations} that defines the model.
#' @param arm An object of class \code{Arm} that defines the arm.
#' @return The model with samplings, solverInputs
#' @export

method( defineModelAdministration, ModelODEDoseInEquations ) = function( model, arm ) {

  # model wrapper function
  wrapper = prop( model, "wrapper")
  # model parameters
  parameters = prop( model, "modelParameters" )
  # administrations and outcome
  administrations = prop( arm, "administrations" )
  # sampling times
  samplingTimes = prop( arm, "samplingTimes" )
  # args for model evaluation
  functionArguments = prop( model, "functionArguments" )
  functionArgumentsSymbols = prop( model, "functionArgumentsSymbol" )
  # model outputs
  outputNames = prop( model, "outputNames" )
  outputFormula = prop( model, "outputFormula" )
  outputFormula = map( outputFormula, ~ parse( text=.x ) )
  outcomesWithAdministration =  prop( model, "outcomesWithAdministration" )

  # define the samplings for all outcomes
  samplings = map( samplingTimes, ~ prop( .x, "samplings" ) ) %>% unlist() %>% sort() %>% unique()
  samplings = unique( c( 0, samplings ) )

  # define solver inputs
  solverInputs = map( administrations, ~ {

    outcome = prop( .x, "outcome" )
    timeDose = prop( .x, "timeDose" )
    tau = prop( .x, "tau" )
    dose = prop( .x, "dose" )

    # repeated dose / one dose /  multiple dose
    if ( tau != 0 ) {
      administrationTime = seq( 0, max( samplings ), tau )
      dose = rep( dose, length( administrationTime ) )
    }
    else if ( length(timeDose) == 1 ) {
      administrationTime = rep( timeDose, 2 )
    } else {
      administrationTime = c( timeDose, max( samplings ) )
    }
    # matrix of time dose interval begin / end
    administrationTime = cbind( administrationTime[ -length( administrationTime ) ], administrationTime[-1] )
    setNames( list( list( administrationTime = administrationTime, dose = dose ) ), outcome )
  }) %>% flatten()

  # evaluate the initial conditions
  initialConditions = evaluateInitialConditions( model, arm )

  # Assign the values to the parameters in the current environment
  mu = set_names(
    map(parameters, ~ .x@distribution@mu),
    map(parameters, ~ .x@name)
  )

  list2env( mu, envir = environment() )

  # function evaluation model ODE
  modelODEDoseInEquations = function( samplingTimes, initialConditions, solverInputs )
  {
    with( c( samplingTimes, initialConditions, solverInputs ),{

      for ( outcomeWithAdministration in outcomesWithAdministration )
      {
        dose = solverInputs[[outcomeWithAdministration]]$dose
        administrationTime = solverInputs[[outcomeWithAdministration]]$administrationTime
        indexTime = which( samplingTimes > administrationTime[, 1] & samplingTimes <= administrationTime[, 2] )
        intervalTimeDose = administrationTime[indexTime, ]
        timeDose = samplingTimes - intervalTimeDose[1]

        # assign doses
        if ( length( indexTime ) == 0 ) {
          # unique dose
          assign( paste0( "dose_", outcomeWithAdministration ), dose[1] )
        } else {
          # multiple doses
          assign( paste0( "dose_", outcomeWithAdministration ), dose[indexTime] )
        }
        # assign time dose
        if ( timeDose >= 0 & length( intervalTimeDose ) !=0 )
        {
          assign( paste0( "t_", outcomeWithAdministration ), timeDose )
        }else{
          assign( paste0( "t_", outcomeWithAdministration ), samplingTimes )
        }
      }
      # evaluate model and model outputs
      evaluationModel = do.call( wrapper, setNames( functionArgumentsSymbols, functionArguments ) )
      evaluationOutputs = map( outputFormula, ~ eval(.) )
      return( c( evaluationModel, evaluationOutputs ) )
    })
  }

  # set the model
  prop( model, "initialConditions" ) = initialConditions
  prop( model, "samplings" ) = samplings
  prop( model, "modelODEDoseInEquations" ) = modelODEDoseInEquations
  prop( model, "solverInputs" ) = solverInputs

  return( model )
}

#' evaluateModel: evaluate the model
#' @name evaluateModel
#' @param model An object of class \code{ModelODEDoseInEquations} that defines the model.
#' @param arm An object of class \code{Arm} that defines the arm.
#' @return A list of dataframes that contains the results for the evaluation of the model.
#' @export

method( evaluateModel, ModelODEDoseInEquations ) = function( model, arm ) {

  initialConditions = prop( model, "initialConditions" )
  samplings = prop( model, "samplings" )

  modelODEDoseInEquations = prop( model, "modelODEDoseInEquations" )
  solverInputs = prop( model, "solverInputs" )
  odeSolverParameters = prop( model, "odeSolverParameters" )
  atol = odeSolverParameters$atol
  rtol = odeSolverParameters$rtol
  samplingTimes = prop( arm, "samplingTimes" )
  outputNames = prop( model, "outputNames" )

  # model evaluation
  evaluationModelTmp = ode( initialConditions, samplings, modelODEDoseInEquations, solverInputs, atol = atol, rtol = rtol )
  evaluationModelTmp = evaluationModelTmp %>% data.frame()

  # filter sampling time
  samplings = map( samplingTimes, ~ prop( .x, "samplings" ) )%>% set_names( outputNames )

  evaluationModel = list()
  for ( outputName in outputNames )
  {
    time = evaluationModelTmp$time %in% samplings[[outputName]]
    evaluationModel[[outputName]] = evaluationModelTmp[ time , c( "time", outputName ) ]
  }

  return( evaluationModel )
}

#' definePKModel: define a PK model from library of model
#' @name definePKModel
#' @param pkModel An object of class \code{ModelODEDoseInEquations} that defines the PK model.
#' @param pfimproject An object of class \code{PFIMProject} that defines the pfimproject.
#' @export

method( definePKModel, list( ModelODEDoseInEquations, PFIMProject ) ) = function( pkModel, pfimproject ) {
  pkModelEquations = prop( pkModel, "modelEquations")
  return( pkModelEquations )
}






