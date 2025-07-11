#' ModelAnalyticInfusionSteadyState
#' @description The class \code{ModelAnalyticInfusionSteadyState} is used to defined an analytic model in infusion steady state.
#' @title ModelAnalyticInfusionSteadyState
#' @inheritParams ModelInfusion
#' @param wrapperModelAnalyticInfusion Wrapper for the ode solver.
#' @param functionArgumentsModelAnalyticInfusion A list giving the functionArguments of the wrapper for the analytic model in infusion.
#' @param functionArgumentsSymbolModelAnalyticInfusion  A list giving the functionArgumentsSymbol of the wrapper for the analytic model in infusion.
#' @param solverInputs A list giving the solver inputs.
#' @include ModelInfusion.R
#' @export

ModelAnalyticInfusionSteadyState = new_class( "ModelAnalyticInfusionSteadyState", package = "PFIM", parent = ModelInfusion,

                                              properties = list( wrapperModelAnalyticInfusion = new_property(class_list, default = list()),
                                                                 functionArgumentsModelAnalyticInfusion = new_property(class_list, default = list()),
                                                                 functionArgumentsSymbolModelAnalyticInfusion = new_property(class_list, default = list()),
                                                                 solverInputs = new_property(class_list, default = list()) ) )

#' defineModelWrapper: define the model wrapper for the ode solver
#' @name defineModelWrapper
#' @param model An object of class \code{ModelAnalyticInfusionSteadyState} that defines the model.
#' @param evaluation An object of class Evaluation that defines the evaluation
#' @return The model with wrapperModelAnalyticInfusion, functionArgumentsModelAnalyticInfusion, functionArgumentsSymbolModelAnalyticInfusion, outputNames, outcomesWithAdministration

method( defineModelWrapper, ModelAnalyticInfusionSteadyState ) = function( model, evaluation ) {

  # outcomes with administration
  outcomesWithAdministration = evaluation %>%
    pluck( "designs" ) %>%
    map( ~ pluck( .x, "arms" ) ) %>%
    unlist() %>%
    map( ~ pluck( .x, "administrations" ) ) %>%
    unlist()%>%
    map( ~ pluck( .x, "outcome" ) ) %>%
    unlist() %>% unique()

  # arguments for the function
  parameters = prop( evaluation, "modelParameters" )
  parameterNames = map_chr( parameters, "name" )
  doseNames = paste( "dose_", outcomesWithAdministration, sep = "" )
  timeNames = paste( "t_", outcomesWithAdministration, sep = "" )
  TinfNames = paste( "Tinf_", outcomesWithAdministration, sep = "" )
  tauName = "tau"

  # names of the equations with admin and no admin
  equations = prop( evaluation, "modelEquations" )
  equationsDuringInfusion = equations$duringInfusion
  equationsAfterInfusion = equations$afterInfusion

  # outputs
  outputs = names( equationsDuringInfusion )
  outputNames = unlist( outputs )

  equationsDuringInfusionWithAdmin = equationsDuringInfusion[ names( equationsDuringInfusion ) %in% outcomesWithAdministration ]
  equationsAfterInfusionWithAdmin  = equationsAfterInfusion[ names( equationsAfterInfusion ) %in% outcomesWithAdministration ]
  equationsDuringInfusionWithNoAdmin = equationsDuringInfusion[ !( names( equationsDuringInfusion ) %in% outcomesWithAdministration ) ]
  equationsAfterInfusionWithNoAdmin  = equationsAfterInfusion[ !( names( equationsAfterInfusion ) %in% outcomesWithAdministration ) ]

  # outputForEvaluation
  outputsForEvaluation = prop( evaluation, "outputs" )
  # pk model
  if ( length(outputsForEvaluation ) == 1 )
  {
    outputAdmin = unlist(outputsForEvaluation[1])
    outputNoAdmin = c()
    # pkpd model
  }else if ( length(outputsForEvaluation ) == 2 )
  {
    outputAdmin = unlist(outputsForEvaluation[1])
    outputNoAdmin = unlist(outputsForEvaluation[2])
  }

  # args for function DuringInfusion
  functionArguments = unique( c( doseNames, TinfNames, outcomesWithAdministration, parameterNames, timeNames, tauName ) )
  functionArgumentsSymbol = map( functionArguments, ~ as.symbol(.x) )

  # create function DuringInfusion
  equationsBodyDuringInfusionWithAdmin = map_chr( names( equationsDuringInfusionWithAdmin ), ~ sprintf( "%s = %s", .x, equationsDuringInfusionWithAdmin[[.x]] ) )
  equationsBodyDuringInfusionWithAdmin = map2_chr( equationsBodyDuringInfusionWithAdmin, timeNames, ~ str_replace_all( .x, "\\bt\\b", .y ) )
  functionBodyDuringInfusionWithAdmin = paste( equationsBodyDuringInfusionWithAdmin, collapse = "\n" )
  functionBodyDuringInfusionWithAdmin = sprintf( paste( "%s\nreturn( list( c (", outputAdmin , ") ) )", collapse = ", " ), functionBodyDuringInfusionWithAdmin )
  functionDefinitionDuringInfusionWithAdmin = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", " ), functionBodyDuringInfusionWithAdmin )
  functionDefinitionDuringInfusionWithAdmin = eval( parse( text = functionDefinitionDuringInfusionWithAdmin ) )

  equationsBodyDuringInfusionWithNoAdmin = map_chr( names( equationsDuringInfusionWithNoAdmin ), ~ sprintf( "%s = %s", .x, equationsDuringInfusionWithNoAdmin[[.x]] ) )
  equationsBodyDuringInfusionWithNoAdmin = map2_chr( equationsBodyDuringInfusionWithNoAdmin, timeNames, ~ str_replace_all( .x, "\\bt\\b", .y ) )
  functionBodyDuringInfusionWithNoAdmin = paste( equationsBodyDuringInfusionWithNoAdmin, collapse = "\n" )
  functionBodyDuringInfusionWithNoAdmin = sprintf( paste( "%s\nreturn( list( c (", outputNoAdmin , ") ) )", collapse = ", " ), functionBodyDuringInfusionWithNoAdmin )
  functionDefinitionDuringInfusionWithNoAdmin = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", " ), functionBodyDuringInfusionWithNoAdmin )
  functionDefinitionDuringInfusionWithNoAdmin = eval( parse( text = functionDefinitionDuringInfusionWithNoAdmin ) )

  # create function afterInfusion
  equationsBodyAfterInfusionWithAdmin = map_chr( names( equationsAfterInfusionWithAdmin ), ~ sprintf( "%s = %s", .x, equationsAfterInfusionWithAdmin[[.x]] ) )
  equationsBodyAfterInfusionWithAdmin = map2_chr( equationsBodyAfterInfusionWithAdmin, timeNames, ~ str_replace_all( .x, "\\bt\\b", .y ) )
  functionBodyAfterInfusionWithAdmin = paste( equationsBodyAfterInfusionWithAdmin, collapse = "\n" )
  functionBodyAfterInfusionWithAdmin = sprintf( paste( "%s\nreturn( list( c (", outputAdmin , ") ) )", collapse = ", " ), functionBodyAfterInfusionWithAdmin )
  functionDefinitionAfterInfusionWithAdmin = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", " ), functionBodyAfterInfusionWithAdmin )
  functionDefinitionAfterInfusionWithAdmin = eval( parse( text = functionDefinitionAfterInfusionWithAdmin ) )

  equationsBodyAfterInfusionWithNoAdmin = map_chr( names( equationsAfterInfusionWithNoAdmin ), ~ sprintf( "%s = %s", .x, equationsAfterInfusionWithNoAdmin[[.x]] ) )
  equationsBodyAfterInfusionWithNoAdmin = map2_chr( equationsBodyAfterInfusionWithNoAdmin, timeNames, ~ str_replace_all( .x, "\\bt\\b", .y ) )
  functionBodyAfterInfusionWithNoAdmin = paste( equationsBodyAfterInfusionWithNoAdmin, collapse = "\n" )
  functionBodyAfterInfusionWithNoAdmin = sprintf( paste( "%s\nreturn( list( c (", outputNoAdmin , ") ) )", collapse = ", " ), functionBodyAfterInfusionWithNoAdmin )
  functionDefinitionAfterInfusionWithNoAdmin = sprintf( "function(%s) { %s }", paste( functionArguments, collapse = ", " ), functionBodyAfterInfusionWithNoAdmin )
  functionDefinitionAfterInfusionWithNoAdmin = eval( parse( text = functionDefinitionAfterInfusionWithNoAdmin ) )

  prop( model, "wrapperModelAnalyticInfusion" ) = list( functionDefinitionDuringInfusionWithAdmin = functionDefinitionDuringInfusionWithAdmin,
                                                        functionDefinitionDuringInfusionWithNoAdmin = functionDefinitionDuringInfusionWithNoAdmin,
                                                        functionDefinitionAfterInfusionWithAdmin = functionDefinitionAfterInfusionWithAdmin,
                                                        functionDefinitionAfterInfusionWithNoAdmin = functionDefinitionAfterInfusionWithNoAdmin )

  prop( model, "functionArgumentsModelAnalyticInfusion" ) = list( functionArguments = functionArguments )
  prop( model, "functionArgumentsSymbolModelAnalyticInfusion" ) = list( functionArgumentsSymbol = functionArgumentsSymbol )

  # define the model
  prop( model, "outputNames") = unlist( outputs )
  prop( model, "outcomesWithAdministration") = outcomesWithAdministration
  return( model )
}

#' defineModelAdministration: define the administration
#' @name defineModelAdministration
#' @param model An object of class \code{ModelAnalyticInfusionSteadyState} that defines the model.
#' @param arm An object of class \code{Arm} that defines the arm.
#' @return The model with samplings, solverInputs
#' @export

method( defineModelAdministration, ModelAnalyticInfusionSteadyState ) = function( model, arm ) {

  # administrations and outcome
  administrations = prop( arm, "administrations" )
  outcomesWithAdministration =  prop( model, "outcomesWithAdministration" )

  # sampling times
  samplingTimes = prop( arm, "samplingTimes" )

  # define the samplings for all response
  samplings = map( samplingTimes, ~ prop( .x, "samplings" ) ) %>% unlist() %>% sort() %>% unique()
  maxSampling = max( samplings )

  # vector during & after infusion
  duringAndAfter = rep( "afterInfusion", length( samplings) )

  # model outputs
  outputNames = prop( model, "outputNames" )

  # define solverInputs
  solverInputs = map( administrations, function(  administration ) {

    timeDose = prop( administration, "timeDose" )
    tau = prop( administration, "tau" )
    dose = prop( administration, "dose" )
    Tinf = prop( administration, "Tinf" )
    Tinfs = map2( timeDose, timeDose + Tinf, c )

    if ( tau != 0 ) {
      timeDose = seq( 0, maxSampling, tau )
      dose = rep( dose, length( timeDose ) )
      Tinf = rep( Tinf, length( timeDose ) )
      Tinfs = map2( timeDose, timeDose + Tinf, c )
    }

    samplingsDuringInfusion = map ( Tinfs, function( Tinfs )
    {
      samplings %>% keep( ~ . >= min( Tinfs ) & . <  max( Tinfs ) )
    }) %>% unlist()%>% unique()

    duringAndAfter[ samplings %in% samplingsDuringInfusion ] = "duringInfusion"

    samplingTimeDoses = timeDose %>% map( ~ ifelse( samplings - .x > 0, samplings - .x, 0 ) )

    indicesDoses = map_int( samplings, function( sampling ) {
      indice = which( sampling >= timeDose )[ length( which( sampling >= timeDose ) ) ]
    })

    data = data.frame( duringAndAfter, indicesDoses, samplings, samplingTimeDoses   )
    colnames( data ) = c( "duringAndAfter", "indicesDoses", "samplings", paste0( rep( "samplingTimeDoses", length( dose ) ), 1:length( dose ) ) )

    list( data = data, dose = dose, Tinf = Tinf, tau = tau )

  }) %>% setNames( outcomesWithAdministration )

  prop( model, "samplings" ) = samplings
  prop( model, "solverInputs" ) = solverInputs
  return( model )
}

#' evaluateModel: evaluate the ModelAnalyticInfusion
#' @name evaluateModel
#' @param model An object of class \code{ModelAnalyticInfusionSteadyState} that defines the model.
#' @param arm An object of class \code{Arm} that defines the arm.
#' @return A list of dataframes that contains the results for the evaluation of the model.
#' @export

method( evaluateModel, ModelAnalyticInfusionSteadyState ) = function( model, arm ) {

  # administrations and outcome
  administrations = prop( arm, "administrations" )
  outcomesWithAdministration = map_chr( administrations, ~ prop( .x, "outcome" ) )

  # outputs
  outputNames = prop( model, "outputNames" ) %>% unlist()

  # solver inputs for time dose and indice dose
  solverInputs = prop( model, "solverInputs")

  # sampling time for model
  samplings = prop( model, "samplings" )

  # model parameters
  parameters = prop( model, "modelParameters")

  # model wrapper model analytic infusion during & after
  wrapperModelAnalyticInfusion = prop( model, "wrapperModelAnalyticInfusion")
  functionDefinitionDuringInfusionWithAdmin = wrapperModelAnalyticInfusion$functionDefinitionDuringInfusionWithAdmin
  functionDefinitionDuringInfusionWithNoAdmin = wrapperModelAnalyticInfusion$functionDefinitionDuringInfusionWithNoAdmin
  functionDefinitionAfterInfusionWithAdmin = wrapperModelAnalyticInfusion$functionDefinitionAfterInfusionWithAdmin
  functionDefinitionAfterInfusionWithNoAdmin = wrapperModelAnalyticInfusion$functionDefinitionAfterInfusionWithNoAdmin

  functionArgumentsModelAnalyticInfusion = prop( model, "functionArgumentsModelAnalyticInfusion" )
  functionArguments = functionArgumentsModelAnalyticInfusion$functionArguments

  functionArgumentsSymbolModelAnalyticInfusion = prop( model, "functionArgumentsSymbolModelAnalyticInfusion" )
  functionArgumentsSymbol = functionArgumentsSymbolModelAnalyticInfusion$functionArgumentsSymbol

  # Assign the values to variables in the current environment
  mu = set_names( map(parameters, ~ .x@distribution@mu), map(parameters, ~ prop(.x,"name")))
  list2env( mu, envir = environment() )

  # evaluation ModelAnalyticInfusion
  evaluationModelTmp = map( seq_along( samplings ), function( iterTime ) {

    evaluationOutcome = map( outcomesWithAdministration, function( outcomeWithAdministration ) {

      data = solverInputs[[outcomeWithAdministration]]$data
      tau = solverInputs[[outcomeWithAdministration]]$tau

      duringAndAfter = data$duringAndAfter[iterTime]
      indicesDoses = data$indicesDoses[iterTime]
      samplings = data[ iterTime, colnames( data ) %>% keep(~ str_detect( .x, "samplingTimeDoses" ) ) ] %>% unname() %>% unlist()

      # evaluation infusion during
      if( duringAndAfter == "duringInfusion")
      {
        # first dose
        if ( indicesDoses == 1 )
        {
          assign( paste0("t_", outcomeWithAdministration ), samplings[indicesDoses] )
          assign( paste0("dose_", outcomeWithAdministration ), solverInputs[[outcomeWithAdministration]]$dose[indicesDoses] )
          assign( paste0("Tinf_", outcomeWithAdministration ), solverInputs[[outcomeWithAdministration]]$Tinf[indicesDoses] )

          evaluationOutcomeWithAdmin  = do.call( functionDefinitionDuringInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()
        }

        # after the first dose
        if ( indicesDoses > 1 )
        {
          samplings = samplings[1:indicesDoses]
          samplingDuring = tail( samplings, 1 )
          samplingAfter = samplings[1:(indicesDoses-1)]

          doseDuring = solverInputs[[outcomeWithAdministration]]$dose[indicesDoses]
          dosesAfter = solverInputs[[outcomeWithAdministration]]$dose[1:(indicesDoses-1)]

          tinfDuring = solverInputs[[outcomeWithAdministration]]$Tinf[indicesDoses]
          tinfAfter = solverInputs[[outcomeWithAdministration]]$Tinf[1:(indicesDoses-1)]

          assign( paste0( "t_", outcomeWithAdministration ), samplingDuring )
          assign( paste0( "dose_", outcomeWithAdministration ), doseDuring )
          assign( paste0( "Tinf_", outcomeWithAdministration ), tinfDuring )

          evaluationOutcomeWithAdmin = do.call( functionDefinitionDuringInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()
          evaluationOutcomeWithAdmin = evaluationOutcomeWithAdmin + sum( map_dbl( 1:( indicesDoses - 1 ), ~ {

            assign( paste0( "t_", outcomeWithAdministration ), samplingAfter[.x] )
            assign( paste0( "dose_", outcomeWithAdministration ), dosesAfter[.x] )
            assign( paste0( "Tinf_", outcomeWithAdministration ), tinfAfter[.x] )

            output = do.call( functionDefinitionAfterInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()

            return( output )

          } ) )
        }
      }
      # evaluation infusion after
      else if( duringAndAfter == "afterInfusion")
      {
        # first dose
        if ( indicesDoses == 1 )
        {
          assign( paste0("t_", outcomeWithAdministration ), samplings[indicesDoses] )
          assign( paste0("dose_", outcomeWithAdministration ), solverInputs[[outcomeWithAdministration]]$dose[indicesDoses] )
          assign( paste0("Tinf_", outcomeWithAdministration ), solverInputs[[outcomeWithAdministration]]$Tinf[indicesDoses] )

          evaluationOutcomeWithAdmin = do.call( functionDefinitionAfterInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()
        }
        # after the first dose
        if ( indicesDoses > 1 )
        {
          samplings =  samplings[1:indicesDoses]
          samplingDuring = tail( samplings, 1 )
          samplingAfter = samplings[1:(indicesDoses-1)]

          doseDuring = solverInputs[[outcomeWithAdministration]]$dose[indicesDoses]
          dosesAfter = solverInputs[[outcomeWithAdministration]]$dose[1:(indicesDoses-1)]

          tinfDuring = solverInputs[[outcomeWithAdministration]]$Tinf[indicesDoses]
          tinfAfter = solverInputs[[outcomeWithAdministration]]$Tinf[1:(indicesDoses-1)]

          assign( paste0( "t_", outcomeWithAdministration ), samplingDuring )
          assign( paste0( "dose_",outcomeWithAdministration ), doseDuring )
          assign( paste0( "Tinf_",outcomeWithAdministration ), tinfDuring )

          evaluationOutcomeWithAdmin = do.call( functionDefinitionAfterInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()
          evaluationOutcomeWithAdmin = evaluationOutcomeWithAdmin + sum( map_dbl( 1:( indicesDoses - 1 ), ~ {

            assign( paste0( "t_", outcomeWithAdministration ), samplingAfter[.x] )
            assign( paste0( "dose_", outcomeWithAdministration ), dosesAfter[.x] )
            assign( paste0( "Tinf_", outcomeWithAdministration ), tinfAfter[.x] )

            output = do.call( functionDefinitionAfterInfusionWithAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()

            return( output )
          } ) )
        }
      }

      # assign values to response PK
      assign( outcomeWithAdministration , evaluationOutcomeWithAdmin )

      # evaluation function response PD
      evaluationOutcomeWithNoAdmin = do.call( functionDefinitionAfterInfusionWithNoAdmin, setNames( functionArgumentsSymbol, functionArguments ) ) %>% unlist()

      # test if response PD or not
      if ( is.null( evaluationOutcomeWithNoAdmin ) )
      {
        data.frame(  evaluationOutcomeWithAdmin )
      }else{
        data.frame(  evaluationOutcomeWithAdmin, evaluationOutcomeWithNoAdmin )
      }
    })
    return( evaluationOutcome )
  })  %>% flatten() %>% reduce( rbind ) %>% cbind( samplings, . ) %>% setNames( c( "time", outputNames ) )

  # filter sampling time
  samplingTimes = prop( arm, "samplingTimes" )
  samplings = map( samplingTimes, ~ prop( .x, "samplings" ) ) %>% set_names( outputNames )

  evaluationModel = list()
  for ( outputName in outputNames )
  {
    time = evaluationModelTmp$time %in% samplings[[outputName]]
    evaluationModel[[outputName]] = evaluationModelTmp[ time , c( "time", outputName ) ]
  }
  return( evaluationModel )
}

#' definePKModel
#' @name definePKModel
#' @param pkModel An object of class \code{ModelAnalyticInfusionSteadyState} that defines the PK model in infusion.
#' @param pfimproject An object of class \code{PFIMProject} that defines the pfimproject.
#' @export

method( definePKModel, list( ModelAnalyticInfusionSteadyState, PFIMProject ) ) = function( pkModel, pfimproject ) {
  pkModelEquations = prop( pkModel, "modelEquations")
  return( pkModelEquations )
}

#' definePKPDModel
#' @name definePKPDModel
#' @param pkModel An object of class \code{ModelAnalyticInfusionSteadyState} that defines the PK model in infusion steady state.
#' @param pkModel An object of class \code{ModelAnalytic} that defines the PD model.
#' @param pfimproject An object of class \code{PFIMProject} that defines the pfimproject.
#' @export

method( definePKPDModel, list( ModelAnalyticInfusionSteadyState, ModelAnalytic, PFIMProject ) ) = function( pkModel, pdModel, pfimproject ) {

  pkModelEquations = prop( pkModel, "modelEquations")
  pdModelEquations = prop( pdModel, "modelEquations")

  equations = list( duringInfusion = c( pkModelEquations$duringInfusion, pdModelEquations ),
                    afterInfusion  = c( pkModelEquations$afterInfusion, pdModelEquations ) )
  return( equations )
}








