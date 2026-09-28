#' @title Optimization
#' @description
#' Design optimization: search over sampling times (or related design variables)
#' using a metaheuristic or exchange algorithm.
#'
#' Project fields use \code{\link{prop}}. Several \code{designs}: all optimizers
#' (discrete and continuous) optimize each in turn
#' (\code{optimisationAlgorithmOutputs$perDesign}).
#' @return An \code{Optimization} object; after \code{run()}, see
#'   \code{optimisationDesign} and \code{optimisationAlgorithmOutputs}.
#' @examples
#' \dontrun{
#' vignette("Example01")
#' }
#' \donttest{
#' source(system.file("examples", "evaluation-minimal.R", package = "PFIM"))
#' class(ev)
#' }
#' @inheritParams PFIMProject
#' @param optimizerParameters See \code{\link{PFIMProject}}; validated when
#'   \code{optimizer} names a built-in algorithm.
#' @param project Nested \code{PFIMProject} (filled by the constructor).
#' @param optimisationDesign List with initial and optimal design evaluations.
#' @param optimisationAlgorithmOutputs Raw outputs from the optimization algorithm.
#' @include PFIMProject.R
#' @include pfim-project-access.R
#' @export

Optimization = new_class( "Optimization",
                          package    = "PFIM",
                          properties = list(
                            project                      = new_property( PFIMProject ),
                            optimisationDesign           = new_property( class_list, default = list() ),
                            optimisationAlgorithmOutputs = new_property( class_list, default = list() )
                          ),
                          constructor = function(
                              name = character(0),
                              modelClass = character(0),
                              modelEquations = list(),
                              modelCovariatesEquation = character(0),
                              modelFromLibrary = list(),
                              modelParameters = list(),
                              modelCovariates = list(),
                              modelError = list(),
                              optimizer = character(0),
                              optimizerParameters = list(),
                              outputs = list(),
                              designs = list(),
                              fimType = character(0),
                              fim = NULL,
                              odeSolverParameters = list(),
                              numberOfOccasions = NA_real_,
                              project = NULL,
                              optimisationDesign = list(),
                              optimisationAlgorithmOutputs = list() ) {
                            if ( is.null( project ) ) {
                              if ( is.null( fim ) )
                                fim = .placeholderFim( fimType )
                              .pfimCheckOptimizerParameters( optimizer, optimizerParameters )
                              project = PFIMProject(
                                name = name,
                                modelClass = modelClass,
                                modelEquations = modelEquations,
                                modelCovariatesEquation = modelCovariatesEquation,
                                modelFromLibrary = modelFromLibrary,
                                modelParameters = modelParameters,
                                modelCovariates = modelCovariates,
                                modelError = modelError,
                                optimizer = optimizer,
                                optimizerParameters = optimizerParameters,
                                outputs = outputs,
                                designs = designs,
                                fimType = fimType,
                                fim = fim,
                                odeSolverParameters = odeSolverParameters,
                                numberOfOccasions = numberOfOccasions
                              )
                            } else {
                              # Same validation when a pre-built PFIMProject is supplied.
                              .pfimCheckOptimizerParameters(
                                prop( project, "optimizer" ),
                                prop( project, "optimizerParameters" )
                              )
                            }
                            new_object(
                              S7_object(),
                              project                      = project,
                              optimisationDesign           = optimisationDesign,
                              optimisationAlgorithmOutputs = optimisationAlgorithmOutputs
                            )
                          }
)
S4_register( Optimization )

#' Default method for \code{Optimization}.
#' @param pfimproject First argument of generic.
#' @return \code{Fim} instance built from nested project settings.
#' @name defineFim
#' @keywords internal
method( defineFim, Optimization ) = function( pfimproject ) {
  defineFim( projectOf( pfimproject ) )
}

#' Default method for \code{Optimization}.
#' @param pfimproject First argument of generic.
#' @return List of model equations resolved from library entries.
#' @name defineModelEquationsFromLibraryOfModel
#' @keywords internal
method( defineModelEquationsFromLibraryOfModel, Optimization ) = function( pfimproject ) {
  res = defineModelEquationsFromLibraryOfModel( projectOf( pfimproject ) )
  projectProp( pfimproject, "modelEquations" ) = res
  res
}

#' Default method for \code{Optimization}.
#' @param pfimproject First argument of generic.
#' @return Concrete \code{Model} object inferred from project equations.
#' @name defineModelType
#' @keywords internal
method( defineModelType, Optimization ) = function( pfimproject ) {
  defineModelType( projectOf( pfimproject ) )
}

#' Instantiate optimization algorithm from project configuration.
#'
#' Looks up \code{optimizer} on the nested project and builds the matching
#' algorithm object (Multiplicative, Fedorov-Wynn, Simplex, PSO, PGBO, ...).
#' @param optimization An \code{Optimization} object.
#' @param ... Optional method arguments.
#' @name defineOptimizationAlgorithm
#' @return An optimization algorithm object.
#' @keywords internal
defineOptimizationAlgorithm  = new_generic( "defineOptimizationAlgorithm", c( "optimization" ) )

#' Evaluate constraint-generated arm candidates.
#'
#' Enumerates the Cartesian product of dose and sampling grids under design
#' constraints, evaluates a FIM for each cell (optionally subsampled via
#' \code{constraints.maxTasks}), and returns matrices for Fedorov-Wynn and
#' multiplicative algorithms.
#' @param optimization An \code{Optimization} object.
#' @param ... Optional method arguments.
#' @name generateFimsFromConstraints
#' @return A list of FIM objects and arm layouts generated from design constraints.
#' @keywords internal
generateFimsFromConstraints  = new_generic( "generateFimsFromConstraints",  c( "optimization" ) )

#' Plot optimized arm weights
#' @param optimization An \code{Optimization} object.
#' @param ... Optional method arguments.
#' @name plotWeights
#' @return A \code{ggplot2} plot object.
#' @export
plotWeights                  = new_generic( "plotWeights",                  c( "optimization" ) )

#' Plot optimized sampling frequencies
#' @param optimization An \code{Optimization} object.
#' @param ... Optional method arguments.
#' @name plotFrequencies
#' @return A \code{ggplot2} plot object.
#' @export
plotFrequencies              = new_generic( "plotFrequencies",              c( "optimization" ) )

#' Optimize design for a given algorithm.
#'
#' Dispatches on the algorithm class. Typical pipeline after \code{run()}:
#' resolve library equations -> define FIM -> call the algorithm-specific
#' \code{optimizeDesign} method, which writes \code{optimisationDesign} and
#' \code{optimisationAlgorithmOutputs}.
#' @param optimizationObject An optimization container object.
#' @param optimizationAlgorithm Optimization algorithm object.
#' @param ... Optional method arguments.
#' @name optimizeDesign
#' @return The updated \code{Optimization} object after design optimization.
#' @keywords internal
optimizeDesign               = new_generic( "optimizeDesign",  c( "optimizationObject", "optimizationAlgorithm" ) )

#' Build constraints table used in reports
#' @param optimizationAlgorithm Optimization algorithm object.
#' @param ... Optional method arguments.
#' @name constraintsTableForReport
#' @return Constraint report tables as a list or data frame.
#' @keywords internal
constraintsTableForReport    = new_generic( "constraintsTableForReport",    c( "optimizationAlgorithm" ) )

#' Extract optimal design evaluation from an optimization object.
#' @param optimization \code{Optimization} object with optimization outputs.
#' @return \code{Evaluation} object for the optimal design.
#' @noRd
#' @keywords internal
.getOptimalEval = function( optimization ) {
  prop( optimization, "optimisationDesign" )$evaluationOptimalDesign
}

#' Nested optimizer outputs (\code{optimisationAlgorithmOutputs}).
#' @noRd
#' @keywords internal
.pfimAlgoOutputs = function( optimization ) {
  prop( optimization, "optimisationAlgorithmOutputs" )
}

#' Optimizer instance stored after \code{run()}.
#' @noRd
#' @keywords internal
.getOptimizationAlgorithm = function( optimization ) {
  .pfimAlgoOutputs( optimization )$optimizationAlgorithm
}

#' Extract optimal FIM and attach evaluation-derived summaries.
#' @param optimization \code{Optimization} object with optimal evaluation.
#' @return \code{Fim} object after \code{setEvaluationFim()} post-processing.
#' @noRd
#' @keywords internal
.getOptimalFim = function( optimization ) {
  eval  = .getOptimalEval( optimization )
  fim   = prop( eval, "fim" )
  setEvaluationFim( fim, eval )
}

#' Instantiate the optimizer from project settings
#' @name defineOptimizationAlgorithm
#' @keywords internal
method( defineOptimizationAlgorithm, Optimization ) = function( optimization ) {
  .pfimInstantiateOptimizer( projectProp( optimization, "optimizer" ) )
}

#' Run design optimization.
#'
#' Pipeline: invalidate model cache -> instantiate algorithm -> define FIM ->
#' resolve library equations if needed -> \code{optimizeDesign()}. Results land
#' in \code{optimisationDesign} (initial + optimal evaluations) and
#' \code{optimisationAlgorithmOutputs}.
#' @param pfimproject An \code{Optimization} object.
#' @return The same \code{Optimization} with optimal design evaluations stored.
#' @examples
#' \dontrun{
#' vignette("Example01")
#' }
#' @name run
#' @export
method( run, Optimization ) = function( pfimproject ) {
  .invalidateEvalModelCache( pfimproject )
  optimizationAlgorithm = defineOptimizationAlgorithm( pfimproject )
  projectProp( pfimproject, "fim" ) = defineFim( pfimproject )

  if ( length( projectProp( pfimproject, "modelFromLibrary" ) ) != 0L )
    projectProp( pfimproject, "modelEquations" ) =
      defineModelEquationsFromLibraryOfModel( pfimproject )

  .pfimValidateProjectOutcomes( pfimproject )
  .pfimValidateOptimizerConstraints( pfimproject )
  pfimproject = optimizeDesign( pfimproject, optimizationAlgorithm )
  od = prop( pfimproject, "optimisationDesign" )
  # Nested project$fim is a prototype during search; copy the labelled optimal
  # FIM so Dcriterion(prop(opt, "fim")) matches getDcriterion(opt).
  if ( length( od ) && !is.null( od$evaluationOptimalDesign ) )
    projectProp( pfimproject, "fim" ) = .getOptimalFim( pfimproject )
  pfimproject
}

#' Default method for \code{Optimization}.
#' @param object First argument of generic.
#' @return Invisibly returns the printed \code{Optimization} object.
#' @name show-methods
#' @keywords internal
method( show, Optimization ) = function( object ) {
  od = prop( object, "optimisationDesign" )
  if ( !length( od ) || is.null( od$evaluationOptimalDesign ) ) {
    cat( "Optimization (not run yet): call run() first.\n" )
    return( invisible( object ) )
  }
  evaluationOptimalDesign = od$evaluationOptimalDesign
  armsData = .armsDataFromEvaluation( evaluationOptimalDesign )
  df = map( armsData, \( x ) as.data.frame( x, stringsAsFactors = FALSE ) ) |> list_rbind()
  colnames( df ) = c( "Arms name", "Number of subjects", "Outcome", "Dose", "Sampling times" )

  fimOptimalDesign = setEvaluationFim(
    prop( evaluationOptimalDesign, "fim" ), evaluationOptimalDesign
  )

  cat( "\n--- Optimal design ---\n\n" )
  print( df )
  .pfimShowOptimalMixtureWeights( object, df )
  showFIM( fimOptimalDesign )

  invisible( object )
}

#' Fisher matrix blocks for the optimal design (after \code{run()}).
#' @name getFisherMatrix
#' @export
method( getFisherMatrix, Optimization ) = function( pfimproject ) {
  fim = .getOptimalFim( pfimproject )
  list(
    fisherMatrix    = prop( fim, "fisherMatrix"    ),
    fixedEffects    = prop( fim, "fixedEffects"    ),
    varianceEffects = prop( fim, "varianceEffects" ),
    singularFim     = isTRUE( prop( fim, "singularFim" ) )
  )
}

#' Standard errors from the optimal design FIM (after \code{run()}).
#' @name getSE
#' @export
method( getSE, Optimization ) = function( pfimproject ) {
  prop( .getOptimalFim( pfimproject ), "SEAndRSE" )$SE
}

#' Relative standard errors from the optimal design FIM (after \code{run()}).
#' @name getRSE
#' @export
method( getRSE, Optimization ) = function( pfimproject ) {
  prop( .getOptimalFim( pfimproject ), "SEAndRSE" )$RSE
}

#' Parameter shrinkage from the Bayesian optimal-design FIM.
#' @name getShrinkage
#' @export
method( getShrinkage, Optimization ) = function( pfimproject ) {
  prop( .getOptimalFim( pfimproject ), "shrinkage" )
}

#' Determinant of the optimal design FIM.
#' @name getDeterminant
#' @export
method( getDeterminant, Optimization ) = function( pfimproject ) {
  .fimDeterminant( prop( .getOptimalFim( pfimproject ), "fisherMatrix" ) )
}

#' Parameter correlations from the optimal design FIM.
#' @name getCorrelationMatrix
#' @export
method( getCorrelationMatrix, Optimization ) = function( pfimproject ) {
  .fimCorrelationMatrix( prop( .getOptimalFim( pfimproject ), "fisherMatrix" ) )
}

#' D-criterion of the optimal design.
#' @name getDcriterion
#' @export
method( getDcriterion, Optimization ) = function( pfimproject ) {
  Dcriterion( .getOptimalFim( pfimproject ) )
}

#' D-criterion of the continuous mixture over retained Multiplicative cells.
#'
#' Differs from \code{getDcriterion()} (implemented protocol) when joint cells
#' or Hamilton allocation change the realised design. Requires
#' \code{MultiplicativeAlgorithm} results.
#' @param pfimproject An \code{Optimization} from \code{MultiplicativeAlgorithm}.
#' @param ... Optional method arguments.
#' @return Numeric D-criterion of the continuous mixture.
#' @name getMixtureDcriterion
#' @export
getMixtureDcriterion = new_generic( "getMixtureDcriterion", c( "pfimproject" ) )

method( getMixtureDcriterion, Optimization ) = function( pfimproject ) {
  mult = .pfimMultAlgorithmOutputs( pfimproject )
  val  = .pfimAlgoOutputs( pfimproject )$mixtureDcriterion
  if ( is.null( val ) )
    val = mult$mixtureDcriterion
  as.numeric( val )
}

#' D-criterion of the single protocol implemented after Multiplicative allocation.
#'
#' Matches \code{getDcriterion()} on the optimal evaluation. Requires
#' \code{MultiplicativeAlgorithm} results.
#' @param pfimproject An \code{Optimization} from \code{MultiplicativeAlgorithm}.
#' @param ... Optional method arguments.
#' @return Numeric D-criterion of the implemented protocol.
#' @name getRealisedDcriterion
#' @export
getRealisedDcriterion = new_generic( "getRealisedDcriterion", c( "pfimproject" ) )

method( getRealisedDcriterion, Optimization ) = function( pfimproject ) {
  mult = .pfimMultAlgorithmOutputs( pfimproject )
  val  = .pfimAlgoOutputs( pfimproject )$realisedDcriterion
  if ( is.null( val ) )
    val = mult$realisedDcriterion
  as.numeric( val )
}

#' Predicted responses for the optimal design evaluation.
#' @name plotEvaluation
#' @usage NULL
#' @export
method( plotEvaluation, Optimization ) = function( pfimproject, plotOptions = list() ) {
  plotEvaluation( .getOptimalEval( pfimproject ), plotOptions )
}

#' Sensitivity-index plots for the optimal design evaluation.
#' @name plotSensitivityIndices
#' @usage NULL
#' @export
method( plotSensitivityIndices, Optimization ) = function( pfimproject, plotOptions = list() ) {
  plotSensitivityIndices( .getOptimalEval( pfimproject ), plotOptions )
}

#' SE bar chart for the optimal design FIM.
#' @name plotSE
#' @usage NULL
#' @export
method( plotSE, Optimization ) = function( pfimproject ) {
  plotSEFIM( .getOptimalFim( pfimproject ), .getOptimalEval( pfimproject ) )
}

#' RSE bar chart for the optimal design FIM.
#' @name plotRSE
#' @usage NULL
#' @export
method( plotRSE, Optimization ) = function( pfimproject ) {
  plotRSEFIM( .getOptimalFim( pfimproject ), .getOptimalEval( pfimproject ) )
}

#' Multiplicative algorithm weights by iteration
#' @name plotWeights
#' @export
method( plotWeights, Optimization ) = function( optimization ) {
  algo = .pfimRequireOptimizerClass(
    optimization, MultiplicativeAlgorithm, "plotWeights"
  )
  plotWeightsMultiplicativeAlgorithm( optimization, algo )
}

#' Fedorov-Wynn optimal frequencies
#' @name plotFrequencies
#' @export
method( plotFrequencies, Optimization ) = function( optimization ) {
  algo = .pfimRequireOptimizerClass(
    optimization, FedorovWynnAlgorithm, "plotFrequencies"
  )
  plotFrequenciesFedorovWynnAlgorithm( optimization, algo )
}

#' HTML optimization report (initial vs optimal design).
#'
#' Rebuilds kable tables and plots for both the initial and optimal evaluations,
#' then hands everything to \code{generateReportOptimization()} with the FIM-type
#' R Markdown template.
#' @rdname Report
#' @name Report
#' @export
method( Report, Optimization ) = function( pfimproject, outputPath, outputFile, plotOptions ) {
  .pfimClearGradientPerfCaches()
  .invalidateEvalModelCache( pfimproject )
  projectName             = .pfimProjectNameOrDefault( pfimproject )
  optimisationDesign      = prop( pfimproject, "optimisationDesign" )
  evaluationInitialDesign = optimisationDesign$evaluationInitialDesign
  evaluationOptimalDesign = optimisationDesign$evaluationOptimalDesign
  evaluationOutputs       = projectProp( pfimproject, "outputs" )
  model                   = rebuildEvalModel( evaluationInitialDesign, finiteDifference = FALSE )
  modelEquations          = prop( model, "modelEquations" )
  modelErrorTable         = .buildModelErrorKable( prop( evaluationInitialDesign, "modelError" ) )
  modelParametersTable    = .buildModelParametersKable( prop( evaluationInitialDesign, "modelParameters" ) )
  modelCovariates         = prop( evaluationInitialDesign, "modelCovariates" )
  hasCov                  = length( modelCovariates ) > 0L
  covariatesTable         = .buildCovariatesKable( modelCovariates )
  # Covariate tests use the *optimal* design FIM (post-optimization inference).
  covariateTestTables = if ( hasCov ) .buildCovariateTestSection( evaluationOptimalDesign ) else NULL
  .buildArmTable = function( evaluation, colnamesVec ) {
    df = map( .armsDataFromEvaluation( evaluation ),
              \( x ) as.data.frame( x, stringsAsFactors = FALSE ) ) |> list_rbind()
    colnames( df ) = colnamesVec
    df
  }
  administrationData = prop( evaluationInitialDesign, "designs" ) |>
    map( function( d ) {
      map( prop( d, "arms" ), \( x ) armAdministration( x, prop( d, "name" ) ) ) |>
        list_flatten()
    } ) |> pluck( 1L ) |>
    map( \( x ) as.data.frame( x, stringsAsFactors = FALSE ) ) |> list_rbind()
  colnames( administrationData ) = c(
    "Design name", "Arms name", "Number of subjects",
    "Outcome", "Dose", "Time dose", "$\\tau$", "$T_{inf}$"
  )
  administrationTable = kbl(
    administrationData,
    align = .kblAlign( ncol( administrationData ), 3L )
  ) |>
    kable_styling( bootstrap_options = "hover", full_width = FALSE,
                   position = "center", font_size = 13 ) |>
    .pfimKableHeaderGray()
  armCols            = c( "Arms name", "Number of subjects", "Outcome", "Dose", "Sampling times" )
  initialDesignData  = .buildArmTable( evaluationInitialDesign, armCols )
  initialDesignTable = kbl(
    initialDesignData,
    align = .kblAlign( ncol( initialDesignData ), 1L )
  ) |>
    kable_styling( bootstrap_options = "hover", full_width = FALSE,
                   position = "center", font_size = 13 ) |>
    .pfimKableHeaderGray()
  fimInitial            = setEvaluationFim( prop( evaluationInitialDesign, "fim" ), evaluationInitialDesign )
  fimInitialDesignTable = tablesForReport( fimInitial, evaluationInitialDesign )
  optimAlgoOutputs      = prop( pfimproject, "optimisationAlgorithmOutputs" )
  optimizationAlgorithm = optimAlgoOutputs$optimizationAlgorithm
  constraintsData       = constraintsTableForReport(
    optimizationAlgorithm,
    map( prop( evaluationInitialDesign, "designs" ), \( x ) prop( x, "arms" ) )
  )
  optimalDesignData  = .buildArmTable( evaluationOptimalDesign, armCols )
  optimalDesignTable = kbl(
    optimalDesignData,
    align = .kblAlign( ncol( optimalDesignData ), 1L )
  ) |>
    kable_styling( bootstrap_options = "hover", full_width = FALSE,
                   position = "center", font_size = 13 ) |>
    .pfimKableHeaderGray()
  fimOptimal                 = setEvaluationFim( prop( evaluationOptimalDesign, "fim" ), evaluationOptimalDesign )
  fimOptimalTable            = tablesForReport( fimOptimal, evaluationOptimalDesign )
  plotsEvaluationData        = plotEvaluation( evaluationOptimalDesign, plotOptions )
  plotSensitivityIndicesData = plotSensitivityIndices( evaluationOptimalDesign, plotOptions )
  plotSEData                 = plotSEFIM( fimOptimal, evaluationOptimalDesign )
  plotRSEData                = plotRSEFIM( fimOptimal, evaluationOptimalDesign )
  reportTables               = c(
    list(
      evaluationOutputs         = evaluationOutputs,
      modelEquations            = modelEquations,
      modelErrorTable           = modelErrorTable,
      modelParametersTable      = modelParametersTable,
      covariatesTable           = covariatesTable,
      covariateTestTables       = covariateTestTables,
      administrationTable       = administrationTable,
      initialDesignTable        = initialDesignTable,
      constraintsTableForReport = constraintsData,
      armsConstraintsTable      = constraintsData,
      fimInitialDesignTable     = fimInitialDesignTable,
      optimalDesignTable        = optimalDesignTable,
      fimOptimalTable           = fimOptimalTable,
      plotsEvaluation           = plotsEvaluationData,
      plotSensitivityIndices    = plotSensitivityIndicesData,
      plotSE                    = plotSEData,
      plotRSE                   = plotRSEData,
      fim                       = fimOptimal,
      evaluationForPlot         = evaluationOptimalDesign,
      pfimproject               = pfimproject,
      projectName               = projectName
    ),
    .covReportFlags( hasCov, covariateTestTables, covariatesTable )
  )
  generateReportOptimization( fimOptimal, optimizationAlgorithm,
                              reportTables, outputFile, outputPath )
}
