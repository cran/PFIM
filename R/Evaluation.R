#' Class "Evaluation"
#'
#' @description A class storing information concerning the evaluation of a design.
#'
#' @name Evaluation-class
#' @aliases Evaluation
#' @docType class
#' @include PFIMProject.R
#' @include GenericMethods.R
#' @include Model.R
#' @include ModelError.R
#' @include Fim.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Evaluation} can be created by calls of the form \code{Evaluation(...)} where (...) are the parameters for the \code{Evaluation} objects.
#'
#'@section Slots for the \code{Evaluation} objects:
#' \describe{
#' \item{\code{name}:}{A string giving the name of the project.}
#' \item{\code{model}:}{A object of class \linkS4class{Model} giving the model.}
#' \item{\code{modelEquations}:}{A list giving the model equations.}
#' \item{\code{modelParameters}:}{A list giving the model parameters.}
#' \item{\code{modelError}:}{A list giving the model error for each outcome of the model.}
#' \item{\code{outcomes}:}{A list giving the model outcomes.}
#' \item{\code{designs}:}{A list giving the designs to be evaluated.}
#' \item{\code{fim}:}{An object of the class \code{Fim} containing the Fisher Information Matrix of the design.}
#' \item{\code{odeSolverParameters}:}{}
#' }

Evaluation = setClass(
  Class = "Evaluation",
  contains = "PFIMProject",
  representation = representation(
    name = "character",
    model = "Model",
    modelEquations = "list",
    modelParameters ="list",
    modelError = "list",
    outcomes = "list",
    designs = "list",
    fim = "Fim",
    odeSolverParameters = "list"),

  prototype = prototype( odeSolverParameters = list( atol = 1e-6, rtol = 1e-6 ) ) )

setMethod(f="initialize",
          signature="Evaluation",
          definition=function(.Object, name, model, modelEquations, modelParameters, modelError, outcomes, designs, fim, odeSolverParameters )
          {
            if(!missing(name))
            {
              .Object@name = name
            }

            if(!missing(model))
            {
              .Object@model = model
            }

            if(!missing(modelEquations))
            {
              .Object@modelEquations = modelEquations
            }

            if(!missing(outcomes))
            {
              .Object@outcomes = outcomes
            }

            if(!missing(designs))
            {
              .Object@designs = designs
            }

            if(!missing(fim))
            {
              if ( fim == "population")
              {
                .Object@fim = PopulationFim()
              }
              else if ( fim == "individual")
              {
                .Object@fim = IndividualFim()
              }
              else if ( fim == "Bayesian")
              {
                .Object@fim = BayesianFim()
              }
            }

            if(!missing(odeSolverParameters))
            {
              .Object@odeSolverParameters = odeSolverParameters
            }

            # ===========================================
            # set the names of the designs
            # ===========================================

            names(.Object@designs)= getNames( designs )

            if(!missing(modelError))
            {
              .Object@modelError = modelError
            }

            if(!missing(modelParameters))
            {
              .Object@modelParameters = modelParameters
            }

            validObject(.Object)
            return (.Object )
          }
)

# ======================================================================================================
# run
# ======================================================================================================

#' @rdname run
#' @export

setMethod(f = "run",
          signature = "Evaluation",
          definition = function( object )
          {
            # =================================================
            # define the new model
            # designs, fim, model error, ode solver parameters
            # =================================================

            designs = getDesigns( object )
            fim = getFim( object )

            # ============================================================
            # define a new model: user defined or from library of models
            # ============================================================

            model = getModel( object )

            modelEquations = getModelEquations( object )
            odeSolverParameters = getOdeSolverParameters( object )
            equations = modelEquations$equations
            outcomes = modelEquations$outcomes
            modelFromLibrary = modelEquations$modelFromLibrary

            model = setEquations( model, equations )
            model = setModelFromLibrary( model, modelFromLibrary )
            model = setOutcomes( model, outcomes )
            model = setParameters( model, getModelParameters( object ) )
            model = setModelError( model, getModelError( object ) )
            model = defineModel( model, designs )

            # ============================================================
            # set outcomes for the evaluation and ode solver parameters
            # ============================================================

            outcomesForEvaluation = getOutcomes( object )
            model = setOutcomesForEvaluation( model, outcomesForEvaluation )
            model = setOdeSolverParameters( model, odeSolverParameters )

            # ===========================
            # set the the model
            # ===========================

            object = setModel( object, model )

            # ==================================
            # evaluate the model for each design
            # ==================================

            for ( design in designs )
            {
              designName = getName( design )
              object@designs[[ designName ]] = EvaluateDesign( design, model, fim )
            }

            return( object )
          })

#' @title show
#' @rdname show
#' @param object object
#' @export

setMethod(f="show",
          signature = "Evaluation",
          definition = function( object )
          {
            # ==================================
            # get initial designs
            # ==================================

            designs = getDesigns( object )
            designsNames = getNames( designs )

            # ==================================
            # get model
            # ==================================

            model = getModel( object )

            for ( designName in designsNames )
            {
              design = designs[[designName]]
              fim = getFim( design )

              fisherMatrix = getFisherMatrix( fim )
              FIMFixedEffects = getFixedEffects( fim )
              FIMVarianceEffects = getVarianceEffects( fim )

              correlationMatrix = getCorrelationMatrix( fim )
              correlationMatrixFixedEffects = correlationMatrix$fixedEffects
              correlationMatrixVarianceEffects = correlationMatrix$varianceEffects

              # ==================================
              # SE and RSE
              # ==================================

              fisherMatrix = getFisherMatrix( fim )
              SE = getSE( fim )

              rseAndParametersValues = getRSE( fim, model )

              RSE = rseAndParametersValues$RSE
              parametersValues = rseAndParametersValues$parametersValues

              SEandRSE = data.frame( parametersValues, SE, RSE )
              colnames( SEandRSE ) = c("Value", "SE","RSE (%)" )

              # ===============================================
              # determinants, condition numbers and Dcriterion
              # ===============================================

              detFim = getDeterminant( fim )
              condFIMFixedEffects = getConditionNumberFixedEffects( fim )
              condFIMVarianceEffects = getConditionNumberVarianceEffects( fim )
              DCriterion = getDcriterion( fim )

              # =================
              # shrinkage
              # =================

              shrinkage = getShrinkage( fim )

              if (!is.null(shrinkage))
              {
                names( shrinkage) = colnames( FIMFixedEffects )
              }

              criteriaFim = t(data.frame( detFim, condFIMFixedEffects, condFIMVarianceEffects, DCriterion ) )

              colnames(criteriaFim) = c("Value")
              rownames(criteriaFim) = c("Determinant",
                                        "Cond number fixed effects",
                                        "Cond number variance components",
                                        "D-criterion")

              # ============================
              # display results in R console
              # ============================

              designName = getName( design )

              cat("*************************\n\n" )
              cat(paste0("Design: ", designName, "\n\n" ))
              cat("*************************\n\n" )

              cat("*************************\n" )
              cat("Fisher information matrix \n" )
              cat("*************************\n" )
              cat("\n" )
              cat("**** Fixed effect","\n\n" )

              print( FIMFixedEffects )

              cat("\n" )
              cat("**** Variance components","\n\n" )

              print( FIMVarianceEffects )

              cat("\n" )
              cat("******************\n" )
              cat("Correlation matrix  \n" )
              cat("******************\n" )
              cat("\n" )
              cat("**** Fixed effect","\n\n" )

              print( correlationMatrixFixedEffects )

              cat("\n" )
              cat("**** Variance components","\n\n" )

              print( correlationMatrixVarianceEffects )

              cat("\n" )
              cat("**********************************************\n" )
              cat("Determinant, condition numbers and D-criterion \n" )
              cat("**********************************************\n" )
              cat("\n" )

              print( criteriaFim, row.names = FALSE )

              cat("\n" )
              cat("**********\n" )
              cat("Shrinkage \n" )
              cat("**********\n" )
              cat("\n" )

              print( shrinkage )

              cat("\n" )
              cat("**********\n" )
              cat("SE and RSE \n" )
              cat("**********\n" )
              cat("\n" )

              print( SEandRSE )

              cat("\n" )
            }
          })

#' Generate all the table for the evaluation report
#'
#' @title reportTablesPlot
#' @param object An object \code{evaluation} from the class \linkS4class{Evaluation}.
#' @param plotOptions A list containing the options for the plots.
#' @return The list \code{tables} containing the tables for the evaluation report.
#' @export

setGeneric("reportTablesPlot",
           function( object, plotOptions )
           {
             standardGeneric("reportTablesPlot")
           })

#' @rdname reportTablesPlot
#' @export

setMethod(f="reportTablesPlot",
          signature("Evaluation"),
          function( object, plotOptions )
          {
            plotOutcomesEvaluation = plotEvaluation( object, plotOptions )
            plotOutcomesGradient = plotSensitivityIndice( object, plotOptions )

            plotSE = plotSE( object, plotOptions )
            plotRSE = plotRSE( object, plotOptions )
            plotShrinkage = plotShrinkage( object, plotOptions )

            tables = list( plotOutcomesEvaluation = plotOutcomesEvaluation,
                           plotOutcomesGradient = plotOutcomesGradient,
                           plotSE = plotSE,
                           plotRSE = plotRSE,
                           plotShrinkage = plotShrinkage )

            return( tables )
          })

# ======================================================================================================
# generateTables
# ======================================================================================================

#' @rdname generateTables
#' @export

setMethod(f="generateTables",
          signature("Evaluation"),
          function( object, plotOptions )
          {
            # ===========================================
            # get model and model error
            # ===========================================

            model = getModel( object )

            # ===========================================
            # get design
            # ===========================================

            designs = getDesigns( object )
            designNames = getNames( designs )
            designName = designNames[[1]]
            design = designs[[designName]]

            # ===========================================
            # get fim
            # ===========================================

            fim = getFim( design )

            # ===========================================
            # tables for model equations
            # ===========================================

            modelEquations = getEquations( model )
            modelOutcomes = getOutcomes( object )
            tablesModelEquations = list( outcomes = modelOutcomes, equations = modelEquations )

            # ===========================================
            # tables for model error
            # tables for model parameters
            # ===========================================

            tablesModelParameters = reportTablesModelParameters( model )
            tablesModelError = reportTablesModelError( model )

            # ===========================================
            # tables for administration
            # ===========================================

            tablesAdministration = reportTablesAdministration( design  )

            # ===========================================
            # tables for design
            # ===========================================

            tablesDesign = reportTablesDesign( design  )

            # ===========================================
            # tables for FIM
            # ===========================================

            tablesFIM = reportTablesFIM( fim, object )

            # ===========================================
            # tables for plot design, SI, SE and RSE
            # ===========================================

            tablesPlot = reportTablesPlot( object, plotOptions )

            # ===========================================
            # tables for report
            # ===========================================

            reportTables = list( tablesModelEquations = tablesModelEquations,
                                 tablesModelError = tablesModelError,
                                 tablesModelParameters = tablesModelParameters,
                                 tablesDesign = tablesDesign,
                                 tablesAdministration = tablesAdministration,
                                 tablesFIM = tablesFIM,
                                 tablesPlot = tablesPlot )

            return( reportTables )

          })

# ======================================================================================================
# Report
# ======================================================================================================

#' @rdname Report
#' @export

setMethod(f="Report",
          signature("Evaluation"),
          function( object, outputPath, outputFile, plotOptions )
          {
            designs = getDesigns( object )
            designNames = getNames( designs )
            designName = designNames[[1]]
            design = designs[[designName]]
            fim = getFim( design )

            generateReportEvaluation( fim, object, outputPath, outputFile, plotOptions )

          })

# ======================================================================================================
# getFisherMatrix, getCorrelationMatrix, getRSE, getDcriterion, getDeterminant
# ======================================================================================================

#' @rdname getFisherMatrix
#' @export

setMethod("getFisherMatrix",
          signature = "Evaluation",
          definition = function (object)
          {
            designs = getDesigns( object )
            designsNames = getNames( designs )

            fisherMatrices = list()

            for ( designName in designsNames )
            {
              design = designs[[designName]]

              fim = getFim( design )

              fisherMatrix = getFisherMatrix( fim )

              fixedEffect = getFixedEffects( fim )

              varianceEffects = getVarianceEffects( fim )

              fisherMatrices[[designName]] = list( fisherMatrix = fisherMatrix,
                                                   fixedEffect = fixedEffect,
                                                   varianceEffects = varianceEffects )
            }

            return( fisherMatrices )

          })

#' @rdname getCorrelationMatrix
#' @export

setMethod("getCorrelationMatrix",
          signature = "Evaluation",
          definition = function (object)
          {
            designs = getDesigns( object )
            designsNames = getNames( designs )

            model = getModel( object )

            correlationMatrix = list()

            for ( designName in designsNames )
            {
              design = designs[[designName]]

              fim = getFim( design )

              correlationMatrix[[designName]] = getCorrelationMatrix( fim )
            }
            return( correlationMatrix )
          })

#' @rdname getSE
#' @export

setMethod("getSE",
          signature = "Evaluation",
          definition = function (object)
          {
            designs = getDesigns( object )

            designsNames = getNames( designs )

            model = getModel( object )

            SE = list()

            for ( designName in designsNames )
            {
              design = designs[[designName]]

              fim = getFim( design )

              SE[[designName]] = getSE( fim )
            }
            return( SE )
          })

#' @rdname getRSE
#' @export

setMethod("getRSE",
          signature = "Evaluation",
          definition = function ( object, model )
          {
            designs = getDesigns( object )
            designsNames = getNames( designs )

            model = getModel( object )

            RSE = list()

            for ( designName in designsNames )
            {
              design = designs[[designName]]

              fim = getFim( design )

              rseAndParametersValues = getRSE( fim, model )

              RSE[[designName]] = rseAndParametersValues$RSE
            }
            return( RSE )
          })

#' @rdname getDcriterion
#' @export

setMethod( "getDcriterion",
           signature = "Evaluation",
           definition = function(object)
           {
             designs = getDesigns( object )
             designsNames = getNames( designs )

             model = getModel( object )

             Dcriterion = list()

             for ( designName in designsNames )
             {
               design = designs[[designName]]

               fim = getFim( design )

               Dcriterion[[designName]] = getDcriterion( fim )

             }

             return( Dcriterion )
           })

#' @rdname getShrinkage
#' @export

setMethod( "getShrinkage",
           signature = "Evaluation",
           definition = function(object)
           {
             designs = getDesigns( object )
             designsNames = getNames( designs )

             model = getModel( object )

             shrinkage = list()

             for ( designName in designsNames )
             {
               design = designs[[designName]]

               fim = getFim( design )

               shrinkage[[designName]] = getShrinkage( fim )

               FIMFixedEffects = getFixedEffects( fim )

               if ( !is.null( shrinkage[[designName]]  ) )
               {
                 names( shrinkage[[designName]]  ) = colnames( FIMFixedEffects )
               }

               return( shrinkage )
             }
           })

#' @rdname getDeterminant
#' @export

setMethod( "getDeterminant",
           signature = "Evaluation",
           definition = function(object)
           {
             designs = getDesigns( object )
             designsNames = getNames( designs )

             model = getModel( object )

             determinant = list()

             for ( designName in designsNames )
             {
               design = designs[[designName]]

               fim = getFim( design )

               determinant[[designName]] = getDeterminant( fim )
             }

             return( determinant )
           })

##########################################################################################################
# END Class "Evaluation"
##########################################################################################################


