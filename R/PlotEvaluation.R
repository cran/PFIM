#' Class "PlotEvaluation"
#'
#' @description A class storing information concerning the design evaluation.
#' The class \code{PlotEvaluation} inherits from the class \code{Evaluation}.
#'
#' @name PlotEvaluation-class
#' @aliases PlotEvaluation
#' @include GenericMethods.R
#' @include Evaluation.R
#' @include Model.R
#' @docType class
#' @export

PlotEvaluation = setClass(
  Class = "PlotEvaluation",
  contains = "Evaluation"
)

#' Graphs of the results of the evaluation.
#'
#' @name plotEvaluation
#' @param object An object from the class \linkS4class{Evaluation}.
#' @param plotOptions A list giving the plot options.
#' @return A list giving the graphs for the evaluation of the responses and sensitivity indices.
#' @export

setGeneric(
  "plotEvaluation",
  function(object, plotOptions )
  {
    standardGeneric("plotEvaluation")
  })

#' @rdname plotEvaluation
#' @export

setMethod(f="plotEvaluation",
          signature("Evaluation"),
          function( object, plotOptions )
          {
            evaluationArm = list()
            plotOutcomesEvaluation = list()

            # get model and designs
            model = getModel( object )
            designs = getDesigns( object )

            for ( design in designs )
            {
              initialDesign = design
              arms = getArms( design )

              for ( arm in arms )
              {
                armName = getName( arm )

                # get data from design for arms
                dataForArmEvaluation = dataForArmEvaluation( design, arm, model )

                # set data for arm evaluation
                arm = setDataForArmEvaluation( arm, dataForArmEvaluation )

                # set new sampling times for plot
                samplingTimes = getSamplingTimes( arm )

                for ( samplingTime in samplingTimes )
                {
                  samplings = getSamplings( samplingTime )
                  samplings = sort( unique( c( samplings, seq( 0, max( samplings ), 0.1 ) ) ) )
                  samplingTime = setSamplings( samplingTime, samplings )
                  arm = setSamplingTime( arm, samplingTime )
                }
                design = setArm( design, arm )
              }

              arms = getArms( design )

              # evaluate model for each arm
              for ( arm in arms )
              {
                armName = getName( arm )

                dataForArmEvaluation = dataForArmEvaluation( design, arm, model )

                arm = setDataForArmEvaluation( arm, dataForArmEvaluation )

                dataForModelEvaluation = setDataForModelEvaluation( model, arm )

                evaluationArm[[armName]] = EvaluateModel( model, dataForModelEvaluation, design )
              }

              # define arms in design
              design = setOutcomesEvaluation( design, evaluationArm )

              designName = getName( design )

              outcomesEvaluationInitialDesign = getOutcomesEvaluation( initialDesign )

              # plot the outcomes
              plotOutcomesEvaluation[[designName]] = plotOutcomesEvaluation( design,
                                                                             outcomesEvaluationInitialDesign,
                                                                             model, plotOptions )
            }

            return( plotOutcomesEvaluation )

          })

#' Graphs of the results of the evaluation.
#'
#' @name plotSensitivityIndice
#' @param object An object from the class \linkS4class{Evaluation}.
#' @param plotOptions A list giving the plot options.
#' @return A list giving the graphs for the evaluation of the responses and sensitivity indices.
#' @export

setGeneric(
  "plotSensitivityIndice",
  function(object, plotOptions )
  {
    standardGeneric("plotSensitivityIndice")
  })

#' @rdname plotSensitivityIndice
#' @export

setMethod(f="plotSensitivityIndice",
          signature("Evaluation"),
          function( object, plotOptions )
          {
            evaluationArm = list()
            plotOutcomesGradient = list()

            # get model and designs
            model = getModel( object )
            designs = getDesigns( object )

            for ( design in designs )
            {
              initialDesign = design

              arms = getArms( design )

              for ( arm in arms )
              {
                armName = getName( arm )

                # get data from design for arms
                dataForArmEvaluation = dataForArmEvaluation( design, arm, model )

                # set data for arm evaluation
                arm = setDataForArmEvaluation( arm, dataForArmEvaluation )

                # set new sampling times for plot
                samplingTimes = getSamplingTimes( arm )

                for ( samplingTime in samplingTimes )
                {
                  samplings = getSamplings( samplingTime )
                  samplings = sort( unique( c( samplings, seq( 0, max( samplings ), 0.1 ) ) ) )
                  samplingTime = setSamplings( samplingTime, samplings )
                  arm = setSamplingTime( arm, samplingTime )
                }
                design = setArm( design, arm )
              }

              arms = getArms( design )

              # evaluate model for each arm
              for ( arm in arms )
              {
                armName = getName( arm )

                dataForArmEvaluation = dataForArmEvaluation( design, arm, model )

                arm = setDataForArmEvaluation( arm, dataForArmEvaluation )

                dataForModelEvaluation = setDataForModelEvaluation( model, arm )

                evaluationGradient = EvaluateModelGradient( model, dataForModelEvaluation, arm )

                evaluationArm[[armName]] = evaluationGradient$outcomesGradient
              }

              # define arms in design
              design = setOutcomesGradient( design, evaluationArm )

              # plot the outcomes
              designName = getName( design )

              outcomesGradientInitialDesign = getOutcomesGradient( initialDesign )

              plotOutcomesGradient[[designName]] = plotOutcomesGradient( design,
                                                                         outcomesGradientInitialDesign, model,
                                                                         plotOptions )
            }
            return( plotOutcomesGradient )
          })

#' Graph the SE.
#'
#' @name plotSE
#' @param object An object from the class \linkS4class{Evaluation}.
#' @param plotOptions A list giving the plot options.
#' @return A graph of the SE.
#' @export

setGeneric(
  "plotSE",
  function( object, plotOptions )
  {
    standardGeneric("plotSE")
  })

#' @rdname plotSE
#' @export

setMethod(f="plotSE",
          signature("PFIMProject"),

          function( object, plotOptions )
          {
            # get initial designs
            designs = getDesigns( object )

            # get model
            model = getModel( object )

            plotOutcome = list()

            for ( designName in names( designs ) )
            {
              design = designs[[designName]]
              fim = getFim( design )
              fimName = class(fim)[1]

              # get the SE
              SEValues = getSE( fim )

              # get the parameters names from the fim
              fisherMatrix = getFisherMatrix( fim )

              # SE and RSE dataframes
              SE = getSE( fim )

              rseAndParametersValues = getRSE( fim, model )
              RSE = rseAndParametersValues$RSE
              parametersValues = rseAndParametersValues$parametersValues

              SEandRSE = data.frame( parametersValues, SE, RSE )
              colnames( SEandRSE ) = c("Value", "SE","RSE (%)" )

              mu = "\u03bc_"
              omega = "\u03c9\u00B2_"
              sigma = "\u03c3_"

              indexMu = which( grepl( mu, rownames( SEandRSE ) ) == TRUE )
              indexOmega = which( grepl( omega, rownames( SEandRSE ) ) == TRUE )
              indexSigma = which( grepl( sigma, rownames( SEandRSE ) ) == TRUE )

              columnNamesFIM = getColumnAndParametersNamesFIM( fim, model )
              indexParameters = c( indexMu, indexOmega, indexSigma )

              if ( length( indexMu ) !=0 )
              {
                parameters = c( columnNamesFIM$namesFIMFixedEffectsParameters )
                columnForPlot = rep( "SE~mu", length( indexMu) )
              }

              if ( length( indexOmega ) !=0 )
              {
                parameters = c( parameters, columnNamesFIM$namesFIMVarianceEffectsParameters )
                columnForPlot = c( columnForPlot, rep( "SE~omega^2", length( indexOmega) ) )
              }

              if ( length( indexSigma ) !=0 )
              {
                parameters = c( parameters, columnNamesFIM$namesFIMModelErrorParameters )
                columnForPlot = c( columnForPlot, rep( "SE~sigma", length( indexSigma) ) )
              }

              SEPlot = data.frame( parameters, SEValues, columnForPlot )
              colnames( SEPlot ) = c("parameter","SEValues","SE")

              # plot
              plotOutcome[[designName]] = ggplot( SEPlot, aes( x = parameters, y = SEValues ) ) +

                theme(legend.position = "none",
                      plot.title = element_text(size=16, hjust = 0.5),
                      axis.title.x = element_text(size=16),
                      axis.title.y = element_text(size=16),
                      axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
                      axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
                      strip.text.x = element_text(size = 16)) +

                geom_bar(stat="identity",
                         width = 0.5,
                         position = position_dodge2(preserve = "single")) +

                scale_x_discrete( guide = guide_axis(check.overlap = TRUE ) ) +

                facet_grid(. ~ SE, labeller= label_parsed, scales="free", space = "free") +

                xlab("Parameter") +
                ylab("SE") +

                ggtitle( paste0( designName,": " , fimName ) )
            }
            return( plotOutcome )
          })

#' Graph of the RSE.
#'
#' @name plotRSE
#' @param object An object from the class \linkS4class{Evaluation}.
#' @param plotOptions A list giving the plot options.
#' @return A graph of the RSE.
#' @export

setGeneric(
  "plotRSE",
  function( object, plotOptions )
  {
    standardGeneric("plotRSE")
  })

#' @rdname plotRSE
#' @export

setMethod(f="plotRSE",
          signature("PFIMProject"),

          function( object, plotOptions )
          {
            # get initial designs
            designs = getDesigns( object )

            # get model
            model = getModel( object )

            plotOutcome = list()

            for ( designName in names( designs ) )
            {
              design = designs[[designName]]
              fim = getFim( design )
              fimName = class(fim)[1]

              # get the SE
              SEValues = getSE( fim )

              # get the parameters names from the fim
              fisherMatrix = getFisherMatrix( fim )

              # SE and RSE dataframes
              SE = getSE( fim )

              rseAndParametersValues = getRSE( fim, model )
              RSEValues = rseAndParametersValues$RSE
              parametersValues = rseAndParametersValues$parametersValues

              SEandRSE = data.frame( parametersValues, SE, RSEValues )
              colnames( SEandRSE ) = c("Value", "SE","RSE (%)" )

              mu = "\u03bc_"
              omega = "\u03c9\u00B2_"
              sigma = "\u03c3_"

              indexMu = which( grepl( mu, rownames( SEandRSE ) ) == TRUE )
              indexOmega = which( grepl( omega, rownames( SEandRSE ) ) == TRUE )
              indexSigma = which( grepl( sigma, rownames( SEandRSE ) ) == TRUE )

              columnNamesFIM = getColumnAndParametersNamesFIM( fim, model )
              indexParameters = c( indexMu, indexOmega, indexSigma )

              if ( length( indexMu ) !=0 )
              {
                parameters = c( columnNamesFIM$namesFIMFixedEffectsParameters )
                columnForPlot = rep( "RSE~mu", length( indexMu) )
              }

              if ( length( indexOmega ) !=0 )
              {
                parameters = c( parameters, columnNamesFIM$namesFIMVarianceEffectsParameters )
                columnForPlot = c( columnForPlot, rep( "RSE~omega^2", length( indexOmega) ) )
              }

              if ( length( indexSigma ) !=0 )
              {
                parameters = c( parameters, columnNamesFIM$namesFIMModelErrorParameters )
                columnForPlot = c( columnForPlot, rep( "RSE~sigma", length( indexSigma) ) )
              }

              RSEPlot = data.frame( parameters, RSEValues, columnForPlot )

              colnames( RSEPlot ) = c("parameter","RSEValues","RSE")

              # ggplot
              plotOutcome[[designName]] = ggplot( RSEPlot, aes( x = parameters, y = RSEValues ) ) +

                theme(legend.position = "none",
                      plot.title = element_text(size=16, hjust = 0.5),
                      axis.title.x = element_text(size=16),
                      axis.title.y = element_text(size=16),
                      axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
                      axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
                      strip.text.x = element_text(size = 16))+

                geom_bar(stat="identity",
                         width = 0.5,
                         position = position_dodge2( preserve = "single" ) ) +

                scale_x_discrete( guide = guide_axis( check.overlap = TRUE ) ) +

                facet_grid(. ~ RSE, labeller = label_parsed, scales = "free", space = "free" ) +

                ggtitle( paste0( designName,": " , fimName ) ) +

                xlab("Parameter") +
                ylab("RSE (%)")

            }
            return( plotOutcome )
          })

#' Graph of the shrinkage.
#'
#' @name plotShrinkage
#' @param object An object from the class \linkS4class{Evaluation}.
#' @param plotOptions A list giving the plot options.
#' @return A graph of the shrinkage.
#' @export

setGeneric(
  "plotShrinkage",
  function( object, plotOptions )
  {
    standardGeneric("plotShrinkage")
  })

#' @rdname plotShrinkage
#' @export

setMethod(f="plotShrinkage",
          signature("PFIMProject"),

          function( object, plotOptions )
          {
            # get initial designs
            designs = getDesigns( object )

            # get model
            model = getModel( object )

            plotOutcome = list()

            for ( designName in names( designs ) )
            {
              design = designs[[designName]]
              fim = getFim( design )
              fimName = class(fim)[1]

              # get the shrinkage
              shrinkage = getShrinkage( fim )

              # null for pop and ind fim
              if ( is.null( shrinkage ) == TRUE )
              {
                plotOutcome[[designName]] = NULL
              }else{

                # model parameter names
                columnNamesFIM = getColumnAndParametersNamesFIM( fim, model )
                parameters = columnNamesFIM$namesFIMFixedEffectsParameters

                shrinkagePlot = data.frame( parameters, shrinkage )

                # plot
                plotOutcome[[designName]] = ggplot( shrinkagePlot, aes( x = parameters, y = shrinkage ) ) +

                  theme(legend.position = "none",
                        plot.title = element_text(size=16, hjust = 0.5),
                        axis.title.x = element_text(size=16),
                        axis.title.y = element_text(size=16),
                        axis.text.x = element_text(size=16, angle = 90, vjust = 0.5),
                        axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
                        strip.text.x = element_text(size = 16))+

                  geom_bar(stat="identity",
                           width = 0.5,
                           position = position_dodge2(preserve = "single")) +

                  scale_x_discrete( guide = guide_axis(check.overlap = TRUE ) ) +

                  ggtitle( paste0( designName,": " , fimName ) ) +

                  xlab("Parameter") +
                  ylab("Shrinkage")

              }
            }
            return( plotOutcome )
          })

# ########################################################################################################################
# END Class PlotEvaluation
# ########################################################################################################################





