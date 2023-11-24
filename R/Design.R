#' Class "Design"
#'
#' @description The class \code{Design} defines information concerning the parametrization of the designs.
#'
#' @name Design-class
#' @aliases Design
#' @docType class
#' @include GenericMethods.R
#' @include Fim.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Design} can be created by calls of the form \code{Design(...)} where (...) are the parameters for the \code{Design} objects.
#'
#'@section Slots for the \code{Design} objects:
#' \describe{
#' \item{\code{name}:}{A string giving the name of the design.}
#' \item{\code{size}:}{An integer giving the number of subjects in the design.}
#' \item{\code{arms}:}{A list of the arms.}
#' \item{\code{outcomesEvaluation}:}{A list of the results of the design evaluation for the outcomes.}
#' \item{\code{outcomesGradient}:}{A list of the results of the design evaluation for the sensitivity indices.}
#' \item{\code{numberOfArms}:}{A numeric giving the number of arms in the design.}
#' \item{\code{fim}:}{An object of the class \code{Fim} containing the Fisher Information Matrix of the design.}
#' }

Design = setClass("Design",
                  representation = representation(
                    name = "character",
                    size = "numeric",
                    arms = "list",
                    outcomesEvaluation = "list",
                    outcomesGradient = "list",
                    numberOfArms = "numeric",
                    fim = "Fim" ) )

setMethod(f="initialize",
          signature="Design",
          definition= function (.Object, name, size, arms, outcomesEvaluation, outcomesGradient, numberOfArms, fim )
          {
            if(!missing(name))
            {
              .Object@name = name
            }
            if(!missing(size))
            {
              .Object@size = size
            }
            if(!missing(arms))
            {
              .Object@arms = unlist(arms)
            }
            if(!missing(outcomesEvaluation))
            {
              .Object@outcomesEvaluation = outcomesEvaluation
            }
            if(!missing(outcomesGradient))
            {
              .Object@outcomesGradient = outcomesGradient
            }
            if(!missing(fim))
            {
              .Object@fim = fim
            }
            if(!missing(numberOfArms))
            {
              .Object@numberOfArms = numberOfArms
            }
            validObject(.Object)
            return (.Object )
          }
)

# ======================================================================================================
# getName
# ======================================================================================================

#' @rdname getName
#' @export

setMethod(f="getName",
          signature="Design",
          definition = function(object)
          {
            return(object@name)
          })

# ======================================================================================================
# setName
# ======================================================================================================

#' @rdname setName
#' @export

setMethod(f="setName",
          signature="Design",
          definition = function(object, name)
          {
            object@name = name
            return(object)
          })

# ======================================================================================================
# getSize
# ======================================================================================================

#' @rdname getSize
#' @export

setMethod(f="getSize",
          signature="Design",
          definition = function(object)
          {
            return(object@size)
          }
)

# ======================================================================================================
# setSize
# ======================================================================================================

#' @rdname setSize
#' @export

setMethod(f="setSize",
          signature="Design",
          definition = function(object,size)
          {
            object@size = size
            return(object)
          }
)
# ======================================================================================================
# getArms
# ======================================================================================================

#' @rdname getArms
#' @export

setMethod(f="getArms",
          signature="Design",
          definition = function(object)
          {
            return(object@arms)
          }
)

# ======================================================================================================
# setArms
# ======================================================================================================

#' @rdname setArms
#' @export

setMethod(f="setArms",
          signature="Design",
          definition = function(object,arms)
          {
            object@arms = arms
            return(object)
          }
)

#' Get the results of the evaluation of the outcomes.
#'
#' @title getOutcomesEvaluation
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @return The list \code{outcomesEvaluation} containing the results of the design evaluation for the outcomes.
#' @export

setGeneric("getOutcomesEvaluation",
           function(object)
           {
             standardGeneric("getOutcomesEvaluation")
           }
)

#' @rdname getOutcomesEvaluation
#' @export

setMethod(f="getOutcomesEvaluation",
          signature="Design",
          definition = function(object)
          {
            return( object@outcomesEvaluation )
          }
)

#' Set the results of the evaluation of the outcomes.
#'
#' @title setOutcomesEvaluation
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param outcomesEvaluation A list containing the evaluation of the outcomes.
#' @return An object \code{Design} with the list \code{outcomesEvaluation} updated.
#' @export

setGeneric("setOutcomesEvaluation",
           function( object, outcomesEvaluation )
           {
             standardGeneric("setOutcomesEvaluation")
           }
)

#' @rdname setOutcomesEvaluation
#' @export

setMethod(f="setOutcomesEvaluation",
          signature="Design",
          definition = function(object,outcomesEvaluation)
          {
            object@outcomesEvaluation = outcomesEvaluation
            return(object)
          }
)

#' Get the results of the evaluation of the outcome gradients.
#'
#' @title getOutcomesGradient
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @return  The list \code{outcomesGradient} containing the results of the design evaluation for the outcome gradients.
#' @export

setGeneric("getOutcomesGradient",
           function(object)
           {
             standardGeneric("getOutcomesGradient")
           }
)

#' @rdname getOutcomesGradient
#' @export

setMethod(f="getOutcomesGradient",
          signature="Design",
          definition = function(object)
          {
            return(object@outcomesGradient)
          }
)

#' Set the results of the evaluation of the outcomes.
#'
#' @title setOutcomesGradient
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param outcomesGradient A list containing the evaluation of the outcome gradients.
#' @return An object \code{Design} with the list \code{outcomesGradient} updated.
#' @export

setGeneric("setOutcomesGradient",
           function(object,outcomesGradient)
           {
             standardGeneric("setOutcomesGradient")
           }
)

#' @rdname setOutcomesGradient
#' @export

setMethod(f="setOutcomesGradient",
          signature="Design",
          definition = function(object,outcomesGradient)
          {
            object@outcomesGradient = outcomesGradient
            return(object)
          }
)

#' @rdname getFim
#' @export

setMethod(f="getFim",
          signature="Design",
          definition = function(object)
          {
            return( object@fim )
          }
)

#' Set the fim of the design.
#'
#' @title setFim
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param fim An object \code{fim} from the class \linkS4class{Fim}.
#' @return An object \code{Design} with the \code{fim} updated.
#' @export

setGeneric("setFim",
           function(object,fim)
           {
             standardGeneric("setFim")
           }
)

#' @rdname setFim
#' @export

setMethod(f="setFim",
          signature="Design",
          definition = function(object,fim)
          {
            object@fim = fim
            return( object )
          }
)

#' Get the number of arms in a design.
#'
#' @title getNumberOfArms
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @return A numeric \code{numberOfArms} giving the number of arms in the design.
#' @export

setGeneric(
  "getNumberOfArms",
  function(object) {
    standardGeneric("getNumberOfArms")
  })

#' @rdname getNumberOfArms
#' @export

setMethod("getNumberOfArms",
          "Design",
          function( object )
          {
            return( object@numberOfArms )
          }
)

#' Set the number of arms in a design.
#'
#' @title setNumberOfArms
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param numberOfArms A numeric \code{numberOfArms} giving the new number of arms in the design.
#' @return An object \code{Design} with the \code{numberOfArms} updated.
#' @export

setGeneric(
  "setNumberOfArms",
  function( object , numberOfArms ) {
    standardGeneric("setNumberOfArms")
  })

#' @rdname setNumberOfArms
#' @export

setMethod("setNumberOfArms",
          "Design",
          function( object, numberOfArms )
          {
            object@numberOfArms = numberOfArms
            return( object )
          }
)

#' Set the arms in a design.
#'
#' @title setArm
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param arm A list of object \code{Arm} giving the arms of the design.
#' @return An object \code{Design} with the list \code{Arm} updated.
#' @export

setGeneric("setArm",
           function( object, arm )
           {
             standardGeneric("setArm")
           }
)

#' @rdname setArm
#' @export

setMethod(f="setArm",
          signature = "Design",
          definition = function( object, arm )
          {
            arms = getArms( object )

            armsNames = lapply( arms, function (x) getName(x))

            armName = getName( arm )

            indexName = which( armsNames == armName )

            object@arms[[indexName]] = arm

            return( object )
          })

#' Evaluate an design
#'
#' @title EvaluateDesign
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param model An object \code{model} from the class \linkS4class{Model}.
#' @param fim An object \code{fim} from the class \linkS4class{Fim}.
#' @return The object \code{Design} with its slot \code{fim}, \code{evaluationOutcomes}, \code{outcomesGradient} updated.
#' @export

setGeneric("EvaluateDesign",
           function( object, model, fim )
           {
             standardGeneric("EvaluateDesign")
           }
)

#' @rdname EvaluateDesign
#' @export

setMethod(f="EvaluateDesign",
          signature = "Design",
          definition = function( object, model, fim )
          {
            fisherMatrix = list()
            evaluationOutcomes =  list()
            outcomesGradient =  list()

            arms = getArms( object )

            # =====================
            # evaluate all arms
            # =====================

            for ( arm in arms )
            {
              armName = getName( arm )
              evaluateArm = EvaluateArm( arm, model, fim )
              evaluationOutcomes[[armName]] = evaluateArm$evaluationOutcomes
              outcomesGradient[[armName]] = evaluateArm$outcomesGradient
              fisherMatrix[[armName]] = getFisherMatrix( evaluateArm$fim )
            }

            # =====================
            # set Fim parameters
            # =====================

            fisherMatrix = Reduce( "+", fisherMatrix )
            fim = setFisherMatrix( fim, fisherMatrix )
            fim = setFixedEffects( fim )
            fim = setVarianceEffects( fim )

            # =====================
            # set the shrinkage
            # =====================

            shrinkage = getShrinkage( evaluateArm$fim )
            fim = setShrinkage( fim, shrinkage )

            # =====================
            # set Fim
            # =====================

            object = setFim( object, fim )

            # ===========================
            # set responses and gradients
            # ===========================

            object = setOutcomesEvaluation( object, evaluationOutcomes )
            object = setOutcomesGradient( object, outcomesGradient )

            return( object )
          })

#' Plot the evaluation of the outcomes.
#'
#' @title plotOutcomesEvaluation
#' @param object An object \code{Design} from the class \linkS4class{Design}.
#' @param initialDesign An object \code{design} from the class \linkS4class{Design}.
#' @param model An object \code{model} from the class \linkS4class{Model}.
#' @param plotOptions A list containing the plot options.
#' @return A list containing the plots the evaluation of the outcomes.
#' @export

setGeneric(
  "plotOutcomesEvaluation",
  function( object, initialDesign, model, plotOptions ) {
    standardGeneric("plotOutcomesEvaluation")
  })

#' @rdname plotOutcomesEvaluation
#' @export

setMethod(f="plotOutcomesEvaluation",
          signature("Design"),
          function( object, initialDesign, model, plotOptions ){

            # ===============================================================
            # evaluated design and initial design for initial sampling times
            # ===============================================================

            maxYAxis = list()
            minYAxis = list()

            initialDesignName = getName( initialDesign )
            designName = getName( object )

            evaluationInitialDesign = getOutcomesEvaluation( initialDesign )
            evaluationDesign = getOutcomesEvaluation( object )

            arms = getArms( object )

            outcomes = getOutcomes( model )
            outcomesNames = names( outcomes )

            for ( arm in arms )
            {
              armName = getName( arm )

              for ( outcomeName in outcomesNames )
              {
                maxYAxis[[designName]][[armName]][[outcomeName]] = max( evaluationDesign[[armName]][[outcomeName]][,outcomeName] )
                minYAxis[[designName]][[armName]][[outcomeName]] = 0
              }
            }

            # ================================
            # plot options
            # ================================

            plotOptions = getPlotOptions( plotOptions, outcomesNames )
            unitXAxis = plotOptions$unitXAxis
            unitYAxis = plotOptions$unitYAxis

            # ================================
            # ggplot
            # ================================

            plotOutcome = list()

            for ( arm in arms )
            {
              armName = getName( arm )

              for ( outcomeName in outcomesNames )
              {
                evaluationDesignPlot = evaluationDesign[[armName]][[outcomeName]]

                evaluationInitialDesignPlot = evaluationInitialDesign[[armName]][[outcomeName]]
                evaluationInitialDesignPlot$time = round( evaluationInitialDesignPlot$time, 2 )

                plotOutcome[[armName]][[outcomeName]] = ggplot() +

                  geom_line( evaluationDesignPlot,
                             mapping = aes_string( x = "time", y = outcomeName ) ) +

                  geom_point( evaluationInitialDesignPlot,
                              mapping = aes_string( x = "time", y = outcomeName ), color = "red" ) +

                  theme( legend.position = "none",plot.title = element_text(hjust = 0.5),
                         axis.title.x.top = element_text(color = "red"),
                         axis.text.x.top = element_text(angle = 90, hjust = 0, color = "red" ) ) +

                  labs(y = paste0( outcomeName," ", "(",unitYAxis[[outcomeName]],") \n"),
                       x = paste0( paste0("Time"," ", "(",unitXAxis,")"),
                                   "\n \n Design: ",  sub("_", " ",designName),
                                   "      Arm: ",  armName)) +

                  coord_cartesian( ylim = c( minYAxis[[designName]][[armName]][[outcomeName]] ,
                                             maxYAxis[[designName]][[armName]][[outcomeName]] ) ) +

                  scale_x_continuous(breaks = scales::pretty_breaks( n = 10 ),
                                     sec.axis = sec_axis(~ . * 1,
                                                         breaks = evaluationInitialDesignPlot$time,
                                                         name = c( "Sampling times" ) ) ) +

                  scale_y_continuous( breaks = scales::pretty_breaks( n = 10 ) )
              }
            }
            return( plotOutcome )
          })

#' Plot the evaluation of the outcome gradients.
#'
#' @title plotOutcomesGradient
#' @param object An object \code{design} from the class \linkS4class{Design}.
#' @param initialDesign An object \code{design} from the class \linkS4class{Design}.
#' @param model An object \code{model} from the class \linkS4class{Model}.
#' @param plotOptions A list containing the plot options.
#' @return A list containing the plots the evaluation of the outcome gradients..
#' @export

setGeneric(
  "plotOutcomesGradient",
  function( object, initialDesign, model, plotOptions ) {
    standardGeneric("plotOutcomesGradient")
  })

#' @rdname plotOutcomesGradient
#' @export

setMethod(f="plotOutcomesGradient",
          signature("Design"),
          function( object, initialDesign, model, plotOptions ){

            # ==============================================================
            # evaluated design and initial design for initial sampling times
            # ==============================================================

            maxYAxis = list()
            minYAxis = list()

            parametersNames = getNames( getParameters( model ) )

            initialDesignName = getName( initialDesign )
            designName = getName( object )

            gradientInitialDesign = getOutcomesGradient( initialDesign )
            gradientDesign = getOutcomesGradient( object )

            arms = getArms( object )

            outcomes = getOutcomes( model )
            outcomesNames = names( outcomes )

            for ( arm in arms )
            {
              armName = getName( arm )

              for ( outcomeName in outcomesNames )
              {
                maxYAxis[[designName]][[armName]][[outcomeName]] = max( gradientDesign[[armName]][[outcomeName]][,parametersNames] )
                minYAxis[[designName]][[armName]][[outcomeName]] = min( gradientDesign[[armName]][[outcomeName]][,parametersNames] )
              }
            }

            # ========================
            # plot options
            # ========================

            plotOptions = getPlotOptions( plotOptions, outcomesNames )
            unitXAxis = plotOptions$unitXAxis
            unitYAxis = plotOptions$unitYAxis

            # ========================
            # ggplot
            # ========================

            plotOutcome = list()

            for ( arm in arms )
            {
              armName = getName( arm )

              for ( outcomeName in outcomesNames )
              {
                gradientDesignPlot = gradientDesign[[armName]][[outcomeName]]
                gradientInitialDesignPlot = as.data.frame( gradientInitialDesign[[armName]][[outcomeName]] )
                gradientInitialDesignPlot$time = round( gradientInitialDesignPlot$time, 2 )

                for ( parameterName in parametersNames)
                {
                  plotOutcome[[armName]][[outcomeName]][[parameterName]] = ggplot() +

                    geom_line( gradientDesignPlot,
                               mapping = aes_string( x = "time", y = parameterName ) ) +

                    geom_point( gradientInitialDesignPlot,
                                mapping = aes_string( x = "time", y = parameterName ), color = "red" ) +

                    theme( legend.position = "none",plot.title = element_text(hjust = 0.5),
                           axis.title.x.top = element_text(color = "red"),
                           axis.text.x.top = element_text(angle = 90, hjust = 0, color = "red" ) ) +

                    labs( y = paste("df/d", parameterName, sep=""),
                          x = paste0(paste0("Time"," ", "(",unitXAxis,")"),
                                     "\n \n Design: ",  sub("_", " ",designName ),
                                     "      Arm: ",  armName,
                                     "      Outcome: ",  outcomeName,
                                     "      Parameter: ",  parameterName ) )+

                    coord_cartesian( ylim = c( minYAxis[[designName]][[armName]][[outcomeName]] ,
                                               maxYAxis[[designName]][[armName]][[outcomeName]]) ) +

                    scale_x_continuous(breaks = scales::pretty_breaks( n = 10 ),
                                       sec.axis = sec_axis(~ . * 1,
                                                           breaks = gradientInitialDesignPlot$time,
                                                           name = c( "Sampling times" ) ) ) +

                    scale_y_continuous( breaks = scales::pretty_breaks( n = 10 ) )
                }
              }
            }

            return( plotOutcome )
          })


#' @title show
#' @rdname show
#' @param object object
#' @export

setMethod(f="show",
          signature="Design",
          definition = function(object)
          {
            designName = getName(object)

            arms = getArms( object )

            optimalDesignSamplingTimes = list()

            for ( arm in arms )
            {
              armName = getName( arm )

              armSize = getSize( arm )
              armSize = round( armSize, 2 )

              samplingTimes = getSamplingTimes(arm)
              outcomes = lapply( samplingTimes, function(x) getOutcome(x) )

              administrations = getAdministrations( arm )

              for (outcome in outcomes )
              {
                samplingTime = getSamplingTime(arm,outcome)
                samplings = getSamplings( samplingTime )
                samplings = sort( round( samplings,2 ) )
                samplings = paste0("(",toString(samplings),")")

                administration = getAdministration( arm , outcome )

                if ( length( administration ) == 0 )
                {
                  dose = "-"
                }else{
                  dose = toString( getDose( administration ) )
                }

                optimalDesignSamplingTimes[[designName]][[armName]][[outcome]] = data.frame( c( designName, armName, armSize,
                                                                                                outcome, dose, samplings ) )
              }
            }

            dataFramOptimalDesign = t(data.frame(optimalDesignSamplingTimes))
            rownames(dataFramOptimalDesign) = NULL
            colnames(dataFramOptimalDesign) = c("Design","Arm","Arm size","Response","Dose", "Sampling times")

            print( dataFramOptimalDesign )
          }
)

#' Generate table for the report.
#'
#' @title reportTablesSamplingConstraints
#' @param object An object \code{design} from the class \linkS4class{Design}.
#' @return A table of the sampling constraints parameters for the report.
#' @export

setGeneric(
  "reportTablesSamplingConstraints",
  function(object) {
    standardGeneric("reportTablesSamplingConstraints")
  })

#' @rdname reportTablesSamplingConstraints
#' @export

setMethod("reportTablesSamplingConstraints",
          signature("Design"),
          function( object )
          {
            listSamplingConstraints = list()

            designName = getName( object )

            arms = getArms( object )

            k=1

            for ( arm in arms )
            {
              armName = getName( arm )

              samplingTimesConstraints = getSamplingTimesConstraints( arm )

              administrationsConstraints = getAdministrationsConstraints( arm )

              if ( length( administrationsConstraints ) != 0 )
              {
                doses = getDose( administrationsConstraints[[1]] )
                doses = toString( doses )

              }else
              {
                doses = "-"
              }

              for( samplingTimesConstraint in samplingTimesConstraints )
              {
                initialSamplings = getSamplings( samplingTimesConstraint )
                fixedTimes = getFixedTimes( samplingTimesConstraint )
                numberOfsamplingsOptimisable = getNumberOfsamplingsOptimisable( samplingTimesConstraint )
                samplingsWindows = getSamplingsWindows( samplingTimesConstraint )
                numberOfTimesByWindows = getNumberOfTimesByWindows( samplingTimesConstraint )
                minSampling = getMinSampling( samplingTimesConstraint )
                outcome = getOutcome( samplingTimesConstraint )

                initialSamplings = paste0( "(",toString( initialSamplings ),")" )

                if ( length( fixedTimes ) == 0 )
                {
                  fixedTimes = "-"
                }else{
                  fixedTimes = toString( fixedTimes )
                }

                if ( length( numberOfsamplingsOptimisable ) == 0 )
                {
                  numberOfsamplingsOptimisable = "-"
                }else{
                  numberOfsamplingsOptimisable = toString( numberOfsamplingsOptimisable )
                }

                if ( length( samplingsWindows ) == 0 )
                {
                  samplingsWindows = "-"
                }else{
                  samplingsWindows = paste0( unlist( lapply( samplingsWindows, function(x) paste0("(",toString(x),")" )  ) ), collapse = ", " )
                }

                if ( length( numberOfTimesByWindows ) == 0 )
                {
                  numberOfTimesByWindows = "-"
                }else{
                  numberOfTimesByWindows = paste0( unlist( lapply( numberOfTimesByWindows, function(x) toString(x) ) ), collapse = ", " )
                }

                if ( length( minSampling ) == 0 )
                {
                  minSampling = "-"
                }else{
                  minSampling = paste0( unlist( lapply( minSampling, function(x) toString(x) ) ), collapse = ", " )
                }

                doses = toString( doses )
                listSamplingConstraints[[k]] = c( designName, armName, outcome, doses,
                                                  initialSamplings, fixedTimes, numberOfsamplingsOptimisable,
                                                  samplingsWindows, numberOfTimesByWindows, minSampling )
                k=k+1
              }
            }

            listSamplingConstraints = do.call( rbind, listSamplingConstraints )

            listSamplingConstraints = as.data.frame( listSamplingConstraints )

            colnames( listSamplingConstraints ) = c("Design","Arm","Outcome","Doses",
                                                    "Initial sampling times","Fixed times", "Number of samplings optimisable",
                                                    "Sampling windows", "Number of sampling times by windows", "Minimal sampling size")

            rownames( listSamplingConstraints ) = NULL

            return( listSamplingConstraints )
          })

#' Generate table for the report.
#'
#' @title reportTablesAdministration
#' @param object An object \code{design} from the class \linkS4class{Design}.
#' @return A table of the administration parameters for the report.
#' @export

setGeneric(
  "reportTablesAdministration",
  function(object) {
    standardGeneric("reportTablesAdministration")
  })

#' @rdname reportTablesAdministration
#' @export

setMethod("reportTablesAdministration",
          signature("Design"),
          function( object )
          {
            administrationTmp = list()
            k=1

            designName = getName( object )

            arms = getArms( object )

            for ( arm in arms )
            {
              armName = getName( arm )

              administrations = getAdministrations( arm )

              for( administration in administrations )
              {
                outcome = getOutcome( administration )
                tau = getTau( administration )
                Tinf = getTinf( administration )
                timeDoses = getTimeDose( administration )
                doses = getDose( administration )

                if ( length( Tinf ) == 0 )
                {
                  Tinf = "-"
                }else{
                  Tinf = toString( Tinf )
                }

                if ( tau == 0 )
                {
                  tau = "-"
                }else{
                  tau = toString( tau )
                }

                timeDoses = toString( timeDoses )
                doses = toString( doses )

                administrationTmp[[k]] = c( designName, armName, outcome, tau, Tinf, timeDoses, doses )
                k=k+1
              }
            }

            administrationTmp = do.call( rbind, administrationTmp )
            colnames( administrationTmp ) = c("Design","Arm","Response","${\\tau}$","${T_{inf}}$","Time dose","Dose")
            rownames( administrationTmp ) = NULL

            administrationTable = knitr::kable( administrationTmp ) %>%
              kable_styling( font_size = 12,
                             latex_options = c("hold_position","striped", "condensed", "bordered" ),
                             full_width = T)

            return( administrationTable )

          })

#' Generate table for the report.
#'
#' @title reportTablesDesign
#' @param object An object \code{design} from the class \linkS4class{Design}.
#' @return A table of the design parameters for the report.
#' @export

setGeneric(
  "reportTablesDesign",
  function(object) {
    standardGeneric("reportTablesDesign")
  })

#' @rdname reportTablesDesign
#' @export

setMethod("reportTablesDesign",
          signature("Design"),
          function( object )
          {
            designName = getName(object)

            arms = getArms( object )

            optimalDesignSamplingTimes = list()

            for ( arm in arms )
            {
              armName = getName( arm )

              armSize = getSize( arm )
              armSize = round( armSize, 2 )

              samplingTimes = getSamplingTimes(arm)
              outcomes = lapply( samplingTimes, function(x) getOutcome(x) )

              administrations = getAdministrations( arm )

              outcomesWithAdministration = unlist( lapply( administrations,function(x) getOutcome(x) ) )

              for (outcome in outcomes )
              {
                samplingTime = getSamplingTime(arm,outcome)
                samplings = getSamplings( samplingTime )
                samplings = round(samplings,2)
                samplings = paste0("(",toString(samplings),")")

                if ( outcome %in% outcomesWithAdministration )
                {
                  administration = getAdministration( arm , outcome )

                  dose = getDose( administration )
                  dose = toString( dose )

                }else{
                  dose = "-"
                }
                optimalDesignSamplingTimes[[designName]][[armName]][[outcome]] = data.frame( c( designName, armName, armSize,  outcome, dose, samplings ) )
              }
            }

            dataFramOptimalDesign = t(data.frame(optimalDesignSamplingTimes))
            rownames(dataFramOptimalDesign) = NULL
            colnames(dataFramOptimalDesign) = c("Design name","Arms name","Number of subjects","Outcome", "Dose", "Sampling times")

            designTable = knitr::kable( dataFramOptimalDesign ) %>%
              kable_styling( font_size = 12,
                             latex_options = c("hold_position","striped", "condensed", "bordered" ),
                             full_width = T)

            return( designTable )
          })

#' Check the validity of he sampling times constraints
#'
#' @title checkValiditySamplingConstraint
#' @param object An object from the class \linkS4class{Design}.
#' @return An error message if a constraint is not valid.
#' @export

setGeneric("checkValiditySamplingConstraint",
           function( object  )
           {
             standardGeneric("checkValiditySamplingConstraint")
           }
)

#' @rdname checkValiditySamplingConstraint
#' @export

setMethod(f="checkValiditySamplingConstraint",
          signature = "Design",
          definition = function( object )
          {
            arms = getArms( object )

            for ( arm in arms )
            {
              armName = getName( arm )
              samplingTimesConstraints = getSamplingTimesConstraints( arm )
              outcomes = unlist( lapply( samplingTimesConstraints, function(x) getOutcome( x ) ) )

              # =================================
              # get samplings window constraints
              # =================================

              samplingsWindow = lapply( samplingTimesConstraints, function(x) getSamplingsWindows(x) )
              names( samplingsWindow ) = outcomes

              # =======================================
              # get numberOfTimesByWindows constraints
              # =======================================

              numberOfTimesByWindows = lapply( samplingTimesConstraints, function(x) getNumberOfTimesByWindows(x) )
              names( numberOfTimesByWindows ) = outcomes

              # =======================================
              # get minimal time step for each windows
              # =======================================

              minSampling = lapply( samplingTimesConstraints, function(x) getMinSampling(x) )
              names( minSampling ) = outcomes

              inputRandomSpaced = list()
              samplingTimesArms = list()

              for ( outcome in outcomes )
              {
                intervalsConstraints = list()

                # ==================================
                # get samplingTimes and samplings
                # ==================================

                samplingTimes = getSamplingTime( arm, outcome )
                samplings = getSamplings( samplingTimes )

                minSamplingAndNumberOfTimesByWindows = as.data.frame( list( minSampling[[outcome]], numberOfTimesByWindows[[outcome]] ) )
                tmp = t( as.data.frame(samplingsWindow[[outcome]] ) )
                inputRandomSpaced[[outcome]] = as.data.frame( do.call( "cbind", list( tmp, minSamplingAndNumberOfTimesByWindows ) ) )

                colnames( inputRandomSpaced[[outcome]] ) = c("min","max","delta","n")
                rownames( inputRandomSpaced[[outcome]] ) = NULL

                if ( sum( numberOfTimesByWindows[[outcome]] ) != length( samplings ) )
                {
                  print ( " ==================================================================================================== ")
                  print( paste0( " The sampling times constraint is not possible for arm ", armName, " and outcome ", outcome ) )
                  print ( " ==================================================================================================== ")

                  stop()
                }

                for( iter in 1:length( inputRandomSpaced[[outcome]]$n ) )
                {
                  min = inputRandomSpaced[[outcome]]$min[iter]
                  max = inputRandomSpaced[[outcome]]$max[iter]
                  delta = inputRandomSpaced[[outcome]]$delta[iter]
                  n = inputRandomSpaced[[outcome]]$n[iter]

                  distance = max-min-(n-1)*delta

                  if ( distance < 0 )
                  {
                    print ( " ==================================================================================================== ")
                    print( paste0( " The sampling times constraint is not possible for arm ", armName, " and outcome ", outcome ) )
                    print ( " ==================================================================================================== ")

                    stop()
                  }
                }
              }
            }
          })

#' Set the sampling times constraint for optimization with PSO, PGBO and Simplex
#'
#' @title setSamplingConstraintForOptimization
#' @param object An object from the class \linkS4class{Design}.
#' @return The arms with the sampling times constraints.
#' @export

setGeneric("setSamplingConstraintForOptimization",
           function( object )
           {
             standardGeneric("setSamplingConstraintForOptimization")
           }
)

#' @rdname setSamplingConstraintForOptimization
#' @export

setMethod(f="setSamplingConstraintForOptimization",
          signature = "Design",
          definition = function( object  )
          {
            arms = getArms( object )

            for ( arm in arms )
            {
              # =============================
              # get the outcomes
              # =============================

              samplingTimes = getSamplingTimes( arm )
              outcomes = lapply(samplingTimes, function(x) getOutcome(x))

              # ======================================================
              # set the sampling time constraints for missing outcomes
              # ie from its sampling times
              # ======================================================

              samplingTimesConstraints = getSamplingTimesConstraints( arm )

              outcomesSamplingTimesConstraints = lapply( samplingTimesConstraints, function(x) getOutcome(x) )
              indexOutcomeNotInSamplingTimesConstraints = which( !( outcomes %in% outcomesSamplingTimesConstraints ) )
              outcomesSamplingNotInTimesConstraints = outcomes[indexOutcomeNotInSamplingTimesConstraints]

              if ( length( outcomesSamplingNotInTimesConstraints ) !=0 )
              {
                for (  outcomeSamplingNotInTimesConstraints in outcomesSamplingNotInTimesConstraints )
                {
                  samplingTime = getSamplingTime( arm, outcomeSamplingNotInTimesConstraints )

                  samplings = getSamplings( samplingTime )

                  newSamplingTimeConstraints  = SamplingTimeConstraints( outcome = outcomeSamplingNotInTimesConstraints,
                                                                         initialSamplings = samplings,
                                                                         samplingsWindows = list( c( min( samplings ),max( samplings ) ) ),
                                                                         numberOfTimesByWindows = length( samplings ),
                                                                         minSampling = 0 )

                  samplingTimesConstraints = append( samplingTimesConstraints, newSamplingTimeConstraints )
                }
              }
              arm = setSamplingTimesConstraints( arm, samplingTimesConstraints )
              object = setArm( object, arm )
            }
            return( object )
          })

##########################################################################################################
# END Class "Design"
##########################################################################################################






