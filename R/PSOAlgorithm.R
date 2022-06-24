
##########################################################################################################
#' Class "PSOAlgorithm"
#'
#' @description
#' The Class "PSOAlgorithm" implements the PSO algortihm : Particle Swarm Optimization
#'
#' @name PSOAlgorithm-class
#' @docType class
#' @include Optimization.R
#' @include Design.R
#' @exportClass PSOAlgorithm

#' @section Objects from the class \code{PSOAlgorithm}:
#' Objects form the xlass \code{PSOAlgorithm} can be created by calls of the form \code{PSOAlgorithm(...)} where
#' (...) are the parameters for the \code{PSOAlgorithm} objects.

#'@section Slots for \code{PSOAlgorithm} objects:
#'  \describe{
#'    \item{\code{maxIteration}:}{A numeric giving the maximum of iterations.}
#'    \item{\code{populationSize}:}{A numeric giving the mpopulation size.}
#'    \item{\code{intertiaWeight}:}{A numeric giving the inertial weight.}
#'    \item{\code{personalLearningCoefficient}:}{A numeric giving the personal learning coefficient.}
#'    \item{\code{globalLearningCoefficient}:}{A numeric giving the global learning coefficient.}
#'    \item{\code{resultsPSO}:}{A list giving the iteration and the results when a new best criteria is found.}
#'  }
##########################################################################################################

PSOAlgorithm<-setClass(
  Class = "PSOAlgorithm",
  contains = "Optimization",
  representation = representation(
    maxIteration = "numeric",
    populationSize = "numeric",
    intertiaWeight = "numeric",
    seed = "numeric",
    personalLearningCoefficient = "numeric",
    globalLearningCoefficient = "numeric",
    resultsOptimization = "list",
    showProcess = "logical",
    OptimalDesign = "Design" ),
  prototype = prototype(
    showProcess = F ) )

setMethod(
  f="initialize",
  signature="PSOAlgorithm",
  definition= function ( .Object,
                         maxIteration,
                         populationSize,
                         intertiaWeight,
                         personalLearningCoefficient,
                         globalLearningCoefficient,
                         seed,
                         showProcess )
  {
    # values by default
    .Object@maxIteration <- 200
    .Object@populationSize <- 200
    .Object@intertiaWeight <- 0.7298
    .Object@personalLearningCoefficient <- 1.4962
    .Object@globalLearningCoefficient <- 1.4962
    .Object@seed <- -1

    .Object@showProcess <- TRUE

    if ( !missing( maxIteration ) )
      .Object@maxIteration <- maxIteration

    if ( !missing( populationSize ) )
      .Object@populationSize <- populationSize

    if ( !missing( intertiaWeight ) )
      .Object@intertiaWeight <- intertiaWeight

    if ( !missing( personalLearningCoefficient ) )
      .Object@personalLearningCoefficient <- personalLearningCoefficient

    if ( !missing( globalLearningCoefficient ) )
      .Object@globalLearningCoefficient <- globalLearningCoefficient

    if ( !missing( seed ) )
      .Object@seed <- seed

    if ( !missing( showProcess ) )
      .Object@showProcess <- showProcess

    validObject( .Object )
    return ( .Object )
  })

setMethod( f = "Optimize",
           signature = "PSOAlgorithm",
           definition = function( object, pfimProject, designs, statistical_model, constraint, typeFim )
           {
             if( object@seed != -1 )
               set.seed( object@seed )

             callNextMethod( object, pfimProject, designs, statistical_model , constraint, typeFim )

             # Initilisation
             # get responses from statistical model
             responses = getResponsesStatisticalModel( statistical_model )
             nameResponses = names( responses )

             position = list()
             velocity = list()
             bestPosition = list()
             cost = list()
             bestCost = list()
             globalBestPosition = list()
             globalBestCost =  Inf
             results = list()
             listDesign = list()
             stepSize = list()

             designA = designs[[1]]
             best = designA
             arms = getArms( designA )
             nameArms = names( arms )

             # initialization PSO
             k = 1
             results = data.frame()

             while ( length(globalBestPosition) ==0){

               for ( iterPop in 1:object@populationSize )
               {
                 constraintsPop = TRUE
                 cost[[ iterPop ]] = Inf
                 bestCost[[ iterPop ]] = Inf

                 for( nameArm in nameArms )
                 {
                   arm = arms[[ nameArm ]]

                   samplings = getSamplings( arm )

                   for( nameResponse in nameResponses )
                   {
                     sample_time = getSampleTime( samplings[[ nameResponse ]])
                     sizeSampleTime = length( sample_time )

                     # Set initial position
                     position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                       sort( runif( sizeSampleTime, min( sample_time ),  max( sample_time )  ) )

                     # Set the best position
                     bestPosition[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                       position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]

                     # Set initial velocity
                     velocity[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                       runif( sizeSampleTime, min( sample_time ), max( sample_time ) )

                     # Model constraints
                     samplingTimes = position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]

                     samplingConstraints = getSamplingConstraintsInArm( arm, nameResponse )

                     for ( samplingConstraint in samplingConstraints )
                     {
                       if ( isLessThanDelay( samplingConstraint, samplingTimes ) == TRUE )
                       {
                         constraintsPop = FALSE
                         break
                       }

                       for ( time in samplingTimes )
                       {
                         if ( isTimeInBetweenBounds( samplingConstraint, time ) == FALSE )
                         {
                           constraintsPop = FALSE
                           break
                         }
                       }
                     }
                     arm <- modifySamplingTimes( arm, nameResponse,  position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]  )
                   } # end response
                   designA  = modifyArm( designA, nameArm, arm)
                 } # end arm

                 # Constraints TRUE : modified the Design with arms and responses

                 if (constraintsPop == TRUE )
                 {
                   # Evaluation
                   evaluatedDesign = EvaluateDesignForEachArm( designA, statistical_model, typeFim )
                   cost[[ iterPop ]] =  1 / getDcriterion( getFimOfDesign( evaluatedDesign ) )

                   # Update personal best

                   for( nameArm in nameArms )
                   {
                     for( nameResponse in nameResponses )
                     {
                       bestPosition[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                         position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]
                     }
                   }

                   # Update bestCost, globalBestCost, globalBestPosition globalBestPosition
                   bestCost[[ iterPop ]] = cost[[ iterPop ]]

                   # index min of bestCost
                   indexMinBestCost = which.min( bestCost )

                   if ( bestCost[[ indexMinBestCost ]] <= globalBestCost )
                   {
                     for( nameArm in nameArms )
                     {
                       for( nameResponse in nameResponses )
                       {
                         globalBestPosition[[ nameResponse ]][[ nameArm ]] =
                           bestPosition[[ nameResponse ]][[ nameArm ]][[ indexMinBestCost ]]
                       }
                     }
                     globalBestCost = bestCost[[ indexMinBestCost ]]
                   }
                 } # end constraintselse
               } # end iterPop

               if ( length(globalBestPosition) != 0)
               {
                 break
               }else{k=k+1}
             }

             # PSO
             for( iter in 1:object@maxIteration )
             {
               if (object@showProcess == TRUE)
               {
                 print( paste0( " PSO iter = ",iter ) )
               }

               if ( iter %in% c(1, object@maxIteration ))
               {
                 results = rbind( c( iter , globalBestCost ) , results)
               }

               for ( iterPop in 1:object@populationSize )
               {
                 constraintsPop = TRUE

                 for( nameArm in nameArms )
                 {
                   arm = arms[[ nameArm ]]

                   samplings = getSamplings( arm )

                   for( nameResponse in nameResponses )
                   {
                     # get the sample time
                     sample_time = getSampleTime( samplings[[ nameResponse ]])
                     sizeSampleTime = length( sample_time )

                     samplingConstraints = getSamplingConstraintsInArm( arm, nameResponse )
                     valuesIntervals = getallowedContinuousSamplingTimes( samplingConstraints[[1]] )
                     minPosition = min( unlist(valuesIntervals ) )
                     maxPosition = max( unlist(valuesIntervals ) )

                     # Update Velocity
                     r1 = runif( sizeSampleTime, 0, 1 )
                     r2 = runif( sizeSampleTime, 0, 1 )

                     # Constricted PSOs
                     # phi = constriction factor
                     phi = object@personalLearningCoefficient + object@globalLearningCoefficient
                     constrictionFactor = 2 / abs( 2 - phi - sqrt( phi * ( phi - 4 ) ) )

                     velocity[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                       constrictionFactor * ( velocity[[ nameResponse ]][[ nameArm ]][[ iterPop ]]  +
                                                object@personalLearningCoefficient * r1 * ( bestPosition[[ nameResponse ]][[ nameArm ]][[ iterPop ]] - position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] ) +
                                                object@globalLearningCoefficient * r2 * ( globalBestPosition[[ nameResponse ]][[ nameArm ]] -  position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] ) )


                     # Update Position
                     position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] =
                       position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] + velocity[[ nameResponse ]][[ nameArm ]][[ iterPop ]]

                     # Apply position limits
                     for( j in 1:sizeSampleTime )
                     {
                       position[[ nameResponse ]][[ nameArm ]][[ iterPop ]][ j ] =
                         min( maxPosition, position[[ nameResponse ]][[ nameArm ]][[ iterPop ]][ j ] )

                       position[[ nameResponse ]][[ nameArm ]][[ iterPop ]][ j ] =
                         max( minPosition, position[[ nameResponse ]][[ nameArm ]][[ iterPop ]][ j ] )
                     }

                     position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] = sort( position[[ nameResponse ]][[ nameArm ]][[ iterPop ]] )

                     # Constraints of the model : windows for sampling times and min time delay

                     samplingTimes =  position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]

                     for (   samplingConstraint in samplingConstraints )
                     {
                       if ( isLessThanDelay( samplingConstraint, samplingTimes ) == TRUE )
                       {
                         constraintsPop = FALSE
                         break
                       }

                       for ( time in samplingTimes )
                       {
                         if ( isTimeInBetweenBounds( samplingConstraint, time ) == FALSE )
                         {
                           constraintsPop = FALSE
                           break
                         }
                       }
                     }
                     arm <- modifySamplingTimes( arm, nameResponse,  position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]  )
                   } # end reponse
                   designA  = modifyArm( designA, nameArm, arm)
                 } # end arm

                 # Constraints and cost and global cost of the model
                 if (constraintsPop == TRUE )
                 {
                   arms = getArms( designA )
                   nameArms = names( arms )

                   # Evaluation
                   evaluatedDesign = EvaluateDesignForEachArm( designA, statistical_model, typeFim )
                   cost[[ iterPop ]] =  1 / getDcriterion( getFimOfDesign( evaluatedDesign ) )

                   if (is.nan( cost[[ iterPop ]]))
                   {
                     cost[[ iterPop ]]=Inf
                   }

                   if (is.nan( bestCost[[ iterPop ]]))
                   {
                     bestCost[[ iterPop ]]=Inf
                   }

                   # Update personal best
                   if ( cost[[ iterPop ]] < bestCost[[ iterPop ]] )
                   {
                     for( nameArm in nameArms )
                     {
                       for( nameResponse in nameResponses )
                       {
                         bestPosition[[ nameResponse ]][[ nameArm ]][[ iterPop ]] = position[[ nameResponse ]][[ nameArm ]][[ iterPop ]]
                       }
                     }
                     bestCost[[ iterPop ]] = cost[[ iterPop ]]
                   }

                   # index min of bestCost
                   indexMinBestCost = which.min( bestCost )

                   if ( bestCost[[ indexMinBestCost ]] < globalBestCost )
                   {
                     # update particle positions
                     for( nameArm in nameArms )
                     {
                       for( nameResponse in nameResponses )
                       {
                         globalBestPosition[[ nameResponse ]][[ nameArm ]] = bestPosition[[ nameResponse ]][[ nameArm ]][[ indexMinBestCost ]]
                       }
                     }

                     globalBestCost = bestCost[[ indexMinBestCost ]]

                     results = rbind( c( iter , 1/globalBestCost ), results )

                     if (object@showProcess == TRUE)
                     {
                       print( paste0( " global best cost = ", globalBestCost ) )
                     }
                     best = designA
                     best@isOptimalDesign = TRUE
                   }
                 } # end constraints
               } # end iterPop
             } #end iter

             # set armSize to 1 for individual and bayesian Fim

             arms = getArms( best )

             if ( class( typeFim ) %in% c( "IndividualFim", "BayesianFim" ) )
             {
               for ( arm in arms)
               {
                 arm = setArmSize( arm, 1 )
                 nameArm = getNameArm( arm )
                 best = modifyArm( best, nameArm, arm)
               }
             }

             colnames( results ) = c("Iteration","Criteria")
             object@OptimalDesign <- best
             object@OptimalDesign@name = paste0( "Design optimized from",constraint@name )
             object@resultsOptimization <- results

             return(object)
           })

########################################################################################################
# END Class "PSO"
########################################################################################################

