##########################################################################################################
#' Class "PGBOAlgorithm"
#'
#' @description
#' The Class "PGBOAlgorithm" implements the PGBO algortihm : Population Genetics Based Optimizer,
#' developped by Hervé Le Nagard [1].
#'
#' @references [1] Rebecca Bauer, France Mentré, Halima Kaddouri, Jacques Le Bras, Hervé Le Nagard,
#' Benefits of a new Metropolis-Hasting based algorithm, in non-linear regression for estimation of ex vivo antimalarial sensitivity in patients infected with two strains,
#' Computers in Biology and Medicine, Volume 55, 2014, Pages 16-25, ISSN 0010-4825
#'
#' @name PGBOAlgorithm-class
#'
#' @docType class
#' @include Optimization.R
#' @include Design.R
#'
#' @exportClass PGBOAlgorithm

#' @section Objects from the Class \code{PGBOAlgorithm}:
#' Objects form the Class \code{PGBOAlgorithm} can be created by calls of the form \code{PGBOAlgorithm(...)} where
#' (...) are the parameters for the \code{PGBOAlgorithm} objects.

#'@section Slots for \code{PGBOAlgorithm} objects:
#'  \describe{
#'    \item{\code{N}:}{A numeric giving the population size.}
#'    \item{\code{muteEffect}:}{A numeric giving the mutation effect.}
#'    \item{\code{max_iteration}:}{A numeric giving the maximum of iterations.}
#'    \item{\code{iteration_fin}:}{A numeric giving the last iteration.}
#'    \item{\code{showProcess}:}{A boolean to show or not the process.}
#'    \item{\code{OptimalDesign}:}{A \code{Design} object giving the optimal design.}
#'    \item{\code{resultsPGBO}:}{A list giving the optimal D-criterion computed during the process.}
#'  }
##########################################################################################################

PGBOAlgorithm<-setClass(
  Class = "PGBOAlgorithm",
  contains = "Optimization",
  representation = representation(
    N = "numeric", # population size
    muteEffect = "numeric", # mutation effect
    maxIteration = "numeric",
    iteration_fin = "numeric",
    seed = "numeric",
    showProcess = "logical",
    OptimalDesign = "Design",
    resultsOptimization = "list"
  ),
  prototype = prototype(
    showProcess = F
  )
)

setMethod(
  f="initialize",
  signature="PGBOAlgorithm",
  definition= function ( .Object,
                         N,
                         muteEffect,
                         maxIteration,
                         seed,
                         showProcess )
  {
    # values by default
    .Object@N <- 50
    .Object@muteEffect <- 0.2
    .Object@maxIteration <- 10e4
    .Object@seed <- -1

    if ( !missing( N ) )
      .Object@N <- N

    if ( !missing( muteEffect ) )
      .Object@muteEffect <- muteEffect

    if ( !missing( maxIteration ) )
      .Object@maxIteration <- maxIteration

    if ( !missing( seed ) )
      .Object@seed <- seed

    if ( !missing( showProcess ) )
      .Object@showProcess <- showProcess

    validObject( .Object )
    return ( .Object )
  }
)

results = data.frame()

# -------------------------------------------------------------------------------------------------------------------
#' Optimization with the PGBO Algorithm.
#'
#' @rdname Optimize
#' @param object A \code{PGBOAlgorithm} object.
#' @param pfimProject A \code{PFIMProject} object.
#' @param designs A list \code{Design} objects.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param typeFim A \code{Fim} object.
#' @return The \code{PGBOAlgorithm} object with:
#' \itemize{
#' \item \code{{resultsOptimization}:}{A dataframe giving the results for each iteration.}
#' \item \code{{OptimalDesign}:}{A  \code{Design} object giving the optimal design.}
#' \item \code{{iteration_fin }:}{A numeric of the final iteration of the process.}
#'}

setMethod( f = "Optimize",
           signature = "PGBOAlgorithm",
           definition = function( object, pfimProject, designs, statistical_model, constraint, typeFim )

           {
             if( object@seed != -1 )
               set.seed( object@seed )

             callNextMethod( object, statistical_model , constraint, typeFim )

             designA = designs[[1]]

             best = designA

             evaluatedDesign = EvaluateDesignForEachArm( designA, statistical_model, typeFim )

             d = 1 / getDcriterion( getFimOfDesign( evaluatedDesign ) )

             fitBase = 0.03

             theta = -log10( fitBase ) / d

             fitA = 10**( -theta * d )
             fitBest = fitA

             N2<- 2 * object@N

             for ( iteration in 1: object@maxIteration )
             {
               if (object@showProcess == TRUE)
               {
                 print( paste0( " PGBO iter = ",iteration ) )
               }

               if ( iteration ==1 | iteration == object@maxIteration)
               {
                 results = rbind( c( iteration , d ) , results)
               }

               found = FALSE

               while ( !found )
               {
                 numberOfTheArm = 1 + floor( getAmountOfArms( designA ) * runif( 1 ) )

                 arms = getArms( designA )

                 arm = arms[[ numberOfTheArm ]]

                 numberOfTheResponse = 1 + floor( getNumberOfSamplings( arm ) * runif( 1 ) )

                 samplingTimes = getSampleTime( getSamplings( arm )[[ numberOfTheResponse ]] )

                 # geometric mean for mutation effect
                 object@muteEffect =  0.05 * exp( mean( log( samplingTimes ) ) )

                 if ( runif( 1 ) < 0.8 )
                 {
                   numberOfTheSampleTimesToChanged = 1 + floor( length( samplingTimes ) * runif( 1 ) )


                   samplingTimes[ numberOfTheSampleTimesToChanged ] = samplingTimes[ numberOfTheSampleTimesToChanged ] + rcauchy( 1 ) * object@muteEffect


                 }else{

                   for ( i in 1:length(samplingTimes))
                   {
                     samplingTimes[ i ] = samplingTimes[ i ] + rnorm( 1 ) * object@muteEffect
                   }
                 }

                 # constraint to add
                 found = TRUE
                 responseName = getResponseNameByIndice( arm, numberOfTheResponse )

                 samplingConstraints = getSamplingConstraintsInArm( arm, responseName )

                 for ( samplingConstraint in samplingConstraints )
                 {
                   if ( isLessThanDelay( samplingConstraint, samplingTimes ) == TRUE )
                   {

                     found = FALSE
                     break
                   }

                   for ( time in samplingTimes )
                   {
                     if ( isTimeInBetweenBounds( samplingConstraint, time ) == FALSE )
                     {
                       found = FALSE
                       break
                     }
                   }
                 }

               }# end while
               # evaluate design with new samplingTimes

               arm <- modifySamplingTimes( arm, responseName, samplingTimes )

               designB <- modifyArm( designA, getNameArm( arm ), arm )

               evaluatedDesign = EvaluateDesignForEachArm( designB, statistical_model, typeFim )

               d = 1 / getDcriterion( getFimOfDesign( evaluatedDesign ) )

               fitB = 10**( -theta * d )

               if ( !is.nan(fitB) && fitB > 0 )
               {
                 if ( fitA == fitB )
                 {
                   proba = 1 / object@N
                 }
                 else
                 {
                   f = fitA / fitB
                   proba = 1 - f**2
                   proba = proba / (1 - f^N2)
                   if ( is.nan(proba) )
                     proba<-0
                 }

                 if (runif(1) < proba)
                 {
                   fitA<-fitB
                   designA = designB

                   if ( fitBest < 1/d )
                   {
                     fitBest = 1/d
                     best = designA
                     best@isOptimalDesign = TRUE

                     if ( object@showProcess == TRUE ){
                       print( paste0('1/D criteria = ',1/d))
                     }
                     results = rbind( c( iteration , d ) , results)
                   }
                 }
               }

               if ( iteration%%1000 == 0 )
               {
                 d =  - log10( fitA ) / theta
                 theta = -log10( fitBase ) / d
                 fitA = fitBase
               }

             }

             colnames( results ) = c("Iteration","Criteria")

             # set armSize to 1 for individual and bayesian Fim

             arms = getArms( best )

               for ( arm in arms)
               {
                 if ( class( typeFim ) %in% c( "IndividualFim", "BayesianFim" ) )
                 {
                 arm = setArmSize( arm, 1 )
                 }
                 nameArm = getNameArm( arm )
                 best = modifyArm( best, nameArm, arm)
             }

             object@resultsOptimization <- results
             object@OptimalDesign <- best
             object@OptimalDesign@name = paste0("Design optimized from",constraint@name)
             object@iteration_fin <- iteration

             return(object)

           })

##########################################################################################################
# END Class "PGBOAlgorithm"
##########################################################################################################






