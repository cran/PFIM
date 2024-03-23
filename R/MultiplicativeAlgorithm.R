#' Class "MultiplicativeAlgorithm"
#'
#' @description The class \code{MultiplicativeAlgorithm} implements the multiplicative algorithm.
#'
#' @name MultiplicativeAlgorithm-class
#' @aliases MultiplicativeAlgorithm
#' @docType class
#' @include Design.R
#' @include GenericMethods.R
#' @include OptimizationAlgorithm.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{MultiplicativeAlgorithm} can be created by calls of the form \code{MultiplicativeAlgorithm(...)} where
#' (...) are the parameters for the \code{MultiplicativeAlgorithm} objects.
#'
#'@section Slots for \code{MultiplicativeAlgorithm} objects:
#' \describe{
#' \item{\code{arms}:}{A list giving the arms.}
#' \item{\code{lambda}:}{A numeric giving the lambda parameter of the multiplicative algorithm.}
#' \item{\code{delta}:}{A numeric giving the delta parameter of the multiplicative algorithm.}
#' \item{\code{numberOfIterations}:}{A numeric giving the maximal number iteration of the optimization process.}
#' \item{\code{optimalWeights}:}{A vector giving the optimal weights.}
#' \item{\code{optimalDesign}:}{An object of the class \code{Design} giving the optimal design.}
#' \item{\code{showProcess}:}{A boolean for showing or not the process of optimization.}
#' }

MultiplicativeAlgorithm = setClass(
  Class = "MultiplicativeAlgorithm",
  contains = "OptimizationAlgorithm",
  representation = representation(
    arms = "list",
    lambda = "numeric",
    delta = "numeric",
    numberOfIterations = "numeric",
    optimalWeights = "vector",
    optimalDesign = "Design",
    showProcess = "logical"
  ))

setMethod( f="initialize",
           signature="MultiplicativeAlgorithm",
           definition= function (.Object, arms, lambda, delta, numberOfIterations, optimalWeights, optimalDesign, showProcess)
           {
             if(!missing(lambda))
             {
               .Object@lambda = lambda
             }
             if(!missing(delta))
             {
               .Object@delta = delta
             }
             if(!missing(numberOfIterations))
             {
               .Object@numberOfIterations = numberOfIterations
             }
             if(!missing(optimalWeights))
             {
               .Object@optimalWeights = optimalWeights
             }
             if(!missing(optimalDesign))
             {
               .Object@optimalDesign = optimalDesign
             }
             if(!missing(showProcess))
             {
               .Object@showProcess = showProcess
             }
             if(!missing(arms))
             {
               .Object@arms = arms
             }
             validObject(.Object)
             return (.Object )
           }
)

# ======================================================================================================
#' Function MultiplicativeAlgorithm_Rcpp
#' @name MultiplicativeAlgorithm_Rcpp
#' @description Run the MultiplicativeAlgorithm_Rcpp in Rcpp
#' @param fisherMatrices_input fisherMatrices_input
#' @param numberOfFisherMatrices_input numberOfFisherMatrices_input
#' @param weights_input weights_input
#' @param numberOfParameters_input numberOfParameters_input
#' @param dim_input dim_input
#' @param lambda_input lambda_input
#' @param delta_input delta_input
#' @param iterationInit_input iterationInit_input
#' @export
# ======================================================================================================

MultiplicativeAlgorithm_Rcpp = function(fisherMatrices_input,
                                        numberOfFisherMatrices_input,
                                        weights_input,
                                        numberOfParameters_input,
                                        dim_input,
                                        lambda_input,
                                        delta_input,
                                        iterationInit_input){
  incltxt <- '

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h> 	/* Mathematical functions */
#include <time.h>	/* Function time used to initialise the random number generator */
#include <float.h>	/* Implementation related constants */
#include <signal.h>	/* Signal handling used to detect arithmetic errors */

/******************************************************************************
 MultiplicativeAlgorithm_Rcpp
*******************************************************************************/

// [[Rcpp::export]]
List MultiplicativeAlgorithm_Rcpp( List fisherMatrices,
                    arma :: vec  numberOfFisherMatrices,
                    arma :: vec  weights,
                    arma :: vec  numberOfParameters,
                    arma :: vec  dim,
                    arma :: vec  lambda,
                    arma :: vec  delta,
                    arma :: vec  iterationInit)
 {

arma :: mat sum_weighted_fims(dim[0],dim[0]);
arma :: vec determinant;
arma :: vec Dcriteria;
arma :: mat derivative_phi(dim[0],dim[0]);
arma :: vec vector_of_multiplier(numberOfFisherMatrices[0]);
arma :: mat matmult(dim[0],dim[0]);

int iter;
for( iter=0 ; iter < iterationInit[0] ; iter ++){

//Rcout << "iteration = " << iter << std::endl;

// sum_weighted_fims =  weights[i]*fims[i]
int i;
for(i=0 ; i<numberOfFisherMatrices[0] ; i++){
sum_weighted_fims += Rcpp::as<arma::mat>( fisherMatrices[i])*weights[i];
}

// determinant of sum_weighted_fims
determinant = det( sum_weighted_fims );

// D-criteria
Dcriteria = pow(determinant,1/dim[0]);

// derivatives of function phi_D
derivative_phi = Dcriteria[0] * inv(sum_weighted_fims)/dim[0];

// vector of multiplier
for(i=0 ; i<numberOfFisherMatrices[0] ; i++){
matmult = derivative_phi * Rcpp::as<arma::mat>( fisherMatrices[i]);
vector_of_multiplier[i] = sum(matmult.diag());
}

// normalization of the weights
weights = weights % pow(vector_of_multiplier,lambda[0]) / sum(weights % pow(vector_of_multiplier,lambda[0]));

// stop criterion
 if (vector_of_multiplier.max()<(1+delta[0])*sum(weights%vector_of_multiplier))
        {
          break;
        }

} // end iteration

// output
return Rcpp::List::create( Rcpp::Named ("weights") = weights,
                           Rcpp::Named ("iterationEnd") = iter);

} // end MultiplicativeAlgorithm_Rcpp
'

MultiplicativeAlgorithm_Rcpp <- inline::cxxfunction(

  signature( fisherMatrices_input = "list",
             numberOfFisherMatrices_input = "integer",
             weights_input = "numeric",
             numberOfParameters_input = "integer",
             dim_input = "integer",
             lambda_input = "numeric",
             delta_input = "numeric",
             iterationInit_input = "integer"),

  plugin = "RcppArmadillo",
  incl = incltxt,
  body = '
          List fisherMatrices = Rcpp::as<List>(fisherMatrices_input);
          arma::vec numberOfFisherMatrices  = Rcpp::as<arma::vec>(numberOfFisherMatrices_input);
          arma::vec weights  = Rcpp::as<arma::vec>(weights_input);
          arma::vec numberOfParameters = Rcpp::as<arma::vec>(numberOfParameters_input);
          arma::vec dim = Rcpp::as<arma::vec>(dim_input);
          arma::vec lambda = Rcpp::as<arma::vec>(lambda_input);
          arma::vec delta = Rcpp::as<arma::vec>(delta_input);
          arma::vec iterationInit = Rcpp::as<arma::vec>(iterationInit_input);

          return Rcpp::wrap( MultiplicativeAlgorithm_Rcpp(  fisherMatrices,
                                                            numberOfFisherMatrices,
                                                            weights,
                                                            numberOfParameters,
                                                            dim,
                                                            lambda,
                                                            delta,
                                                            iterationInit ) );')

output = MultiplicativeAlgorithm_Rcpp( fisherMatrices_input,
                                       numberOfFisherMatrices_input,
                                       weights_input,
                                       numberOfParameters_input,
                                       dim_input, lambda_input,
                                       delta_input,
                                       iterationInit_input )
return( output )

}

#' Get the parameter lambda.
#' @name getLambda
#' @param object An object from the class \linkS4class{MultiplicativeAlgorithm}.
#' @return A numeric giving the parameter lambda.
#' @export

setGeneric(
  "getLambda",
  function(object) {
    standardGeneric("getLambda")
  })

#' @rdname getLambda
#' @export

setMethod("getLambda",
          "MultiplicativeAlgorithm",
          function(object)
          {
            return( object@lambda )
          })

#' Get the parameter delta
#' @name getDelta
#' @param object An object from the class \linkS4class{MultiplicativeAlgorithm}.
#' @return A numeric giving the parameter delta.
#' @export

setGeneric(
  "getDelta",
  function(object) {
    standardGeneric("getDelta")
  })

#' @rdname getDelta
#' @export

setMethod("getDelta",
          "MultiplicativeAlgorithm",
          function(object)
          {
            return( object@delta )
          })

#' Get the number of iterations.
#' @name getNumberOfIterations
#' @param object An object from the class \linkS4class{MultiplicativeAlgorithm}.
#' @return A numeric giving the number of iterations.
#' @export

setGeneric(
  "getNumberOfIterations",
  function(object) {
    standardGeneric("getNumberOfIterations")
  })

#' @rdname getNumberOfIterations
#' @export

setMethod("getNumberOfIterations",
          "MultiplicativeAlgorithm",
          function(object)
          {
            return( object@numberOfIterations )
          })

#' Get the optimal weights.
#' @name getOptimalWeights
#' @param object An object from the class \linkS4class{MultiplicativeAlgorithm}.
#' @return A vector giving the optimal weights.
#' @export

setGeneric("getOptimalWeights",
           function(object )
           {
             standardGeneric("getOptimalWeights")
           })

#' @rdname getOptimalWeights
#' @export

setMethod(f="getOptimalWeights",
          signature="MultiplicativeAlgorithm",
          definition = function(object)
          {
            return( object@optimalWeights )
          }
)

#' Set the optimal weights.
#' @name setOptimalWeights
#' @param object An object from the class \linkS4class{MultiplicativeAlgorithm}.
#' @param optimalWeights A vector giving the optimal weights.
#' @return The object with the updated optimal weights.
#' @export

setGeneric("setOptimalWeights",
           function( object, optimalWeights )
           {
             standardGeneric("setOptimalWeights")
           })

#' @rdname setOptimalWeights
#' @export

setMethod(f="setOptimalWeights",
          signature="MultiplicativeAlgorithm",
          definition = function( object, optimalWeights )
          {
            object@optimalWeights = optimalWeights
            return( object )
          }
)

# ======================================================================================================
# setParameters
# ======================================================================================================

#' @rdname setParameters
#' @export

setMethod("setParameters",
          "MultiplicativeAlgorithm",
          function( object, parameters ) {
            object@parameters = parameters
            object@name = "MultiplicativeAlgorithm"
            object@lambda = parameters$lambda
            object@delta = parameters$delta
            object@numberOfIterations = parameters$numberOfIterations
            object@showProcess = parameters$showProcess
            return( object )
          })

# ======================================================================================================
# optimize
# ======================================================================================================

#' @rdname optimize
#' @export

setMethod(f = "optimize",
          signature = "MultiplicativeAlgorithm",
          definition = function( object, optimizerParameters, optimizationObject )
          {
            # =======================================================
            # generate Fims from constraints
            # =======================================================

            fims = generateFimsFromConstraints( optimizationObject )

            fisherMatrices = fims$listFims
            fisherMatricesArms = fims$listArms

            # rename arms
            for ( k in 1:length( fisherMatricesArms ) )
            {
              fisherMatricesArms[[k]] = setName( fisherMatricesArms[[k]], name = paste0("Arm", k ) )
            }

            # =======================================================
            # MultiplicativeAlgorithm parameters
            # =======================================================

            numberOfFisherMatrices = length( fisherMatrices )
            weights = rep( 1/numberOfFisherMatrices,numberOfFisherMatrices )
            numberOfParameters = length( getModelParameters( optimizationObject ) )
            dim = dim( fisherMatrices[[1]] )[[1]]
            lambda = getLambda( object )
            delta = getDelta( object )
            numberOfIterations = getNumberOfIterations( object )

            # =======================================================
            # run the MultiplicativeAlgorithm
            # =======================================================

            multiplicativeAlgorithmOutput = MultiplicativeAlgorithm_Rcpp( fisherMatrices, numberOfFisherMatrices,
                                                                          weights, numberOfParameters,dim, lambda,
                                                                          delta, numberOfIterations )
            # =======================================================
            # get weights and final iteration
            # =======================================================

            weights = multiplicativeAlgorithmOutput[["weights"]]
            iterationEnd = multiplicativeAlgorithmOutput[["iterationEnd"]]

            # =======================================================
            # get the constraint on the number of arms
            # =======================================================

            designs = getDesigns( optimizationObject )
            design = designs[[1]]
            numberOfArmsConstraint = getNumberOfArms( design  )

            # =======================================================
            # get the FIM
            # =======================================================

            fim = getFim( optimizationObject )

            # =======================================================
            # create design for optimal design
            # =======================================================

            optimalDesign = Design( name = c( "Design optimized" ) )

            if ( is( fim,"PopulationFim") )
            {
              # =======================================================
              # optimal weights & number of individuals
              # =======================================================

              weightsIndex = which( weights > mean( weights ) )

              intermediateNumberOfIndividualPerGroup = numberOfArmsConstraint*weights[weightsIndex]
              numberOfIndividualPerGroup = intermediateNumberOfIndividualPerGroup / sum( intermediateNumberOfIndividualPerGroup )*numberOfArmsConstraint

              armList = list()

              k=1
              for( weightIndex in weightsIndex )
              {
                arm = fisherMatricesArms[[weightIndex]]
                armSize = numberOfIndividualPerGroup[ which( weightIndex == weightsIndex ) ]
                armName = paste0( "Arm", weightIndex )
                arm = setName( arm, armName )
                arm = setSize( arm, armSize )
                armList[[k]] = arm
                k=k+1
              }
              optimalDesign = setArms( optimalDesign, armList )
            }
            else if( is( fim,"IndividualFim") | is( fim,"BayesianFim" ) )
            {
              indexMaxWeights = which( weights == max( weights ) )
              weights = weights[indexMaxWeights]
              arm = fisherMatricesArms[[indexMaxWeights]]
              armSize = 1
              arm = setSize( arm, armSize )
              armName = paste0("Arm", indexMaxWeights )
              arm = setName( arm, armName )

              optimalDesign = setArms( optimalDesign, list( arm ) )
            }

            object = setArms( object, fisherMatricesArms )
            object = setOptimalDesign( object, optimalDesign )
            object = setOptimalWeights( object, weights )

            return( object )
          }
)

#' @rdname getDataFrameResults
#' @export

setMethod(f="getDataFrameResults",
          signature="MultiplicativeAlgorithm",
          definition = function( object, threshold )
          {
            # =======================================================
            # get optimal weights and optimal design
            # =======================================================

            optimalWeights = unlist( getOptimalWeights( object ) )
            designs = getOptimalDesign( object )

            # =======================================================
            # get arms and outcomes
            # =======================================================

            arms = getArms( object )
            armNames = unlist( lapply( arms, function(x) getName(x) ) )
            samplingTimes = getSamplingTimes( arms[[1]] )
            outcomes = lapply( samplingTimes, function(x) getOutcome(x) )
            outcomes = unlist( outcomes )

            armsTableSamplings = list()

            for ( outcome in outcomes )
            {
              samplingTimes = lapply( arms, function(x) getSamplingTime( x, outcome ) )
              samplings = lapply( samplingTimes, function(x) getSamplings( x ) )
              samplings = lapply( samplings, function(x) toString( sort( x ) ) )
              armsTableSamplings[[outcome]] = paste0("(",unlist( samplings ), ")" )
            }

            # =======================================================
            # arm name and weight
            # =======================================================

            data = data.frame( armNames = armNames, optimalWeights = optimalWeights, armsTableSamplings )

            # weight threshold
            data = data[ data$optimalWeights > threshold, ]
            data = data[ order( data$optimalWeights, decreasing = TRUE),]
            data = cbind( rev(seq(1,dim(data)[1] ) ), data )

            colnames( data ) = c( "number", "Arm", "Weights", outcomes )
            rownames( data ) = NULL

            return( data )
          })

# ======================================================================================================
# plotWeights
# ======================================================================================================

#' @rdname plotWeights
#' @export

setMethod(f="plotWeights",
          signature = "MultiplicativeAlgorithm",
          definition = function( object, threshold )
          {
            data = getDataFrameResults( object, threshold )

            plotData = ggplot(data, aes( x = number, y = data[,3] ) ) +

              theme(legend.position = "none",
                    axis.title.x.top = element_text(color = "red" , vjust = 2.0),
                    axis.text.x.top = element_text(angle = 90, hjust = 0, color = "red" ),
                    plot.title = element_text(size=16, hjust = 0.5),
                    axis.title.x = element_text(size=16),
                    axis.title.y = element_text(size=16),
                    axis.text.x = element_text(size=16, angle = 0, vjust = 0.5),
                    axis.text.y = element_text(size=16, angle = 0, vjust = 0.5, hjust=0.5),
                    strip.text.x = element_text(size = 16))+


              geom_bar(width = 0.5,position="identity", stat="identity") +

              scale_y_continuous(paste0("\n Weights\n", paste0("Threshold = ", threshold ) ), limits=c(0,1.05),
                                 scales::pretty_breaks(n = 10), expand = c(0, 0)) +

              scale_x_continuous("Arms \n",
                                 breaks = max(data$number):min(data$number),
                                 labels = ((data$Arm))) +

              coord_flip()

            return( plotData )
          }
)

# ======================================================================================================
# show
# ======================================================================================================

#' @title show
#' @rdname show
#' @param object object
#' @export

setMethod(f="show",
          signature = "MultiplicativeAlgorithm",
          definition = function( object )
          {
            dataFrameResults = getDataFrameResults( object, threshold = 0.001 )

            rownames( dataFrameResults ) = NULL
            dataFrameResults = dataFrameResults[,2:dim(dataFrameResults)[2]]

            cat( " ************************************************* ")
            cat("\n")
            cat( " Arm, weight and sampling times for each response  ")
            cat("\n")
            cat( " ************************************************* ")
            cat("\n\n")

            print( dataFrameResults )

          })

# ======================================================================================================
# generateReportOptimization
# ======================================================================================================

#' @rdname generateReportOptimization
#' @export

setMethod( "generateReportOptimization",
           signature = "MultiplicativeAlgorithm",
           definition = function( object, optimizationObject, outputPath, outputFile, plotOptions )
           {
             # ===================================================
             # projectName and outputs tables
             # ===================================================

             projectName = getName( optimizationObject )

             evaluationFIMResults = getEvaluationFIMResults( optimizationObject )
             fimType = is( getFim( evaluationFIMResults ) )[1]

             evaluationFIMIntialDesignResults = getEvaluationInitialDesignResults( optimizationObject )

             tablesEvaluationFIMIntialDesignResults = generateTables( evaluationFIMIntialDesignResults, plotOptions )

             tablesOptimizationObject = generateTables( optimizationObject, plotOptions )

             plotWeights = plotWeights( optimizationObject, threshold = plotOptions$threshold )

             # =======================================================
             # markdown template
             # =======================================================

             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "template_multiplicativeAlgorithm.rmd" )

             rmarkdown::render( input = nameInputFile,
                                output_file = outputFile,
                                output_dir = outputPath,
                                params = list(
                                  object = "object",
                                  plotOptions = "plotOptions",
                                  projectName = "projectName",
                                  fimType = "fimType",
                                  plotWeights = "plotWeights",
                                  tablesEvaluationFIMIntialDesignResults = "tablesEvaluationFIMIntialDesignResults",
                                  tablesOptimizationObject = "tablesOptimizationObject" ) )

           })

##############################################################################
# END Class MultiplicativeAlgorithm
##############################################################################




