##################################################################################
#' Class "MultiplicativeAlgorithm"

#' @description
#' Class "MultiplicativeAlgorithm" implements the Multiplicative algorithm.
#'
#' @name MultiplicativeAlgorithm-class
#' @aliases MultiplicativeAlgorithmClass
#' @docType class
#' @include Optimization.R
#' @include Design.R
#' @exportClass MultiplicativeAlgorithm
#'
#' @section Objects from the class:
#' Objects form the class \code{MultiplicativeAlgorithm} can be created by calls of the form \code{MultiplicativeAlgorithm(...)} where
#' (...) are the parameters for the \code{MultiplicativeAlgorithm} objects.
#'
#'@section Slots for \code{MultiplicativeAlgorithm} objects:
#' \describe{
#' \item{\code{lambda}:}{A numeric giving the lambda parameter of the multiplicative algorithm.}
#' \item{\code{delta}:}{A numeric giving the delta parameter of the multiplicative algorithm.}
#' \item{\code{iteration_init}:}{A numeric giving the first iteration of the optimization process.}
#' \item{\code{iteration_fin}:}{A numeric giving the last iteration of the optimization process.}
#' \item{\code{FinalWeights}:}{A vector givint hte optimal weights.}
#' \item{\code{showProcess}:}{A boolean for showing or not the process of optimization.}
#' \item{\code{OptimalDesign}:}{A object from the class \code{Design}}
#' \item{\code{allArms}:}{A list of all arms.}
#' }
##################################################################################

MultiplicativeAlgorithm<-setClass(
  Class = "MultiplicativeAlgorithm",
  contains = "Optimization",
  representation = representation(
    lambda = "numeric",
    delta = "numeric",
    iteration_init = "numeric",
    iteration_fin = "numeric",
    FinalWeights = "vector",
    showProcess = "logical",
    OptimalDesign = "Design",
    allArms = "list"
  ),
  prototype = prototype(
    showProcess = F
  )
)

setMethod(
  f="initialize",
  signature="MultiplicativeAlgorithm",
  definition= function (.Object, lambda, iteration_init, delta, showProcess)
  {
    if(!missing(lambda))
      .Object@lambda<-lambda
    if(!missing(iteration_init))
      .Object@iteration_init<-iteration_init
    if(!missing(delta))
      .Object@delta<-delta
    if(!missing(showProcess))
      .Object@showProcess<-showProcess
    validObject(.Object)
    return (.Object )
  }
)

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

MultiplicativeAlgorithm_Rcpp = function(fisherMatrices_input,
                                        numberOfFisherMatrices_input,
                                        weights_input,
                                        numberOfParameters_input,
                                        dim_input, lambda_input, delta_input, iterationInit_input){
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
return Rcpp::List::create( Rcpp::Named ("weights ") = weights,
                           Rcpp::Named ("iterationEnd ") = iter);

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
                                       dim_input, lambda_input, delta_input, iterationInit_input )

return( output )

}

# -------------------------------------------------------------------------------------------------------------------
#' Shows the process for FIM computing.
#'
#' @rdname setShowProcess
#' @param object A \code{MultiplicativeAlgorithm} object.
#' @param ifShow A boolean.
#' @return Shows the process for FIM computing.

setMethod(f = "setShowProcess",
          signature = "MultiplicativeAlgorithm",
          definition = function(object, ifShow)
          {
            object <- callNextMethod(object, ifShow)
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Prepare the Fisher Informations Matrices.
#'
#' @rdname PrepareFIMs
#' @param object A \code{MultiplicativeAlgorithm} object.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param cond_init : cond_init
#' @param constraint : A \code{Constraint} object.
#' @param typeFim : A character string giving the r=type of FIM : Population, Individual or Bayesian.
#' @return A list \code{FIMs} of the Fisher Informations Matrices.

setMethod(
  f="PrepareFIMs",
  signature = "MultiplicativeAlgorithm",
  definition = function(object, statistical_model , cond_init, constraint, typeFim) #, armBase, initial_designs)
  {
    FIMs <- callNextMethod( object, statistical_model , cond_init, constraint, typeFim )
    return(FIMs)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Optimization with the Multiplicative Algorithm.
#'
#' @rdname Optimize
#' @param object A \code{MultiplicativeAlgorithm} object.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param constraint : A \code{Constraint} object.
#' @param typeFim : A character string giving the r=type of FIM : Population, Individual or Bayesian.
#' @return The \code{MultiplicativeAlgorithm} object with:
#' \itemize{
#' \item \code{{OptimalDesign}:}{A \code{Design} object giving the optimal design.}
#' \item \code{{FinalWeights}:}{A list of the optimal weights.}
#' \item \code{{iteration_final }:}{A numeric of the final iteration of the process.}
#' \item \code{{allArms}:}{A list of all the arms in the optimal design.}
#'}

setMethod(f = "Optimize",
          signature = "MultiplicativeAlgorithm",
          definition = function(object, statistical_model, cond_init, constraint, typeFim)
          {
            callNextMethod( object, statistical_model , cond_init, constraint, typeFim )

            FIMandDesigns = PrepareFIMs( object, statistical_model , cond_init, constraint, typeFim )

            fisherMatrices = FIMandDesigns@FisherMatrices

            numberOfFisherMatrices = length( fisherMatrices )
            weights = rep(1/numberOfFisherMatrices,numberOfFisherMatrices)
            numberOfParameters = length(getModelParameters(statistical_model))
            dim = dim(fisherMatrices[[1]])[[1]]
            lambda = object@lambda
            delta = object@delta
            iterationInit = object@iteration_init

            output = MultiplicativeAlgorithm_Rcpp( fisherMatrices,
                                                   numberOfFisherMatrices,
                                                   weights,
                                                   numberOfParameters,
                                                   dim, lambda, delta, iterationInit )

            weights = output$`weights`
            iterationEnd = output$`iterationEnd`

            amountOfArmConstraint =  constraint@amountOfArm

            if(object@showProcess)
            {
              cat("\n Iteration used : ",iterationEnd,"\n")
            }

            # optimal design
            optimalDesign <- Design( name = paste0( "Design optimized from ", constraint@name ) )
            optimalDesign@isOptimalDesign <- TRUE

            # cases: PopulationFim, IndividualFim, BayesianFim
            if ( class( typeFim ) %in% c("PopulationFim") )
            {
              # optimal weights
              if(amountOfArmConstraint > 0){
                v <- which( weights >= min( tail( sort( weights ),amountOfArmConstraint ) ) )
              }else{
                v<-which( weights > mean( weights ) )
              }

              totalNumberOfIndividualsConstraint = getTotalNumberOfIndividuals( constraint )
              intermediateNumberOfIndividualPerGroup = totalNumberOfIndividualsConstraint*weights[v]
              numberOfIndividualPerGroup =
                intermediateNumberOfIndividualPerGroup / sum(intermediateNumberOfIndividualPerGroup ) * totalNumberOfIndividualsConstraint

              for(ind in v)
              {
                arm = FIMandDesigns@arms[[ind]]
                arm = setArmSize( arm, numberOfIndividualPerGroup[ which( ind == v ) ] )
                arm = setInitialConditions( arm, cond_init )

                optimalDesign = addArm( optimalDesign, arm )
              }

            } else if( class( typeFim ) %in% c( "IndividualFim","BayesianFim" ) )
            {
              indexMaxWeights = which( weights == max( weights ) )
              weights = weights[indexMaxWeights]
              arm = FIMandDesigns@arms[[indexMaxWeights]]
              arm = setArmSize( arm, 1 )
              arm <- setInitialConditions( arm, cond_init )
              optimalDesign = addArm( optimalDesign, arm )
              FIMandDesigns@arms = FIMandDesigns@arms[indexMaxWeights]
            }

            object@OptimalDesign = optimalDesign
            object@FinalWeights = weights
            object@iteration_fin = iterationEnd
            object@allArms = FIMandDesigns@arms

            return(object)

          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the frame with weight vector after optimisation.
#'
#' @name getWeightFrame
#' @param object A \code{MultiplicativeAlgorithm} object.
#' @return The data frame \code{armFrame} with weight vector after optimisation.

setGeneric("getWeightFrame",
           function(object)
           {
             standardGeneric("getWeightFrame")
           }
)

setMethod("getWeightFrame",
          "MultiplicativeAlgorithm",
          function(object)
          {

            weights <- object@FinalWeights
            design <- Design()

            i = 0
            for(arm in object@allArms)
            {
              i = i + 1
              arm = setArmSize( arm, weights[i] )
              design = addArm( design, arm )
            }

            frameDesign <- summaryArmData( design )

            return( frameDesign )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the \code{delta} parameters for the Multiplicative algorithm.
#'
#' @name setDelta
#' @param object \code{MultiplicativeAlgorithm} object.
#' @param values values
#' @return The \code{MultiplicativeAlgorithm} object with the new value of \code{delta}.

setGeneric("setDelta",
           function(object, values)
           {
             standardGeneric("setDelta")
           }
)

setMethod( f="setDelta",
           signature="MultiplicativeAlgorithm",
           definition = function(object, values)
           {
             object@delta <- values
             validObject(object)
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the number of iterations for the multiplicative algorithm.
#'
#' @name setIteration
#' @param object \code{MultiplicativeAlgorithm} object.
#' @param values A numeric.
#' @return The \code{MultiplicativeAlgorithm} object with the new values of the number of iterations.

setGeneric("setIteration",
           function(object, values)
           {
             standardGeneric("setIteration")
           }
)

setMethod( f="setIteration",
           signature="MultiplicativeAlgorithm",
           definition = function(object, values)
           {
             object@iteration_init <- values
             validObject(object)
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the end of the process for the multiplicative algorithm.
#'
#' @rdname show
#' @param object A \code{MultiplicativeAlgorithm} object.
#' @return Print the end of the process for the multiplicative algorithm.

setMethod("show",
          "MultiplicativeAlgorithm",
          function(object)
          {

            if(object@iteration_init == object@iteration_fin)
              cat("\n This design is optimized by multiplicative algorithm -- convergence criterion NOT achieved.\n\n")
            else
              cat("\n This design is optimized by multiplicative algorithm -- convergence criterion achieved.\n\n")
          }
)

##########################################################################################################
# END Class "MultiplicativeAlgorithm"
##########################################################################################################
