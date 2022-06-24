##########################################################################################################
#' Class "SimplexAlgorithm"
#'
#' @description
#' The Class "SimplexAlgorithm" implements the Nelder-Mead method (also downhill simplex method, amoeba method) [1].
#'
#' @references [1]
#' Nelder JA, Mead R (1965) A Simplex Method for Function Minimization. Comput J 7: 308-313.
#'
#' @name SimplexAlgorithm-class
#' @docType class
#' @include Optimization.R
#' @include Design.R
#' @exportClass SimplexAlgorithm
#'
#' @section Objects from the class:
#' Objects form the class \code{SimplexAlgorithm} can be created by calls of the form \code{SimplexAlgorithm(...)} where
#' (...) are the parameters for the \code{SimplexAlgorithm} objects.

#'@section Slots for \code{SimplexAlgorithm} objects:
#'  \describe{
#'    \item{\code{pct_initial_simplex_building}:}{A numeric giving the percentage of initial vertices for the simplex algorithm.}
#'    \item{\code{max_iteration}:}{A numeric giving the maximum of iterations.}
#'    \item{\code{tolerance}:}{A numeric giving the tolerance criteria for stopping the algorithm.}
#'    \item{\code{showProcess}:}{A boolean to show or not the process.}
#'    \item{\code{OptimalDesign}:}{A \code{Design} object giving the optimal design.}
#'  }
##########################################################################################################

SimplexAlgorithm<-setClass(
  Class = "SimplexAlgorithm",
  contains = "Optimization",
  representation = representation(
    pct_initial_simplex_building = "numeric",
    max_iteration = "numeric",
    tolerance = "numeric",
    showProcess = "logical",
    OptimalDesign = "Design"
  ),
  prototype = prototype(
    showProcess = F
  )
)

setMethod(
  f="initialize",
  signature="SimplexAlgorithm",
  definition= function ( .Object,
                         pct_initial_simplex_building,
                         max_iteration,
                         tolerance,
                         showProcess )
  {

    .Object@pct_initial_simplex_building <- 20
    .Object@max_iteration <- 5000
    .Object@tolerance <- 1e-6

    if ( !missing( pct_initial_simplex_building ) )
      .Object@pct_initial_simplex_building <- pct_initial_simplex_building

    if ( !missing( max_iteration ) )
      .Object@max_iteration <- max_iteration

    if ( !missing( tolerance ) )
      .Object@tolerance <- tolerance

    if ( !missing( showProcess ) )
      .Object@showProcess <- showProcess

    validObject( .Object )
    return ( .Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the process of optimization.
#'
#' @rdname setShowProcess
#' @param object A \code{SimplexAlgorithm} object.
#' @param ifShow A boolean.
#' @return Show \code{SimplexAlgorithm} object.

setMethod(f = "setShowProcess",
          signature = "SimplexAlgorithm",
          definition = function(object, ifShow)
          {
            object <- callNextMethod(object, ifShow)
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
# function nelder simplex amoeba
#
# add of a Splus function for the Simplex (taken on the Splus User Group)
# ~/rtns/S/optfcn.q
# Splus functions useful in numerical optimization.
#   Daniel Heitjan, 13 September 1990
#   Revised, 10 March 1992
#   Style changes, 94.02.26

#' function fun.amoeba
#' @name fun.amoeba
#' @param p input is a matrix p whose ndim+1 rows are ndim-dimensional vectors which are the vertices of the starting simplex.
#' @param y vector whose components must be pre-initialized to the values of funk evaluated at the ndim+1 vertices (rows) of p.
#' @param ftol the fractional convergence tolerance to be achieved in the function value.
#' @param itmax maximal number of iterations.
#' @param funk multidimensional function to be optimized.
#' @param data a fixed set of data.
#' @return A list containing the components of the optimized simplex.

fun.amoeba<-function(p,y,ftol,itmax,funk,data){

  ## Multidimensional minimization of the function funk(x,data) where x
  ## is an ndim-dimensional vector and data is a fixed set of data, by
  ## the downhill simplex method of Nelder and Mead.  The structure data
  ## is arbitrary and is evaluated only in funk.  Input is a matrix p
  ## whose ndim+1 rows are ndim-dimensional vectors which are the
  ## vertices of the starting simplex, and a data vector in a suitable
  ## format.  Also input is the vector y of length ndim+1, whose
  ## components must be pre-initialized to the values of funk evaluated
  ## at the ndim+1 vertices (rows) of p  and ftol the fractional
  ## convergence tolerance to be achieved in the function value (n.b.!).
  ## The output list will contain components p, ndim+1 new points all
  ## within ftol of a minimum function value, y, the function value,
  ## iter, the number of iterations taken, and converge, a convergence
  ## indicator.
  ##   Translated from *Numerical Recipes*, 13 March 1989
  ##   Revised for model fitting, 16 March 1989
  ##   Revised to handle univariate parameters, 29 July 1989
  ##   Comments and printing revised, 28 April 1990
  ##   Stopping criterion revised, 3 May 1990
  ##   Error in contraction routine fixed, 4 May 1990
  ##   Replaced 'order' with 'sort.list', 4 May 1990
  ##   Modified digits to print, 20 June 1990
  ##   Changed print to cat, 13 September 1990
  ##   Modify printing, 8 February 1991
  ##   Added verbose mode, 94.12.25
  ##   Daniel F. Heitjan
  ##

  ## Three parameters which define the expansions and contractions.
  alpha<-1.0
  beta<-0.5
  gamma<-2.0
  ## A parameter that governs stopping if the function is converging to
  ## zero.
  eps<-1.e-10
  mpts<-nrow(p)
  iter<-0
  contin<-T
  converge<-F

  while (contin) {

    ## First we must determine which point is the highest (worst),
    ## next highest, and lowest (best).
    ord<-sort.list(y)
    ilo<-ord[1]
    ihi<-ord[mpts]
    inhi<-ord[mpts-1]

    ## Compute the fractional range from highest to lowest and return if
    ## satisfactory.
    rtol<-2.*abs(y[ihi]-y[ilo])/(abs(y[ihi])+abs(y[ilo])+eps)

    if ((rtol<ftol)||(iter==itmax)) {
      contin<-F
      converge<-T
      if (iter==itmax) { converge<-F }
    } else {
      if (iter==itmax) cat('Amoeba exceeding maximum iterations.\n')
      iter<-iter+1

      ##
      ## Begin a new iteration.  Compute the vector average of all points
      ## except the highest, i.e. the center of the face of the simplex across
      ## from the high point.  We will subsequently explore along the ray from
      ## the high point through that center.
      pbar<-matrix(p[-ihi,],(mpts-1),(mpts-1))
      pbar<-apply(pbar,2,mean)

      ##
      ## Extrapolate by a factor alpha through the face, i.e. reflect the
      ## simplex from the high point.  Evaluate the function at the reflected
      ## point.
      pr<-(1+alpha)*pbar-alpha*p[ihi,]
      ypr<-funk(pr,data)

      if (ypr<=y[ilo]) {
        ## Gives a result better than the best point, so try an additional
        ## extrapolation by a factor gamma, and check out the function there.
        prr<-gamma*pr+(1-gamma)*pbar
        yprr<-funk(prr,data)
        if (yprr<y[ilo]) {
          ## The additional extrapolation succeeded, and replaces the highest point.
          p[ihi,]<-prr
          y[ihi]<-yprr
        } else {
          ## The additional extrapolation failed, but we can still use the
          ## reflected point.
          p[ihi,]<-pr
          y[ihi]<-ypr
        }
      } else {
        if (ypr>=y[inhi]) {
          ## The reflected point is worse than the second-highest.
          if (ypr<y[ihi]) {
            ## If it's better than the highest, then replace the highest,
            p[ihi,]<-pr
            y[ihi]<-ypr
          }
          ## but look for an intermediate lower point, in other words, perform a
          ## contraction of the simplex along one dimension.  Then evaluate the
          ## function.
          prr<-beta*p[ihi,]+(1-beta)*pbar
          yprr<-funk(prr,data)
          if (yprr<y[ihi]) {
            ## Contraction gives an improvement, so accept it.
            p[ihi,]<-prr
            y[ihi]<-yprr
          } else {
            ## Can't seem to get rid of that high point.  Better contract around the
            ## lowest (best) point.
            p<-0.5*(p+matrix(p[ilo,],nrow=nrow(p),ncol=ncol(p),byrow=T))
            for (j in 1:length(y)) {
              if (j!=ilo) {
                y[j]<-funk(p[j,],data)
              }
            }
          }
        } else {
          ## We arrive here if the original reflection gives a middling point.
          ## Replace the old high point and continue
          p[ihi,]<-pr
          y[ihi]<-ypr
        }
      }
    }
  }
  return(list(p=p,y=y,iter=iter,converge=converge))
} # end function amoeba

# -------------------------------------------------------------------------------------------------------------------
#' Compute the fisher.simplex
#'
#' @name fisher.simplex
#' @param samplingTimes A \code{SamplnigTimes} object.
#' @param data A list containing the design, arm, response names, statistical model, constraint and FIM.
#' @return The fisher.simplex giving the evalation of the optimization criterion ( i.e. D-criterion)

fisher.simplex <- function( samplingTimes, data )
{
  design = data[[1]]
  arm = data[[2]]
  responseName = names(getSamplings(arm))[1]
  statistical_model = data[[3]]
  constraint = data[[4]]
  typeFim = data[[5]]

  # test for constraints on lower and upper bounds

  samplingConstraints = getSamplingConstraints( constraint )

  for ( samplingConstraint in samplingConstraints )
  {
    if ( isLessThanDelay( samplingConstraint, samplingTimes ) == TRUE )
    {
      return( 66e66 )
    }

    for ( time in samplingTimes )
    {
      if ( isTimeInBetweenBounds( samplingConstraint, time ) == FALSE )
      {
        return( 66e66 )
      }
    }
  }

  arm <- modifySamplingTimes( arm,  responseName , samplingTimes )

  design <- modifyArm( design, getNameArm( arm ), arm )

  evaluatedDesign = EvaluateDesignForEachArm( design, statistical_model, typeFim )

  d = 1 / getDcriterion( getFimOfDesign( evaluatedDesign ) )

  return( d )

}

# -------------------------------------------------------------------------------------------------------------------
#' Design optimization withe the Simplex algorithm.
#'
#' @rdname Optimize
#' @param object An \code{Optimize} object.
#' @param designs A \code{Design} object.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param constraint A \code{Constraint} object.
#' @param typeFim A \code{FIM} object.
#' @return A \code{Design} object giving the optimal design.

setMethod( f = "Optimize",
           signature = "SimplexAlgorithm",
           definition = function( object, designs, statistical_model, constraint, typeFim )
           {
             optimalDCriteria = list()
             samplingTimesSimplex = list()
             indexOptimalDCriteria = list()
             optimalsamplingTimes = list()

             callNextMethod( object, statistical_model , constraint, typeFim )

             for ( design in designs )
             {
               setOptimalDesign( object ) <- design

               arms = getArms( design )

               for ( arm in arms )
               {
                 nameArm = getNameArm( arm )

                 samplingTimes = getSampleTime( getSamplings( arm )[[1]] )

                 initialSampling =  matrix( rep( samplingTimes,length( samplingTimes ) + 1 ),
                                            ncol = length( samplingTimes ), byrow=TRUE )

                 y = c( fisher.simplex( samplingTimes, data = list( design, arm, statistical_model, constraint , typeFim ) ) )

                 for ( i in 1:length( samplingTimes ) )
                 {
                   initialSampling[i+1,i] = initialSampling[i+1,i] * ( 1-object@pct_initial_simplex_building/100 )
                   y[i+1] = fisher.simplex( initialSampling[i+1,], data = list( design, arm,
                                                                                statistical_model, constraint , typeFim ) )
                 }

                 opti = fun.amoeba( initialSampling, y, object@tolerance, object@max_iteration, fisher.simplex,
                                    data = list( design, arm, statistical_model, constraint , typeFim ) )

                 optimalDCriteria[[nameArm]] = opti$y
                 samplingTimesSimplex[[nameArm]] = opti$p
                 indexOptimalDCriteria[[nameArm]] = which( optimalDCriteria[[nameArm]] == min(  optimalDCriteria[[nameArm]] ) )
                 optimalsamplingTimes[[nameArm]] = samplingTimesSimplex[[nameArm]][ indexOptimalDCriteria[[nameArm]],]

               } # end arm
             }# end design

             # create optimal design
             optimalDesign <- Design( name = paste0( "Design optimized from ",constraint@name ) )
             optimalDesign@isOptimalDesign <- TRUE

             for ( arm in arms)
             {
               if ( class( typeFim ) %in% c( "IndividualFim", "BayesianFim" ) )
               {
                 arm = setArmSize( arm, 1 )
               }
               arm@samplings[[1]] = setSampleTime(arm@samplings[[1]], optimalsamplingTimes[[nameArm]])
               optimalDesign = addArm( optimalDesign, arm )
             }

             object@OptimalDesign = optimalDesign

             return( object )

           }
               )

########################################################################################################################
# END Class SimplexAlgorithm
########################################################################################################################







