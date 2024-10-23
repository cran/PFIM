#' Class "SimplexAlgorithm"
#'
#' @description Class "SimplexAlgorithm" implements the Multiplicative algorithm.
#'
#' @name SimplexAlgorithm-class
#' @aliases SimplexAlgorithm
#' @docType class
#' @include OptimizationAlgorithm.R
#' @include Design.R
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class \code{SimplexAlgorithm}:
#' Objects form the class \code{SimplexAlgorithm} can be created by calls of the form \code{SimplexAlgorithm(...)} where
#' (...) are the parameters for the \code{SimplexAlgorithm} objects.
#'
#'@section Slots for \code{SamplingTimes} objects:
#'  \describe{
#'    \item{\code{pctInitialSimplexBuilding}:}{A numeric giving the percentage of the initial simplex.}
#'    \item{\code{maxIteration}:}{A numeric giving the number of maximum iteration.}
#'    \item{\code{tolerance}:}{A numeric giving the tolerance threshold.}
#'    \item{\code{showProcess}:}{A boolean to show or not the process.}
#'    \item{\code{optimalDesign}:}{A \code{Design} object giving the optimal design.}
#'    \item{\code{iterationAndCriteria}:}{A list giving the optimal criteria at each iteration.}
#'  }

SimplexAlgorithm = setClass(
  Class = "SimplexAlgorithm",
  contains = "OptimizationAlgorithm",
  representation = representation(
    pctInitialSimplexBuilding = "numeric",
    maxIteration = "numeric",
    tolerance = "numeric",
    showProcess = "logical",
    optimalDesign = "Design",
    iterationAndCriteria = "list"
  ),
  prototype = prototype(
    showProcess = F
  )
)

#' initialize
#' @param .Object .Object
#' @param pctInitialSimplexBuilding pctInitialSimplexBuilding
#' @param maxIteration maxIteration
#' @param tolerance tolerance
#' @param optimalDesigns optimalDesigns
#' @param iterationAndCriteria iterationAndCriteria
#' @param showProcess showProcess
#' @return SimplexAlgorithm
#' @export

setMethod( f="initialize",
           signature="SimplexAlgorithm",
           definition= function ( .Object,
                                  pctInitialSimplexBuilding,
                                  maxIteration,
                                  tolerance,
                                  optimalDesigns,
                                  iterationAndCriteria,
                                  showProcess )
           {
             .Object@pctInitialSimplexBuilding = 20
             .Object@maxIteration = 5000
             .Object@tolerance = 1e-6
             .Object@showProcess = F
             if ( !missing( pctInitialSimplexBuilding ) )
             {
               .Object@pctInitialSimplexBuilding = pctInitialSimplexBuilding
             }
             if ( !missing( maxIteration ) )
             {
               .Object@maxIteration = maxIteration
             }
             if ( !missing( tolerance ) )
             {
               .Object@tolerance = tolerance
             }
             if ( !missing( showProcess ) )
             {
               .Object@showProcess = showProcess
             }
             if ( !missing( optimalDesigns ) )
             {
               .Object@optimalDesigns = optimalDesigns
             }
             if ( !missing( iterationAndCriteria ) )
             {
               .Object@iterationAndCriteria = iterationAndCriteria
             }
             validObject( .Object )
             return ( .Object )
           }
)

#' @rdname setParameters
#' @export

setMethod("setParameters",
          "SimplexAlgorithm",
          function( object, parameters ) {
            object@pctInitialSimplexBuilding = parameters$pctInitialSimplexBuilding
            object@name = "SimplexAlgorithm"
            object@maxIteration = parameters$maxIteration
            object@showProcess = parameters$showProcess
            return( object )
          })

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
#' @param outcomes A vector giving the outcomes.
#' @param data a fixed set of data.
#' @param showProcess A boolean for showing the process or not.
#' @return A list containing the components of the optimized simplex.
#' 'getColumnAndParametersNamesFIMInLatex.
#' @export

fun.amoeba = function(p,y,ftol,itmax,funk,outcomes,data,showProcess){
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
  # ===============================================================
  ## Three parameters which define the expansions and contractions.
  # ===============================================================

  alpha = 1.0
  beta = 0.5
  gamma = 2.0

  # ========================================================================
  ## A parameter that governs stopping if the function is converging to zero.
  # ========================================================================

  eps = 1.e-10
  mpts = nrow(p)
  iter = 0
  contin = T
  converge = F
  results = data.frame()

  while (contin) {

    # ========================
    # show process
    # ========================

    if ( showProcess == TRUE )
    {
      print( paste0('iter = ',iter))
      print( paste0('Criterion = ', 1/min(y) ) )
    }

    results = rbind( c( iter , 1/min(y) ) , results)

    # ============================================================
    ## First we must determine which point is the highest (worst),
    ## next highest, and lowest (best).
    # ============================================================

    ord=sort.list(y)
    ilo=ord[1]
    ihi=ord[mpts]
    inhi=ord[mpts-1]

    # ===================================================================
    ## Compute the fractional range from highest to lowest and return if
    ## satisfactory.
    # ===================================================================

    rtol=2.*abs(y[ihi]-y[ilo])/(abs(y[ihi])+abs(y[ilo])+eps)

    if ((rtol<ftol)||(iter==itmax))
    {
      contin=F
      converge=T
      if (iter==itmax) { converge=F }
    } else {
      if (iter==itmax) cat('Amoeba exceeding maximum iterations.\n')
      iter=iter+1

      # ======================================================================
      ## Begin a new iteration.  Compute the vector average of all points
      ## except the highest, i.e. the center of the face of the simplex across
      ## from the high point.  We will subsequently explore along the ray from
      ## the high point through that center.
      # ======================================================================

      pbar=matrix(p[-ihi,],(mpts-1),(mpts-1))
      pbar=apply(pbar,2,mean)

      # ======================================================================
      ## Extrapolate by a factor alpha through the face, i.e. reflect the
      ## simplex from the high point.  Evaluate the function at the reflected
      ## point.
      # ======================================================================

      pr=(1+alpha)*pbar-alpha*p[ihi,]
      ypr=funk(pr,data,outcomes)
      if (ypr<=y[ilo])
      {
        # ======================================================================
        ## Gives a result better than the best point, so try an additional
        ## extrapolation by a factor gamma, and check out the function there.
        # ======================================================================
        prr=gamma*pr+(1-gamma)*pbar
        yprr=funk(prr,data,outcomes)
        if (yprr<y[ilo])
        {
          # ========================================================================
          ## The additional extrapolation succeeded, and replaces the highest point.
          # ========================================================================
          p[ihi,]=prr
          y[ihi]=yprr
        } else {
          # ========================================================================
          ## The additional extrapolation failed, but we can still use the
          ## reflected point.
          # ========================================================================
          p[ihi,]=pr
          y[ihi]=ypr
        }
      } else {
        if (ypr>=y[inhi])
        {
          # ========================================================================
          ## The reflected point is worse than the second-highest.
          # ========================================================================
          if (ypr<y[ihi])
          {
            # ========================================================================
            ## If it's better than the highest, then replace the highest,
            # ========================================================================
            p[ihi,]=pr
            y[ihi]=ypr
          }
          # ========================================================================
          ## but look for an intermediate lower point, in other words, perform a
          ## contraction of the simplex along one dimension.  Then evaluate the
          ## function.
          # ========================================================================
          prr=beta*p[ihi,]+(1-beta)*pbar
          yprr=funk(prr,data,outcomes)
          if (yprr<y[ihi])
          {
            # ========================================================================
            ## Contraction gives an improvement, so accept it.
            # ========================================================================
            p[ihi,]=prr
            y[ihi]=yprr
          } else {
            # ========================================================================
            ## Can't seem to get rid of that high point.  Better contract around the
            ## lowest (best) point.
            # ========================================================================
            p=0.5*(p+matrix(p[ilo,],nrow=nrow(p),ncol=ncol(p),byrow=T))
            for (j in 1:length(y)) {
              if (j!=ilo) {
                y[j]=funk(p[j,],data,outcomes)
              }
            }
          }
        } else {
          # ========================================================================
          ## We arrive here if the original reflection gives a middling point.
          ## Replace the old high point and continue
          # ========================================================================
          p[ihi,]=pr
          y[ihi]=ypr
        }
      }
    }
  }
  return(list(p=p,y=y,iter=iter,converge=converge, results = results ))
} # end function amoeba

#' Compute the fisher.simplex
#'
#' @name fisher.simplex
#' @param simplex A list giving the parameters of the simplex.
#' @param optimizationObject An object from the class \linkS4class{Optimization}.
#' @param outcomes A vector giving the outcomes of the arms.
#' @return A list giving the results of the optimization.
#' @export

fisher.simplex = function( simplex, optimizationObject, outcomes )
{
  # ===============================================
  # get outcomes
  # ===============================================

  modelEquations = getModelEquations( optimizationObject )
  outcomesForEvaluation = modelEquations$outcomes

  # ===============================================
  # set fim for evaluation
  # ===============================================

  fimOptimization = getFim( optimizationObject )
  fimEvaluation = setFimTypeToString( fimOptimization )

  # ===============================================
  # designs and arms for optimization
  # ===============================================

  designs = getDesigns( optimizationObject )

  samplingTimeConstraintsForContinuousOptimization = list( )

  for ( design in designs )
  {
    designName = getName( design )

    arms = getArms( design )

    # ===============================================
    # set sampling times in each arm
    # ===============================================

    listArms = list()

    for ( arm in arms )
    {
      armName = getName( arm )

      # ===============================================
      # get constraints
      # ===============================================

      samplingTimesArms = list()

      for ( outcome in outcomes[[armName]] )
      {
        # ===============================================
        # get the samplings times by design-arm-outcome
        # ===============================================

        namesSamplings = toString( c( designName, armName, outcome ) )
        indexSamplingTimes = which( names( simplex ) == namesSamplings )
        newSamplings = simplex[indexSamplingTimes]
        names( newSamplings ) = NULL

        # =======================================================
        # check the constraints on the samplings times
        # =======================================================

        samplingTimesConstraints = getSamplingTimeConstraint( arm, outcome )

        samplingTimeConstraintsForContinuousOptimization[[ armName ]][[ outcome ]] =
          checkSamplingTimeConstraintsForContinuousOptimization( samplingTimesConstraints, arm, newSamplings, outcome )

        # =======================================================
        # set the samplingTimes
        # =======================================================

        samplingTime = getSamplingTime( arm, outcome )
        samplingTime = setSamplings( samplingTime, newSamplings )
        arm = setSamplingTime( arm, samplingTime )
      }
      design = setArm( design, arm )
    }

    samplingTimeConstraintsForContinuousOptimization = unlist( samplingTimeConstraintsForContinuousOptimization )

    # =======================================================
    # if constraints are satisfied evaluate design
    # =======================================================

    if( all( samplingTimeConstraintsForContinuousOptimization ) == TRUE )
    {
      # ==================================
      # get parameters for the evaluation
      # ==================================

      modelEquations = getModelEquations( optimizationObject )
      modelParameters = getModelParameters( optimizationObject )
      modelError = getModelError( optimizationObject )
      outcomes = getOutcomes( optimizationObject )
      designs = getDesigns( optimizationObject )
      odeSolverParameters = getOdeSolverParameters( optimizationObject )

      # ================================
      # evaluate fim
      # ================================

      evaluationFIM = Evaluation( name = "",
                                  modelEquations = modelEquations,
                                  modelParameters = modelParameters,
                                  modelError = modelError,
                                  outcomes = outcomesForEvaluation,
                                  designs = list( design ),
                                  fim = fimEvaluation,
                                  odeSolverParameters = odeSolverParameters )

      evaluationFIM =  run( evaluationFIM )

      # ================================
      # get criteria
      # ================================

      designs = getDesigns( evaluationFIM )
      fim = getFim( designs[[1]] )
      Dcriterion = 1/getDcriterion( fim )

    }else{
      Dcriterion = 1
    }
  }

  # ================================
  # in case of Inf
  # ================================

  Dcriterion[is.infinite(Dcriterion)] = 1.0

  return( Dcriterion )
}

#' @rdname optimize
#' @export

setMethod( f = "optimize",
           signature = "SimplexAlgorithm",
           definition = function( object, optimizerParameters, optimizationObject )
           {
             # ================================
             # get simplex parameters
             # ================================

             optimizationParameters = getOptimizerParameters( optimizationObject )
             showProcess = optimizationParameters$showProcess

             designs = getDesigns( optimizationObject )
             design = designs[[1]]

             # ==============================================
             # check validity of the samplingTimesConstraints
             # ==============================================

             checkValiditySamplingConstraint( design )

             # ===============================================
             # in case of partial sampling constraints
             # set the new arms with the sampling constraints
             # ===============================================

             design = setSamplingConstraintForOptimization( design )

             designName = getName( design )
             arms = getArms( design )

             # ===============================================
             # set the new designs in the optimizationObject
             # ===============================================

             optimizationObject = setDesigns( optimizationObject, designs = list( design ) )

             # ===============================================
             # get arm outcomes
             # ===============================================

             outcomes = list()

             for ( arm in arms )
             {
               armName = getName( arm )
               outcomes[[armName]] = unlist( lapply( getSamplingTimesConstraints( arm ), function( x ) getOutcome( x ) ) )
             }

             # ================================
             # create the initial simplex
             # ================================

             namesSamplingsSimplex = list()
             samplingsSimplex = list()
             initialSimplex = list()

             k=1
             for ( arm in arms )
             {
               armName = getName( arm )

               for ( outcome in outcomes[[armName]] )
               {
                 samplingTimeConstraint = getSamplingTimeConstraint( arm, outcome )
                 samplingsSimplex[[k]] = getSamplings( samplingTimeConstraint )
                 namesSamplingsSimplex[[k]] = rep( toString( c( designName, armName, outcome ) ), length( samplingsSimplex[[k]] ) )
                 k=k+1
               }
             }

             samplingsSimplex = unlist( samplingsSimplex )
             samplingsSimplex = matrix( rep( samplingsSimplex,length( samplingsSimplex ) + 1 ), ncol = length( samplingsSimplex ), byrow=TRUE )
             colnames( samplingsSimplex ) = unlist( namesSamplingsSimplex )
             samplingsSimplex = t( apply( samplingsSimplex,1,sort.int ) )

             # ================================
             # percentage initial simplex
             # ================================

             for ( i in 1:dim( samplingsSimplex )[2] )
             {
               samplingsSimplex[i+1,i] = samplingsSimplex[i+1,i]*( 1-optimizerParameters$pctInitialSimplexBuilding/100 )
             }

             # ================================
             # evaluate criteria
             # ================================

             y = c()

             for ( i in 1:dim( samplingsSimplex )[1] )
             {
               y[i] = fisher.simplex( samplingsSimplex[i,], optimizationObject, outcomes )
             }

             # ================================
             # Run the simplex
             # ================================

             opti = fun.amoeba( samplingsSimplex, y,
                                optimizerParameters$tolerance,
                                optimizerParameters$maxIteration,
                                fisher.simplex,
                                outcomes,
                                data = optimizationObject, showProcess )

             optimalDCriteria = opti$y
             samplingTimesSimplex = opti$p
             indexOptimalDCriteria = which( opti$y == min( opti$y ) )
             results = opti$results
             optimalsamplingTimes = samplingTimesSimplex[indexOptimalDCriteria,]

             # ================================
             # set optimal design
             # ================================

             namesDesignArmOutcome = unique(names( optimalsamplingTimes ))
             designs = getDesigns( optimizationObject )

             for ( design in designs )
             {
               designName = getName( design )
               arms = getArms( design )

               for ( arm in arms )
               {
                 armName = getName( arm )

                 for ( outcome in outcomes[[armName]] )
                 {
                   namesSamplings = toString(c( designName, armName, outcome ))
                   indexSamplingTimes = which( names( optimalsamplingTimes ) == namesSamplings )
                   samplings = optimalsamplingTimes[indexSamplingTimes]
                   names(samplings) = NULL
                   samplingTimes = SamplingTimes( outcome, samplings = samplings )
                   arm = setSamplingTime( arm, samplingTimes )
                 }
                 design = setArm( design, arm )
               }
               object = setOptimalDesign( object, design )
             }

             # ================================
             # Iteration & Criteria
             # ================================

             colnames( results ) = c("Iteration","Criterion")
             results = results[rev(rownames(results)),]
             object = setIterationAndCriteria( object, results )
             return( object )
           })

#' @title show
#' @rdname show
#' @param object object
#' @export

setMethod(f="show",
          signature = "SimplexAlgorithm",
          definition = function( object )
          {
            cat( " ************************************************* ")
            cat("\n")
            cat( " Criterion ")
            cat("\n")
            cat( " ************************************************* ")
            cat("\n")
            iterationAndCriteria = getIterationAndCriteria( object )
            rownames(iterationAndCriteria )=NULL

            # ================================
            # keep only criterion change
            # ================================

            iterationAndCriteriaUnique = iterationAndCriteria[!duplicated(iterationAndCriteria$Criteria),]

            iterationAndCriteriaUnique = rbind(iterationAndCriteriaUnique,
                                               iterationAndCriteria[max(iterationAndCriteria$Iteration)+1,])

            print( iterationAndCriteriaUnique )
          })

# ======================================================================================================
# generateReportOptimization
# ======================================================================================================

#' @rdname generateReportOptimization
#' @export

setMethod( "generateReportOptimization",
           signature = "SimplexAlgorithm",
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

             # ============================
             # markdown template
             # ============================

             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "template_SimplexAlgorithm.rmd" )

             rmarkdown::render( input = nameInputFile,
                                output_file = outputFile,
                                output_dir = outputPath,
                                params = list(
                                  object = "object",
                                  plotOptions = "plotOptions",
                                  projectName = "projectName",
                                  fimType = "fimType",
                                  tablesEvaluationFIMIntialDesignResults = "tablesEvaluationFIMIntialDesignResults",
                                  tablesOptimizationObject = "tablesOptimizationObject" ) )

           })

##############################################################################
# END Class SimplexAlgorithm
##############################################################################
