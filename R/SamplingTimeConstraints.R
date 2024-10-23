#' Class "SamplingTimeConstraints"
#'
#' @description
#' The class "SamplingTimeConstraints" implements the constraints for the sampling times.
#'
#' @name SamplingTimeConstraints-class
#' @aliases SamplingTimeConstraints
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class \code{SamplingTimeConstraints}:
#' Objects form the class \code{SamplingTimeConstraints} can be created by calls of the form \code{SamplingTimeConstraints(...)} where
#' (...) are the parameters for the \code{SamplingTimeConstraints} objects.

#'@section Slots for \code{SamplingTimeConstraints} objects:
#'  \describe{
#'    \item{\code{outcome}:}{A string giving the outcome.}
#'    \item{\code{initialSamplings}:}{A vector giving the sampling times.}
#'    \item{\code{fixedTimes}:}{A vector giving the fixed  sampling times.}
#'    \item{\code{numberOfsamplingsOptimisable}:}{A vector giving the sampling times to be optimized.}
#'   \item{\code{samplingsWindows}:}{A list giving the windows for the sampling times.}
#'    \item{\code{numberOfTimesByWindows}:}{A vector giving the number of sampling times by windows.}
#'    \item{\code{minSampling}:}{A numeric giving the minimal sampling times.}
#'  }

SamplingTimeConstraints = setClass("SamplingTimeConstraints",
                                   representation = representation( outcome = "character",
                                                                    initialSamplings = "vector",
                                                                    fixedTimes = "vector",
                                                                    numberOfsamplingsOptimisable = "vector",
                                                                    samplingsWindows = "list",
                                                                    numberOfTimesByWindows = "vector",
                                                                    minSampling = "numeric" ) )
#' initialize
#' @param .Object .Object
#' @param outcome outcome
#' @param initialSamplings initialSamplings
#' @param fixedTimes fixedTimes
#' @param numberOfsamplingsOptimisable numberOfsamplingsOptimisable
#' @param samplingsWindows samplingsWindows
#' @param numberOfTimesByWindows numberOfTimesByWindows
#' @param minSampling minSampling
#' @return SamplingTimeConstraints
#' @export
#'
setMethod( f="initialize",
           signature="SamplingTimeConstraints",
           definition= function (.Object, outcome, initialSamplings, fixedTimes, numberOfsamplingsOptimisable,  samplingsWindows, numberOfTimesByWindows, minSampling )
           {
             if(!missing(outcome))
             {
               .Object@outcome = outcome
             }
             if(!missing(initialSamplings))
             {
               .Object@initialSamplings = initialSamplings
             }
             if(!missing(fixedTimes))
             {
               .Object@fixedTimes = fixedTimes
             }else
             {
               .Object@fixedTimes = rep(NA,0)
             }
             if(!missing(numberOfTimesByWindows))
             {
               .Object@numberOfTimesByWindows = numberOfTimesByWindows
             }
             if(!missing(numberOfsamplingsOptimisable))
             {
               .Object@numberOfsamplingsOptimisable = numberOfsamplingsOptimisable
             }
             if(!missing(minSampling))
             {
               .Object@minSampling = minSampling
             }
             if(!missing(samplingsWindows))
             {
               .Object@samplingsWindows = samplingsWindows
             }

             validObject(.Object)
             return (.Object )
           }
)

# ======================================================================================================
# getOutcome
# ======================================================================================================

#' @rdname getOutcome
#' @export
#'
setMethod(f="getOutcome",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@outcome )
          }
)

# ======================================================================================================
# getSamplings
# ======================================================================================================

#' @rdname getSamplings
#' @export
#'
setMethod(f="getSamplings",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@initialSamplings )
          }
)

#' Get the fixed sampling times.
#'
#' @name getFixedTimes
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A vector giving the foxed sampling times.
#' @export

setGeneric("getFixedTimes",
           function(object)
           {
             standardGeneric("getFixedTimes")
           }
)

#' @rdname getFixedTimes
#' @export
#'
setMethod(f="getFixedTimes",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@fixedTimes )
          }
)

#' Get the number of sampling times by windows.
#'
#' @name getNumberOfTimesByWindows
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A vector giving the number of sampling times by windows.
#' @export

setGeneric("getNumberOfTimesByWindows",
           function(object)
           {
             standardGeneric("getNumberOfTimesByWindows")
           }
)

#' @rdname getNumberOfTimesByWindows
#' @export
#'
setMethod(f="getNumberOfTimesByWindows",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@numberOfTimesByWindows )
          }
)

#' Get the minimal sampling times.
#'
#' @name getMinSampling
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A numeric giving the minimal sampling times.
#' @export

setGeneric("getMinSampling",
           function(object)
           {
             standardGeneric("getMinSampling")
           }
)

#' @rdname getMinSampling
#' @export
#'
setMethod(f="getMinSampling",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@minSampling )
          }
)

#' Get the windows for the sampling times.
#'
#' @name getSamplingsWindows
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A list giving the vector of the windows for the sampling times.
#' @export

setGeneric("getSamplingsWindows",
           function(object)
           {
             standardGeneric("getSamplingsWindows")
           }
)

#' @rdname getSamplingsWindows
#' @export
#'
setMethod(f="getSamplingsWindows",
          signature="SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@samplingsWindows )
          }
)

#' Get the number of sampling times that are optimisable.
#'
#' @name getNumberOfsamplingsOptimisable
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A vector giving the number of sampling times that are optimisable.
#' @export

setGeneric("getNumberOfsamplingsOptimisable",
           function(object)
           {
             standardGeneric("getNumberOfsamplingsOptimisable")
           }
)

#' @rdname getNumberOfsamplingsOptimisable
#' @export
#'
setMethod(f="getNumberOfsamplingsOptimisable",
          signature = "SamplingTimeConstraints",
          definition = function(object)
          {
            return( object@numberOfsamplingsOptimisable )
          }
)

#' Check for the samplingTime constraints for continuous optimization
#'
#' @name checkSamplingTimeConstraintsForContinuousOptimization
#' @param arm An object from the class \linkS4class{Arm}.
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @param newSamplings A vector giving the new sampling.
#' @param outcome The outcomes for the model.
#' @return A list of Boolean giving true if
#' the minimal sampling times is in the vector of sampling times & the number of sampling for each windows is respected
#' false otherwise.
#' @export
#'
setGeneric("checkSamplingTimeConstraintsForContinuousOptimization",
           function( object, arm, newSamplings, outcome )
           {
             standardGeneric("checkSamplingTimeConstraintsForContinuousOptimization")
           }
)

#' @rdname checkSamplingTimeConstraintsForContinuousOptimization
#' @export
#'
setMethod(f="checkSamplingTimeConstraintsForContinuousOptimization",
          signature = "SamplingTimeConstraints",
          definition = function( object, arm, newSamplings, outcome )
          {
            armName = getName( arm )

            # ===========================================
            # get min sampling constraint
            # ===========================================

            minSampling = getMinSampling( object )

            # ===========================================
            # get samplings window constraints
            # ===========================================

            samplingsWindow = getSamplingsWindows( object )

            # ===========================================
            # get numberOfTimesByWindows constraints
            # ===========================================

            numberOfTimesByWindows = getNumberOfTimesByWindows( object )

            # ===========================================
            # set new sampling times
            # ===========================================

            samplingTimes = getSamplingTime( arm, outcome )
            samplingTimes = setSamplings( samplingTimes, newSamplings )
            samplings = getSamplings( samplingTimes )

            # =======================================================
            # get the constraints min, max, delta and n
            # =======================================================

            minSamplingAndNumberOfTimesByWindows = as.data.frame( list( minSampling, numberOfTimesByWindows ) )
            data = t( as.data.frame(samplingsWindow ) )
            inputRandomSpaced = as.data.frame( do.call( "cbind", list( data, minSamplingAndNumberOfTimesByWindows ) ) )
            colnames( inputRandomSpaced ) = c("min","max","delta","n")
            rownames( inputRandomSpaced ) = NULL

            testForConstraintsWindowsLength = list()
            testForConstraintsMinimalSampling = list()

            for( iter in 1:length( inputRandomSpaced$n ) )
            {
              min = inputRandomSpaced$min[iter]
              max = inputRandomSpaced$max[iter]

              # =======================================================
              # check the constraint numberOfTimesByWindows
              # =======================================================

              testForConstraintsWindowsLength[[ armName ]][[ outcome ]][[iter]] = samplings[ samplings >= min  & samplings <= max ]

              # =======================================================
              # check the constraint minSampling
              # =======================================================

              testForConstraintsMinimalSampling[[ armName ]][[ outcome ]][[iter]] = diff( testForConstraintsWindowsLength[[ armName ]][[ outcome ]][[ iter ]] )

              # case for one sampling
              if ( length( testForConstraintsMinimalSampling[[ armName ]][[ outcome ]][[iter]] ) == 0 )
              {
                testForConstraintsMinimalSampling[[ armName ]][[ outcome ]][[iter]] = 0
              }
            }

            # =======================================================
            # tests for the constraint
            # =======================================================

            testForConstraintsWindowsLengthtmp = unlist( lapply( testForConstraintsWindowsLength[[ armName ]][[ outcome ]], function(x) length(x) ) )

            if ( all( testForConstraintsWindowsLengthtmp == numberOfTimesByWindows ) )
            {
              constraintWindowsLength = TRUE
            }else{
              constraintWindowsLength = FALSE
            }

            testForConstraintsMinimalSamplingTmp = unlist( lapply( testForConstraintsMinimalSampling[[ armName ]][[ outcome ]], function(x) min(x) ) )

            if ( all( testForConstraintsMinimalSamplingTmp >= minSampling ) )
            {
              constraintMinimalSampling = TRUE
            }else{
              constraintMinimalSampling = FALSE
            }

            return( list( constraintWindowsLength = constraintWindowsLength,
                          constraintMinimalSampling = constraintMinimalSampling ) )
          }
)

#' Generate samplings from sampling constraints
#'
#' @name generateSamplingsFromSamplingConstraints
#' @param object An object from the class \linkS4class{SamplingTimeConstraints}.
#' @return A list of sampling times generated from the sampling constraints.
#' @export

setGeneric("generateSamplingsFromSamplingConstraints",
           function( object )
           {
             standardGeneric("generateSamplingsFromSamplingConstraints")
           })

#' @rdname generateSamplingsFromSamplingConstraints
#' @export
#'
setMethod(f="generateSamplingsFromSamplingConstraints",
          signature = "SamplingTimeConstraints",
          definition = function( object )
          {
            outcome = getOutcome( object )

            # ===========================================
            # get min sampling constraint
            # ===========================================

            minSampling = getMinSampling( object )

            # ===========================================
            # get samplings window constraints
            # ===========================================

            samplingsWindow = getSamplingsWindows( object )

            # ===========================================
            # get numberOfTimesByWindows constraints
            # ===========================================

            numberOfTimesByWindows = getNumberOfTimesByWindows( object )

            # =======================================================
            # generate samplings from sampling constraints
            # =======================================================

            intervalsConstraints = list()

            minSamplingAndNumberOfTimesByWindows = as.data.frame( list( minSampling, numberOfTimesByWindows ) )
            data = t( as.data.frame( samplingsWindow ) )
            inputRandomSpaced = as.data.frame( do.call( "cbind", list( data, minSamplingAndNumberOfTimesByWindows ) ) )

            colnames( inputRandomSpaced ) = c("min","max","delta","n")
            rownames( inputRandomSpaced ) = NULL

            for( iter in 1:length( inputRandomSpaced$n ) )
            {
              min = inputRandomSpaced$min[iter]
              max = inputRandomSpaced$max[iter]
              delta = inputRandomSpaced$delta[iter]
              n = inputRandomSpaced$n[iter]

              distance = max-min-(n-1)*delta

              ind = runif(n,0,1)
              tmp = distance*sort( ind )
              interval = min + tmp + delta * seq(0,n-1,1)

              intervalsConstraints[[outcome]] = append( intervalsConstraints[[outcome]], interval )
            }

            return( intervalsConstraints )

          })

###########################################################################################
# End class SamplingTimeConstraints
###########################################################################################




