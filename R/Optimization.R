##################################################################################
#' Class "Optimization"
#'
#' @description
#' A class storing information concerning Optimization.
#'
#' @name Optimization-class
#' @aliases Optimization
#' @docType class
#' @include Design.R
#' @exportClass Optimization
#'
#' @section Objects from the class \code{Optimization}:
#' Objects form the class \code{Optimization} can be created by calls of the form \code{Optimization(...)} where
#' (...) are the parameters for the \code{Optimization} objects.
#'
#'@section Slots for \code{Optimization} objects:
#'  \describe{
#'    \item{\code{showProcess}:}{A logical if the optimization process is shown or not.}
#'    \item{\code{FisherMatrices}:}{A list of all the Fisher matrices used in the optimization process.}
#'    \item{\code{combinedTimes}:}{A list giving all he combinaison of n elements for a vector of times. }
#'    \item{\code{arms}:}{A list giving all the arms on which the optimization processis done.}
#'  }
##################################################################################

Optimization <-setClass(
  Class = "Optimization",
  representation = representation(
    showProcess = "logical",
    FisherMatrices = "list",
    combinedTimes = "list",
    initialElementaryProtocols = "list",
    #OptimalDesign = "Design",# warning : add for simplex
    arms = "list"
  ),
  prototype = prototype(
    FisherMatrices = list(),
    combinedTimes = list()
  )

)

setMethod(
  f="initialize",
  signature="Optimization",
  definition=function(.Object)
  {
    validObject(.Object)
    return (.Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the process for the optimization.
#'
#' @name setShowProcess
#' @rdname setShowProcess
#' @aliases setShowProcess
#' @param object A\code{Optimization} object.
#' @param ifShow TRUE or FALSE
#' @return Show process for the optimization.
setGeneric("setShowProcess",
           function(object, ifShow)
           {
             standardGeneric("setShowProcess")
           }

)

setMethod(f = "setShowProcess",
          signature = "Optimization",
          definition = function(object, ifShow)
          {
            object@showProcess <- ifShow
            return(object)
          }
)
# -------------------------------------------------------------------------------------------------------------------
#' Set the optimization process.
#'
#' @name Optimize
#' @rdname Optimize
#' @aliases Optimize
#' @param object An \code{Optimization} object.
#' @param pfimProject An \code{PFIMProject} object.
#' @param designs A list of \code{design} object.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param cond_init A list giving the initial conditions.
#' @param constraint A \code{Constraint} object.
#' @param typeFim A \code{Fim} object : type of FIM, Population, Individual or Bayesian.
#' @return A \code{design} object giving the optimal design.

setGeneric("Optimize",
           function(object,  pfimProject, designs, statistical_model, cond_init, constraint, typeFim)
           {
             standardGeneric("Optimize")
           }

)

setMethod(
  f = "Optimize",
  signature = "Optimization",
  definition = function( object, pfimProject, designs, statistical_model, cond_init, constraint, typeFim )
  {

  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Prepare the FIMs for the optimization.
#'
#' @name PrepareFIMs
#' @rdname PrepareFIMs
#' @aliases PrepareFIMs
#' @param object A \code{Optimization} object.
#' @param statistical_model A \code{StatisticalModel} object.
#' @param cond_init A list giving the initial conditions.
#' @param constraint A \code{Constraint} object.
#' @param typeFim : Type of FIM, Population, Individual or Bayesian.
#' @return A list \code{result} of all the FIMs.

setGeneric("PrepareFIMs",
           function( object, statistical_model, cond_init, constraint, typeFim )
           {
             standardGeneric("PrepareFIMs")
           }
)
setMethod(
  f = "PrepareFIMs",
  signature = "Optimization",
  definition = function( object, statistical_model, cond_init, constraint, typeFim )
  {

    # constraint@PossibleArms :
    if( length(constraint@PossibleArms) > 0 )
    {
      start <- Sys.time()
      if(object@showProcess) cat("\n Evaluation FIMs\n")
      FisherMatrices <- list()
      Q <- 0
      for (arm in constraint@PossibleArms)
      {
        Q <- Q+1

        results = EvaluateStatisticalModel(arm, statistical_model, typeFim )

        FisherMatrices[[Q]] = getMfisher( results$resultFim )
        object@arms[[Q]] <- arm

      }
      end <- Sys.time()
      if(object@showProcess) print(end-start)
      object@FisherMatrices <- FisherMatrices
      return(object)
    }
    else
    {
      start <- Sys.time()
      if(object@showProcess) cat("\n Process of FIM evaluation :\n")

      doses <- list()
      armBase <- list()
      objectiveProtocolNumber <- list()
      fixedTimes <- list()
      times <- list()
      RespName <- list()

      constraintNumber <- 0
      totalNumberOfFIMs <- 1

      if(length(constraint@samplingConstraints) > 0)
      {

        for(sampConstraint in constraint@samplingConstraints)
        {
          constraintNumber <- constraintNumber + 1

          RespName[[constraintNumber]] <- getResponseName(sampConstraint)

          for(adminConstraint in constraint@administrationConstraints){

            if( RespName[[constraintNumber]] == getResponseName(adminConstraint))
            {
              doses[[constraintNumber]] <- getAllowedDoses(adminConstraint)
            }else{
              doses[[constraintNumber]] = 0.0
            }
          }

          if ( class( typeFim)  %in% c( "PopulationFim" , "IndividualFim", "BayesianFim" ) )
          {
            armBase[[constraintNumber]] <- getallowedDiscretSamplingTimes( sampConstraint )

            objectiveProtocolNumber[[constraintNumber]] <- getnumberOfSamplingTimes( sampConstraint )

            fixedTimes[[ constraintNumber ]] <- getfixedTimes( sampConstraint )

            object = Combinaison( object,
                                  armBase[[constraintNumber]],
                                  objectiveProtocolNumber[[constraintNumber]],
                                  fixedTimes[[ constraintNumber ]], 1, c())

            times[[constraintNumber]] = object@combinedTimes
          }

          totalNumberOfFIMs <- totalNumberOfFIMs * length(doses[[constraintNumber]]) * length(times[[constraintNumber]])

        }
      }
      else
      {
        # no sampling constraints
        for(adminConstraint in constraint@administrationConstraints)
        {
          constraintNumber <- constraintNumber + 1
          RespName[[constraintNumber]] = getResponseName(adminConstraint)
          doses[[constraintNumber]] <- getAllowedDoses(adminConstraint)

          totalNumberOfFIMs <- totalNumberOfFIMs * length(doses[[constraintNumber]])
        }
      }

      result = EvaluateFIMsAndDesigns( object, 1, RespName, doses, times, list(), list(),
                                       statistical_model, cond_init, totalNumberOfFIMs, typeFim )



      # ------------------------------------------------------------------------------
      # part for Fedorov-Wynn algorithm
      # ------------------------------------------------------------------------------

      # administrationConstraints
      administrationConstraints = constraint@administrationConstraints
      allowedDoses = getAllowedDoses( administrationConstraints[[1]] )
      numberOfAllowedDoses = length( allowedDoses )

      samplingConstraints = getSamplingConstraints( constraint )
      numberOfSamplingTimes = getnumberOfSamplingTimes( samplingConstraints[[1]] )

      list = list()
      combinedList = list()
      combinedTimes2 = list()

      for ( i in 1:length( times ) )
      {
        list[[i]] = do.call( rbind,times[[i]] )
      }

      #  if ( lengths(list) == 1 )
      #  {
      #   combinedTimes = as.matrix( do.call( rbind, replicate( numberOfAllowedDoses, unlist(list), simplify = FALSE ) ) )
      #  }
      #  else
      # {
      combinationElements = expand.grid(lapply(list, function(x) seq_len(nrow(x))))
      combinedList = do.call( cbind, Map( function(x, y) x[y, ], list, combinationElements ) )
      combinedTimes = as.matrix( do.call( rbind, replicate( numberOfAllowedDoses, combinedList, simplify = FALSE ) ) )
      #}

      lengthSamplingTimesForEachElementaryProtocol = lapply( object@initialElementaryProtocols, length )
      totalNumberOfSamplingTimesForEachElementaryProtocol =
        unlist( lengthSamplingTimesForEachElementaryProtocol ) *  getTotalNumberOfIndividuals( constraint )
      total_cost = sum( totalNumberOfSamplingTimesForEachElementaryProtocol )

      # in initFedo.C : Fisher matrices = vector of lower element fisher matrix + diagonal
      # elements = [(1,1) ,(2,1:2),(3,1:3),etc ..]
      # number of elements = n*(n+1)/2 ; n = dim Fisher matrix

      fisherMatrices = result@FisherMatrices
      nb_dimensions = dim(fisherMatrices[[1]])[[1]]
      dimVectorTriangularInfWithDiagFisherMatrices = nb_dimensions*(nb_dimensions+1)/2
      fisherMatrices = lapply( fisherMatrices, function( x ) x[ rev( lower.tri( t( x ), diag=TRUE ) ) ] )
      fisherMatrices = matrix( unlist( fisherMatrices ), ncol = dimVectorTriangularInfWithDiagFisherMatrices, byrow = TRUE )

      elementary_protocols = list()
      elementary_protocols$nb_protocols = dim( combinedTimes )[1]
      elementary_protocols$nb_times = dim( combinedTimes )[2]
      elementary_protocols$nb_dimensions = nb_dimensions
      elementary_protocols$total_cost = total_cost
      elementary_protocols$samplingTimes = combinedTimes
      elementary_protocols$fisherMatrices = fisherMatrices

      result@initialElementaryProtocols = elementary_protocols

      end <- Sys.time()
      if(object@showProcess)
        print(end-start)

      return(result)
    }
  }
)


# -------------------------------------------------------------------------------------------------------------------
#' Get the matrix of all the combination of the elementary protocols.
#'
#' @name getElementaryProtocols
#' @param object An \code{Optimization} object.
#' @return A matrix giving  all the combination of the elementary protocols.

setGeneric("getElementaryProtocols",
           function(object)
           {
             standardGeneric("getElementaryProtocols")
           }
)

setMethod( f="getElementaryProtocols",
           signature="Optimization",
           definition = function(object)
           {
             return(object@initialElementaryProtocols)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Create all the possible combinaison for each Design and each Arms.
#'
#' @name Combinaison
#' @param object A \code{Optimization} object.
#' @param times  A \code{SAmplingTimes} object.
#' @param nTimesForEachVector the number of sampling times for each vector of sampling times.
#' @param fixedTimes the fixed sampling times.
#' @param n parameter n
#' @param combin parameter combin
#' @return All the possible combination for each Design and each Arms.

setGeneric("Combinaison",
           function(object, times, nTimesForEachVector, fixedTimes, n, combin)
           {
             standardGeneric("Combinaison")
           }
)
setMethod(f = "Combinaison",
          signature = "Optimization",
          definition = function(object, times, nTimesForEachVector, fixedTimes, n, combin)
          {

            object@combinedTimes = list()

            r<-combn( times[[n]], nTimesForEachVector[ n ])

            for(i in 1:dim(r)[2] )
            {
              newCombin=c( combin, r[,i] )

              if( n == length(times) )
              {
                if( all(fixedTimes %in% newCombin))
                  object@combinedTimes[[length(object@combinedTimes)+1]] <- newCombin
              }
              else{
                object = Combinaison( object, times, nTimesForEachVector, fixedTimes, n+1, newCombin )
              }}


            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the FIMs and the Designs.
#'
#' @name EvaluateFIMsAndDesigns
#' @param object An \code{Optimization} object.
#' @param responseNumber A numeric giving the number of responses.
#' @param responseNames A character string giving the name of the response
#' @param doses A vector of numeric values giving the doses.
#' @param times A vector of numeric values giving the times doses.
#' @param admin An \code{Administration} object giving the administration parameters.
#' @param samp An \code{SamplingTimes} object giving the sampling times parameters.
#' @param statistical_model  A \code{statisticalModel} object.
#' @param cond_init cond_init
#' @param totalNumberOfFIMs A numeric giving the total number of FIMs.
#' @param typeFim A character strgin gving the type of the FIM.
#' @return An \code{Optimization} object giving the results of the evaluation of the FIMs and the Designs.

setGeneric("EvaluateFIMsAndDesigns",
           function( object, responseNumber, responseNames, doses, times, admin, samp, statistical_model, cond_init,totalNumberOfFIMs, typeFim )
           {
             standardGeneric("EvaluateFIMsAndDesigns")
           })

setMethod(
  f = "EvaluateFIMsAndDesigns",
  signature = "Optimization",
  definition = function( object, responseNumber, responseNames, doses, times, admin, samp, statistical_model,cond_init, totalNumberOfFIMs, typeFim )
  {

    currentResponse <- responseNames[[ responseNumber ]]

    Q <- length(object@FisherMatrices)

    for(dose in doses[[ responseNumber ]])
    {
      admin[[ currentResponse ]] <- Administration( outcome = currentResponse, time_dose = c(0), amount_dose = dose )

      if(length(times) > 0)
      {
        for(timeIndice in 1:length(times[[ responseNumber ]]))
        {
          samp[[ currentResponse ]] <- SamplingTimes( outcome = currentResponse, sample_time = times[[ responseNumber ]][[timeIndice]] )

          if(responseNumber == length(responseNames))
          {
            Q <- length(object@FisherMatrices)

            results = Evaluate(statistical_model, admin, samp , cond_init, typeFim )

            object@FisherMatrices[[ Q+1 ]] <- getMfisher( results$fim )

            OneArm <- Arm(name = paste0("Arm ",(Q+1)))

            for(adm in admin)
              OneArm <- addAdministration( OneArm,  adm )
            for(sam in samp)
              OneArm <- addSampling( OneArm, sam )
            object@arms[[ Q+1]] <- OneArm
            if(object@showProcess)
            {
              percent <- (Q+1) / totalNumberOfFIMs * 100

              cat(sprintf('\r %d / %d', Q+1,totalNumberOfFIMs))

              if ((Q+1) == totalNumberOfFIMs)  cat('\n Evaluation of FIMs terminated. Total FIM number = ',totalNumberOfFIMs, "\n")
            }
          }
          else
          {
            object = EvaluateFIMsAndDesigns( object, responseNumber+1, responseNames, doses,
                                             times, admin, samp, statistical_model, cond_init, totalNumberOfFIMs, typeFim )
          }
        }
      }
      else
      {
        samp[[ currentResponse ]] <- SamplingTimes( outcome = currentResponse, sample_time = c(0) )

        if(responseNumber == length(responseNames))
        {
          Q <- length(object@FisherMatrices)

          results = Evaluate(statistical_model, admin, samp , cond_init, typeFim )

          object@FisherMatrices[[ Q+1 ]] <-as.matrix( results$fim )

          OneArm <- Arm(name = paste0("Arm ",(Q+1)))
          for(adm in admin)
            OneArm <- addAdministration( OneArm,  adm )
          object@arms[[ Q+1]] <- OneArm
          if(object@showProcess)
          {
            percent <- (Q+1) / totalNumberOfFIMs * 100
            cat(sprintf('\r[%-100s] %d%%', paste(rep('=', percent ), collapse = ''), floor(percent)))
            if ((Q+1) == totalNumberOfFIMs)  cat('\n Evaluation of FIMs terminated. Total FIM number = ',totalNumberOfFIMs, "\n")
          }
        }
        else
        {
          object = EvaluateFIMsAndDesigns( object, responseNumber+1, responseNames, doses, times, admin, samp,
                                           statistical_model, cond_init, totalNumberOfFIMs, typeFim )
        }
      }
    }
    return(object)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the optimal design.
#'
#' @name getOptimalDesign
#' @param object An \code{Design} object.
#' @return An \code{Design} object giving the optimal design.

setGeneric("getOptimalDesign",
           function(object)
           {
             standardGeneric("getOptimalDesign")
           }
)

setMethod("getOptimalDesign",
          "Optimization",
          function(object)
          {
            return(object@OptimalDesign)
          }

)

# -------------------------------------------------------------------------------------------------------------------
#' Set an optimal design.
#'
#' @name setOptimalDesign<-
#' @aliases setOptimalDesign
#' @param object A \code{PFIM} object.
#' @param value A \code{Design} object.
#' @return The \code{PFIM} object with the optimal design.

setGeneric("setOptimalDesign<-",
           function(object, value)
           {
             standardGeneric("setOptimalDesign<-")
           }
)
setReplaceMethod( f="setOptimalDesign",
                  signature="Optimization",
                  definition = function(object, value)
                  {
                    object@OptimalDesign <- value
                    validObject(object)
                    return(object)
                  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show for an \code{Optimization} object.
#'
#' @rdname show
#' @param object An \code{Optimization} object.
#' @return The content of an \code{Optimization} object.

setMethod("show",
          "Optimization",
          function(object)
          {

          }
)

#####################################################################################################
# END Class "Optimization"
#####################################################################################################











