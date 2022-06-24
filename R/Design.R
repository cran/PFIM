#####################################################################################################################################
#' Class "Design"
#'
#' @description The class \code{Design} defines information concerning the parametrization of the designs.
#'
#' @name Design-class
#' @aliases Design
#' @docType class
#' @include Optimization.R
#' @include Fim.R
#' @exportClass Design
#'
#' @section Objects from the class Design:
#' Objects form the class \code{Design} can be created by calls of the form \code{Design(...)} where
#' (...) are the parameters for the \code{Design} objects.
#'
#'@section Slots for the \code{Design} objects:
#' \describe{
#' \item{\code{isOptimalDesign}:}{A Boolean for testing if the Design is optimal (isOptimalDesign=TRUE) or not.}
#' \item{\code{name}:}{A character string giving the name of the design - optional.}
#' \item{\code{total_size}:}{A numeric giving the total number of subjects in the design - optional.}
#' \item{\code{arms}:}{List of objects from the class \linkS4class{Arm}.}
#' \item{\code{number_samples}:}{A numeric giving the raint on the number of samples for one subject - optional.
#' Default to the set of possible number of sampling points present in the sampling windows
#' defining the different arms or the design spaces.}
#' \item{\code{arms}:}{A list of arm objects from the class \linkS4class{Arm}.}
#' \item{\code{amountOfArm}:}{A numeric giving the number of arms in the study.}
#' \item{\code{optimizationResult}:}{An optimization object from the class \linkS4class{Optimization} giving the results from the optimizsation process.}
#' \item{\code{fimOfDesign}:}{A character string giving the Fisher Information Matrix of the design (Population, Individual or Bayesian).}
#' \item{\code{concentration}:}{A list giving the result of the evaluaton for the responses.}
#' \item{\code{sensitivityindices}:}{A list giving the result of the sensitivity indices for the responses.}
#' }
#'
#####################################################################################################################################

Design <- setClass(
  Class = "Design",
  representation = representation(
    isOptimalDesign = "logical",
    name = "character",
    total_size = "numeric",
    number_samples  = "vector",
    arms = "list",
    amountOfArm = "numeric",
    optimizationResult = "Optimization",
    fimOfDesign = "Fim",
    concentration = "list",
    sensitivityindices = "list"
  ),
  prototype = prototype(
    isOptimalDesign = FALSE,
    name =  paste("Design_",  ceiling( runif(1) * 100000 ), sep="" ),
    total_size = 0,
    arms = list(),
    amountOfArm = 0,
    optimizationResult = Optimization(),
    fimOfDesign = Fim()
  )
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "Design",
  definition = function (.Object, name, total_size, number_samples, amountOfArm, arms, fimOfDesign,
                         concentration, sensitivityindices )
  {
    if(!missing(name))
      .Object@name<-name

    if(!missing(total_size))
      .Object@total_size<-total_size

    if(!missing(number_samples))
      .Object@number_samples<-number_samples

    if(!missing(amountOfArm))
      .Object@amountOfArm<-amountOfArm

    if(!missing(arms))
      .Object@arms<-arms

    if(!missing(fimOfDesign))
      .Object@fimOfDesign<-fimOfDesign

    if(!missing(concentration))
      .Object@concentration<-concentration

    if(!missing(sensitivityindices))
      .Object@sensitivityindices<-sensitivityindices

    return (.Object )
  }
)

##########################################################################################################

#' Get the results of the optimization process.
#'
#' @name getOptimizationResult
#' @param object \code{Design} object.
#' @return A \code{Optimization} object giving the results of the optimization process.

setGeneric("getOptimizationResult",
           function(object)
           {
             standardGeneric("getOptimizationResult")
           }
)

setMethod("getOptimizationResult",
          "Design",
          function(object)
          {

            return(object@optimizationResult)
          }
)

##########################################################################################################

#' Modify an arm of a design.
#'
#' @name modifyArm
#' @param object A \code{Design} object.
#' @param name A character string giving the name of the \code{Arm} object to be modified in the \code{Design} object.
#' @param arm An \code{Arm} object.
#' @return The \code{Design} object with the modified arm.

setGeneric("modifyArm",
           function(object, name, arm)
           {
             standardGeneric("modifyArm")
           }
)

setMethod("modifyArm",
          "Design",
          function(object, name, arm)
          {

            if ( name %in% names( object@arms ) )
            {
              object@arms[[ name ]] <- arm
            }
            return(object)
          }
)

##########################################################################################################

#' Get the name of the design.
#'
#' @name getNameDesign
#' @param object \code{Design} object.
#' @return A character string \code{name} giving the name of design.
#

setGeneric("getNameDesign",
           function(object)
           {
             standardGeneric("getNameDesign")
           }
)

setMethod("getNameDesign",
          "Design",
          function(object)
          {
            return(object@name)
          }
)

##########################################################################################################

#' Set the name of the design.
#'
#' @name setNameDesign
#' @param object \code{Design} object.
#' @param name A character string \code{name} giving the new name of design.
#' @return The \code{Design} object with its new name.

setGeneric("setNameDesign",
           function(object, name)
           {
             standardGeneric("setNameDesign")
           }
)

setMethod("setNameDesign",
          "Design",
          function(object, name)
          {
            object@name = name
            return( object )

          }
)

##########################################################################################################

#' Get the total size of a design.
#'
#' @name getTotalSize
#' @param object \code{Design} object.
#' @return A numeric \code{total_size} giving the size of a design.

setGeneric("getTotalSize",
           function(object)
           {
             standardGeneric("getTotalSize")
           }
)

setMethod("getTotalSize",
          "Design",
          function(object)
          {
            return(object@total_size)
          }
)

##########################################################################################################

#' Set the total size of a Design.
#'
#' @name setTotalSize<-
#' @param object A \code{Design} object.
#' @param value A numeric giving the new value of the size of the design.
#' @return The \code{Design} object with the new size.

setGeneric("setTotalSize<-",
           function(object, value)
           {
             standardGeneric("setTotalSize<-")
           }
)

setReplaceMethod( f="setTotalSize",
                  signature="Design",
                  definition = function(object, value)
                  {
                    object@total_size <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Get the number of sampled in a Design.
#'
#' @name getNumberSamples
#' @param object \code{Design} object.
#' @return A numeric \code{number_samples} giving the number of sample of the design.

setGeneric("getNumberSamples",
           function(object)
           {
             standardGeneric("getNumberSamples")
           }
)

setMethod("getNumberSamples",
          "Design",
          function(object)
          {
            return(object@number_samples)
          }
)

##########################################################################################################

#' Set the number of Sample in a Design.
#'
#' @name setNumberSamples<-
#' @param object A \code{Design} object.
#' @param value A numeric giving the new value of samples.
#' @return The \code{Design} object with the new number of samples.

setGeneric("setNumberSamples<-",
           function(object, value)
           {
             standardGeneric("setNumberSamples<-")
           }
)

setReplaceMethod( f="setNumberSamples",
                  signature="Design",
                  definition = function(object, value)
                  {
                    object@number_samples <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Get the amount of arms in a Design.
#'
#' @name getAmountOfArms
#' @param object A \code{Design} object.
#' @return A numeric \code{amountOfArm} giving the number of arms in the design.

setGeneric("getAmountOfArms",
           function(object)
           {
             standardGeneric("getAmountOfArms")
           }
)

setMethod("getAmountOfArms",
          "Design",
          function(object)
          {
            return(length(object@arms))
          }
)

##########################################################################################################

#' Set the amount of arms in a Design.
#'
#' @name setAmountOfArms
#' @param object A \code{Design} object.
#' @param value A numeric giving the new value of the amount of arms in the design.
#' @return The \code{Design} object with the new vamue of amount of arms.

setGeneric("setAmountOfArms",
           function(object, value)
           {
             standardGeneric("setAmountOfArms")
           }
)

setMethod(f="setAmountOfArms",
          signature="Design",
          definition = function(object, value)
          {
            object@amountOfArm <- value
            validObject(object)
            return(object)
          }
)

##########################################################################################################

#' Get the Fisher Information Matrix of a design.
#'
#' @name getFimOfDesign
#' @param object A \code{Design} object.
#' @return A \code{Fim} object giving the Fisher Information Matrix of a design.

setGeneric("getFimOfDesign",
           function(object)
           {
             standardGeneric("getFimOfDesign")
           }
)

setMethod("getFimOfDesign",
          signature = "Design",
          definition = function(object)
          {
            return(object@fimOfDesign)
          }
)

##########################################################################################################

#' Get the arms of a design.
#'
#' @name getArms
#' @param object A \code{Design} object.
#' @return A list \code{arms} of the arms of a design.

setGeneric("getArms",
           function(object)
           {
             standardGeneric("getArms")
           }
)

setMethod("getArms",
          "Design",
          function(object)
          {
            return(object@arms)
          }
)

##########################################################################################################

#' Set the arms of a design.
#'
#' @name setArms
#' @param object A \code{Design} object.
#' @param value A \code{Arm} object.
#' @return The design \code{Design} with the new arm.

setGeneric("setArms",
           function(object, value)
           {
             standardGeneric("setArms")
           }
)

setMethod( f="setArms",
           signature="Design",
           definition = function(object, value)
           {
             for(arm in value){
               object@arms[[ getNameArm(arm) ]] <- arm
               object@total_size = object@total_size + getArmSize(arm)
               object@amountOfArm = object@amountOfArm + 1
             }
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Add an arm to a design.
#' @name addArm
#' @param object A \code{Design} object.
#' @param arm An \code{Arm} object.
#' @return The \code{Design} object with the new arm.

setGeneric("addArm",
           function(object, arm)
           {
             standardGeneric("addArm")
           }
)

setMethod( f="addArm",
           signature="Design",
           definition = function(object, arm)
           {
             if(length(object@arms[[ getNameArm( arm ) ]]) == 0)
             {
               object@arms[[ getNameArm( arm ) ]] <- arm
               object@total_size = object@total_size + getArmSize(arm)
               object@amountOfArm = object@amountOfArm + 1
             }
             else
               cat("You have already used this name for an arm.\n")

             validObject(object)
             return(object)
           })

##########################################################################################################

#' Add arms to a design.
#' @name addArms
#' @param object A \code{Design} object.
#' @param listOfArms A list of \code{Arm} object.
#' @return The \code{Design} object with the new arms.

setGeneric("addArms",
           function(object, listOfArms)
           {
             standardGeneric("addArms")
           }
)

setMethod( f="addArms",
           signature="Design",
           definition = function(object, listOfArms)
           {
             for ( i in 1:length( listOfArms) ){

               arm = listOfArms[[i]]

               if(length(object@arms[[ getNameArm( arm ) ]]) == 0)
               {
                 object@arms[[ getNameArm( arm ) ]] <- arm
                 object@total_size = object@total_size + getArmSize(arm)
                 object@amountOfArm = object@amountOfArm + 1
               }
               else
                 cat("You have already used this name for an arm.\n")

               validObject(object)
             }
             return(object)
           })

##########################################################################################################

#' Get the evaluated concentration and sensitivity indices of a design.
#'
#' @name getEvaluationDesign
#' @param object A \code{Design} object.
#' @return The object \code{Design} evaluated for each of its arm.

setGeneric("getEvaluationDesign",
           function(object )
           {
             standardGeneric("getEvaluationDesign")
           }
)

setMethod(f="getEvaluationDesign",
          signature=  "Design",
          definition=function( object )
          {
            evaluationResponse = list()
            sensitivityIndicesResponse = list()

            nameDesign = getNameDesign( object )

            arms = getArms( object )

            for (arm in arms)
            {
              nameArm = getNameArm( arm )
              evaluationResponse[[nameArm]] = object@concentration[[ nameArm ]]
              sensitivityIndicesResponse[[nameArm]] = object@sensitivityindices[[ nameArm ]]

            }

            return( list( evaluationResponse = evaluationResponse,
                          sensitivityIndicesResponse = sensitivityIndicesResponse ) )
          })

##########################################################################################################
#' Evaluate Design for each arm.
#'
#' @name EvaluateDesignForEachArm
#' @param object A \code{Design} object.
#' @param statistical_model A \code{statisticalModel} object.
#' @param fim A \code{fim} object.
#' @return The object \code{Design} evaluated for each of its arm.

setGeneric("EvaluateDesignForEachArm",
           function(object, statistical_model, fim )
           {
             standardGeneric("EvaluateDesignForEachArm")
           }
)

setMethod(f="EvaluateDesignForEachArm",
          signature=  "Design",
          definition=function(object, statistical_model, fim )
          {
            MF_i_total = NA
            first = T

            classFim = class( fim )
            arms = getArms( object )

            if ( length( arms )==0 ) {  }
            else
            {
              for ( arm in arms )
              {
                results = EvaluateStatisticalModel( arm, statistical_model, fim )
                nameArm = getNameArm( arm )

                object@concentration[[ nameArm ]] = results$concentrationModel
                object@sensitivityindices[[ nameArm ]] = results$sensitivityIndicesModel

                resultFIM = results$resultFim

                MF_i = getMfisher( resultFIM )

                if ( first )
                {
                  first = F
                  MF_i_total = MF_i
                }
                else
                  MF_i_total = MF_i_total + MF_i
              }
            }

            if(object@isOptimalDesign)
              fim@isOptimizationResult <- TRUE

            resultFIM@mfisher = as.matrix(MF_i_total)
            object@fimOfDesign <- resultFIM

            return( object )
          }
)

##########################################################################################################

#' Show the data of an arm for a design.
#'
#' @name showArmData
#' @rdname showArmData
#' @aliases showArmData
#' @param object A \code{Design} object.
#' @return Return a character string giving the the data summary of an arm for a design.

setGeneric("showArmData",
           function(object)
           {
             standardGeneric("showArmData")
           }
)

setMethod(f = "showArmData",
          "Design",
          function(object)
          {
            armData = summaryArmData(object)
            print(armData)
            cat("\n Total size of design : ", getTotalSize(object),"\n")
          }
)

##########################################################################################################

#' Gives a summary of all the parameters of an arm for a design.
#'
#' @name summaryArmData
#' @param object A \code{Design} object.
#' @return Display a summary of all the parameters of the arms for a design.

setGeneric("summaryArmData",
           function( object )
           {
             standardGeneric("summaryArmData")
           }
)

setMethod("summaryArmData",
          "Design",
          function( object )
          {
            dataArm = list()
            dataRespPK = list()
            dataRespPD = list()

            arms = getArms( object )

            roundSamplingTimes = 2

            for( arm in arms )
            {
              armSize = getArmSize( arm )
              armSize = round( armSize, roundSamplingTimes )

              nameArm = getNameArm( arm )

              admin = getAdministration( arm )
              samplings = getSamplings( arm )

              namesResponses = names( samplings )
              namesRespPK = namesResponses[grep("RespPK",namesResponses)]
              namesRespPD = namesResponses[grep("RespPD",namesResponses)]

              for ( nameResponsePK in namesRespPK )
              {

                timeDosesData = getTimeDose( admin[[nameResponsePK]] )
                dosesData = getAmountDose(  admin[[nameResponsePK]]  )
                tau = getTau( admin[[nameResponsePK]] )
                Tinf = getTinf( admin[[nameResponsePK]] )
                samplingTimes = getSampleTime( samplings[[nameResponsePK]] )

                if ( tau !=0 )
                {
                  n = max( samplingTimes )%/%tau
                  timeDosesData =  c(0:n)*tau
                  dosesData = rep( dosesData, n )
                }

                if ( unique( Tinf ) == 0 )
                {
                  Tinf="-"
                }else{
                  Tinf = toString( Tinf )
                }

                if ( tau == 0 )
                {
                  tau="-"
                }else{
                  tau = toString( tau )
                }

                timeDosesData = toString( timeDosesData )
                dosesData = toString( dosesData )

                samplingTimes = sort(samplingTimes)
                samplingTimes = paste0( "(", toString(samplingTimes), ")")

                data = data.frame( I( c( nameArm, nameResponsePK, tau,
                                         Tinf, timeDosesData, dosesData, samplingTimes, armSize ) ) )

                dataRespPK[[nameResponsePK]] = t(data)
                rownames( dataRespPK[[nameResponsePK]] )=NULL
              }

              for ( nameResponsePD in namesRespPD )
              {
                samplingTimes = sort(getSampleTime(samplings[[nameResponsePD]]))
                samplingTimes = paste0( "(", toString( samplingTimes ), ")")
                data = data.frame(I(c(nameArm, nameResponsePD, " ", " ", " ", " ", samplingTimes, armSize )))

                dataRespPD[[nameResponsePD]] =  t(data)
                rownames( dataRespPD[[nameResponsePD]] )=NULL
              }

              dataArm[[nameArm]]  = rbind( do.call( rbind, dataRespPK  ),
                                           do.call( rbind, dataRespPD ) )

            }

            dataArm <- as.data.frame( do.call( rbind,dataArm ) )

            colnames(dataArm) =  c( "Arm_name", "Response", "tau", "Tinf", "Time_dose", "Amount_dose" ,"Sampling_times", "Number_of_subjects")
            rownames(dataArm) =  NULL

            return(dataArm)
          })

##########################################################################################################

#' Show a design.
#'
#' @rdname show
#' @param object A \code{Design} object.
#' @return Return the FIM of the design and the data summary of the arm in the design.

setMethod(f = "show",
          signature = "Design",
          definition = function(object)
          {

            cat("\n\n**************************** ",object@name," ******************************* \n\n")

            print( summaryArmData(object) )

          }
)

##########################################################################################################

#' summary
#'
#' @rdname summary
#' @param object A \code{Design} object.
#' @return Return a list giving the name, the number of individuals, the total size of the design, the
#' the summary of all the parameters of the arms for a design and the amount of arm in the design.

setMethod("summary",
          "Design",
          function(object)
          {
            return(list(name = object@name,
                        numberOfIndividuals = object@total_size,
                        arms = summaryArmData(object),
                        amountOfArms = object@amountOfArm))
          }

)

##########################################################################################################
# END Class "Design"
##########################################################################################################







