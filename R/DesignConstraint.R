#####################################################################################################################################
#' Class "DesignConstraint"
#'
#' @description The class \code{DesignConstraint}
#' defines information concerning the parametrization of the constraints on a design.
#'
#' @name DesignConstraint-class
#' @aliases DesignConstraint
#' @docType class
#' @include Constraint.R
#' @exportClass DesignConstraint
#'
#' @section Objects from the class \code{DesignConstraint}:
#' Objects form the class \linkS4class{DesignConstraint} can be created by calls of the form \code{DesignConstraint(...)} where
#' (...) are the parameters for the \code{DesignConstraint} objects.
#'
#' @section Slots for the \code{DesignConstraint} objects:
#' \describe{
#' \item{\code{name}:}{A character string giving the name of the design - optional.}
#' \item{\code{PossibleArms}:}{A list of arms for optimization.}
#' \item{\code{totalNumberOfIndividuals}:}{A numeric giving the total number of individuals in the design.}
#' \item{\code{amountOfArm}:}{A numeric giving the number of arms in the design.}
#' \item{\code{samplingConstraints}:}{A list giving the sampling constraints for the design.}
#' \item{\code{administrationConstraints}:}{A list giving the administration constraints for the design.}
#' }
#'


DesignConstraint <- setClass(
  Class = "DesignConstraint",
  contains = "Constraint",
  slots = c(
    name = "character",
    PossibleArms = "list",
    totalNumberOfIndividuals = "numeric",
    amountOfArm = "numeric",
    samplingConstraints = "list",
    administrationConstraints = "list"
  ),
  prototype = prototype(
    name = paste("Constraint_", ceiling( runif(1) * 100000 ), sep="" ),
    PossibleArms = list(),
    totalNumberOfIndividuals = 1,
    amountOfArm = 0,
    samplingConstraints = list(),
    administrationConstraints = list()
  ),
  validity = function(object)
  {
    return(TRUE)
  }
)

setMethod(
  f="initialize",
  signature="DesignConstraint",
  definition= function (.Object, name, samplingConstraints)
  {
    if(!missing(name))
      .Object@name<-name

    if(!missing(samplingConstraints))
      .Object@samplingConstraints<-samplingConstraints

    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################

#' Set the possible arms in a Design or the case when lots of arms are defined and aim to optimise amoung several of them.
#'
#' @param object A \code{DesignConstraint} object.
#' @param design A \code{Design} object.
#' @param choice A vector of arm's serial number, to form an arm-space
#' @return The \code{DesignConstraint} object with all the possible arms

setGeneric("setPossibleArms",
           function( object, design, choice )
           {
             standardGeneric("setPossibleArms")
           }
)

setMethod (f="setPossibleArms",
           signature="DesignConstraint",
           def=function( object, design, choice )
           {
             allArms <- getArms(design)
             for(c in choice)
               object@PossibleArms[[length(object@PossibleArms) + 1]] <- allArms[[c]]
             return(object)
           }
)

##########################################################################################################

#' Set amount of arms in a  \code{DesignConstraint} object for the case we aim to obtain a fixed amount of arms as result.
#'
#' @name setAmountOfArmsAim
#' @param object A \code{DesignConstraint} object.
#' @param value A numeric.
#' @return A numeric \code{amountOfArm} giving the amount of arms for the case we aim to obtain a fixed amount of arms as result.

setGeneric("setAmountOfArmsAim",
           function(object, value )
           {
             standardGeneric("setAmountOfArmsAim")
           }
)

setMethod( f="setAmountOfArmsAim",
           signature="DesignConstraint",
           definition = function(object, value )
           {
             object@amountOfArm  <- value
             return(object)
           }
)

##########################################################################################################
#' Set the total number of individuals in a \code{DesignConstraint} object.
#'
#' @name setTotalNumberOfIndividuals
#' @param object \code{DesignConstraint} object.
#' @param totalNumberOfIndividual Total number of individual to be set.
#' @return The \code{DesignConstraint} object with the total number of individual.

setGeneric("setTotalNumberOfIndividuals",
           function(object, totalNumberOfIndividual)
           {
             standardGeneric("setTotalNumberOfIndividuals")
           }
)

setMethod( f="setTotalNumberOfIndividuals",
           signature="DesignConstraint",
           definition = function(object, totalNumberOfIndividual)
           {
             object@totalNumberOfIndividuals  <- totalNumberOfIndividual
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' get the total number of individuals in a \code{DesignConstraint} object.
#'
#' @name getTotalNumberOfIndividuals
#' @param object \code{DesignConstraint} object.
#' @return The \code{DesignConstraint} object with the total number of individual.

setGeneric("getTotalNumberOfIndividuals",
           function(object)
           {
             standardGeneric("getTotalNumberOfIndividuals")
           }
)

setMethod( f="getTotalNumberOfIndividuals",
           signature="DesignConstraint",
           definition = function(object)
           {
             return(object@totalNumberOfIndividuals)
           }
)

##########################################################################################################

#' Add a constraint on the sampling for a design.
#'
#' @name addSamplingConstraint
#' @param object A \code{DesignConstraint} object.
#' @param value A \code{SamplingConstraint} object.
#' @return The \code{DesignConstraint} object constraints with the constraints from \code{SamplingConstraint} object added.

setGeneric("addSamplingConstraint",
           function(object, value)
           {
             standardGeneric("addSamplingConstraint")
           }
)
setMethod (f="addSamplingConstraint",
           signature="DesignConstraint",
           definition = function(object,value)
           {

             object@samplingConstraints[[ length(object@samplingConstraints) + 1]] <- value
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Add design constraints on the sampling for a design.
#'
#' @name addDesignConstraints
#' @param object A \code{DesignConstraint} object.
#' @param listOfConstraints A list of \code{Constraint} object.
#' @return The \code{DesignConstraint} object constraints with the constraints from \code{listOfConstraints}.

setGeneric("addDesignConstraints",
           function(object, listOfConstraints)
           {
             standardGeneric("addDesignConstraints")
           }
)
setMethod(f="addDesignConstraints",
          signature="DesignConstraint",
          definition = function(object,listOfConstraints)
          {
            for ( i in 1:length( listOfConstraints)){
              object@samplingConstraints[[ i ]] <- listOfConstraints[[i]]
            }
            return(object)
          }
)

##########################################################################################################

#' Add constraints on the administration for a \code{DesignConstraint} object.
#'
#' @name addAdministrationConstraint
#' @param object A \code{DesignConstraint} object.
#' @param value An \code{AdministrationConstraint} object.
#' @return The \code{DesignConstraint} object with its administration constraints.

setGeneric("addAdministrationConstraint",
           function(object, value)
           {
             standardGeneric("addAdministrationConstraint")
           }
)
setMethod (f="addAdministrationConstraint",
           signature="DesignConstraint",
           definition = function(object,value)
           {
             object@administrationConstraints[[ length(object@administrationConstraints) + 1]] <- value
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Get the name of the  \code{DesignConstraint} object.
#'
#' @name getNameDesignConstraint
#' @param object A \code{DesignConstraint} object.
#' @return A character string \code{name} giving the name of \code{DesignConstraint} object.

setGeneric("getNameDesignConstraint",
           function(object)
           {
             standardGeneric("getNameDesignConstraint")
           }
)

setMethod("getNameDesignConstraint",
          "DesignConstraint",
          function(object)
          {
            return(object@name)
          }
)

##########################################################################################################

#' Get the constraints on the sampling for a \code{DesignConstraint} object.
#'
#' @name getSamplingConstraints
#' @param object \code{DesignConstraint} object.
#' @param responseName A character string giving the name of the response.
#' @return The lists of constraints \code{samplingConstraint} for a \code{DesignConstraint} object.


setGeneric("getSamplingConstraints",
           function(object, responseName)
           {
             standardGeneric("getSamplingConstraints")
           }
)
setMethod (f="getSamplingConstraints",
           signature="DesignConstraint",
           definition = function(object, responseName)
           {
             if(missing(responseName))
               return(object@samplingConstraints)

             constraintsOfTheResponse <- list()

             for (i in object@samplingConstraints){

               if(getResponseName(i)==responseName)
                 constraintsOfTheResponse <- append( constraintsOfTheResponse,i )
             }
             return(constraintsOfTheResponse)
           }
)

##########################################################################################################

#' Get the constraints on the administration for a  \code{DesignConstraint} object.
#'
#' @name getAdministrationConstraint
#' @param object A \code{DesignConstraint} object.
#' @return The list of constraints on the administration given by \code{administrationConstraint} for a  \code{DesignConstraint} object.

setGeneric("getAdministrationConstraint",
           function(object)
           {
             standardGeneric("getAdministrationConstraint")
           }
)
setMethod (f="getAdministrationConstraint",
           signature="DesignConstraint",
           definition = function(object)
           {
             return(object@administrationConstraint)
           }
)

##########################################################################################################

#' Show the content of a design.
#'
#' @rdname show
#' @param object A \code{DesignConstraint} object.
#' @return Show the content of a design.

setMethod(f = "show",
          signature = "DesignConstraint",
          definition = function(object)
          {
            cat("\n\n*********************** ",object@name, " ******************************\n")
            if(length(object@PossibleArms)>0)
            {
              armSpace <- Design()
              armSpace <- setArms( armSpace, object@PossibleArms)
              showArmData(armSpace)
            }

            if(length(object@samplingConstraints) > 0)
            {
              for(sampConstraint in object@samplingConstraints)
              {
                cat("\n")
                cat("Corresponding response name : ",sampConstraint@response,"\n")

                constraintLineSamp <- c("      ", "Allowed_discret_sampling_times" = toString(getallowedDiscretSamplingTimes(sampConstraint)))
                constraintLineSamp <- c(constraintLineSamp, "Number_of_sampling_times" = toString(getnumberOfSamplingTimes(sampConstraint)))
                constraintLineSamp <- c(constraintLineSamp, "Fixed_times" = toString(getfixedTimes(sampConstraint)))

                print(data.frame("  *** Sampling times constraint ***" = constraintLineSamp, stringsAsFactors=FALSE, check.names=FALSE))
                cat("\n")

                for(adminConstraint in object@administrationConstraints)
                {
                  if(sampConstraint@response == adminConstraint@response)
                  {
                    constraintLineAdmin <- c("Allowed_doses" = toString(getAllowedDoses(adminConstraint)))
                    constraintLineAdmin <- c(constraintLineAdmin, "Dose_optimisability" = getDoseOptimisability(adminConstraint))
                    constraintLineAdmin <- c(constraintLineAdmin, "Number_of_doses" = getNumberOfDoses(adminConstraint))

                    print(data.frame("  *** Administration constraint ***" = constraintLineAdmin, stringsAsFactors=FALSE, check.names=FALSE))
                    cat("\n")
                  }
                }
              }
            }
            else
            {
              for(adminConstraint in object@administrationConstraints)
              {
                cat("\n")
                cat("Corresponding response name : ",adminConstraint@response,"\n")

                constraintLineAdmin <- c("Allowed_doses" = toString(getAllowedDoses(adminConstraint)))
                constraintLineAdmin <- c(constraintLineAdmin, "Dose_optimisability" = getDoseOptimisability(adminConstraint))
                constraintLineAdmin <- c(constraintLineAdmin, "Number_of_doses" = getNumberOfDoses(adminConstraint))

                print(data.frame("  *** Administration constraint ***" = constraintLineAdmin, stringsAsFactors=FALSE, check.names=FALSE))
                cat("\n")
              }
            }

          }
)

##########################################################################################################
# END Class "DesignConstraint"
##########################################################################################################

