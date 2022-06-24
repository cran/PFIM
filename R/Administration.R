#####################################################################################################################################
#' Class "Administration"
#'
#' @description
#' The class \code{Administration} defines information concerning the parametrization and the type of administration:
#' single dose, multiple doses. Constraints can also be added on the allowed times, doses and infusion duration.
#'
#' @name Administration-class
#' @aliases Administration
#' @docType class
#' @include Constraint.R
#' @exportClass Administration
#'
#' @section Objects from the class:
#' Objects form the class \code{Administration} can be created by calls of the form \code{Administration(...)} where
#' (...) are the parameters for the \code{Administration} objects.
#'
#' @section Slots for \code{Administration} objects:
#'  \describe{
#'    \item{\code{outcome}:}{A character string giving the name for the reponse of the model.}
#'    \item{\code{time_dose}:}{A numeric vector giving the times when doses are given. By default set to 0.}
#'    \item{\code{amount_dose}:}{A numeric vector giving the amount of doses.}
#'    \item{\code{tau}:}{A numeric giving the frequency.}
#'    \item{\code{Tinf}:}{A numeric vector giving the infusion duration Tinf (Tinf can be null).}
#'    \item{\code{allowed_time}:}{Constraint object containing the constraints on allowed times.}
#'    \item{\code{allowed_dose}:}{Constraint object containing the constraints on allowed dose.}
#'    \item{\code{allowed_tinf}:}{Constraint object containing the constraints on Tinf.}
#'  }
#'
#####################################################################################################################################

Administration <- setClass(Class="Administration",
                           representation=representation(
                             outcome="character",
                             time_dose="vector",  	# vector of times when doses are given
                             amount_dose='numeric',   # vector of amount of doses
                             Tinf="vector",    # vector of times giving the duration of infusion (can be null)
                             tau="numeric",    # tau frequency
                             allowed_time="Constraint",	# Constraint object containing the constraints on times
                             allowed_dose="Constraint",  # Constraint object containing the constraints on doses
                             allowed_tinf="Constraint"  # Constraint object containing the constraints on tinf
                           ),
                           prototype=prototype(
                             time_dose=c(0),
                             Tinf=c(0),
                             tau=c(0)
                           ) )

# Initialize method
setMethod(
  f = "initialize",
  signature = "Administration",
  definition = function (.Object, outcome, time_dose, amount_dose, Tinf, tau, allowed_time, allowed_dose, allowed_tinf)
  {
    .Object@time_dose<-c(0)

    if(!missing(outcome))
      .Object@outcome<-as.character(outcome)

    if(!missing(time_dose))
    {
      if ( is.vector( time_dose ) )
        .Object@time_dose<-time_dose
      else
        .Object@time_dose<-c( time_dose )
    }

    if(!missing(amount_dose))
    {
      if ( is.vector( amount_dose ) )
        .Object@amount_dose<-amount_dose
      else
        .Object@amount_dose<-c( amount_dose )
    }
    if(!missing(Tinf))
    {
      if ( is.vector( Tinf ) )
        .Object@Tinf<-Tinf
      else
        .Object@Tinf<-c( Tinf )
    }

    if(!missing(tau))
      .Object@tau<-tau

    if(!missing(allowed_time))
      .Object@allowed_time<-allowed_time

    if(!missing(allowed_dose))
      .Object@allowed_dose<-allowed_dose

    if(!missing(allowed_tinf))
      .Object@allowed_tinf<-allowed_tinf

    return (.Object )
  }
)

##########################################################################################################

#' Get the name of the outcome of an object \code{Administration}.
#'
#' @name getNameAdministration
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return A character string giving the name for the response of the object \code{Administration}.

setGeneric("getNameAdministration",
           function(object)
           {
             standardGeneric("getNameAdministration")
           }
)
setMethod("getNameAdministration",
          "Administration",
          function(object)
          {
            return(object@outcome)
          }
)

##########################################################################################################

#' Test if an object \code{Administration}for a model is multi-doses or not.
#'
#' @name is.multidose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return A boolean that gives TRUE if the administration is multi-doses, FALSE otherwise.

setGeneric("is.multidose",
           function(object)
           {
             standardGeneric("is.multidose")
           }
)
setMethod(f="is.multidose",
          signature="Administration",
          definition = function(object)
          {
            if ( length( object@time_dose) > 1 || !exists( "object@tau" ) )
              return( T )
            return( F )
          }
)

##########################################################################################################

#' Get the times vector when doses are given.
#'
#' @name getTimeDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The vector \code{time_dose} giving the times when the doses are given.

setGeneric("getTimeDose",
           function(object)
           {
             standardGeneric("getTimeDose")
           }
)
setMethod(f="getTimeDose",
          signature="Administration",
          definition = function(object)
          {
            return(object@time_dose)
          }
)

##########################################################################################################

#' Set the times vector when doses are given.
#'
#' @name setTimeDose<-
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value of the time dose.
#' @return The object \code{Administration} with its new times vector for doses.

setGeneric("setTimeDose<-",
           function(object, value)
           {
             standardGeneric("setTimeDose<-")
           }
)
setReplaceMethod( f="setTimeDose",
                  signature="Administration",
                  definition = function(object, value)
                  {
                    object@time_dose <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Get the amount of doses.
#'
#' @name getAmountDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{amount_dose} giving the amount of doses.

setGeneric("getAmountDose",
           function(object)
           {
             standardGeneric("getAmountDose")
           }
)

setMethod(f="getAmountDose",
          signature="Administration",
          definition=function(object)
          {
            return(object@amount_dose)
          }
)

##########################################################################################################

#' Set the amount of dose
#'
#' @name setAmountDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value of the amount of dose.
#' @return The numeric \code{amount_dose} giving the new value of the amount of dose.

setGeneric("setAmountDose",
           function(object, value)
           {
             standardGeneric("setAmountDose")
           }
)
setMethod( f="setAmountDose",
           signature="Administration",
           definition = function(object, value)
           {
             if(length(value) > 1)
               cat("\n Only one dose value is allowed in an arm.\n")
             else
               object@amount_dose <- value
             validObject(object)
             return(object)
           }
)
##########################################################################################################

#' Get the frequency \code{tau}.
#'
#' @name getTau
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{tau} giving the frequency \code{tau}.

setGeneric("getTau",
           function(object)
           {
             standardGeneric("getTau")
           }
)
setMethod(f="getTau",
          signature="Administration",
          definition=function(object)
          {
            return(object@tau)
          }
)

##########################################################################################################

#' Get the infusion duration.
#'
#' @name getTinf
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{Tinf} giving the infusion duration Tinf.

setGeneric("getTinf",
           function(object)
           {
             standardGeneric("getTinf")
           }
)
setMethod(f="getTinf",
          signature="Administration",
          definition=function(object)
          {
            return(object@Tinf)
          }
)

##########################################################################################################

#' Set the infusion duration.
#'
#' @name setTinf
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value for the infusion duration Tinf.
#' @return The object \code{Administration} with its new value of the infusion duration Tinf.

setGeneric("setTinf",
           function(object, value)
           {
             standardGeneric("setTinf")
           }
)
setMethod( f="setTinf",
           signature="Administration",
           definition = function(object, value)
           {
             object@Tinf = value
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Set the infusion lag tau.
#'
#' @name setTau
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value for the infusion lag tau.
#' @return The object \code{Administration} object with its new value of the infusion lag tau.

setGeneric("setTau",
           function(object, value)
           {
             standardGeneric("setTau")
           }
)
setMethod( f="setTau",
           signature="Administration",
           definition = function(object, value)
           {
             object@tau = value
             validObject(object)
             return(object)
           }
)

##########################################################################################################

#' Get the constraints on allowed times.
#'
#' @name getAllowedTime
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric vector \code{allowed_time} giving the constraints on allowed times.
#
setGeneric("getAllowedTime",
           function(object)
           {
             standardGeneric("getAllowedTime")
           }
)
setMethod(f="getAllowedTime",
          signature="Administration",
          definition=function(object)
          {
            return(object@allowed_time)
          }
)

##########################################################################################################

#' Set the constraints on allowed times.
#'
#' @name setAllowedTime<-
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A vector of the numeric values for the new constraints on allowed times.
#' @return The \code{Administration} object with the new constraints on allowed times.

setGeneric("setAllowedTime<-",
           function(object, value)
           {
             standardGeneric("setAllowedTime<-")
           }
)
setReplaceMethod( f="setAllowedTime",
                  signature="Administration",
                  definition = function(object, value)
                  {
                    object@allowed_time <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Get the constraints on allowed dose
#'
#' @name getAllowedDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The vector \code{allowed_dose} of the numeric values of the constraints on allowed dose.

setGeneric("getAllowedDose",
           function(object)
           {
             standardGeneric("getAllowedDose")
           }
)
setMethod(f="getAllowedDose",
          signature="Administration",
          definition=function(object)
          {
            return(object@allowed_dose)
          }
)

##########################################################################################################

#' Set the constraints on allowed dose.
#'
#' @name setAllowedDose<-
#' @aliases setAllowedDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value for the new dose value.
#' @return The \code{Administration} object with the new constraints on the allowed dose.

setGeneric("setAllowedDose<-",
           function(object, value)
           {
             standardGeneric("setAllowedDose<-")
           }
)

setReplaceMethod( f="setAllowedDose",
                  signature="Administration",
                  definition = function(object, value)
                  {
                    object@allowed_dose <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################

#' Get the constraints on Tinf.
#'
#' @name getAllowedTinf
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The vector \code{allowed_tinf} giving the constraints on Tinf.

setGeneric("getAllowedTinf",
           function(object)
           {
             standardGeneric("getAllowedTinf")
           }
)
setMethod("getAllowedTinf",
          "Administration",
          function(object)
          {
            return(object@allowed_tinf)
          }
)

##########################################################################################################

#' Set the constraints on Tinf.
#'
#' @name setAllowedTinf<-
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param value A numeric value for the new constraints on Tinf.
#' @return The \code{Administration} object with the new constraints on Tinf.

setGeneric("setAllowedTinf<-",
           function(object, value)
           {
             standardGeneric("setAllowedTinf<-")
           }
)
setReplaceMethod( f="setAllowedTinf",
                  signature="Administration",
                  definition = function(object, value)
                  {
                    object@allowed_tinf <- value
                    validObject(object)
                    return(object)
                  }
)

##########################################################################################################
# END Class "Administration"
##########################################################################################################


