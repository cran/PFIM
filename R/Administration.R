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
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Administration} can be created by calls of the form \code{Administration(...)} where
#' (...) are the parameters for the \code{Administration} objects.
#'
#' @section Slots for \code{Administration} objects:
#'  \describe{
#'    \item{\code{outcome}:}{A character string giving the name for the response of the model.}
#'    \item{\code{timeDose}:}{A numeric vector giving the times when doses are given.}
#'    \item{\code{dose}:}{A numeric vector giving the amount of doses.}
#'    \item{\code{Tinf}:}{A numeric vector giving the infusion duration Tinf (Tinf can be null).}
#'    \item{\code{tau}:}{A numeric giving the frequency.}
#'  }

Administration = setClass(Class="Administration",
                          representation=representation(
                            outcome = "character",
                            timeDose = "vector",
                            dose = 'numeric',
                            Tinf = "vector",
                            tau = "numeric"),
                          prototype=prototype( timeDose=c(0), Tinf=c(0), tau=c(0) ) )

setMethod(
  f = "initialize",
  signature = "Administration",
  definition = function (.Object, outcome, timeDose, dose, Tinf, tau)
  {
    if(!missing(outcome))
    {
      .Object@outcome = outcome
    }

    if(!missing(timeDose))
    {
      if ( is.vector( timeDose ) )
        .Object@timeDose = timeDose
      else
        .Object@timeDose = c( timeDose )
    }
    if(!missing(dose))
    {
      if ( is.vector( dose ) )
        .Object@dose = dose
      else
        .Object@dose = c( dose )
    }
    if(!missing(Tinf))
    {
      if ( is.vector( Tinf ) )
        .Object@Tinf=Tinf
      else
        .Object@Tinf=c( Tinf )
    }

    if(!missing(tau))
    {
      .Object@tau=tau
    }
    return (.Object )
  }
)

# ======================================================================================================
# getOutcome
# ======================================================================================================

setMethod(f="getOutcome",
          signature="Administration",
          definition = function(object)
          {
            return(object@outcome)
          })

# ======================================================================================================
# setOutcome
# ======================================================================================================

setMethod(f="setOutcome",
          signature="Administration",
          definition = function(object,outcome)
          {
            object@outcome = outcome
            return(object)
          })

# ======================================================================================================
#' Get the times vector when doses are given.
#'
#' @name getTimeDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The vector \code{timeDose} giving the times when the doses are given.
# ======================================================================================================

setGeneric("getTimeDose",
           function(object)
           {
             standardGeneric("getTimeDose")
           })

setMethod(f="getTimeDose",
          signature="Administration",
          definition = function(object)
          {
            return(object@timeDose)
          })

# ======================================================================================================
#' Set the times vector when doses are given.
#'
#' @name setTimeDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param timeDose A numeric value of the time dose.
#' @return The object \code{Administration} with its new times vector for doses.
# ======================================================================================================

setGeneric("setTimeDose",
           function(object,timeDose)
           {
             standardGeneric("setTimeDose")
           })

setMethod(f="setTimeDose",
          signature="Administration",
          definition = function(object,timeDose)
          {
            object@timeDose = timeDose
            return(object)
          })

# ======================================================================================================
#' Get the amount of doses.
#'
#' @name getDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{amount_dose} giving the amount of doses.
# ======================================================================================================

setGeneric("getDose",
           function(object)
           {
             standardGeneric("getDose")
           })

setMethod(f="getDose",
          signature="Administration",
          definition = function(object)
          {
            return(object@dose)
          })

# ======================================================================================================
#' Set the amount of dose
#'
#' @name setDose
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param dose A numeric value of the amount of dose.
#' @return The numeric \code{amount_dose} giving the new value of the amount of dose.
# ======================================================================================================

setGeneric("setDose",
           function(object,dose)
           {
             standardGeneric("setDose")
           })

setMethod(f="setDose",
          signature="Administration",
          definition = function(object,dose)
          {
            object@dose = dose
            return(object)
          })

# ======================================================================================================
#' Get the infusion duration.
#'
#' @name getTinf
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{Tinf} giving the infusion duration Tinf.
# ======================================================================================================

setGeneric("getTinf",
           function(object)
           {
             standardGeneric("getTinf")
           })

setMethod(f="getTinf",
          signature="Administration",
          definition = function(object)
          {
            return(object@Tinf)
          })

# ======================================================================================================
#' Set the infusion duration.
#'
#' @name setTinf
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param Tinf A numeric value for the infusion duration Tinf.
#' @return The object \code{Administration} with its new value of the infusion duration Tinf.
# ======================================================================================================

setGeneric("setTinf",
           function(object,Tinf)
           {
             standardGeneric("setTinf")
           })

setMethod(f="setTinf",
          signature="Administration",
          definition = function(object,Tinf)
          {
            object@Tinf = Tinf
            return(object)
          })

# ======================================================================================================
#' Get the frequency \code{tau}.
#'
#' @name getTau
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @return The numeric \code{tau} giving the frequency \code{tau}.
# ======================================================================================================
setGeneric("getTau",
           function(object)
           {
             standardGeneric("getTau")
           })

setMethod(f="getTau",
          signature="Administration",
          definition = function(object)
          {
            return(object@tau)
          })

# ======================================================================================================
#' Set the frequency \code{tau}.
#'
#' @name setTau
#' @param object An object \code{Administration} from the class \linkS4class{Administration}.
#' @param tau A numeric value for the infusion lag tau.
#' @return The object \code{Administration} object with its new value of the infusion lag tau.
# ======================================================================================================

setGeneric("setTau",
           function(object,tau)
           {
             standardGeneric("setTau")
           })

setMethod(f="setTau",
          signature="Administration",
          definition = function(object,tau)
          {
            object@tau = tau
            return(object)
          })

##########################################################################################################
# END Class "Administration"
##########################################################################################################


