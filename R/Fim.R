#' Class "Fim"
#'
#' @description
#' A class storing information regarding the Fisher matrix.
#' Type of the Fisher information: population ("PopulationFIM"), individual ("IndividualFIM") or Bayesian ("BayesianFIM").
#'
#' @name Fim-class
#' @aliases Fim
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Fim} can be created by calls of the form \code{Fim(...)} where
#' (...) are the parameters for the \code{Fim} objects.
#'
#'@section Slots for \code{Fim} objects:
#' \describe{
#' \item{\code{fisherMatrix}:}{A matrix giving the Fisher matrix.}
#' \item{\code{fixedEffects}:}{A matrix giving the fixed effects of the Fisher matrix.}
#' \item{\code{varianceEffects}:}{A matrix giving the variance effects of the Fisher matrix.}
#' \item{\code{shrinkage}:}{A vector giving the shrinkage value of the parameters.}
#' }

Fim = setClass(
  Class="Fim",
  representation=representation(
    fisherMatrix = "matrix",
    fixedEffects = "matrix",
    varianceEffects = "matrix",
    shrinkage = "vector"
  ))

# Initialize method
setMethod(
  f="initialize",
  signature="Fim",
  definition= function ( .Object, fisherMatrix, fixedEffects,  varianceEffects, shrinkage )
  {
    if(!missing(fisherMatrix))
    {
      .Object@fisherMatrix = fisherMatrix
    }

    if(!missing(fixedEffects))
    {
      .Object@fixedEffects = fixedEffects
    }

    if(!missing(varianceEffects))
    {
      .Object@varianceEffects = varianceEffects
    }

    if(!missing(shrinkage))
    {
      .Object@shrinkage = shrinkage
    }

    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
#' Evaluate the Fisher matrix ( population, individual and Bayesian )
#'
#' @name EvaluateFisherMatrix
#' @param object An object from the class \linkS4class{Fim}.
#' @param model An object from the class \linkS4class{Model}.
#' @param arm An object from the class \linkS4class{Arm}.
#' @param modelEvaluation A list containing the evaluation results.
#' @param modelVariance A list containing the model variance.
#' @return An object from the class \linkS4class{Fim} containing the Fisher matrix.
# ======================================================================================================

setGeneric("EvaluateFisherMatrix",
           function( object, model, arm, modelEvaluation, modelVariance )
           {
             standardGeneric( "EvaluateFisherMatrix" )
           })

# ======================================================================================================
#' Evaluate the variance of the Fisher information matrix.
#'
#' @name EvaluateVarianceFIM
#' @param object An object from the class \linkS4class{Fim}.
#' @param model An object from the class \linkS4class{Model}.
#' @param arm An object from the class \linkS4class{Arm}.
#' @param modelEvaluation A list containing the evaluation results.
#' @param modelVariance A list containing the model variance.
#' @return A list containing the matrices of the variance of the FIM.
# ======================================================================================================

setGeneric("EvaluateVarianceFIM",
           function( object, model, arm, modelEvaluation, modelVariance )
           {
             standardGeneric( "EvaluateVarianceFIM" )
           })

# ======================================================================================================
#' Get the FIM.
#'
#' @name getFisherMatrix
#' @param object An object from the class \linkS4class{Fim}.
#' @return A matrix giving the FIM.
# ======================================================================================================

setGeneric("getFisherMatrix",
           function(object)
           {
             standardGeneric("getFisherMatrix")
           })

setMethod("getFisherMatrix",
          "Fim",
          function(object)
          {
            return(object@fisherMatrix)
          })

# ======================================================================================================
#' Set the FIM.
#'
#' @name setFisherMatrix
#' @param object An object from the class \linkS4class{Fim}.
#' @param value A matrix giving the FIM.
#' @return The object from the class \linkS4class{Fim} with the FIM updated.
# ======================================================================================================

setGeneric("setFisherMatrix",
           function(object, value)
           {
             standardGeneric("setFisherMatrix")
           })

setMethod("setFisherMatrix",
          "Fim",
          function(object, value)
          {
            object@fisherMatrix = value
            return(object)
          })

# ======================================================================================================
#' Get the matrix of fixed effects.
#'
#' @name getFixedEffects
#' @param object An object from the class \linkS4class{Fim}.
#' @return The matrix of the fixed effects.
# ======================================================================================================

setGeneric("getFixedEffects",
           function(object)
           {
             standardGeneric("getFixedEffects")
           })

setMethod("getFixedEffects",
          "Fim",
          function(object)
          {
            fisherMatrix = getFisherMatrix( object )

            colnames( object@fixedEffects ) = colnames(fisherMatrix)[1:dim(object@fixedEffects)[1]]
            rownames( object@fixedEffects ) = colnames( object@fixedEffects )

            return( object@fixedEffects)
          })

# ======================================================================================================
#' Set the fixed effects.
#'
#' @name setFixedEffects
#' @param object An object from the class \linkS4class{Fim}.
#' @return Update the matrix of the fixed effects.
# ======================================================================================================

setGeneric("setFixedEffects",
           function(object)
           {
             standardGeneric("setFixedEffects")
           })

setMethod(
  "setFixedEffects",
  "Fim",
  definition = function ( object )
  {
    mu = "\u03bc_"
    fisherMatrix = getFisherMatrix( object )
    colnamesFim = colnames( fisherMatrix )
    indexMu = which( grepl( mu, colnamesFim ) == TRUE )
    object@fixedEffects = as.matrix(fisherMatrix[indexMu,indexMu])

    return( object )
  })

# ======================================================================================================
#' Get the matrix of the variance effects.
#'
#' @name getVarianceEffects
#' @param object An object from the class \linkS4class{Fim}.
#' @return The matrix of the variance effects.
# ======================================================================================================

setGeneric("getVarianceEffects",
           function(object)
           {
             standardGeneric("getVarianceEffects")
           })

setMethod("getVarianceEffects",
          "Fim",
          function(object)
          {
            return(object@varianceEffects)
          })

# ======================================================================================================
#' Set the matrix of the variance effects.
#'
#' @name setVarianceEffects
#' @param object An object from the class \linkS4class{Fim}.
#' @return Update the matrix of the variance effects.
# ======================================================================================================

setGeneric("setVarianceEffects",
           function(object)
           {
             standardGeneric("setVarianceEffects")
           })

setMethod(
  "setVarianceEffects",
  "Fim",
  definition = function ( object )
  {
    omega = "\u03c9\u00B2_"
    sigma = "\u03c3_"

    fisherMatrix = getFisherMatrix( object )
    colnamesFim = colnames( fisherMatrix )

    indexOmega = which( grepl( omega, colnamesFim ) == TRUE )
    indexSigma = which( grepl( sigma, colnamesFim ) == TRUE )

    indexOmegaSigma = c( indexOmega, indexSigma )

    if ( length( indexOmegaSigma ) !=0 )
    {
      # ==============================
      # population & individual fim
      # ==============================

      object@varianceEffects = as.matrix( fisherMatrix[ indexOmegaSigma, indexOmegaSigma ] )

    }else{
      # ==============================
      # Bayesian fim
      # ==============================

      object@varianceEffects = as.matrix(NA)
    }

    return( object )
  })

# ======================================================================================================
#' Get the determinant of the fim.
#'
#' @name getDeterminant
#' @param object An object from the class \linkS4class{Fim}.
#' @return A numeric giving the determinant of the fim.
# ======================================================================================================

setGeneric("getDeterminant",
           function(object)
           {
             standardGeneric("getDeterminant")
           })

setMethod(
  "getDeterminant",
  signature = "Fim",
  definition = function ( object )
  {
    fisherMatrix = getFisherMatrix( object )
    determinant = det( fisherMatrix )
    return(determinant)
  })

# ======================================================================================================
#' Get the D criterion of the fim.
#'
#' @name getDcriterion
#' @param object An object from the class \linkS4class{Fim}.
#' @return  A numeric giving the D criterion of the fim.
# ======================================================================================================

setGeneric("getDcriterion",
           function(object)
           {
             standardGeneric("getDcriterion")
           })

setMethod(
  "getDcriterion",
  signature = "Fim",
  definition = function(object)
  {
    fisherMatrix = getFisherMatrix( object )
    Dcriterion = det(fisherMatrix)**(1/dim(fisherMatrix)[1])
    return(Dcriterion)
  })

# ======================================================================================================
#' Get the correlation matrix.
#'
#' @name getCorrelationMatrix
#' @param object An object from the class \linkS4class{Fim}.
#' @return The correlation matrix of the fim.
# ======================================================================================================

setGeneric("getCorrelationMatrix",
           function(object)
           {
             standardGeneric("getCorrelationMatrix")
           })

setMethod(
  "getCorrelationMatrix",
  signature = "Fim",
  definition = function (object)
  {
    # ==============================
    # correlation Matrix
    # ==============================

    fisherMatrix = getFisherMatrix( object )
    colnamesFim = colnames( fisherMatrix )

    if ( rcond( fisherMatrix ) <= .Machine$double.eps )
    {
      correlationMatrix = cov2cor( pinv( fisherMatrix ) )
      colnames( correlationMatrix ) = colnames( fisherMatrix )
      rownames( correlationMatrix ) = rownames( fisherMatrix )
    }else{
      correlationMatrix = cov2cor(solve( fisherMatrix ) )
    }

    # ==============================
    # fixed effects
    # ==============================

    mu = "\u03bc_"
    indexMu = which( grepl( mu, colnamesFim ) == TRUE )
    fixedEffects = correlationMatrix[indexMu,indexMu]
    fixedEffects = as.matrix( fixedEffects )
    colnames( fixedEffects ) = colnamesFim[indexMu]
    rownames( fixedEffects ) = colnames( fixedEffects )

    # ==============================
    # variance effects
    # ==============================

    omega = "\u03c9\u00B2_"
    sigma = "\u03c3_"

    indexOmega = which( grepl( omega, colnamesFim ) == TRUE )
    indexSigma = which( grepl( sigma, colnamesFim ) == TRUE )

    indexOmegaSigma = c( indexOmega, indexSigma )

    if ( length( indexOmegaSigma ) !=0 )
    {
      varianceEffects = correlationMatrix[indexOmegaSigma,indexOmegaSigma]
    }else{
      varianceEffects = NULL
    }

    return( list( correlationMatrix = correlationMatrix, fixedEffects = fixedEffects, varianceEffects = varianceEffects ) )
  })

# ======================================================================================================
#' Get the SE.
#'
#' @name getSE
#' @param object An object from the class \linkS4class{Fim}.
#' @return A vector giving the SE.
# ======================================================================================================

setGeneric("getSE",
           function(object)
           {
             standardGeneric("getSE")
           })

setMethod(
  "getSE",
  signature = "Fim",
  definition = function (object)
  {
    fisherMatrix = getFisherMatrix( object )
    fisherMatrixTmp = fisherMatrix

    if ( rcond( fisherMatrixTmp ) <= .Machine$double.eps )
    {
      SE = sqrt(diag(pinv(fisherMatrixTmp)))
      names(SE) = colnames( fisherMatrixTmp )
    }else{
      SE = sqrt(diag(solve(fisherMatrix)))
    }

    return(SE)
  })

# ======================================================================================================
#' Get the RSE
#'
#' @name getRSE
#' @param object An object from the class \linkS4class{Fim}.
#' @param model An object from the class \linkS4class{Model}.
#' @return A vector giving the RSE.
# ======================================================================================================

setGeneric("getRSE",
           function(object, model)
           {
             standardGeneric("getRSE")
           })

# ======================================================================================================
#' Get the shrinkage.
#'
#' @name getShrinkage
#' @param object An object from the class \linkS4class{Fim}.
#' @return A vector giving the shrinkage of the Bayesian fim.
# ======================================================================================================

setGeneric("getShrinkage",
           function(object)
           {
             standardGeneric("getShrinkage")
           })

# ======================================================================================================
#' Set the shrinkage.
#'
#' @name setShrinkage
#' @param object An object from the class \linkS4class{Fim}.
#' @param value A vector giving the shrinkage of the Bayesian fim.
#' @return The object with the updated shrinkage.
# ======================================================================================================

setGeneric("setShrinkage",
           function(object, value)
           {
             standardGeneric("setShrinkage")
           })

# ======================================================================================================
#' Get the eigenvalues of the fim.
#'
#' @name getEigenValues
#' @param object An object from the class \linkS4class{Fim}.
#' @return A vector giving the eigenvalues of the fim.
# ======================================================================================================

setGeneric("getEigenValues",
           function(object)
           {
             standardGeneric("getEigenValues")
           })

setMethod(
  "getEigenValues",
  signature = "Fim",
  definition = function (object)
  {
    fisherMatrix = getFisherMatrix( object )
    eigenValues = eigen(fisherMatrix)$values
    return(eigenValues)
  })

# ======================================================================================================
#' Get the condition number of the matrix of the fixed effects.
#'
#' @name getConditionNumberFixedEffects
#' @param object An object from the class \linkS4class{Fim}.
#' @return A numeric giving the condition number of the matrix of the fixed effects.
# ======================================================================================================

setGeneric("getConditionNumberFixedEffects",
           function(object)
           {
             standardGeneric("getConditionNumberFixedEffects")
           })

setMethod(
  "getConditionNumberFixedEffects",
  signature = "Fim",
  definition = function (object)
  {
    fisherMatrix = getFixedEffects( object )
    conditionNumberFixedEffects = cond(fisherMatrix)
    return(conditionNumberFixedEffects)
  })

# ======================================================================================================
#' Get the condition number of the matrix of the variance effects.
#'
#' @name getConditionNumberVarianceEffects
#' @param object An object from the class \linkS4class{Fim}..
#' @return A numeric giving the condition number of the matrix of the variance effects.
# ======================================================================================================

setGeneric("getConditionNumberVarianceEffects",
           function(object)
           {
             standardGeneric("getConditionNumberVarianceEffects")
           })

setMethod(
  "getConditionNumberVarianceEffects",
  signature = "Fim",
  definition = function (object)
  {
    varianceEffects = getVarianceEffects( object )
    conditionNumberVarianceEffects = cond( varianceEffects )
    return( conditionNumberVarianceEffects )
  })

# ======================================================================================================
#' Get the names of the names of the parameters associated to each column of the fim.
#'
#' @name getColumnAndParametersNamesFIM
#' @param object An object from the class \linkS4class{Fim}.
#' @param model An object from the class \linkS4class{Model}.
#' @return A list giving the names of the parameters associated to each column of the fim.
# ======================================================================================================

setGeneric("getColumnAndParametersNamesFIM",
           function(object, model )
           {
             standardGeneric("getColumnAndParametersNamesFIM")
           })

# ======================================================================================================
#' #' Get the names of the names of the parameters associated to each column of the fim in Latex format.
#'
#' @name getColumnAndParametersNamesFIMInLatex
#' @param object An object from the class \linkS4class{Fim}.
#' @param model An object from the class \linkS4class{Model}.
#' @return A list giving the names of the parameters associated to each column of the fim in Latex format.
# ======================================================================================================

setGeneric("getColumnAndParametersNamesFIMInLatex",
           function(object, model )
           {
             standardGeneric("getColumnAndParametersNamesFIMInLatex")
           })


# ======================================================================================================
#' Generate the tables for the report.
#'
#' @name reportTablesFIM
#' @param object An object from the class \linkS4class{Fim}.
#' @param evaluationObject A list giving the results of the evaluation of the model.
#' @return A list giving the table in kable format for the report.
# ======================================================================================================

setGeneric("reportTablesFIM",
           function( object, evaluationObject )
           {
             standardGeneric("reportTablesFIM")
           })

# ======================================================================================================
#' Generate the report for the evaluation
#'
#' @name generateReportEvaluation
#' @param object An object from the class \linkS4class{Fim}.
#' @param evaluationObject A list giving the results of the evaluation of the model.
#' @param outputPath A string giving the output path.
#' @param outputFile A string giving the name of the output file.
#' @param plotOptions A list giving the plot options.
#' @return Return the report for the evaluation in html.
# ======================================================================================================

setGeneric("generateReportEvaluation",
           function( object, evaluationObject, outputPath, outputFile, plotOptions )
           {
             standardGeneric("generateReportEvaluation")
           })

# ======================================================================================================
#' Convert the type of the object fim to a string.
#'
#' @name setFimTypeToString
#' @param object An object from the class \linkS4class{Fim}.
#' @return The type of the object fim convert as a string.
# ======================================================================================================

setGeneric("setFimTypeToString",
           function( object )
           {
             standardGeneric("setFimTypeToString")
           })

setMethod(
  "setFimTypeToString",
  signature = "Fim",
  definition = function( object )
  {
    if ( is( object, "PopulationFim" ) )
    {
      object = "population"
    }
    else if ( is( object, "IndividualFim" ) )
    {
      object = "individual"
    }
    else if ( is( object, "BayesianFim" ) )
    {
      object = "Bayesian"
    }
    return( object )
  })

##########################################################################################################
# End Class "Fim"
##########################################################################################################
