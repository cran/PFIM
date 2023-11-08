#' Class "BayesianFim"
#'
#' @description The class \code{BayesianFim} represents the population Fisher information matrix.
#' The class \code{BayesianFim} inherits from the class \code{Fim}.
#'
#' @name BayesianFim-class
#' @aliases BayesianFim
#' @docType class
#' @include Fim.R
#' @include GenericMethods.R
#' @export

BayesianFim = setClass(
  Class="BayesianFim",
  contains = "Fim" )

# ======================================================================================================
# EvaluateFisherMatrix
# ======================================================================================================

setMethod("EvaluateFisherMatrix",
          "BayesianFim",
          function
          ( object, model, arm, modelEvaluation, modelVariance )
          {
            # =====================================
            # fixed parameters
            # =====================================

            parameters = getParameters( model )

            modelParametersName = getNames( parameters )

            fixedParameters = getFixedParameters( model )

            indexfixedMu = fixedParameters$parameterfixedMu
            indexfixedOmega = fixedParameters$parameterfixedOmega

            indexFixed = unique( c( indexfixedMu, indexfixedOmega ) )

            # =====================================
            # variance for the FIM
            # =====================================

            evaluateVarianceFIM = EvaluateVarianceFIM( IndividualFim(), model, arm, modelEvaluation, modelVariance )

            # =====================================
            # elements for the Bayesian fim
            # =====================================

            outcomesAllGradient = as.matrix( modelEvaluation$outcomesAllGradient[,modelParametersName] )

            V = evaluateVarianceFIM$V
            MFbeta = t( outcomesAllGradient ) %*% chol2inv(chol(V)) %*% outcomesAllGradient

            mu = c()
            omega = c()

            for ( parameter in parameters )
            {
              tmp = getOmega( parameter )
              omega = c( omega, tmp**2 )

              distribution = getDistribution( parameter )

              if ( is( distribution, "LogNormal" ) )
              {
                tmp = getMu( parameter )
                mu = c( mu, tmp )
              }
              else if ( is( distribution, "Normal" ) )
              {
                mu = c( mu, 1 )
              }
            }

            # =====================================
            # remove fixed parameters
            # =====================================

            if ( length( indexFixed ) != 0 )
            {
              mu = mu[-c(indexFixed)]
              omega = omega[-c(indexFixed)]
              MFbeta = MFbeta[-c(indexFixed),-c(indexFixed)]
            }

            # =====================================
            # compute MFBeta
            # =====================================

            if ( length( mu ) == 1)
            {
              mu = as.matrix(mu)
            }else{
              mu = diag( mu )
            }

            if ( length( omega ) == 1)
            {
              omega = as.matrix(omega)
            }else{
              omega = diag( omega )
            }

            MFbeta = t( mu ) %*% MFbeta %*% mu + solve( mu %*% omega %*% mu )

            # =====================================
            # Fisher Matrix
            # =====================================

            fisherMatrix = as.matrix( MFbeta )

            # set col and row names
            columnAndParametersNamesFIM = getColumnAndParametersNamesFIM( object, model )

            colnames( fisherMatrix ) = c( columnAndParametersNamesFIM$namesParametersMu )
            rownames( fisherMatrix ) = colnames( fisherMatrix )

            object = setFisherMatrix( object, fisherMatrix )

            # =====================================
            # shrinkage
            # =====================================

            shrinkage = diag( chol2inv( chol( fisherMatrix ) )%*% chol2inv( chol( mu %*% omega %*% mu ) ) ) * 100
            object = setShrinkage( object, shrinkage )

            return( object )

          })

# ======================================================================================================
# getRSE
# ======================================================================================================

setMethod(
  "getRSE",
  signature = "BayesianFim",
  definition = function (object, model )
  {
    # parameter values
    parameters = getParameters( model )
    modelParametersValues = getModelParametersValues( model )
    fixedParameters = getFixedParameters( model )

    indexfixedMu = fixedParameters$parameterfixedMu
    indexfixedOmega = fixedParameters$parameterfixedOmega
    indexFixed = unique( c( indexfixedMu,indexfixedOmega ) )
    indexNoFixed = seq_along( parameters )

    mu =  modelParametersValues$mu

    if ( length( indexFixed ) != 0 )
    {
      indexNoFixed = indexNoFixed[ -c( indexFixed ) ]
      mu = mu[indexNoFixed]
    }

    SE = getSE( object )
    RSE = SE/mu*100

    return( list( RSE = RSE,
                  parametersValues = mu ) )
  })

# ======================================================================================================
# getConditionNumberVarianceEffects
# ======================================================================================================

setMethod(
  "getConditionNumberVarianceEffects",
  signature = "BayesianFim",
  definition = function (object)
  {
    return( NA )
  })

# ======================================================================================================
# getShrinkage
# ======================================================================================================

setMethod(
  "getShrinkage",
  signature = "BayesianFim",
  definition = function (object)
  {
    return(object@shrinkage)
  })

# ======================================================================================================
# setShrinkage
# ======================================================================================================

setMethod(
  "setShrinkage",
  signature = "BayesianFim",
  definition = function (object,value)
  {
    object@shrinkage = value
    return(object)
  })

# ======================================================================================================
# getColumnAndParametersNamesFIM
# ======================================================================================================

setMethod(
  "getColumnAndParametersNamesFIM",
  signature = "BayesianFim",
  definition = function( object, model )
  {
    # =====================================
    # model parameters
    # =====================================

    parameters = getParameters( model )
    modelParametersName = getNames( parameters )
    fixedParameters = getFixedParameters( model )
    parameterfixedMu = fixedParameters$parameterfixedMu
    parameterfixedOmega = fixedParameters$parameterfixedOmega
    indexFixed = unique( c( parameterfixedMu, parameterfixedOmega ) )

    # =====================================
    # Greek letter for names
    # =====================================

    greeksLetter = c( mu = "\u03bc_" )

    # =====================================
    # names of the parameters
    # =====================================

    namesParametersMu = modelParametersName

    if ( length( indexFixed ) !=0 )
    {
      namesParametersMu = modelParametersName[ -c( indexFixed ) ]
    }

    namesFIMFixedEffectsParameters = namesParametersMu
    namesParametersMu = paste0( greeksLetter['mu'], namesParametersMu )

    colnamesFIM = list( namesFIMFixedEffectsParameters = namesFIMFixedEffectsParameters,
                        namesParametersMu = namesParametersMu )

    return( colnamesFIM )
  })

# ======================================================================================================
# getColumnAndParametersNamesFIMInLatex
# ======================================================================================================

setMethod(
  "getColumnAndParametersNamesFIMInLatex",
  signature = "BayesianFim",
  definition = function( object, model )
  {
    # =====================================
    # model parameters
    # =====================================

    parameters = getParameters( model )
    modelParametersName = getNames( parameters )
    fixedParameters = getFixedParameters( model )
    parameterfixedMu = fixedParameters$parameterfixedMu
    parameterfixedOmega = fixedParameters$parameterfixedOmega
    indexFixed = unique( c( parameterfixedMu, parameterfixedOmega ) )

    # =====================================
    # Greek letter for names
    # =====================================

    greeksLetter = c( mu = "\\mu_")

    # =====================================
    # names of the parameters
    # =====================================

    namesParametersMu = modelParametersName

    if ( length( indexFixed ) !=0 )
    {
      namesParametersMu = modelParametersName[ -c( indexFixed ) ]
    }

    namesParametersMu = paste0( greeksLetter['mu'], "{",namesParametersMu , "}" )
    namesParametersMu = paste0('$',namesParametersMu,"$")

    # =====================================
    # names of the parameters
    # =====================================

    colnamesFIM = list( namesParametersMu = namesParametersMu )

    return( colnamesFIM )
  })

# ======================================================================================================
# reportTablesFIM
# ======================================================================================================

setMethod(
  "reportTablesFIM",
  signature = "BayesianFim",
  definition = function( object, evaluationObject )
  {
    model = getModel( evaluationObject )
    modelEquations = getEquations( model )
    modelOutcomes = getOutcomes( model )
    modelError = getModelError( model )
    modelParameters = getParameters( model )

    # =====================================
    # get initial designs
    # =====================================

    designs = getDesigns( evaluationObject )
    designNames = getNames( designs )
    designName = designNames[[1]]
    design = designs[[designName]]

    columnAndParametersNamesFIM = getColumnAndParametersNamesFIMInLatex( object, model )
    muAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersMu

    # =====================================
    # FIMFixedEffects
    # =====================================

    FIMFixedEffects = getFixedEffects( object )
    FIMFixedEffects = as.matrix( FIMFixedEffects )

    colnames( FIMFixedEffects ) = muAndParameterNamesLatex
    rownames( FIMFixedEffects ) = muAndParameterNamesLatex

    # =====================================
    # correlation Matrix
    # =====================================

    correlationMatrix = getCorrelationMatrix( object )

    correlationMatrixFixedEffects = correlationMatrix$fixedEffects
    correlationMatrixFixedEffects = as.matrix( correlationMatrixFixedEffects )

    colnames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex
    rownames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex

    # =====================================
    # shrinkage
    # =====================================

    shrinkage = getShrinkage( object )
    names( shrinkage ) = colnames( FIMFixedEffects )

    # =====================================
    # SE and RSE
    # =====================================

    fisherMatrix = getFisherMatrix( object )
    SE = getSE( object )

    rseAndParametersValues = getRSE( object, model )
    RSE = rseAndParametersValues$RSE
    parametersValues = rseAndParametersValues$parametersValues

    SE = round( SE, 3 )
    RSE = round( RSE, 3 )

    SEandRSE = data.frame( parametersValues, SE, RSE, shrinkage )
    colnames( SEandRSE ) = c("Value", "SE","RSE (%)", "shrinkage" )
    rownames( SEandRSE ) = c( muAndParameterNamesLatex )

    # ==============================================
    # determinants, condition numbers and Dcriterion
    # ==============================================

    detFim = getDeterminant( object )
    condFIMFixedEffects = getConditionNumberFixedEffects( object )
    DCriterion = getDcriterion( object )

    # =====================================
    # criteriaFim
    # =====================================

    criteriaFim = t( data.frame( detFim, condFIMFixedEffects, DCriterion ) )

    colnames( criteriaFim ) = c("Value")
    rownames( criteriaFim ) = c("Determinant",
                                "Cond number fixed effects",
                                "D-criterion")

    # =====================================
    # kable tables
    # =====================================

    # =====================================
    # FIMFixedEffects
    # =====================================

    FIMFixedEffectsTable = knitr::kable( FIMFixedEffects ) %>%
      kable_styling( font_size = 12,
                     latex_options = c("hold_position","striped", "condensed", "bordered" ),
                     full_width = T)

    # =====================================
    # correlationMatrixFixedEffects
    # =====================================

    correlationMatrixFixedEffectsTable = knitr::kable( correlationMatrixFixedEffects ) %>%
      kable_styling( font_size = 12,
                     latex_options = c("hold_position","striped", "condensed", "bordered" ),
                     full_width = T)

    # =====================================
    # criteriaFim
    # =====================================

    rownames( criteriaFim ) = c("","Fixed effects","")
    colnames( criteriaFim ) = NULL

    criteriaFimTable = knitr::kable( t(criteriaFim) ) %>%
      kable_styling( font_size = 12,
                     latex_options = c("hold_position","striped", "condensed", "bordered" ),
                     full_width = T) %>%
      add_header_above(c("Determinant" = 1, "Condition numbers" = 1, "D-criterion" = 1))

    # =====================================
    # SEandRSE
    # =====================================

    SEandRSETable = knitr::kable( SEandRSE ) %>%
      kable_styling( font_size = 12,
                     latex_options = c("hold_position","striped", "condensed", "bordered" ),
                     full_width = T)

    tablesBayesianFim = list( FIMFixedEffectsTable = FIMFixedEffectsTable,
                              correlationMatrixFixedEffectsTable = correlationMatrixFixedEffectsTable,
                              criteriaFimTable = criteriaFimTable,
                              SEandRSETable = SEandRSETable )

    return( tablesBayesianFim )

  })

# ======================================================================================================
# generateReportEvaluation
# ======================================================================================================

setMethod(
  "generateReportEvaluation",
  signature = "BayesianFim",
  definition = function( object, evaluationObject, outputPath, outputFile, plotOptions )
  {
    path = system.file(package = "PFIM")
    path = paste0( path, "/rmarkdown/templates/skeleton/" )
    nameInputFile = paste0( path, "templateEvaluationBayesianFim.rmd" )

    projectName = getName( evaluationObject )

    tablesEvaluationFIMIntialDesignResults = generateTables( evaluationObject, plotOptions )

    rmarkdown::render( input = nameInputFile,
                       output_file = outputFile,
                       output_dir = outputPath,
                       params = list(
                         plotOptions = "plotOptions",
                         projectName = "projectName",
                         tablesEvaluationFIMIntialDesignResults = "tablesEvaluationFIMIntialDesignResults" ) )
  })

##########################################################################################################
# End class BayesianFim
##########################################################################################################
