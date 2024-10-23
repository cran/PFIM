#' Class "PopulationFim"
#'
#' @description
#' A class storing information regarding the population Fisher matrix.
#' The class \code{PopulationFim} inherits from the class \code{Fim}.
#'
#' @name PopulationFim-class
#' @aliases PopulationFim
#' @docType class
#' @include Fim.R
#' @include GenericMethods.R
#' @export

PopulationFim = setClass(
  Class="PopulationFim",
  contains = "Fim"
)

# ======================================================================================================
# EvaluateFisherMatrix
# ======================================================================================================

#' @rdname EvaluateFisherMatrix
#' @export
#'
setMethod("EvaluateFisherMatrix",
          "PopulationFim",
          function( object, model, arm, modelEvaluation, modelVariance )
          {
            # =======================================================
            # name and fixed parameters
            # =======================================================

            parameters = getParameters( model )

            modelParametersName = getNames( parameters )

            fixedParameters = getFixedParameters( model )
            parameterfixedMu = fixedParameters$parameterfixedMu
            parameterfixedOmega = fixedParameters$parameterfixedOmega

            # =======================================================
            # variance for the FIM
            # =======================================================

            evaluateVarianceFIM = EvaluateVarianceFIM( object, model, arm, modelEvaluation, modelVariance )

            # =======================================================
            # components of the Fim
            # =======================================================

            MFVar = evaluateVarianceFIM$MFVar
            V = evaluateVarianceFIM$V

            outcomesAllGradient = as.matrix( modelEvaluation$outcomesAllGradient[,modelParametersName] )

            if ( length( parameterfixedMu ) != 0 )
            {
              outcomesAllGradient = outcomesAllGradient[, -c( parameterfixedMu ) ]
            }

            MFbeta = t( outcomesAllGradient ) %*% chol2inv(chol(V)) %*% outcomesAllGradient

            if ( length( parameterfixedOmega ) != 0 )
            {
              MFVar = MFVar[ -c( parameterfixedOmega ),-c( parameterfixedOmega ) ]
            }

            # =======================================================
            # Fisher Matrix
            # =======================================================

            fisherMatrix = as.matrix( bdiag( MFbeta, MFVar ) )

            # =======================================================
            # adjust Fisher matrix with the number of individuals
            # =======================================================

            fisherMatrix = fisherMatrix * getSize( arm )

            # =======================================================
            # set col and row names
            # =======================================================

            columnAndParametersNamesFIM = getColumnAndParametersNamesFIM( object, model )

            colnames( fisherMatrix ) = c( columnAndParametersNamesFIM$namesParametersMu,
                                          columnAndParametersNamesFIM$namesParametersOmega,
                                          columnAndParametersNamesFIM$namesParametersSigma )

            rownames( fisherMatrix ) = colnames( fisherMatrix )

            # =======================================================
            # set Fisher matrix, fixed effects and variance effects
            # =======================================================

            object = setFisherMatrix( object, fisherMatrix )

            return( object )
          })

# ======================================================================================================
# EvaluateVarianceFIM
# ======================================================================================================

#' function computeVMat
#' @name computeVMat
#' @param varParam1 varParam1
#' @param varParam2 varParam2
#' @param invCholV invCholV
#' @return VMat
#' @export

computeVMat = function( varParam1, varParam2, invCholV )
{
  1/2 * sum( diag( invCholV %*% varParam1 %*% invCholV %*% varParam2 ) )
}

#' @rdname EvaluateVarianceFIM
#' @export
#'
setMethod("EvaluateVarianceFIM",
          "PopulationFim",
          function( object, model, arm, modelEvaluation, modelVariance )
          {
            # =======================================================
            # matrix Omega
            # =======================================================

            omegaFIM = list()

            parameters = getParameters( model )
            numberOfParameters = getNumberOfParameters( model )
            modelParametersNames = getNames( parameters )

            for ( i in 1:length( parameters ) )
            {
              parameter = parameters[[i]]
              parameterName = getName( parameter )
              omega = getOmega( parameter )
              omegaFIM[[parameterName]] = omega**2
            }

            omegaFIM = diag( unlist( omegaFIM ), numberOfParameters, numberOfParameters )

            # =======================================================
            # responses gradient adjusted
            # =======================================================

            outcomesAllGradient = modelEvaluation$outcomesAllGradient

            adjustedGradient = data.frame( matrix ( 0.0, ncol = numberOfParameters, nrow = dim( outcomesAllGradient )[1] ) )
            colnames( adjustedGradient ) = modelParametersNames

            for ( parameter in parameters )
            {
              distribution = getDistribution( parameter )
              parameterName = getName( parameter )

              adjustedGradient[,parameterName] = getAdjustedGradient( distribution, outcomesAllGradient[,parameterName] )
            }

            adjustedGradient = as.matrix( adjustedGradient )

            # =======================================================
            # V matrix
            # =======================================================

            errorVariance = modelVariance$errorVariance

            V = adjustedGradient %*% omegaFIM %*% t( adjustedGradient ) + errorVariance

            # =======================================================
            # B Block
            # =======================================================

            dVdOmega = list()

            i = 1

            for ( parameter in parameters )
            {
              parameterName = getName( parameter )

              dOmega = matrix( 0, ncol = numberOfParameters, nrow = numberOfParameters )

              dOmega[ i, i ] = 1

              dVdOmega[[ parameterName ]] = adjustedGradient %*% dOmega %*% t( adjustedGradient  )

              i=i+1
            }

            sigmaDerivatives = modelVariance$sigmaDerivatives

            # =======================================================
            # for taking account number of model error
            # =======================================================

            sigmaDerivatives = lapply( sigmaDerivatives, function( x, n = dim( outcomesAllGradient )[1] ) x[1:n,1:n] )

            dVdLambda = c( dVdOmega, sigmaDerivatives )

            # =======================================================
            # VMat matrix
            # =======================================================

            invCholV = chol2inv( chol( V ) )

            VMat = outer( dVdLambda, dVdLambda, Vectorize( function(x, y) computeVMat( x, y, invCholV ) ) )

            MFVar = matrix( VMat, nrow = length( dVdLambda ), ncol = length( dVdLambda ) )

            MFVar = bdiag( MFVar )

            V = bdiag( V )

            return( list( V = V, MFVar = MFVar ) )

          })

# ======================================================================================================
# getRSE
# ======================================================================================================

#' @rdname getRSE
#' @export
#'
setMethod( "getRSE",
           signature = "PopulationFim",
           definition = function (object, model)
           {
             # parameter values
             parameters = getParameters( model )
             fixedParameters = getFixedParameters( model )
             modelParametersValues = getModelParametersValues( model )
             modelErrorParametersValues = getModelErrorParametersValues( model )

             mu = modelParametersValues$mu
             omega = modelParametersValues$omega**2

             # =======================================================
             # fixed mu and omega
             # =======================================================

             indexFixedMu = fixedParameters$parameterfixedMu
             indexFixedOmega = fixedParameters$parameterfixedOmega
             indexNoFixed = seq_along( parameters )

             if ( length( indexFixedMu ) != 0 )
             {
               indexNoFixedMu = indexNoFixed[ -c( indexFixedMu ) ]
               mu = mu[indexNoFixedMu]
             }

             if ( length( indexFixedOmega ) != 0 )
             {
               indexNoFixedOmega = indexNoFixed[ -c( indexFixedOmega ) ]
               omega = omega[indexNoFixedOmega]
             }

             parametersValues = c( mu, omega, modelErrorParametersValues )

             SE = getSE( object )
             RSE = SE/parametersValues*100

             return( list( RSE = RSE,
                           parametersValues = parametersValues ) )
           })

# ======================================================================================================
# getShrinkage
# ======================================================================================================

#' @rdname getShrinkage
#' @export
#'
setMethod( "getShrinkage",
           signature = "PopulationFim",
           definition = function (object)
           {
             return(NULL)
           })

# ======================================================================================================
# setShrinkage
# ======================================================================================================

#' @rdname setShrinkage
#' @export
#'
setMethod( "setShrinkage",
           signature = "PopulationFim",
           definition = function (object,value)
           {
             object@shrinkage = NA
             return(object)
           })

# ======================================================================================================
# getColumnAndParametersNamesFIM
# ======================================================================================================

#' @rdname getColumnAndParametersNamesFIM
#' @export
#'
setMethod( "getColumnAndParametersNamesFIM",
           signature = "PopulationFim",
           definition = function( object, model )
           {
             parameters = getParameters( model )
             modelParametersName = getNames( parameters )
             fixedParameters = getFixedParameters( model )
             parameterfixedMu = fixedParameters$parameterfixedMu
             parameterfixedOmega = fixedParameters$parameterfixedOmega

             modelError = getModelError( model )

             # ---------------------------------------------------
             # Greek letter for names
             # ---------------------------------------------------

             greeksLetter = c( mu = "\u03bc_",
                               omega = "\u03c9\u00B2_",
                               sigma = "\u03c3_",
                               sigmaInter = '\u03c3_inter',
                               sigmaSlope = '\u03c3_slope' )

             namesParametersMu = modelParametersName
             namesParametersOmega = modelParametersName

             # ---------------------------------------------------
             # mu and omega
             # ---------------------------------------------------

             if ( length( parameterfixedMu ) !=0 )
             {
               namesParametersMu = modelParametersName[ -c( parameterfixedMu ) ]
             }

             if ( length( parameterfixedOmega ) !=0 )
             {
               namesParametersOmega = modelParametersName[ -c( parameterfixedOmega ) ]
             }

             namesFIMFixedEffectsParameters = namesParametersMu
             namesFIMVarianceEffectsParameters = namesParametersOmega

             namesParametersMu = paste0( greeksLetter['mu'], namesParametersMu )
             namesParametersOmega = paste0( greeksLetter['omega'], namesParametersOmega )

             # ---------------------------------------------------
             # sigma
             # ---------------------------------------------------

             sigmaInterSlope = c()
             namesFIMModelErrorParameters = c()

             for ( modelErrorResponse in modelError )
             {
               outcomeName = getOutcome( modelErrorResponse )

               sigmaInterSlopeTmp = c( getSigmaInter( modelErrorResponse ), getSigmaSlope( modelErrorResponse ) )
               namesFIMModelErrorParameters = c( namesFIMModelErrorParameters , paste0( c( 'inter', 'slope' ), "_", outcomeName ) )
               sigmaInterSlopeNames = paste0( c( greeksLetter['sigmaInter'], greeksLetter['sigmaSlope'] ), "_", outcomeName )
               names( sigmaInterSlopeTmp ) = sigmaInterSlopeNames

               sigmaInterSlope = c( sigmaInterSlope, sigmaInterSlopeTmp )
             }

             namesParametersSigma = names( sigmaInterSlope[ sigmaInterSlope != 0 ] )
             namesFIMModelErrorParameters = namesFIMModelErrorParameters[ sigmaInterSlope != 0 ]

             # ---------------------------------------------------
             # names of the parameters
             # ---------------------------------------------------

             colnamesFIM = list(

               namesFIMFixedEffectsParameters = namesFIMFixedEffectsParameters,
               namesFIMVarianceEffectsParameters = namesFIMVarianceEffectsParameters,
               namesFIMModelErrorParameters = namesFIMModelErrorParameters,

               namesParametersMu = namesParametersMu,
               namesParametersOmega = namesParametersOmega,
               namesParametersSigma = namesParametersSigma )

             return( colnamesFIM )
           })

# ======================================================================================================
# getColumnAndParametersNamesFIMInLatex
# ======================================================================================================

#' @rdname getColumnAndParametersNamesFIMInLatex
#' @export
#'
setMethod( "getColumnAndParametersNamesFIMInLatex",
           signature = "PopulationFim",
           definition = function( object, model )
           {
             parameters = getParameters( model )
             modelParametersName = getNames( parameters )
             fixedParameters = getFixedParameters( model )
             parameterfixedMu = fixedParameters$parameterfixedMu
             parameterfixedOmega = fixedParameters$parameterfixedOmega

             modelError = getModelError( model )

             # =======================================================
             # Greek letter for names
             # =======================================================

             greeksLetter = c( mu = "\\mu_",
                               omega = "\\omega^2_",
                               sigma = "\\sigma_",
                               sigmaInter = '{\\sigma_{inter}}_',
                               sigmaSlope = '{\\sigma_{slope}}_' )

             mu = c()
             omega = c()
             sigma = c()

             namesParametersMu = modelParametersName
             namesParametersOmega = modelParametersName

             # =======================================================
             # mu and omega
             # =======================================================

             for ( parameter in parameters )
             {
               mu = c( mu, getMu( parameter ) )
               omega = c( omega, getOmega( parameter ) )
             }

             if ( length( parameterfixedMu ) !=0 )
             {
               mu = mu[ -c( parameterfixedMu ) ]
               namesParametersMu = modelParametersName[ -c( parameterfixedMu ) ]
             }

             if ( length( parameterfixedOmega ) !=0 )
             {
               omega = omega[ -c( parameterfixedOmega ) ]
               namesParametersOmega = modelParametersName[ -c( parameterfixedOmega ) ]
             }

             namesFIMFixedEffectsParameters = namesParametersMu
             namesFIMVarianceEffectsParameters = namesParametersOmega

             namesParametersMu = paste0( greeksLetter['mu'], "{",namesParametersMu , "}" )
             namesParametersOmega = paste0( greeksLetter['omega'], "{", namesParametersOmega , "}" )

             # =======================================================
             # sigma
             # =======================================================

             sigmaInterSlope = c()
             namesFIMModelErrorParameters = c()

             for ( modelErrorResponse in modelError )
             {
               outcomeName = getOutcome( modelErrorResponse )

               sigmaInterSlopeTmp = c( getSigmaInter( modelErrorResponse ), getSigmaSlope( modelErrorResponse ) )
               namesFIMModelErrorParameters = c( namesFIMModelErrorParameters , paste0( c( 'inter', 'slope' ), "{", outcomeName, "}" ) )
               sigmaInterSlopeNames = paste0( c( greeksLetter['sigmaInter'], greeksLetter['sigmaSlope'] ), "{", outcomeName, "}" )
               names( sigmaInterSlopeTmp ) = sigmaInterSlopeNames

               sigmaInterSlope = c( sigmaInterSlope, sigmaInterSlopeTmp )
             }

             namesParametersSigma = names( sigmaInterSlope[ sigmaInterSlope != 0 ] )

             namesParametersMu = paste0('$',namesParametersMu,"$")
             namesParametersOmega = paste0('$',namesParametersOmega,"$")
             namesParametersSigma = paste0('$',namesParametersSigma,"$")

             # =======================================================
             # names of the parameters
             # =======================================================

             colnamesFIM = list(

               namesParametersMu = namesParametersMu,
               namesParametersOmega = namesParametersOmega,
               namesParametersSigma = namesParametersSigma )

             return( colnamesFIM )
           })

# ======================================================================================================
# reportTablesFIM
# ======================================================================================================

#' @rdname reportTablesFIM
#' @export
#'
setMethod( "reportTablesFIM",
           signature = "PopulationFim",
           definition = function( object, evaluationObject )
           {
             model = getModel( evaluationObject )
             modelEquations = getEquations( model )
             modelOutcomes = getOutcomes( model )
             modelError = getModelError( model )
             modelParameters = getParameters( model )

             # =======================================================
             # get initial designs
             # =======================================================

             designs = getDesigns( evaluationObject )
             designNames = getNames( designs )
             designName = designNames[[1]]
             design = designs[[designName]]

             columnAndParametersNamesFIM = getColumnAndParametersNamesFIMInLatex( object, model )

             muAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersMu
             omegaAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersOmega
             sigmaAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersSigma

             # =======================================================
             # FIMFixedEffects
             # =======================================================

             FIMFixedEffects = getFixedEffects( object )
             FIMFixedEffects = as.matrix( FIMFixedEffects )
             colnames( FIMFixedEffects ) = muAndParameterNamesLatex
             rownames( FIMFixedEffects ) = muAndParameterNamesLatex

             # =======================================================
             # FIMVarianceEffects
             # =======================================================

             FIMVarianceEffects = getVarianceEffects( object )
             FIMVarianceEffects = as.matrix( FIMVarianceEffects )
             colnames( FIMVarianceEffects ) = c( omegaAndParameterNamesLatex, sigmaAndParameterNamesLatex )
             rownames( FIMVarianceEffects ) = c( omegaAndParameterNamesLatex, sigmaAndParameterNamesLatex )

             # =======================================================
             # correlation Matrix
             # =======================================================

             correlationMatrix = getCorrelationMatrix( object )

             correlationMatrixFixedEffects = as.matrix( correlationMatrix$fixedEffects )
             correlationMatrixVarianceEffects = as.matrix( correlationMatrix$varianceEffects )

             colnames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex
             rownames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex

             colnames( correlationMatrixVarianceEffects ) = c( omegaAndParameterNamesLatex, sigmaAndParameterNamesLatex )
             rownames( correlationMatrixVarianceEffects ) = c( omegaAndParameterNamesLatex, sigmaAndParameterNamesLatex )

             # =======================================================
             # SE and RSE
             # =======================================================

             fisherMatrix = getFisherMatrix( object )
             SE = getSE( object )

             rseAndParametersValues = getRSE( object, model )

             RSE = rseAndParametersValues$RSE
             parametersValues = rseAndParametersValues$parametersValues

             SE = round( SE, 3 )
             RSE = round( RSE, 3 )

             SEandRSE = data.frame( parametersValues, SE, RSE )
             colnames( SEandRSE ) = c("Value", "SE","RSE (%)" )
             rownames( SEandRSE ) = c( muAndParameterNamesLatex, omegaAndParameterNamesLatex, sigmaAndParameterNamesLatex )

             # =======================================================
             # determinants, condition numbers and Dcriterion
             # =======================================================

             detFim = getDeterminant( object )
             condFIMFixedEffects = getConditionNumberFixedEffects( object )
             condFIMVarianceEffects = getConditionNumberVarianceEffects( object )
             DCriterion = getDcriterion( object )

             # =======================================================
             # criteriaFim
             # =======================================================

             criteriaFim = t( data.frame( detFim, condFIMFixedEffects, condFIMVarianceEffects, DCriterion ) )

             colnames( criteriaFim ) = c("Value")
             rownames( criteriaFim ) = c("Determinant",
                                         "Cond number fixed effects",
                                         "Cond number variance components",
                                         "D-criterion")

             # =======================================================
             # kable tables
             # =======================================================

             # =======================================================
             # FIMFixedEffects
             # =======================================================

             FIMFixedEffectsTable = knitr::kable( FIMFixedEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # =======================================================
             # FIMVarianceEffectsTable
             # =======================================================

             FIMVarianceEffectsTable = knitr::kable( FIMVarianceEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # =======================================================
             # correlationMatrixFixedEffects
             # =======================================================

             correlationMatrixFixedEffectsTable = knitr::kable( correlationMatrixFixedEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # =======================================================
             # correlationMatrixVarianceEffects
             # =======================================================

             correlationMatrixVarianceEffectsTable = knitr::kable( correlationMatrixVarianceEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # =======================================================
             # criteriaFim
             # =======================================================

             rownames( criteriaFim ) = c("","Fixed effects","Variance effects","")
             colnames( criteriaFim ) = NULL

             criteriaFimTable = knitr::kable( t(criteriaFim) ) %>%
               kable_styling( font_size = 12,  position = "center",
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T) %>%
               add_header_above(c("Determinant" = 1, "Condition numbers" = 2, "D-criterion" = 1))

             # =======================================================
             # SEandRSE
             # =======================================================

             SEandRSETable = knitr::kable( SEandRSE ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             tablesPopulationFim = list( FIMFixedEffectsTable = FIMFixedEffectsTable,
                                         FIMVarianceEffectsTable = FIMVarianceEffectsTable,
                                         correlationMatrixFixedEffectsTable = correlationMatrixFixedEffectsTable,
                                         correlationMatrixVarianceEffectsTable = correlationMatrixVarianceEffectsTable,
                                         criteriaFimTable = criteriaFimTable,
                                         SEandRSETable = SEandRSETable )

             return( tablesPopulationFim )

           })

# ======================================================================================================
# generateReportEvaluation
# ======================================================================================================

#' @rdname generateReportEvaluation
#' @export
#'
setMethod( "generateReportEvaluation",
           signature = "PopulationFim",
           definition = function( object, evaluationObject, outputPath, outputFile, plotOptions )
           {
             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "templateEvaluationPopulationFim.rmd" )

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
# End class PopulationFim
##########################################################################################################

