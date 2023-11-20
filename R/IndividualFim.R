#' Class "Fim"
#'
#' @description
#' A class storing information regarding the individual Fisher matrix.
#' The class \code{IndividualFim} inherits from the class \code{Fim}.
#'
#' @name IndividualFim-class
#' @aliases IndividualFim
#' @docType class
#' @include Fim.R
#' @include GenericMethods.R
#' @export

IndividualFim = setClass(
  Class="IndividualFim",
  contains = "Fim"
)

# ======================================================================================================
# EvaluateFisherMatrix
# ======================================================================================================

#' @rdname EvaluateFisherMatrix
#' @export

setMethod("EvaluateFisherMatrix",
          "IndividualFim",
          function( object, model, arm, modelEvaluation, modelVariance )
          {
            # ==============================
            # fixed parameters
            # ==============================

            parameters = getParameters( model )

            modelParametersName = getNames( parameters )

            fixedParameters = getFixedParameters( model )

            parameterfixedMu = fixedParameters$parameterfixedMu
            parameterfixedOmega = fixedParameters$parameterfixedOmega

            # ==============================
            # variance for the FIM
            # ==============================

            evaluateVarianceFIM = EvaluateVarianceFIM( object, model, arm, modelEvaluation, modelVariance )

            # ==============================
            # components of the FIM
            # ==============================

            MFVar = evaluateVarianceFIM$MFVar
            V = evaluateVarianceFIM$V

            outcomesAllGradient = as.matrix( modelEvaluation$outcomesAllGradient[,modelParametersName] )

            if ( length( parameterfixedMu ) !=0 )
            {
              outcomesAllGradient = outcomesAllGradient[, -c( parameterfixedMu )  ]
            }

            MFbeta =  t( outcomesAllGradient ) %*% chol2inv(chol(V)) %*% outcomesAllGradient

            # ==============================
            # Fisher Matrix
            # ==============================

            fisherMatrix = as.matrix( bdiag( MFbeta, MFVar ) )

            # set col and row names
            columnAndParametersNamesFIM = getColumnAndParametersNamesFIM( object, model )

            colnames( fisherMatrix ) = c( columnAndParametersNamesFIM$namesParametersMu,
                                          columnAndParametersNamesFIM$namesParametersSigma )

            rownames( fisherMatrix ) = colnames( fisherMatrix )

            object = setFisherMatrix( object, fisherMatrix )

            return( object )
          })

# ======================================================================================================
# EvaluateVarianceFIM
# ======================================================================================================

#' @rdname EvaluateVarianceFIM
#' @export

setMethod("EvaluateVarianceFIM",
          "IndividualFim",
          function( object, model, arm, modelEvaluation, modelVariance )
          {
            VMat = c()

            sigmaDerivatives = modelVariance$sigmaDerivatives
            outcomesAllGradient = modelEvaluation$outcomesAllGradient
            sigmaDerivatives = lapply( sigmaDerivatives, function( x, n=dim( outcomesAllGradient )[1] ) x[1:n,1:n] )

            V = as.matrix( modelVariance$errorVariance )

            for ( sigmaDerivatives1 in sigmaDerivatives )
            {
              for ( sigmaDerivatives2 in sigmaDerivatives )
              {
                VMat = c( VMat, 1/2 * ( sum( diag( chol2inv(chol(V)) %*% sigmaDerivatives1 %*% chol2inv(chol(V)) %*% sigmaDerivatives2 ) ) ) )
              }
            }

            MFVar = matrix( VMat, nrow = length( sigmaDerivatives ), ncol = length( sigmaDerivatives ) )

            MFVar = as.matrix( bdiag( MFVar ) )

            V = as.matrix( bdiag( V ) )

            return( list( V = V, MFVar = MFVar ) )

          })

# ======================================================================================================
# getRSE
# ======================================================================================================

#' @rdname getRSE
#' @export

setMethod( "getRSE",
           signature = "IndividualFim",
           definition = function (object, model)
           {
             # parameter values
             parameters = getParameters( model )
             fixedParameters = getFixedParameters( model )
             modelParametersValues = getModelParametersValues( model )
             modelErrorParametersValues = getModelErrorParametersValues( model )

             mu =  modelParametersValues$mu
             indexFixedMu = fixedParameters$parameterfixedMu

             if ( length( indexFixedMu ) != 0 )
             {
               for ( parameter in parameters )
               {
                 mu = c( mu, getMu( parameter ) )
               }

               indexNoFixedMu = seq_along( parameters )
               indexNoFixedMu = indexNoFixedMu[ -c( indexFixedMu ) ]
               mu = mu[indexNoFixedMu]
             }

             parametersValues = c( mu, modelErrorParametersValues )

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

setMethod( "getShrinkage",
           signature = "IndividualFim",
           definition = function (object)
           {
             return(NULL)
           })

#' @rdname setShrinkage
#' @export

setMethod( "setShrinkage",
           signature = "IndividualFim",
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

setMethod( "getColumnAndParametersNamesFIM",
           signature = "IndividualFim",
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

             # ==============================
             # sigma
             # ==============================

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

             # ==============================
             # names of the parameters
             # ==============================

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

setMethod( "getColumnAndParametersNamesFIMInLatex",
           signature = "IndividualFim",
           definition = function( object, model )
           {
             parameters = getParameters( model )
             modelParametersName = getNames( parameters )
             fixedParameters = getFixedParameters( model )
             parameterfixedMu = fixedParameters$parameterfixedMu
             parameterfixedOmega = fixedParameters$parameterfixedOmega

             modelError = getModelError( model )

             # ==============================
             # Greek letter for names
             # ==============================

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

             # ==============================
             # mu and omega
             # ==============================

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

             # ==============================
             # sigma
             # ==============================

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

             # ==============================
             # names of the parameters
             # ==============================

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

setMethod( "reportTablesFIM",
           signature = "IndividualFim",
           definition = function( object, evaluationObject )
           {
             model = getModel( evaluationObject )
             modelEquations = getEquations( model )
             modelOutcomes = getOutcomes( model )
             modelError = getModelError( model )
             modelParameters = getParameters( model )

             # ==============================
             # get initial designs
             # ==============================

             designs = getDesigns( evaluationObject )
             designNames = getNames( designs )
             designName = designNames[[1]]
             design = designs[[designName]]

             columnAndParametersNamesFIM = getColumnAndParametersNamesFIMInLatex( object, model )
             muAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersMu
             omegaAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersOmega
             sigmaAndParameterNamesLatex = columnAndParametersNamesFIM$namesParametersSigma

             # ==============================
             # FIMFixedEffects
             # ==============================

             FIMFixedEffects = getFixedEffects( object )
             FIMFixedEffects = as.matrix( FIMFixedEffects )

             colnames( FIMFixedEffects ) = muAndParameterNamesLatex
             rownames( FIMFixedEffects ) = muAndParameterNamesLatex

             # ==============================
             # correlation Matrix
             # ==============================

             correlationMatrix = getCorrelationMatrix( object )

             correlationMatrixFixedEffects = correlationMatrix$fixedEffects
             correlationMatrixFixedEffects = as.matrix( correlationMatrixFixedEffects )

             colnames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex
             rownames( correlationMatrixFixedEffects ) = muAndParameterNamesLatex

             # ==============================
             # SE and RSE
             # ==============================

             fisherMatrix = getFisherMatrix( object )
             SE = getSE( object )

             rseAndParametersValues = getRSE( object, model )
             RSE = rseAndParametersValues$RSE
             parametersValues = rseAndParametersValues$parametersValues

             SE = round( SE, 3 )
             RSE = round( RSE, 3 )

             SEandRSE = data.frame( parametersValues, SE, RSE )
             colnames( SEandRSE ) = c("Value", "SE","RSE (%)" )
             rownames( SEandRSE ) = c( muAndParameterNamesLatex, sigmaAndParameterNamesLatex )

             # ==============================================
             # determinants, condition numbers and Dcriterion
             # ==============================================

             detFim = getDeterminant( object )
             condFIMFixedEffects = getConditionNumberFixedEffects( object )
             DCriterion = getDcriterion( object )

             # ==============================================
             # criteriaFim
             # ==============================================

             criteriaFim = t( data.frame( detFim, condFIMFixedEffects, DCriterion ) )

             colnames( criteriaFim ) = c("Value")
             rownames( criteriaFim ) = c("Determinant",
                                         "Cond number fixed effects",
                                         "D-criterion")

             # ==============================================
             # kable tables
             # ==============================================

             # ==============================================
             # FIMFixedEffects
             # ==============================================

             FIMFixedEffectsTable = knitr::kable( FIMFixedEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # ==============================================
             # correlationMatrixFixedEffects
             # ==============================================

             correlationMatrixFixedEffectsTable = knitr::kable( correlationMatrixFixedEffects ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             # ==============================================
             # criteriaFim
             # ==============================================

             rownames( criteriaFim ) = c("","Fixed effects","")
             colnames( criteriaFim ) = NULL

             criteriaFimTable = knitr::kable( t(criteriaFim) ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T) %>%
               add_header_above(c("Determinant" = 1, "Condition numbers" = 1, "D-criterion" = 1))

             # ==============================================
             # SEandRSE
             # ==============================================

             SEandRSETable = knitr::kable( SEandRSE ) %>%
               kable_styling( font_size = 12,
                              latex_options = c("hold_position","striped", "condensed", "bordered" ),
                              full_width = T)

             tablesIndividualFim = list( FIMFixedEffectsTable = FIMFixedEffectsTable,
                                         correlationMatrixFixedEffectsTable = correlationMatrixFixedEffectsTable,
                                         criteriaFimTable = criteriaFimTable,
                                         SEandRSETable = SEandRSETable )

             return( tablesIndividualFim )

           })

# ======================================================================================================
# generateReportEvaluation
# ======================================================================================================

#' @rdname generateReportEvaluation
#' @export

setMethod( "generateReportEvaluation",
           signature = "IndividualFim",
           definition = function( object, evaluationObject, outputPath, outputFile, plotOptions )
           {
             path = system.file(package = "PFIM")
             path = paste0( path, "/rmarkdown/templates/skeleton/" )
             nameInputFile = paste0( path, "templateEvaluationIndividualFim.rmd" )

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
# End class IndividualFim
##########################################################################################################
