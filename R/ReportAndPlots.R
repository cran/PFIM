##################################################################################
##' Class "ReportAndPlots"
##'
##' @description
##' The class \code{ReportAndPlots} defines the htmal reports for the evaluation and the optimization.
##'
##' @name ReportAndPlots-class
##' @aliases ReportAndPlots
##' @include PFIMProject.R
##' @docType class
##' @exportClass ReportAndPlots
##'
##' @section Objects from the class: \code{ReportAndPlots} objects are typically created by calls to \code{{ReportAndPlots}} and contain the following slots:
##'
##' \describe{
##' \item{\code{object}:}{An object from the class \code{ReportAndPlots}}
##' }
##################################################################################

ReportAndPlots<-setClass(
  Class = "ReportAndPlots",
  contains = "PFIMProject"
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrModelEquations.
#'
#' @name knitrModelEquations
#' @param object An object \code{knitrModelEquations} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrModelEquations.

setGeneric(
  "knitrModelEquations",
  function( object ) {
    standardGeneric("knitrModelEquations")
  })

setMethod(f="knitrModelEquations",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            modelEquations = getEquationsStatisticalModel( statisticalModel )
            responses = getResponsesStatisticalModel( statisticalModel )
            nameModelEquations = "Model equations:"

            # case repeated and multi doses
            designs = getDesign( object )

            numberOfFirstDose = 0

            for ( design in designs )
            {
              arms = getArms( design )

              for ( arm in arms )
              {
                admins = getAdministration( arm )

                for ( admin in admins )
                {
                  tau = getTau( admin )
                  timeDose = getTimeDose( admin )

                  if ( tau > 0 | length( timeDose ) > 1 )
                  {
                    numberOfFirstDose = numberOfFirstDose + 1
                  }
                }
              }
            }

            if ( numberOfFirstDose == 1 )
            {
              nameModelEquations = "Model equations (for the first dose):"
            } else if ( numberOfFirstDose == 2 )
            {
              nameModelEquations = "Model equations (for the first doses):"
            }

            # knitr model equations
            if( class( modelEquations ) %in% c("ModelEquations") )
            {
              cat( nameModelEquations, "\n\n" )

              for( response in responses )
              {
                nameResponse = getNameResponse( response )
                equation = getEquation( modelEquations, nameResponse )

                cat( paste0(nameResponse ," = ", equation,"\n" ) )
              }
            }

            # ModelInfusionEquations
            else if( class( modelEquations ) %in% c( "ModelInfusionEquations" ) )
            {
              namesResponses = names( responses )
              equations = getEquations( modelEquations )

              cat( nameModelEquations, "\n\n" )

              for (nameResponse in namesResponses )
              {
                nameEquationsDuringInfusion = paste0( c( "DuringInfusion_" ), nameResponse )
                nameEquationsAfterInfusion = paste0( c( "AfterInfusion_" ), nameResponse )

                cat( paste0( nameEquationsDuringInfusion ," = ", equations[[nameEquationsDuringInfusion]],"\n" ) )
                cat( paste0( nameEquationsAfterInfusion ," = ", equations[[nameEquationsAfterInfusion]],"\n" ) )
              }
            }

            # ModelODEquations
            else if( class(modelEquations) %in% c("ModelODEquations" ))
            {
              derivativeEqs <- getDerivatives( modelEquations )

              cat("Model responses:","\n\n" )

              for( response in responses)
              {
                nameResponse = getNameResponse( response )

                equation = getEquation( modelEquations, nameResponse )

                cat( paste0( nameResponse ," = ", equation,"\n" ) )

              }

              cat("\n")
              cat( nameModelEquations, "\n\n" )

              for(i in 1:length(derivativeEqs))
              {
                cat( paste0( names(derivativeEqs)[i]," = ",derivativeEqs[[i]] ),"\n"  )
              }
            }

            # ModelInfusionODEquations
            else if(class(modelEquations) %in% c("ModelInfusionODEquations"))
            {
              responses = getResponsesStatisticalModel(statisticalModel)
              namesResponses = names( responses )

              cat("Model responses:","\n\n" )

              equationsDuringInfusion = modelEquations@duringInfusionResponsesEquations
              equationsAfterInfusion = modelEquations@afterInfusionResponsesEquations

              for (nameResponse in namesResponses )
              {
                nameEquationsDuringInfusion = paste0( c( "DuringInfusion_" ), nameResponse )
                nameEquationsAfterInfusion = paste0( c( "AfterInfusion_" ), nameResponse )

                cat( paste0( nameEquationsDuringInfusion ," = ", equationsDuringInfusion[[nameEquationsDuringInfusion]],"\n" ) )
                cat( paste0( nameEquationsAfterInfusion ," = ", equationsAfterInfusion[[nameEquationsAfterInfusion]],"\n" ) )
              }

              cat("\n")
              cat( nameModelEquations, "\n\n" )

              derivatives = getDerivatives( modelEquations )

              namesDerivatives = names( derivatives )

              for (namesDerivative in namesDerivatives )
              {
                cat( paste0( namesDerivative ," = ", derivatives[[namesDerivative]],"\n" ) )
              }
            }
          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrModelError.
#'
#' @name knitrModelError
#' @param object An object \code{knitrModelError} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrModelError.

setGeneric(
  "knitrModelError",
  function( object ) {
    standardGeneric("knitrModelError")
  })

setMethod(f="knitrModelError",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            modelEquations =  getEquationsStatisticalModel( statisticalModel )
            responses = getResponsesStatisticalModel( statisticalModel )
            namesResponses = names( responses )

            sigmaSlope = list()
            sigmaInter = list()

            if( class( modelEquations ) %in% c("ModelEquations","ModelInfusionEquations") )
            {
              for( response in responses )
              {
                modelError = getModelError( response )
                nameResponse = response@name
                sigmaSlope[[nameResponse]] = getSigmaSlope(modelError)
                sigmaInter[[nameResponse]] = getSigmaInter(modelError)
              }
            }
            else if(class(modelEquations) %in% c("ModelODEquations","ModelInfusionODEquations"))
            {
              for( response in responses )
              {
                modelError = getModelError( response )
                nameResponse = response@name
                sigmaSlope[[nameResponse]] = getSigmaSlope(modelError)
                sigmaInter[[nameResponse]] = getSigmaInter(modelError)
              }
            }

            sigmaSlope = unlist(sigmaSlope)
            sigmaInter = unlist(sigmaInter)

            modelErrorFrame <- matrix(c( namesResponses,
                                         as.character(sigmaSlope),
                                         as.character(sigmaInter)),ncol=3)

            knitr::kable(modelErrorFrame,
                         col.names = c( "Responses","$\\sigma_{slope}$","$\\sigma_{inter}$" ) ) %>%
              kable_styling(fixed_thead = T)

          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrModelParameters.
#'
#' @name knitrModelParameters
#' @param object An object \code{knitrModelParameters} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrModelParameters.

setGeneric(
  "knitrModelParameters",
  function( object ) {
    standardGeneric("knitrModelParameters")
  })

setMethod(f="knitrModelParameters",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            modelParameters = getModelParameters( statisticalModel )
            namesParameters = names( modelParameters )
            modelEquations = getEquationsStatisticalModel(statisticalModel)

            # get class of the Fim
            designs = getDesign( object )
            design = designs[[1]]
            fimOfDesign = getFimOfDesign( design )
            typeOfFim = class( fimOfDesign )[1]

            nameCol <- c()
            muCol <- c()
            omegaCol <- c()
            distributionCol <- list()

            k=1
            for( parameter in modelParameters )
            {
              nameCol <- c( nameCol, getNameModelParameter( parameter ) )

              muCol <- c( muCol, getMu( parameter ) )
              omegaCol <- c( omegaCol, getOmega( parameter ) )

              distribution = getDistribution( parameter )
              distribution = class(distribution)[1]

              if ( distribution== "LogNormalDistribution")
              {
                distributionCol[k]="LogNormal"
              }else if (distribution== "NormalDistribution")
              {
                distributionCol[k]="Normal"
              }

              k=k+1
            }

            fixedMuParameters = lapply( modelParameters, slot, name = "fixedMu")
            fixedOmegaParameters = lapply( modelParameters, slot, name = "fixedOmega")

            parameterFrame <- matrix(c(
              namesParameters,
              as.character(muCol),
              as.character(omegaCol**2),
              distributionCol,
              fixedMuParameters,
              fixedOmegaParameters),ncol=6)

            parameterFrame[parameterFrame==FALSE] = "No"
            parameterFrame[parameterFrame==TRUE] = "Yes"
            colnames( parameterFrame ) = c("Parameters","$\\mu$","$\\omega^2$","Distribution","$\\mu$ fixed", "$\\omega^2$ fixed" )

            if ( typeOfFim %in% c( "IndividualFim","BayesianFim" ) )
            {
              parameterFrame =  parameterFrame[,c( 1,2,4,5), drop = FALSE]
              colnames( parameterFrame ) = c("Parameters","$\\mu$","Distribution","$\\mu$ fixed" )
            }

            knitr::kable( parameterFrame ) %>%
              kable_styling(fixed_thead = T)

          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrAdministrationParameters.
#'
#' @name knitrAdministrationParameters
#' @param object An object \code{knitrAdministrationParameters} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrAdministrationParameters.

setGeneric(
  "knitrAdministrationParameters",
  function( object ) {
    standardGeneric("knitrAdministrationParameters")
  })

setMethod(f="knitrAdministrationParameters",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            modelEquations = getEquationsStatisticalModel( statisticalModel )

            if ( class( modelEquations ) %in% c( "ModelODEquations", "ModelInfusionODEquations" ) )
            {
              modelEquations = getDerivatives( modelEquations )
            } else if ( class( modelEquations ) %in% c( "ModelEquations", "ModelInfusionEquations" ) )
            {
              modelEquations = getEquations( modelEquations )
            }

            # for : steady state models & infusion models
            designs = getDesign( object )
            indexInitialDesigns = which( lapply( designs, slot, "isOptimalDesign" ) == FALSE )
            designs = designs[indexInitialDesigns]

            infusionData = data.frame()

            for ( nameDesign in  names( designs ) )
            {
              design = designs[[nameDesign]]

              arms = getArms( design )

              for ( nameArm in  names( arms ) )
              {
                admin = getAdministration( arms[[nameArm]] )
                samplingTimes = getSamplings(arms[[nameArm]])
                namesResponse = names( admin )

                for( nameResponse in namesResponse )
                {
                  tau = getTau( admin[[nameResponse]] )
                  Tinf = getTinf( admin[[nameResponse]] )
                  timeDosesData = getTimeDose( admin[[nameResponse]] )
                  dosesData = getAmountDose(  admin[[nameResponse]]  )

                  if ( unique( Tinf ) == 0 )
                  {
                    Tinf="-"
                  }else{
                    Tinf = toString( Tinf )
                  }

                  # repeatead and multi-doses
                  if ( tau !=0 )
                  {
                    samplingTimes = samplingTimes[[nameResponse]]
                    samplingTimes = getSampleTime( samplingTimes )
                    n = max( samplingTimes )%/%tau+1
                    timeDosesData =  seq(0,max(samplingTimes),tau)
                    dosesData = rep( dosesData, n )
                    tau = toString( tau )
                  }

                  # steady-state model
                  indexRespPKinEquations = which( grepl( nameResponse, modelEquations ) == TRUE )

                  # bolus model
                  if ( length( indexRespPKinEquations) == 0 )
                  {
                    if ( unique( grepl( "tau", modelEquations ) ) == TRUE )
                    {
                      timeDosesData = 0.0
                      dosesData = unique( dosesData )
                      tau = toString( tau )

                    }else{
                      # no steady-state model
                      tau="-"
                    }
                  }

                  if ( length( indexRespPKinEquations) != 0 )
                  {
                    if ( unique( grepl( "tau", modelEquations[indexRespPKinEquations] ) ) == TRUE )
                    {
                      timeDosesData = 0.0
                      dosesData = unique( dosesData )
                      tau = toString( tau )

                    }else{
                      # no steady-state model
                      tau="-"
                    }
                  }

                  timeDosesData = toString( timeDosesData )
                  dosesData =  toString( dosesData )

                  data = c( nameDesign, nameArm, nameResponse, tau, Tinf, timeDosesData, dosesData )
                  infusionData = rbind( infusionData, data, stringsAsFactors = FALSE )
                }
              }
            }

            colnames( infusionData ) = c( "Design", " Arm", "Response", "${\\tau}$" ,"${T_{inf}}$", "Time dose", "Dose" )

            knitr::kable(infusionData) %>%
              kable_styling(fixed_thead = T)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrInitialDesigns.
#'
#' @name knitrInitialDesigns
#' @param object An object \code{knitrInitialDesigns} from the class \linkS4class{ReportAndPlots}.
#' @return The table kknitrInitialDesigns.

setGeneric(
  "knitrInitialDesigns",
  function( object ) {
    standardGeneric("knitrInitialDesigns")
  })

setMethod(f="knitrInitialDesigns",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            responses = getResponsesStatisticalModel( statisticalModel )
            numberOfResponses = length(responses)

            allDesigns = getDesign( object )
            indexInitialDesigns = which( lapply( allDesigns, slot, "isOptimalDesign" ) == FALSE )
            designs = allDesigns[indexInitialDesigns]

            namesDesigns = names( designs )

            dataDesigns = list()

            for ( nameDesign in namesDesigns )
            {
              design = designs[[nameDesign]]
              fimOfDesign = getFimOfDesign( design )
              arms = getArms( design )

              if ( class( fimOfDesign ) %in% c( "IndividualFim","BayesianFim" ) )
              {
                for ( arm in arms )
                {
                  nameArm = getNameArm( arm)
                  design@arms[[ nameArm ]] <- setArmSize( arm, 1 )
                }
              }
              dataDesigns[[nameDesign]] = summaryArmData( design )
            }

            dataDesigns = do.call( rbind, dataDesigns )

            namesDesigns = sapply( namesDesigns, function (x) rep(x,as.numeric(numberOfResponses)))
            namesDesigns = as.vector( namesDesigns )
            dataDesigns = cbind( namesDesigns, dataDesigns )
            rownames( dataDesigns )=NULL

            dataDesigns = dataDesigns[, c( "namesDesigns", "Arm_name", "Response","Sampling_times", "Number_of_subjects" )]

            knitr::kable(dataDesigns,
                         col.names = c( "Design name", "Arms name", "Response","Sampling times","Number of subjects" )) %>%
              kable_styling(fixed_thead = T)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrFIM.
#'
#' @name knitrFIM
#' @param object An object \code{knitrFIM} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrFIM.

setGeneric(
  "knitrFIM",
  function( object ) {
    standardGeneric("knitrFIM")
  })

setMethod(f="knitrFIM",
          signature("PFIMProject"),

          function( object ){

            statisticalModel = getStatisticalModel( object )
            modelParameters = getModelParameters( statisticalModel )
            modelEquations = getEquationsStatisticalModel( statisticalModel )
            responses = getResponsesStatisticalModel( statisticalModel )
            namesResponses = names( responses )
            namesParameters = names( modelParameters )
            optimizationAlgorithm = object@optimizationAlgorithm
            rowNamesSigma = list()

            # mu and omega values of the parameters
            muValueParameters = unlist( lapply( modelParameters, slot, "mu" ) )
            omegaValueParameters = unlist( lapply( modelParameters, slot, "omega" ) )
            omegaValueParameters = omegaValueParameters ** 2

            # sigma values from error model
            sigmaValue = c()

            for ( nameResponse in namesResponses )
            {
              modelError = getModelError( responses[[nameResponse]] )
              sigmaInter = getSigmaInter( modelError )
              sigmaSlope = getSigmaSlope( modelError )

              if ( sigmaInter != 0)
              {
                rowNamesSigma = append( rowNamesSigma, paste0("${{\\sigma}_{inter}}_{",nameResponse,"}$") )
                sigmaValue = c(sigmaValue,sigmaInter)
              }
              if ( sigmaSlope != 0)
              {
                rowNamesSigma = append( rowNamesSigma, paste0("${{\\sigma}_{slope}}_{",nameResponse,"}$") )
                sigmaValue = c(sigmaValue,sigmaSlope)
              }
            }

            rowNamesSigma = unlist( rowNamesSigma )

            # Fim for Evaluation / Optimization
            if ( length( optimizationAlgorithm) ==0  )
            {
              fimOfDesign = getFimOfDesign(object@designs[[ 1 ]])
              fimOfInitialDesign = fimOfDesign
            }else if ( length( optimizationAlgorithm) != 0 )
            {
              fimOfDesign = getFimOfDesign(object@designs[[2]])
              fimOfInitialDesign = getFimOfDesign(object@designs[[ 1 ]])
            }

            # type of FIM
            typeOfFIM = class( fimOfDesign )

            # correlation matrix
            correlationMatrix = getCorr( fimOfDesign )

            # Fisher matrix
            matrixFisher = getMfisher( fimOfDesign )

            parametersIndicesMuFIM = fimOfDesign@parametersIndicesMuFIM
            parametersIndicesOmegaFIM = fimOfDesign@parametersIndicesOmegaFIM

            namesParametersMu = namesParameters[parametersIndicesMuFIM]
            namesParametersMu = paste0( "$\\mu_{",namesParametersMu,"}$" )

            namesParametersOmega = namesParameters[parametersIndicesOmegaFIM]
            namesParametersOmega = paste0( "$\\omega^2_{",namesParametersOmega,"}$" )

            namesParametersSigma = rowNamesSigma

            numberParametersMu = length( namesParametersMu )
            numberParametersOmega = length( namesParametersOmega )
            numberParametersSigma = length( namesParametersSigma )

            if ( typeOfFIM == "PopulationFim")
            {
              colnames( matrixFisher ) = c( namesParametersMu, namesParametersOmega, namesParametersSigma )
              rownames( matrixFisher ) = c( namesParametersMu, namesParametersOmega, namesParametersSigma )

              colnames( correlationMatrix ) = c( namesParametersMu, namesParametersOmega, namesParametersSigma )
              rownames( correlationMatrix ) = c( namesParametersMu, namesParametersOmega, namesParametersSigma )

              populationFIMFixedEffects = as.matrix(matrixFisher[1:numberParametersMu, 1:numberParametersMu])

              colnames( populationFIMFixedEffects ) = c( namesParametersMu )
              rownames( populationFIMFixedEffects ) = c( namesParametersMu )

              populationFIMRandomEffects = as.matrix(matrixFisher[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                   ( 1+numberParametersMu):dim(matrixFisher)[1]] )

              populationCorrelationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])

              colnames( populationCorrelationFixedEffects ) = c( namesParametersMu )
              rownames( populationCorrelationFixedEffects ) = c( namesParametersMu )

              populationCorrelationRandomEffects = as.matrix(correlationMatrix[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                                ( 1+numberParametersMu):dim(matrixFisher)[1]] )
              # FIM criteria
              detFim = det( matrixFisher )
              DCriterion = getDcriterion( fimOfDesign )
              condFIMFixedEffects = c()
              condFIMRandomEffects = c()
              condFIMFixedEffects = cond(populationFIMFixedEffects)
              condFIMRandomEffects = cond(populationFIMRandomEffects)
              criteriaFim = matrix(c(detFim,condFIMFixedEffects,condFIMRandomEffects,DCriterion),1,4)
              colnames(criteriaFim) = c(" ", "Fixed effects", "Random effects", "")

              # criteria for the initial design
              matrixFisherInitialDesign = getMfisher( fimOfInitialDesign )
              FIMFixedEffectsInitialDesign = as.matrix( matrixFisherInitialDesign[1:numberParametersMu, 1:numberParametersMu])
              FIMRandomEffectsInitialDesign = as.matrix( matrixFisherInitialDesign[( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                                   ( 1+numberParametersMu):dim(matrixFisher)[1]] )

              detFimInitialDesign = det( matrixFisherInitialDesign )
              DCriterionInitialDesign = getDcriterion( fimOfInitialDesign )
              condFIMFixedEffectsInitialDesign = c()
              condFIMRandomEffectsInitialDesign = c()
              condFIMFixedEffectsInitialDesign = cond( FIMFixedEffectsInitialDesign )
              condFIMRandomEffectsInitialDesign = cond( FIMRandomEffectsInitialDesign )

              criteriaFimInitialDesign = matrix(c(detFimInitialDesign,
                                                  condFIMFixedEffectsInitialDesign,
                                                  condFIMRandomEffectsInitialDesign,
                                                  DCriterionInitialDesign),1,4)

              colnames(criteriaFimInitialDesign) = c(" ", "Fixed effects", "Random effects", "")

              # SE and RSE
              SE = getSE( fimOfDesign )

              SE_mu = SE[1:numberParametersMu]
              SE_omega = SE[numberParametersMu+c(1:numberParametersOmega)]
              SE_sigma = tail(SE, numberParametersSigma)

              RSE_mu = SE_mu/muValueParameters[parametersIndicesMuFIM] * 100
              RSE_omega = SE_omega/omegaValueParameters[parametersIndicesOmegaFIM]*100
              RSE_sigma = SE_sigma / sigmaValue * 100

              SE = c( SE_mu, SE_omega, SE_sigma )
              RSE = c( RSE_mu, RSE_omega, RSE_sigma )

              rowNamesParameters = c( namesParametersMu, namesParametersOmega, namesParametersSigma )

              se_rse = data.frame( rowNamesParameters,
                                   c(muValueParameters[parametersIndicesMuFIM],
                                     omegaValueParameters[parametersIndicesOmegaFIM],
                                     sigmaValue),
                                   SE, RSE)
              rownames(se_rse) = NULL
              colnames(se_rse) = c("Parameters","Values", "SE","RSE (%)")

              # Kable
              populationFIMFixedEffects = knitr::kable(populationFIMFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              populationFIMRandomEffects = knitr::kable(populationFIMRandomEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              populationCorrelationFixedEffects = knitr::kable(populationCorrelationFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              populationCorrelationRandomEffects = knitr::kable(populationCorrelationRandomEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              criteriaFim = knitr::kable(criteriaFim) %>%
                kable_styling(fixed_thead = T) %>%
                add_header_above(c("Determinant" = 1, "Condition numbers" = 2, "D-criterion" = 1))

              criteriaFimInitialDesign = knitr::kable(criteriaFimInitialDesign) %>%
                kable_styling(fixed_thead = T) %>%
                add_header_above(c("Determinant" = 1, "Condition numbers" = 2, "D-criterion" = 1))

              se_rse = knitr::kable(se_rse) %>%
                kable_styling(fixed_thead = T)


              return( knitrFIM = list(
                typeOfFIM = typeOfFIM,
                fIMFixedEffects = populationFIMFixedEffects,
                fIMRandomEffects = populationFIMRandomEffects,
                correlationFixedEffects = populationCorrelationFixedEffects,
                correlationRandomEffects = populationCorrelationRandomEffects,
                criteriaFim = criteriaFim,
                criteriaFimInitialDesign = criteriaFimInitialDesign,
                se_rse = se_rse) )

            }else if( typeOfFIM == "IndividualFim" )
            {
              colnames( matrixFisher ) = c( namesParametersMu, namesParametersSigma )
              rownames( matrixFisher ) = c( namesParametersMu, namesParametersSigma )

              colnames( correlationMatrix ) = c( namesParametersMu, namesParametersSigma )
              rownames( correlationMatrix ) = c( namesParametersMu, namesParametersSigma )

              individualFIMFixedEffects = as.matrix(matrixFisher[1:numberParametersMu, 1:numberParametersMu])

              individualFIMRandomEffects = as.matrix(matrixFisher[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                   ( 1+numberParametersMu):dim(matrixFisher)[1]] )

              individualCorrelationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])

              individualCorrelationRandomEffects = as.matrix(correlationMatrix[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                                ( 1+numberParametersMu):dim(matrixFisher)[1]] )
              # FIM criteria
              detFim = det( matrixFisher )
              DCriterion = getDcriterion( fimOfDesign )
              condFIMFixedEffects = c()
              condFIMRandomEffects = c()
              condFIMFixedEffects = cond(individualFIMFixedEffects)
              condFIMRandomEffects = cond(individualFIMRandomEffects)
              criteriaFim = matrix(c(detFim,
                                     condFIMFixedEffects, condFIMRandomEffects,DCriterion),1,4)
              colnames(criteriaFim) = c(" ", "Fixed effects", "Random effects", "")


              # criteria for the initial design
              matrixFisherInitialDesign = getMfisher( fimOfInitialDesign )
              FIMFixedEffectsInitialDesign = as.matrix( matrixFisherInitialDesign[1:numberParametersMu, 1:numberParametersMu])
              FIMRandomEffectsInitialDesign = as.matrix( matrixFisherInitialDesign[( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                                   ( 1+numberParametersMu):dim(matrixFisher)[1]] )

              detFimInitialDesign = det( matrixFisherInitialDesign )
              DCriterionInitialDesign = getDcriterion( fimOfInitialDesign )
              condFIMFixedEffectsInitialDesign = c()
              condFIMRandomEffectsInitialDesign = c()
              condFIMFixedEffectsInitialDesign = cond(FIMFixedEffectsInitialDesign)
              condFIMRandomEffectsInitialDesign = cond(FIMRandomEffectsInitialDesign)

              criteriaFimInitialDesign = matrix(c(detFimInitialDesign,
                                                  condFIMFixedEffectsInitialDesign,
                                                  condFIMRandomEffectsInitialDesign,
                                                  DCriterionInitialDesign),1,4)

              colnames( criteriaFimInitialDesign ) = c(" ", "Fixed effects", "Random effects", "")

              # SE and RSE
              SE = getSE( fimOfDesign )

              SE_mu = SE[1:numberParametersMu]
              SE_sigma = tail(SE, numberParametersSigma)

              RSE_mu = SE_mu/muValueParameters[parametersIndicesMuFIM] * 100
              RSE_sigma = SE_sigma / sigmaValue * 100

              SE = c( SE_mu,  SE_sigma )
              RSE = c( RSE_mu, RSE_sigma )

              rowNamesParameters = c( namesParametersMu, namesParametersSigma )

              se_rse = data.frame( rowNamesParameters,
                                   c(muValueParameters[parametersIndicesMuFIM],
                                     sigmaValue),
                                   SE, RSE)

              rownames(se_rse) = NULL
              colnames(se_rse) = c("Parameters","Values", "SE","RSE (%)")

              # Kable
              individualFIMFixedEffects = knitr::kable(individualFIMFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              individualFIMRandomEffects = knitr::kable(individualFIMRandomEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              individualCorrelationFixedEffects = knitr::kable(individualCorrelationFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              individualCorrelationRandomEffects = knitr::kable(individualCorrelationRandomEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              criteriaFim = knitr::kable(criteriaFim) %>%
                kable_styling(fixed_thead = T) %>%
                add_header_above(c("Determinant" = 1, "Condition numbers" = 2, "D-criterion" = 1))

              criteriaFimInitialDesign = knitr::kable( criteriaFimInitialDesign ) %>%
                kable_styling(fixed_thead = T) %>%
                add_header_above(c("Determinant" = 1, "Condition numbers" = 2, "D-criterion" = 1))

              se_rse = knitr::kable(se_rse) %>%
                kable_styling(fixed_thead = T)

              return( knitrFIM = list(
                typeOfFIM = typeOfFIM,
                fIMFixedEffects = individualFIMFixedEffects,
                fIMRandomEffects = individualFIMRandomEffects,
                correlationFixedEffects = individualCorrelationFixedEffects,
                correlationRandomEffects = individualCorrelationRandomEffects,
                criteriaFimInitialDesign = criteriaFimInitialDesign,
                criteriaFim = criteriaFim,
                se_rse = se_rse) )

            }else if( typeOfFIM == "BayesianFim" )
            {
              colnames( matrixFisher ) = c( namesParametersMu )
              rownames( matrixFisher ) = c( namesParametersMu )

              colnames( correlationMatrix ) = c( namesParametersMu )
              rownames( correlationMatrix ) = c( namesParametersMu )

              bayesianFIMFixedEffects = as.matrix(matrixFisher[1:numberParametersMu, 1:numberParametersMu])
              bayesianlCorrelationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])

              # FIM criteria
              detFim = det( matrixFisher )
              DCriterion = getDcriterion( fimOfDesign )
              condFIMFixedEffects = c()
              condFIMFixedEffects = cond(bayesianFIMFixedEffects)

              criteriaFim = matrix(c(detFim, condFIMFixedEffects, DCriterion),1,3)
              colnames(criteriaFim) = c("Determinant", "Fixed effects", "D-criterion")

              # criteria for the initial design
              matrixFisherInitialDesign = getMfisher( fimOfInitialDesign )
              FIMFixedEffectsInitialDesign = as.matrix( matrixFisherInitialDesign[1:numberParametersMu, 1:numberParametersMu])

              detFimInitialDesign = det( matrixFisherInitialDesign )
              DCriterionInitialDesign = getDcriterion( fimOfInitialDesign )
              condFIMFixedEffectsInitialDesign = c()
              condFIMFixedEffectsInitialDesign = cond( FIMFixedEffectsInitialDesign )

              criteriaFimInitialDesign = matrix( c( detFimInitialDesign,
                                                    condFIMFixedEffectsInitialDesign,
                                                    DCriterionInitialDesign ),1 ,3 )

              colnames( criteriaFimInitialDesign ) = c("Determinant", "Fixed effects", "D-criterion")

              # SE, RSE and shrinkage
              SE = getSE( fimOfDesign )

              SE_mu = SE[1:numberParametersMu]
              RSE_mu = SE_mu/muValueParameters[parametersIndicesMuFIM] * 100
              shrinkage = getShrinkage(fimOfDesign)

              SE = c( SE_mu )
              RSE = c( RSE_mu )

              rowNamesParameters = c( namesParametersMu )

              se_rse = data.frame( rowNamesParameters,
                                   muValueParameters[parametersIndicesMuFIM],
                                   SE, RSE, shrinkage)

              rownames(se_rse) = NULL
              colnames(se_rse) = c("Parameters","Values", "SE","RSE (%)" ,"Shrinkage")

              # Kable
              bayesianFIMFixedEffects = knitr::kable(bayesianFIMFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              bayesianlCorrelationFixedEffects = knitr::kable(bayesianlCorrelationFixedEffects) %>%
                kable_styling(position = "center",fixed_thead = T)

              criteriaFim = knitr::kable(criteriaFim) %>%
                kable_styling(fixed_thead = T)

              criteriaFimInitialDesign = knitr::kable(criteriaFimInitialDesign) %>%
                kable_styling(fixed_thead = T)

              se_rse = knitr::kable(se_rse) %>%
                kable_styling(fixed_thead = T)

              return( knitrFIM = list(
                typeOfFIM = typeOfFIM,
                fIMFixedEffects = bayesianFIMFixedEffects,
                correlationFixedEffects = bayesianlCorrelationFixedEffects,
                criteriaFim = criteriaFim,
                criteriaFimInitialDesign = criteriaFimInitialDesign,
                se_rse = se_rse) )
            }
          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the table knitrOptimalDesign.
#'
#' @name knitrOptimalDesign
#' @param object An object \code{knitrFIM} from the class \linkS4class{ReportAndPlots}.
#' @return The table knitrOptimalDesign.

setGeneric(
  "knitrOptimalDesign",
  function( object ) {
    standardGeneric("knitrOptimalDesign")
  })

setMethod(f="knitrOptimalDesign",
          signature("PFIMProject"),

          function( object ){

            designs = getDesign( object )
            optimizationAlgorithm = object@optimizationAlgorithm

            indexOptimalDesign = which( lapply( designs, slot, "isOptimalDesign" ) == TRUE )
            optimalDesign = designs[[indexOptimalDesign]]

            arms = getArms( optimalDesign )

            dataArm = list()
            dataRespPK = list()
            dataRespPD = list()

            # round arm size depending of the optimization algorithm
            valueRoundSamplingTimes = Inf
            valueRoundArmSize = Inf

            if ( optimizationAlgorithm %in% c( "PSOAlgorithm","PGBOAlgorithm" ) )
            {
              valueRoundSamplingTimes = 4

            } else if ( optimizationAlgorithm %in% c( "MultiplicativeAlgorithm","FedorovWynnAlgortihm","SimplexAlgorithm" ) )
            {
              valueRoundSamplingTimes = 2
              valueRoundArmSize = 2
            }

            for( arm in arms )
            {
              armSize = getArmSize( arm )
              armSize = round( armSize, valueRoundArmSize )

              nameArm = getNameArm(arm)

              admins = getAdministration(arm)
              samplingTimes = getSamplings(arm)

              namesResponses = names( samplingTimes )
              namesRespPK = namesResponses[grep("RespPK",namesResponses)]
              namesRespPD = namesResponses[grep("RespPD",namesResponses)]

              for ( nameResponsePK in namesRespPK )
              {
                timeDose = as.numeric(getTimeDose(admins[[nameResponsePK]]))
                amountDose = as.numeric(getAmountDose(admins[[nameResponsePK]]))
                sampleTime = sort(getSampleTime(samplingTimes[[nameResponsePK]]))
                sampleTime = round( sampleTime, valueRoundSamplingTimes )
                sampleTime = paste0( "(", toString(sampleTime), ")")

                timeDose = paste0( "(", toString(timeDose), ")")
                amountDose = paste0( "(", toString(amountDose), ")")

                data = data.frame(I(c(nameArm, nameResponsePK, timeDose, amountDose, sampleTime, armSize )))

                dataRespPK[[nameResponsePK]] = t(data)
                rownames( dataRespPK[[nameResponsePK]] )=NULL
              }

              for ( nameResponsePD in namesRespPD )
              {
                sampleTime = sort(getSampleTime(samplingTimes[[nameResponsePD]]))
                sampleTime = round( sampleTime, valueRoundSamplingTimes )
                sampleTime = paste0( "(", toString(sampleTime), ")")
                data = data.frame(I(c(nameArm, nameResponsePD, " ", " ", sampleTime, armSize )))

                dataRespPD[[nameResponsePD]] =  t(data)
                rownames( dataRespPD[[nameResponsePD]] )=NULL
              }

              dataArm[[nameArm]]  = rbind( do.call( rbind, dataRespPK  ),
                                           do.call( rbind, dataRespPD ) )
            }

            dataArm <- as.data.frame( do.call(rbind,dataArm) )
            colnames(dataArm) =  c( "Arm name", " Response", "Time dose", "Amount dose" ,"Sampling times", "Number of subjects")
            dataArm = dataArm[order( dataArm[,c("Number of subjects")],decreasing = TRUE),]
            rownames(dataArm) = NULL

            knitr::kable(dataArm) %>%
              kable_styling(fixed_thead = T)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Generate the html report for the evaluation.
#'
#' @name PFIMProjectReportEvaluation
#' @param object \code{PFIMProject} object.
#' @param inputPath A string giving the input path.
#' @param outputPath A string giving the output path.
#' @param plotOptions A list giving the options.
#' @return The html report for the evaluation.

setGeneric(
  "PFIMProjectReportEvaluation",
  function( object, inputPath, outputPath, plotOptions ) {
    standardGeneric("PFIMProjectReportEvaluation")
  })

setMethod(f="PFIMProjectReportEvaluation",
          signature("PFIMProject"),

          function( object, inputPath, outputPath, plotOptions ){

            projectName =  object@name

            # path to save report
            inputPath = "inst/rmarkdown/templates/skeleton/"

            if ( is.null( outputPath ) )
            {
              outputPath = "inst/report/"
            }

            # plot responses, si, se, rse, bayesian shrinkage
            plotResponses = plotResponse( object, plotOptions )
            plotSensitivity = plotSensitivity( object, plotOptions )
            plotSE = plotSE( object )
            plotRSE = plotRSE( object )
            plotShrinkage = plotShrinkage( object )

            # Markdown template
            name_markdown_file = "template_evaluation.rmd"
            name_input_file = paste0(inputPath, name_markdown_file)
            name_output_file = paste0( "PFIMReport_Evaluation_", projectName,".html" )


            rmarkdown::render( input = name_input_file,
                               output_file =  name_output_file,
                               output_dir = outputPath,
                               params = list(
                                 projectName = "projectName",
                                 object ="object",
                                 knitrModelEquations = "knitrModelEquations",
                                 knitrModelError = "knitrModelError",
                                 knitrModelParameters = "knitrModelParameters",
                                 knitrAdministrationParameters = "knitrAdministrationParameters",
                                 knitrInitialDesigns = "knitrInitialDesigns",
                                 knitrFIM = "knitrFIM",
                                 plotResponses = "plotResponse",
                                 plotSensitivity = "plotSensitivity",
                                 plotShrinkage = "plotShrinkage",
                                 plotSE = "plotSE",
                                 plotRSE = "plotRSE"))
          })

# -------------------------------------------------------------------------------------------------------------------
#' Generate the html report for the optimization.
#'
#' @name PFIMProjectReportOptimization
#' @param object \code{PFIMProject} object.
#' @param inputPath A string giving the input path.
#' @param outputPath A string giving the output path.
#' @param plotOptions A list giving the options.
#' @return The html report for the optimization.

setGeneric(
  "PFIMProjectReportOptimization",
  function( object, inputPath, outputPath, plotOptions ) {
    standardGeneric("PFIMProjectReportOptimization")
  })

setMethod(f="PFIMProjectReportOptimization",
          signature("PFIMProject"),

          function( object, inputPath, outputPath, plotOptions ){

            # name of the project and optimization algorithm
            projectName =  object@name
            projectName = gsub(" ", "",  projectName, fixed = TRUE)
            optimizationAlgorithm = object@optimizationAlgorithm

            #get threshold for weight
            weightThreshold = plotOptions$weightThreshold
            if (length( weightThreshold ) == 0){ weightThreshold = 0.01 }

            # template for the MultiplicativeAlgorithm
            if ( optimizationAlgorithm == "MultiplicativeAlgorithm" )
            {
              optimizationAlgorithm = "Multiplicative algorithm"

              name_output_file = paste0( "PFIMReport_Optimization_MultiplicativeAlgorithm_", projectName,".html" )

              # names input/output files
              name_markdown_file = "template_multiplicativeAlgorithm.rmd"
              name_input_file = paste0(inputPath, name_markdown_file)

              # plot responses; si, se, rse, bayesian shrinkage
              plotResponses = plotResponse( object, plotOptions )
              plotSE = plotSE( object )
              plotRSE = plotRSE( object )
              plotShrinkage = plotShrinkage( object )

              rmarkdown::render( input = name_input_file,
                                 output_file = name_output_file,
                                 output_dir = outputPath,
                                 params = list(
                                   projectName = "projectName",
                                   optimizationAlgorithm = "optimizationAlgorithm",
                                   object ="object",
                                   knitrModelEquations = "knitrModelEquations",
                                   knitrModelError = "knitrModelError",
                                   knitrModelParameters = "knitrModelParameters",
                                   knitrAdministrationParameters = "knitrAdministrationParameters",
                                   knitrInitialDesigns = "knitrInitialDesigns",
                                   knitrFIM = "knitrFIM",
                                   knitrOptimalDesign ="knitrOptimalDesign",
                                   weightThreshold = "weightThreshold",
                                   plotResponses ="plotResponses",
                                   plotSE = "plotSE",
                                   plotRSE = "plotRSE",
                                   plotShrinkage ="plotShrinkage" ) )

            } else if ( optimizationAlgorithm == "FedorovWynnAlgorithm" ) {

              optimizationAlgorithm = "FedorovWynn algorithm"

              # names input/output files
              name_markdown_file = "template_FedorovAlgorithm.rmd"
              name_input_file = paste0(inputPath, name_markdown_file)
              name_output_file = paste0( "PFIMReport_Optimization_FedorovWynnAlgorithm_", projectName,".html" )

              # plot responses; si, se, rse, bayesian shrinkage
              plotResponses = plotResponse( object, plotOptions )
              plotSE = plotSE( object )
              plotRSE = plotRSE( object )
              plotShrinkage = plotShrinkage( object )

              rmarkdown::render( input = name_input_file,
                                 output_file = name_output_file,
                                 output_dir = outputPath,
                                 params = list(
                                   projectName = "projectName",
                                   optimizationAlgorithm = "optimizationAlgorithm",
                                   object ="object",
                                   knitrModelEquations = "knitrModelEquations",
                                   knitrModelError = "knitrModelError",
                                   knitrModelParameters = "knitrModelParameters",
                                   knitrAdministrationParameters = "knitrAdministrationParameters",
                                   knitrInitialDesigns = "knitrInitialDesigns",
                                   knitrFIM = "knitrFIM",
                                   knitrOptimalDesign ="knitrOptimalDesign",
                                   plotResponses = "plotResponses",
                                   plotSE = "plotSE",
                                   plotRSE = "plotRSE",
                                   plotShrinkage ="plotShrinkage"
                                 ))

            } else if ( optimizationAlgorithm == "SimplexAlgorithm" ) {

              optimizationAlgorithm = "Simplex algorithm"

              name_markdown_file = "template_SimplexAlgorithm.rmd"
              name_input_file = paste0(inputPath, name_markdown_file)
              name_output_file = paste0( "PFIMReport_Optimization_SimplexAlgorithm_", projectName,".html" )

              # plot responses; si, se, rse, bayesian shrinkage
              plotResponses = plotResponse( object, plotOptions )
              plotSE = plotSE( object )
              plotRSE = plotRSE( object )
              plotShrinkage = plotShrinkage( object )

              rmarkdown::render( input = name_input_file,
                                 output_file = name_output_file,
                                 output_dir = outputPath,
                                 params = list(
                                   projectName = "projectName",
                                   optimizationAlgorithm = "optimizationAlgorithm",
                                   object ="object",
                                   knitrModelEquations = "knitrModelEquations",
                                   knitrModelError = "knitrModelError",
                                   knitrModelParameters = "knitrModelParameters",
                                   knitrAdministrationParameters = "knitrAdministrationParameters",
                                   knitrInitialDesigns = "knitrInitialDesigns",
                                   knitrFIM = "knitrFIM",
                                   knitrOptimalDesign = "knitrOptimalDesign",
                                   plotResponses = "plotResponses",
                                   plotSE = "plotSE",
                                   plotRSE = "plotRSE",
                                   plotShrinkage ="plotShrinkage"
                                 ))

            } else if ( optimizationAlgorithm == "PSOAlgorithm" ) {

              optimizationAlgorithm = "PSO algorithm"

              # names input/output files
              name_markdown_file = "template_PSOAlgorithm.rmd"
              name_input_file = paste0(inputPath, name_markdown_file)
              name_output_file = paste0( "PFIMReport_Optimization_PSOAlgorithm_", projectName,".html" )

              # plot responses; si, se, rse, bayesian shrinkage
              plotResponses = plotResponse( object, plotOptions )
              plotSE = plotSE( object )
              plotRSE = plotRSE( object )
              plotShrinkage = plotShrinkage( object )

              rmarkdown::render( input = name_input_file,
                                 output_file = name_output_file,
                                 output_dir = outputPath,
                                 params = list(
                                   projectName = "projectName",
                                   optimizationAlgorithm = "optimizationAlgorithm",
                                   object ="object",
                                   knitrModelEquations = "knitrModelEquations",
                                   knitrModelError = "knitrModelError",
                                   knitrModelParameters = "knitrModelParameters",
                                   knitrAdministrationParameters = "knitrAdministrationParameters",
                                   knitrInitialDesigns = "knitrInitialDesigns",
                                   knitrFIM = "knitrFIM",
                                   knitrOptimalDesign = "knitrOptimalDesign",
                                   plotResponses = "plotResponses",
                                   plotSE = "plotSE",
                                   plotRSE = "plotRSE",
                                   plotShrinkage ="plotShrinkage"
                                 ))

            } else if ( optimizationAlgorithm == "PGBOAlgorithm" ) {

              optimizationAlgorithm = "PGBO algorithm"

              # names input/output files
              name_markdown_file = "template_PGBOAlgorithm.rmd"
              name_input_file = paste0(inputPath, name_markdown_file)
              name_output_file = paste0( "PFIMReport_Optimization_PGBOAlgorithm_", projectName,".html" )

              # plot responses; si, se, rse, bayesian shrinkage
              plotResponses = plotResponse( object, plotOptions )
              plotSE = plotSE( object )
              plotRSE = plotRSE( object )
              plotShrinkage = plotShrinkage( object )

              rmarkdown::render( input = name_input_file,
                                 output_file = name_output_file,
                                 output_dir = outputPath,
                                 params = list(
                                   projectName = "projectName",
                                   optimizationAlgorithm = "optimizationAlgorithm",
                                   object ="object",
                                   knitrModelEquations = "knitrModelEquations",
                                   knitrModelError = "knitrModelError",
                                   knitrModelParameters = "knitrModelParameters",
                                   knitrAdministrationParameters = "knitrAdministrationParameters",
                                   knitrInitialDesigns = "knitrInitialDesigns",
                                   knitrFIM = "knitrFIM",
                                   knitrOptimalDesign ="knitrOptimalDesign",
                                   plotResponses ="plotResponses",
                                   plotSE = "plotSE",
                                   plotRSE = "plotRSE",
                                   plotShrinkage ="plotShrinkage"
                                 ))
            }
          })

##########################################################################################################
# END Class "ReportAndPlots"
##########################################################################################################











