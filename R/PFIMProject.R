#################################################################################
#' Class "PFIMProject"
#'
#' @description
#' The class \code{PFIMProject} implements the evaluation of the Fisher Information Matrix through the use
#' of a statistical model. This class also plot the graphic for the evolution over time of the
#' concentraton, the sensitivity indices and the standard errors (SE, RSE) of a model.
#'
#' @name PFIMProject-class
#' @aliases PFIMProject
#' @docType class
#' @include StatisticalModel.R
#' @include DesignConstraint.R
#' @exportClass PFIMProject

#' @section Objects from the class \code{PFIMProject}:
#' Objects form the class \code{PFIMProject} can be created by calls of the form \code{PFIMProject(...)} where
#' (...) are the parameters for the \code{PFIMProject} objects.
#'
#'@section The slots for the \code{PFIMProject} objects:
#' \describe{
#' \item{\code{name}:}{A character strings giving the name of the project.}
#' \item{\code{previous_fim}:}{A matrix of numerical values giving the information matrix obtained from a previous study.}
#' \item{\code{fim}:}{A list of Fims (population or individual or Bayesian information).}
#' \item{\code{statistical_model}:}{A list of StatisticalModels }
#' \item{\code{designs}:}{A list of all designs.}
#' \item{\code{constraints}:}{design constraint.}
#' \item{\code{graph_options}:}{List of graphical options.}
#' }
#################################################################################

PFIMProject<-setClass(
  Class="PFIMProject",
  representation=representation(
    name = "character",
    previous_fim = "list",
    fim = "list",
    statistical_model = "StatisticalModel",
    designs = "list",
    constraint = "DesignConstraint",
    computeFim = "logical",
    optimizationAlgorithm="character"
  ),
  prototype=prototype(),
  validity=function(object)
  {
    if(is(object@previous_fim, "Fim")==TRUE)
      stop("All elements of the slot previous_fim have to be of type Fim or its subclasses")
    if(any(lapply(object@designs, class)!="Design"))
      stop("All elements of the slot design have to be of type Design")
    return(TRUE)
  }
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "PFIMProject",
  definition = function(.Object, name, previous_fim, fim, statistical_model,
                        designs, constraint, computeFIM, optimizationAlgorithm)
  {
    if(!missing(name))
      .Object@name <- name

    if(!missing(previous_fim))
      .Object@previous_fim <- previous_fim

    if(!missing(fim))
      .Object@fim <- fim

    if(!missing(statistical_model))
      .Object@statistical_model <- statistical_model

    if(!missing(designs))
      .Object@designs <- designs

    if(!missing(constraint))
      .Object@constraint <- constraint

    if(!missing(computeFIM))
      .Object@computeFIM <- computeFIM

    if(!missing(optimizationAlgorithm))
      .Object@optimizationAlgorithm <- optimizationAlgorithm

    validObject(.Object)
    return(.Object)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the name of a PFIMProject project.
#'
#' @name getNamePFIMProject
#' @param object A \code{PFIMProject} object.
#' @return The character string \code{name} giving the name of a PFIMProject project.

setGeneric("getNamePFIMProject",
           function(object)
           {
             standardGeneric("getNamePFIMProject")
           })

setMethod("getNamePFIMProject",
          "PFIMProject",
          function(object)
          {
            return(object@name)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Set the name of a PFIMProject projet.
#'
#' @name setNamePFIMProject
#' @param object A \code{PFIMProject} object.
#' @param value A character string giving the new name of the PFIMProject project.
#' @return The \code{PFIMProject} object with a new name.

setGeneric("setNamePFIMProject",
           function(object, value)
           {
             standardGeneric("setNamePFIMProject")
           })

setMethod( f="setNamePFIMProject",
           signature="PFIMProject",
           definition = function(object, value)
           {
             object@name = value
             return(object)
           })

# -------------------------------------------------------------------------------------------------------------------
#' Get the Fisher Information Matrices.
#'
#' @name getFims
#' @param object A \code{PFIMProject} object.
#' @return A list \code{fimList} giving the Fisher Information Matrices for all the designs of a PFIMProject project.

setGeneric("getFims",
           function(object)
           {
             standardGeneric("getFims")
           })

setMethod("getFims",
          "PFIMProject",
          function(object)
          {
            fimList = list()
            for(design in object@designs)
              fimList[[ getNameDesign(design) ]] = getFimOfDesign( design )
            return( fimList )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the Fisher Information Matrix.
#'
#' @name getFim
#' @param object A \code{PFIMProject} object.
#' @param ... A list giving the index of the Fim.
#' @return A \code{Fim} object giving the Fisher Information Matrix of a design.

setGeneric("getFim",
           function(object, ...)
           {
             standardGeneric("getFim")
           })

setMethod("getFim",
          "PFIMProject",
          function(object, ... )
          {
            # set index to 0: will be used later for previous_fim
            index = 0

            list_index = list(...)

            if ( length( list_index ) == 0 )
            {
              index=1
            }

            design = object@designs[[ index ]]
            fimOfDesign = getFimOfDesign( design )
            matrixFisher  = getMfisher( fimOfDesign )
            return( matrixFisher )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the fim matrices from all designs of a PFIMProject object.
#'
#' @name getFisherMatrices
#' @param object A \code{PFIMProject} object.
#' @return A list of matrices \code{fimList} giving the Fisher Information Matrix of all the designs of a PFIMProject project.

setGeneric("getFisherMatrices",
           function(object)
           {
             standardGeneric("getFisherMatrices")
           }
)

setMethod("getFisherMatrices",
          "PFIMProject",
          function(object)
          {
            fimList = list()
            for(design in object@designs)
            {
              fimList[[ getNameDesign(design) ]] = getMfisher( getFimOfDesign( design ) )
            }
            return( fimList )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the \code{StatisticalModel} object of the PFIMProject object.
#'
#' @name getStatisticalModel
#' @param object A \code{PFIMProject} object.
#' @return Return the object \code{statistical_model} of the \linkS4class{StatisticalModel} of the PFIMProject object.

setGeneric("getStatisticalModel",
           function(object)
           {
             standardGeneric("getStatisticalModel")
           }
)

setMethod("getStatisticalModel",
          "PFIMProject",
          function(object)
          {
            return(object@statistical_model)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Define the \code{StatisticalModel} object of the PFIMProject object.
#'
#' @name defineStatisticalModel
#' @param object A \code{PFIMProject} object.
#' @param value A \code{StatisticalModel} or \code{ODEStatisticalModel} object.
#' @return The \code{StatisticalModel} object of the PFIMProject object.

setGeneric("defineStatisticalModel",
           function(object, value)
           {
             standardGeneric("defineStatisticalModel")
           }
)

setMethod( f="defineStatisticalModel",
           signature="PFIMProject",
           definition = function(object, value)
           {
             object@statistical_model <- value
             validObject(object)
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the evaluated responses of the model.
#'
#' @name getEvaluationResponses
#' @param object A \code{Design} object.
#' @return The object \code{Design} evaluated for each of its arm.

setGeneric("getEvaluationResponses",
           function(object )
           {
             standardGeneric("getEvaluationResponses")
           }
)

setMethod(f="getEvaluationResponses",
          signature=  "PFIMProject",
          definition=function(object )
          {
            evaluationResponse = list()
            sensitivityIndicesResponse = list()

            designs = getDesign( object )
            namesDesigns = names( designs )

            for ( nameDesign in namesDesigns )
            {
              design = designs[[nameDesign]]
              evaluationResults = getEvaluationDesign( design )

              evaluationResponse[[nameDesign]] = evaluationResults$evaluationResponse
              sensitivityIndicesResponse[[nameDesign]] = evaluationResults$sensitivityIndicesResponse
            }

            return( list( evaluationResponse = evaluationResponse,
                          sensitivityIndicesResponse = sensitivityIndicesResponse) )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Add a design to the \code{PFIMProject} object.
#'
#' @name addDesign
#' @param object A \code{PFIMProject} object.
#' @param design A \code{Design} object.
#' @return The \code{PFIMProject} object with the \code{Design} object added.

setGeneric("addDesign",
           function(object, design)
           {
             standardGeneric("addDesign")
           }
)

setMethod( f="addDesign",
           signature="PFIMProject",
           definition = function(object, design)
           {
             object@designs[[ getNameDesign( design ) ]] <- design
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Add a list of designs to the \code{PFIMProject} object.
#'
#' @name addDesigns
#' @param object A \code{PFIMProject} object.
#' @param listOfDesigns A list of \code{Design} objects.
#' @return The \code{PFIMProject} object with the \code{Design} objects added.

setGeneric("addDesigns",
           function(object, listOfDesigns)
           {
             standardGeneric("addDesigns")
           }
)

setMethod( f="addDesigns",
           signature="PFIMProject",
           definition = function(object, listOfDesigns)
           {
             for ( i in 1:length(listOfDesigns)){
               value = listOfDesigns[[i]]
               object@designs[[ getNameDesign( value ) ]] <- value
               validObject(object)
             }
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the design of \code{PFIMProject} object.
#'
#' @name getDesign
#' @param object \code{PFIMProject} object.
#' @return The list \code{design} of the designs in the \code{PFIMProject} object.

setGeneric("getDesign",
           function(object)
           {
             standardGeneric("getDesign")
           }
)

setMethod("getDesign",
          "PFIMProject",
          function(object)
          {
            return(object@designs)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the design of \code{PFIMProject} object.
#'
#' @name setDesign
#' @param object A \code{PFIMProject} object.
#' @param value A \code{Design} object.
#' @return The \code{PFIMProject} object with the new \code{Designs}.

setGeneric("setDesign",
           function(object,value)
           {
             standardGeneric("setDesign")
           }
)

setMethod("setDesign",
          "PFIMProject",
          function(object,value)
          {
            object@designs<- value
            validObject(object)
            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Set the constraint to the PFIMProject projet.
#'
#' @name setConstraint
#' @param object A \code{PFIMProject} object.
#' @param constraint The constraint to set
#' @return The \code{PFIMProject} object with the \code{constraint}.

setGeneric("setConstraint",
           function(object, constraint)
           {
             standardGeneric("setConstraint")
           }
)

setMethod( f="setConstraint",
           signature="PFIMProject",
           definition = function(object, constraint)
           {
             object@constraint <- constraint
             validObject(object)
             return(object)
           }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate the design for each arm.
#'
#' @name EvaluateDesign
#' @param object A \code{PFIMProject} object.
#' @param fimType A character string giving the type of FIM: "Population", "Individual" or "Bayesian".
#' @param TheDesign  A \code{Design} object to be evaluated.
#' @return The \code{PFIMProject} object with the list \code{designs} that contains the evaluation of each design for each arm.

setGeneric("EvaluateDesign",
           function(object, fimType, TheDesign)
           {
             standardGeneric("EvaluateDesign")
           }
)

setMethod(f="EvaluateDesign",
          signature=  "PFIMProject",
          definition=function( object, fimType, TheDesign )
          {

            if( !missing( TheDesign ) )
            {
              evaluatedDesign = EvaluateDesignForEachArm( TheDesign , object@statistical_model, fimType)

              object@designs[[ getNameDesign(TheDesign) ]] <- evaluatedDesign
            }
            else
            {
              for ( design in object@designs )
              {
                evaluatedDesign = EvaluateDesignForEachArm( design, object@statistical_model, fimType)
                object@designs[[ getNameDesign(design) ]] <- evaluatedDesign
              }
            }
            return( object )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate a design for each arm for a Population FIM.
#'
#' @name EvaluatePopulationFIM
#' @param object A \code{PFIMProject} object.
#' @return The \code{PFIMProject} object with the list \code{designs} that contains the evaluation of the Population FIM of each design for each arm.

setGeneric("EvaluatePopulationFIM",
           function( object )
           {
             standardGeneric("EvaluatePopulationFIM")
           }
)

setMethod(f="EvaluatePopulationFIM",
          signature=  "PFIMProject",
          definition=function( object )
          {
            for ( design in object@designs )
            {
              object@designs[[ getNameDesign(design) ]] = EvaluateDesignForEachArm( design,
                                                                                    object@statistical_model,
                                                                                    PopulationFim() )
            }
            return( object )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate design for each arm for a Bayesian FIM.
#'
#' @name EvaluateBayesianFIM
#' @param object A \code{PFIMProject} object.
#' @return The \code{PFIMProject} object with the list \code{designs} that contains the evaluation of the Bayesian FIM of each design for each arm.

setGeneric("EvaluateBayesianFIM",
           function(object)
           {
             standardGeneric("EvaluateBayesianFIM")
           })

setMethod(f="EvaluateBayesianFIM",
          signature=  "PFIMProject",
          definition=function( object )
          {
            designs = getDesign( object )

            # set arm size to 1 for Individual Fim
            for ( design in designs )
            {
              arms = getArms( design )
              nameDesign = getNameDesign( design )

              for ( arm in arms )
              {
                nameArm = getNameArm( arm )
                design@arms[[ nameArm ]] <- setArmSize( arm, 1 )
                object@designs[[ nameDesign ]] <- design
              }
            }

            designs = getDesign( object )

            statisticalModel = getStatisticalModel( object )

            for ( design in designs )
            {
              evaluatedDesign = EvaluateDesignForEachArm( design, statisticalModel, BayesianFim() )
              object@designs[[ getNameDesign( design ) ]] <- evaluatedDesign
            }
            return( object )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate design for each arm for a Individual FIM.
#'
#' @name EvaluateIndividualFIM
#' @param object \code{PFIMProject} object.
#' @return The \code{PFIMProject} object with the list \code{designs} that contains the evaluation of the Individual FIM of each design for each arm.

setGeneric("EvaluateIndividualFIM",
           function(object)
           {
             standardGeneric("EvaluateIndividualFIM")
           }
)

setMethod(f="EvaluateIndividualFIM",
          signature=  "PFIMProject",
          definition=function( object )
          {
            designs = getDesign( object )

            # set arm size to 1 for Individual Fim
            for ( design in designs )
            {
              nameDesign = getNameDesign( design )
              arms = getArms( design )

              for ( arm in arms )
              {
                nameArm = getNameArm( arm )
                design@arms[[ nameArm ]] <- setArmSize( arm, 1 )
                object@designs[[ nameDesign ]] <- design
              }
            }

            designs = getDesign( object )

            statisticalModel = getStatisticalModel( object )

            for ( design in designs )
            {
              evaluatedDesign = EvaluateDesignForEachArm( design, statisticalModel, IndividualFim() )
              object@designs[[ getNameDesign( design ) ]] <- evaluatedDesign
            }
            return( object )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Optimize the designs for each arms.
#'
#' @name OptimizeDesign
#' @param object A \code{PFIMProject} object.
#' @param optimizer A \code{Optimization} object.
#' @param typeOfFim A character string giving the type of Fisher Information Matrix (Population, Indidivual or Bayesian).
#' @return The \code{PFIMProject} object with the optimized designs for each arms.

setGeneric("OptimizeDesign",
           function(object, optimizer, typeOfFim )
           {
             standardGeneric("OptimizeDesign")
           }
)

setMethod(f = "OptimizeDesign",
          signature = "PFIMProject",
          definition = function( object, optimizer, typeOfFim )
          {

            # get the name of the optimization algorithm used
            object@optimizationAlgorithm <- class( optimizer )

            statisticalModel = getStatisticalModel( object )
            designs = getDesign( object )

            # get the constraints
            constraints = object@constraint

            # get the initial conditions
            cond_init = list()

            for ( design in  designs ){
              arms = getArms( design )
              for ( arm in arms ){
                cond_init = append( cond_init, getCondInit( arm ) )
              }
            }

            # Run the optimization
            optimizationResult <- Optimize(optimizer,
                                           object,
                                           designs,
                                           statisticalModel,
                                           cond_init, constraints, typeOfFim )

            # evaluate the optimal design
            evaluatedDesign = EvaluateDesignForEachArm( optimizationResult@OptimalDesign,
                                                        statisticalModel, typeOfFim )

            # add the optimal design to the project
            evaluatedDesign@optimizationResult <- optimizationResult

            object <- addDesign( object, evaluatedDesign )

            # add the evaluation of the initial designs
            for ( design in designs )
            {
              evaluatedInitialDesign = EvaluateDesignForEachArm( design, statisticalModel, typeOfFim )
              object <- addDesign(object, evaluatedInitialDesign)
            }

            return(object)
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show the Fisher Information Matrix for all the designs.
#'
#' @name showFims
#' @param object A \code{PFIMProject} object.
#' @return Show the Fisher Information Matrix \code{fimOfDesign} for all the designs.

setGeneric("showFims",
           function(object)
           {
             standardGeneric("showFims")
           }
)

setMethod(f = "showFims",
          signature = "PFIMProject",
          definition = function(object)
          {
            for(design in object@designs)
            {
              cat("\n FIM evaluated for design: ", getNameDesign(design), "\n")
              show(design@fimOfDesign)
            }
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show all the Designs.
#'
#' @name showDesigns
#' @param object A \code{PFIMProject} object.
#' @return Show all the design \code{designs} in the \code{PFIMProject} object.

setGeneric("showDesigns",
           function(object)
           {
             standardGeneric("showDesigns")
           }
)

setMethod(f = "showDesigns",
          signature = "PFIMProject",
          definition = function(object)
          {
            for(design in object@designs)
            {
              show(design)
              cat("\n\n\n")
            }
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Show all the constraints of the \code{PFIMProject} object.
#'
#' @name showConstraints
#' @param object A \code{PFIMProject} object.
#' @return Show the all the objects \code{Constraints} in the \code{PFIMProject} object.

setGeneric("showConstraints",
           function(object)
           {
             standardGeneric("showConstraints")
           }
)

setMethod(f = "showConstraints",
          signature = "PFIMProject",
          definition = function(object)
          {
            show( object@constraint )
            cat("\n\n\n")
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' show the content of the \code{PFIMProject} object.
#'
#' @rdname show
#' @param object \code{PFIMProject} object.
#' @return Summary of the PFIMProject \code{PFIMProject} object given by the:
#' PopulationFim, IndividualFim, Fim, Determinant, Dcriterion,
#' Eigenvalues, SEfixedEffects,
#' SEstandardDeviationOfRandomEffect, SEerrorModel, Designs.

setMethod("show",
          "PFIMProject",
          function(object)
          {
            statisticalModel = getStatisticalModel( object )
            modelParameters = getModelParameters( statisticalModel )
            modelEquations = getEquationsStatisticalModel( statisticalModel )
            responses = getResponsesStatisticalModel( statisticalModel )
            namesResponses = names(responses)
            namesParameters = names( modelParameters )
            numberOfResponses = length( responses )

            # mu and omega values of the parameters
            muValueParameters = unlist( lapply( modelParameters, slot, "mu" ) )
            omegaValueParameters = unlist( lapply( modelParameters, slot, "omega" ) )
            omegaValueParameters = omegaValueParameters ** 2

            # sigma values from error model
            sigmaValue = c()
            rowNamesSigma = list()

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

            # summary for the designs
            allDesigns = getDesign( object )
            dataDesigns = list()

            indexDesignsOptimization = which( lapply( allDesigns, slot, "isOptimalDesign" ) == TRUE )
            indexDesignsEvaluation = which( lapply( allDesigns, slot, "isOptimalDesign" ) == FALSE )

            if ( length( object@optimizationAlgorithm) !=0 )
            {
              # designs optimization
              designs = allDesigns[indexDesignsOptimization]

              cat("\n" )
              cat("*****************************\n" )
              cat(" Optimal designs \n" )
              cat("*****************************\n")
              cat("\n" )
              print( designs )
              cat("\n\n" )

            }else if ( length( object@optimizationAlgorithm) ==0)
            {
              # designs evaluation
              designs = allDesigns[indexDesignsEvaluation]

              cat("\n" )
              cat("**************\n" )
              cat("Initial design \n" )
              cat("**************\n")
              cat("\n" )
              print( designs )
              cat("\n\n" )
            }

            namesDesigns = names( designs )

            for ( nameDesign in namesDesigns )
            {
              design = designs[[nameDesign]]

              fimOfDesign <- getFimOfDesign( design )

              # type of FIM
              typeOfFIM = class( fimOfDesign )

              # Fisher matrix
              matrixFisher = getMfisher( fimOfDesign )

              if ( length( matrixFisher ) == 0 ){

                showDesigns( object )

              }else{

                detFim = det( matrixFisher )
                DCriterion = getDcriterion( fimOfDesign )

                # correlation matrix
                correlationMatrix = getCorr( fimOfDesign )

                parametersIndicesMuFIM = fimOfDesign@parametersIndicesMuFIM
                parametersIndicesOmegaFIM = fimOfDesign@parametersIndicesOmegaFIM

                namesParametersMu = namesParameters[parametersIndicesMuFIM]
                namesParametersOmega = namesParameters[parametersIndicesOmegaFIM]
                namesParametersSigma = rowNamesSigma

                numberParametersMu = length( namesParametersMu )
                numberParametersOmega = length( namesParametersOmega )
                numberParametersSigma = length( namesParametersSigma )

                if ( typeOfFIM == "PopulationFim")
                {
                  FIMFixedEffects = as.matrix(matrixFisher[1:numberParametersMu, 1:numberParametersMu])
                  FIMRandomEffects = as.matrix(matrixFisher[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                             ( 1+numberParametersMu):dim(matrixFisher)[1]] )
                  correlationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])
                  correlationRandomEffects = as.matrix(correlationMatrix[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                          ( 1+numberParametersMu):dim(matrixFisher)[1]] )
                  # FIM criteria
                  detFim = det( matrixFisher )
                  DCriterion = getDcriterion( fimOfDesign )
                  condFIMFixedEffects = c()
                  condFIMRandomEffects = c()
                  condFIMFixedEffects = cond(FIMFixedEffects)
                  condFIMRandomEffects = cond(FIMRandomEffects)
                  criteriaFim = data.frame(detFim,condFIMFixedEffects,condFIMRandomEffects,DCriterion)
                  criteriaFim = t(criteriaFim)
                  rownames(criteriaFim) = c("Determinant", "Cond number fixed effects",
                                            "cond number variance components", "D-criterion")

                  # SE and RSE
                  SE = getSE( fimOfDesign )
                  RSE_mu = SE[1:numberParametersMu]/muValueParameters[parametersIndicesMuFIM] * 100
                  RSE_omega = SE[numberParametersMu+c(1:numberParametersOmega)]/omegaValueParameters[parametersIndicesOmegaFIM]*100
                  RSE_sigma = tail(SE, numberParametersSigma) / sigmaValue * 100
                  RSE = c( RSE_mu, RSE_omega, RSE_sigma )

                  valueParameters = c( muValueParameters[parametersIndicesMuFIM],
                                       omegaValueParameters[parametersIndicesOmegaFIM],
                                       sigmaValue )

                  se_rse = data.frame( valueParameters, SE, RSE )

                  rownames(se_rse) = colnames( matrixFisher )
                  colnames(se_rse) = c("Value", "SE","RSE (%)")

                }else if( typeOfFIM == "IndividualFim" )
                {
                  FIMFixedEffects = as.matrix(matrixFisher[1:numberParametersMu, 1:numberParametersMu])

                  FIMRandomEffects = as.matrix(matrixFisher[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                             ( 1+numberParametersMu):dim(matrixFisher)[1]] )

                  correlationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])

                  correlationRandomEffects = as.matrix(correlationMatrix[ ( 1+numberParametersMu):dim(matrixFisher)[1],
                                                                          ( 1+numberParametersMu):dim(matrixFisher)[1]] )
                  # FIM criteria
                  detFim = det( matrixFisher )
                  DCriterion = getDcriterion( fimOfDesign )
                  condFIMFixedEffects = c()
                  condFIMRandomEffects = c()
                  condFIMFixedEffects = cond(FIMFixedEffects)
                  condFIMRandomEffects = cond(FIMRandomEffects)
                  criteriaFim = data.frame(detFim,condFIMFixedEffects,condFIMRandomEffects,DCriterion)
                  criteriaFim = t(criteriaFim)
                  rownames(criteriaFim) = c("Determinant", "Cond number fixed effects", "cond number variance components", "D-criterion")

                  # SE and RSE
                  SE = getSE( fimOfDesign )

                  RSE_mu = SE[1:numberParametersMu]/muValueParameters[parametersIndicesMuFIM]*100
                  RSE_sigma =  SE[numberParametersMu + c(1:numberParametersSigma)]/sigmaValue*100
                  RSE = c( RSE_mu, RSE_sigma )

                  valueParameters = c( muValueParameters[parametersIndicesMuFIM],
                                       sigmaValue )

                  rowNamesParameters = c( namesParametersMu, namesParametersSigma )
                  se_rse = data.frame(valueParameters, SE, RSE)
                  rownames(se_rse) = rownames( matrixFisher )
                  colnames(se_rse) = c("Value", "SE","RSE (%)")

                }else if( typeOfFIM == "BayesianFim" )
                {

                  FIMFixedEffects = matrixFisher[1:numberParametersMu, 1:numberParametersMu]
                  correlationFixedEffects = as.matrix(correlationMatrix[1:numberParametersMu, 1:numberParametersMu])
                  FIMRandomEffects = NULL
                  correlationRandomEffects = NULL

                  # FIM criteria
                  detFim = det( matrixFisher )
                  DCriterion = getDcriterion( fimOfDesign )
                  condFIMFixedEffects = c()
                  condFIMRandomEffects = c()

                  condFIMFixedEffects = cond(FIMFixedEffects)
                  criteriaFim = data.frame(detFim, condFIMFixedEffects, DCriterion)
                  criteriaFim = t(criteriaFim)
                  rownames(criteriaFim) = c("Determinant", "Cond number fixed effects", "D-critera")

                  # SE, RSE and shrinkage
                  SE = getSE( fimOfDesign )
                  RSE = SE[1:numberParametersMu]/muValueParameters[parametersIndicesMuFIM] * 100
                  shrinkage = getShrinkage(fimOfDesign)
                  rowNamesParameters = colnames( matrixFisher )
                  se_rse = data.frame( muValueParameters[parametersIndicesMuFIM], SE, RSE, shrinkage )
                  colnames(se_rse) = c("Values", "SE","RSE (%)" ,"shrinkage")

                }

                # display summary

                cat("*************************\n" )
                cat("Fisher information matrix \n" )
                cat("*************************\n" )
                cat("\n" )
                cat("**** Fixed effect","\n\n" )
                print( FIMFixedEffects )
                cat("\n" )
                cat("**** Variance components","\n\n" )
                print( FIMRandomEffects )
                cat("\n" )
                cat("******************\n" )
                cat("Correlation matrix  \n" )
                cat("******************\n" )
                cat("\n" )
                cat("**** Fixed effect","\n\n" )
                print( correlationFixedEffects )
                cat("\n" )
                cat("**** Variance components","\n\n" )
                print( correlationRandomEffects )
                cat("\n" )
                cat("**********************************************\n" )
                cat("Determinant, condition numbers and D-criterion \n" )
                cat("**********************************************\n" )
                cat("\n" )
                print( criteriaFim, row.names = FALSE )
                cat("\n" )
                cat("**********\n" )
                cat("SE and RSE \n" )
                cat("**********\n" )
                cat("\n" )
                print( se_rse )
                cat("\n" )
              } # end test fim
            } # end loop design
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the weights for the optimal designs.
#'
#' @name getWeights
#' @param object A \code{PFIMProject} object.
#' @return A data frame \code{weights} giving the weights of the optimal designs.

setGeneric("getWeights",
           function(object)
           {
             standardGeneric("getWeights")
           }
)

setMethod("getWeights",
          signature = "PFIMProject",
          definition = function(object)
          {
            weights <- list()
            designs = getDesign( object )
            indexDesignsOptimization = which( lapply( designs, slot, "isOptimalDesign" ) == TRUE )
            designs = designs[indexDesignsOptimization]

            for(design in designs)
            {
              nameDesign = getNameDesign( design )

              if ( dim(getMfisher(getFimOfDesign(design)))[1] !=0){

                weights[[length(weights)+1]] = getWeightFrame(design@optimizationResult)
                names( weights ) = nameDesign
              }
            }
            return( weights )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Plot the D criteria over time.
#'
#' @name plotCriteria
#' @param object \code{PFIMProject} object.
#' @param ... A list giving the plot options.
#' @return A plot of the D criteria over iterations for a Design optimization.

setGeneric(
  "plotCriteria",
  function(object,...) {
    standardGeneric("plotCriteria")
  })

setMethod(f="plotCriteria",
          signature("PFIMProject"),

          function(object,...){

            optimalDesign = getDesign( object )
            optimizationResult = getOptimizationResult( optimalDesign[[2]] )
            results = optimizationResult@resultsOptimization
            maxIter = optimizationResult@maxIteration
            results = do.call(cbind, lapply(results, rev))
            results = as.data.frame(results)

            p <- ggplot(results, aes(Iteration,Criteria)) +
              theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
              labs(y = paste0("1/D"), x = paste0(paste0("Iteration")))+
              geom_step()+
              annotate("text",  x=Inf, y = Inf, label = paste0(" \n", "iter = ", max(results$Iteration)," \n" , "D = ",
                                                               round(1/min(results$Criteria),2)), vjust=1, hjust=1) +
              expand_limits( y = range(results$Criteria), x = 0) +
              coord_cartesian(xlim =c(0,maxIter), ylim = c(0, 1.1*max(results$Criteria)))+
              geom_point(results, mapping = aes(x = Iteration, y = Criteria),color="red")+
              scale_x_continuous(limits=c(0,maxIter),  breaks = scales::pretty_breaks(n = 10))+
              scale_y_continuous(limits=c(0,max(results$Criteria)),breaks = scales::pretty_breaks(n = 10))
            print(p)

          })

# -------------------------------------------------------------------------------------------------------------------
#' Plot the optimal weights for the Multiplicative algorithm.
#'
#' @name plotWeightOptimisation
#' @param object A \code{PFIMProject} object.
#' @param threshold A numeric giving the threshold for the weights.
#' @return A barplot of the optimal weights above the threshold.

setGeneric(
  "plotWeightOptimisation",
  function(object,threshold) {
    standardGeneric("plotWeightOptimisation")
  })

setMethod(f="plotWeightOptimisation",
          signature("PFIMProject"),
          function(object,threshold){

            listOutputs = list()

            weights = getWeights( object )

            namesDesigns = names( weights )

            for ( nameDesign in namesDesigns )
            {
              weights = weights[[ nameDesign ]]
              weights = as.data.frame( weights )
              weights = weights[, c("Arm_name", "Response", "Time_dose", "Amount_dose", "Sampling_times", "Number_of_subjects")]
              colnames( weights ) = c( "Arm_name","Response","Time_dose", "Amount dose","Sampling_times","Importance")
              weights =  split(weights, weights$Response )
              namesResponses = names( weights )

              # ordering names responses RespPK first
              if ( length( grep("RespPD",namesResponses) ) !=0 )
              {
                namesResponses = rev(namesResponses)
              }else{
                namesResponses = namesResponses
              }

              for ( nameResponse in namesResponses )
              {
                weightsResponse = weights[[nameResponse]]
                weightsResponse = weightsResponse[order(unlist(weightsResponse$Importance),decreasing = TRUE),]
                weightsResponse$Importance = as.numeric(weightsResponse$Importance)

                plotFrame = as.data.frame(subset(weightsResponse, weightsResponse$Importance>threshold))
                plotFrame = plotFrame[!duplicated(plotFrame[,c("Arm_name")]),]
                xlab = rev(seq(length(plotFrame$Arm_name)))
                plotFrame$x = xlab

                # filter response

                p <- ggplot(plotFrame, aes(x =  x, y = Importance)) +

                  theme(axis.text.x.top = element_text(angle = 90, hjust = 0,colour="red")) +

                  geom_bar(width = 0.5,position="identity", stat="identity") +

                  scale_y_continuous("\nWeights", limits=c(0,1.05),
                                     scales::pretty_breaks(n = 10), expand = c(0, 0)) +

                  scale_x_continuous("Arms \n",
                                     breaks = max(plotFrame$x):min(plotFrame$x),
                                     labels = ((plotFrame$Arm_name)),
                                     sec.axis = sec_axis(~ . * 1,
                                                         breaks = (plotFrame$x),
                                                         labels = (plotFrame$Sampling_times),
                                                         name = "Optimal sampling times \n  "))  +
                  coord_flip() +

                  ggtitle( paste0( nameDesign, "\n", "Response: ", nameResponse ) )

                listOutputs$plot[[ nameDesign ]][[nameResponse]] = p
                listOutputs$weightDataFrameTreshold[[ nameDesign ]][[nameResponse]] = plotFrame
                listOutputs$weightDataFrame[[ nameDesign ]][[nameResponse]] = weights
              }
            }
            return( listOutputs )
          })


# -------------------------------------------------------------------------------------------------------------------
#' Plot the frequencies for the FedorovWynn algorithm.
#'
#' @name plotFrequenciesOptimisation
#' @param object A \code{PFIMProject} object.
#' @return A barplot of the the frequencies for the FedorovWynn algorithm..

setGeneric(
  "plotFrequenciesOptimisation",
  function(object) {
    standardGeneric("plotFrequenciesOptimisation")
  })

setMethod(f="plotFrequenciesOptimisation",
          signature("PFIMProject"),
          function(object){

            listOutputs = list()

            designs = getDesign( object )

            indexDesignsOptimization = which( lapply( designs, slot, "isOptimalDesign" ) == TRUE )
            optimalDesign = designs[indexDesignsOptimization]
            nameOptimalDesign = names( optimalDesign )
            optimalDesign =  optimalDesign[[nameOptimalDesign]]
            numberOfArms = length( getArms( optimalDesign ) )
            optimizationResult = getOptimizationResult( optimalDesign )
            optimalFrequencies = optimizationResult@optimalFrequencies
            optimalFrequencies = optimalFrequencies[1:numberOfArms]

            plotFrame = summaryArmData( optimalDesign )
            colNamesPlotFrame = c("Arm_name","Response","Sampling_times")
            plotFrame = plotFrame[,colNamesPlotFrame]
            rownames( plotFrame ) = NULL
            plotFrame = split(plotFrame, plotFrame$Response )
            responseNames = names( plotFrame )

            for ( responseName in responseNames )
            {
              plotFrameResponse = plotFrame[[responseName]]
              x=1:dim(plotFrameResponse)[1]
              plotFrameResponse = cbind( x = x, plotFrameResponse, optimalFrequencies )

              p <- ggplot(plotFrameResponse, aes(x =  x, y = optimalFrequencies)) +

                theme(axis.text.x.top = element_text(angle = 90, hjust = 0,colour="red")) +

                geom_bar(width = 0.5,position="identity", stat="identity") +

                scale_y_continuous("\n Weights", limits=c(0,1.05),
                                   scales::pretty_breaks(n = 10), expand = c(0, 0)) +

                scale_x_continuous( "Arms \n", breaks = max(plotFrameResponse$x):min(plotFrameResponse$x),
                                    labels = ((plotFrameResponse$Arm_name)),
                                    sec.axis = sec_axis(~ . * 1,
                                                        breaks = (plotFrameResponse$x),
                                                        labels = (plotFrameResponse$Sampling_times),
                                                        name = paste0( "Optimal sampling times  \n  ")))  +

                ggtitle( paste0( nameOptimalDesign, "\n", "Response: ", responseName ) ) +

                coord_flip()

              listOutputs$plot[[ nameOptimalDesign ]][[responseName]] = p
            }
            return( listOutputs )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Plot the concentration over time of a model.
#'
#' @name plotResponse
#' @param object \code{PFIMProject} object.
#' @param plotOptions A list giving the plot options.
#' @return A list containing the plots of the concentration over time of a model.

setGeneric(
  "plotResponse",
  function(object, plotOptions ) {
    standardGeneric("plotResponse")
  })

setMethod(f="plotResponse",
          signature("PFIMProject"),

          function(object, plotOptions  ){

            # for plot
            object@statistical_model@computeFIM = FALSE

            # evaluate FIM
            evaluationPop  <- EvaluatePopulationFIM( object )

            # model components
            designs = getDesign( evaluationPop )
            statisticalModel = getStatisticalModel( object )
            responses = getResponsesStatisticalModel( statisticalModel )

            responseNames = names( responses )
            parameters = names( getModelParameters( statisticalModel ) )

            # options for plot
            if ( length ( plotOptions ) !=0 )
            {
              if( length( plotOptions$unitTime ) == 0 ){
                unitXAxis = c()
              }else{
                unitXAxis = plotOptions$unitTime
              }

              if( length( plotOptions$unitResponses ) == 0 ){
                unitYAxis = vector("list",length(responseNames))
              }else{
                unitYAxis = plotOptions$unitResponses
              }
              unitYAxis = as.list( unitYAxis )
              names( unitYAxis ) = responseNames
            }else{
              unitXAxis = c()
              unitYAxis = c()
            }

            # round arm size depending of the optimization algorithm
            optimizationAlgorithm = object@optimizationAlgorithm

            valueRoundSamplingTimes = 2

            if ( length( optimizationAlgorithm) !=0 ){

              valueRoundSamplingTimes = Inf

              if ( optimizationAlgorithm %in% c( "PSOAlgorithm","PGBOAlgorithm" ) )
              {
                valueRoundSamplingTimes = 4
              }
              else if ( optimizationAlgorithm %in% c( "MultiplicativeAlgorithm","FedorovWynnAlgortihm","SimplexAlgorithm" ) )
              {
                valueRoundSamplingTimes = 2
              }
            }

            # remove dose from parameters
            if (  "dose" %in% parameters )
            {
              parameters = parameters[ -grep( "dose", parameters ) ]
            }

            plotResponses = list()

            k = 1
            concentrationDesign = list()
            sensitivityIndiceDesign = list()

            # select designs if evaluation or population

            namePlotSamingTime ="Sampling times"

            if ( length( optimizationAlgorithm) !=0 )
            {
              indexDesignsOptimization = which( lapply( designs, slot, "isOptimalDesign" ) == TRUE )
              designs = designs[indexDesignsOptimization]

              namePlotSamingTime = "Optimal sampling times"
            }

            # plot
            for ( nameDesign in names( designs ) )
            {
              design = designs[[ nameDesign ]]

              arms = getArms( design )

              for ( arm in arms)
              {
                nameArm = getNameArm( arm )
                samplingsArms = getSamplings(arms[[nameArm]])
                concentrationDesign[[ nameArm ]] = design@concentration[[nameArm]]
                dataConcentrationDesign = concentrationDesign[[nameArm]]

                minXAxis = min( dataConcentrationDesign$time )
                maxXAxis = max( dataConcentrationDesign$time )
                minYAxis = 0
                maxYAxis = lapply(dataConcentrationDesign, FUN = max)
                maxYAxis[['time']] <- NULL

                for( responseName in responseNames )
                {
                  samplingTimeResponse = getSampleTime(samplingsArms[[responseName]])

                  tmp = dataConcentrationDesign[dataConcentrationDesign$time %in% samplingTimeResponse,  ]
                  tmp = as.data.frame( tmp )

                  dataConcentrationDesignSamplingTimeResponse = data.frame( time = tmp$time,
                                                                            response = tmp[, responseName ] )

                  dataConcentrationDesignSamplingTimeResponse$time = round( dataConcentrationDesignSamplingTimeResponse$time,
                                                                            valueRoundSamplingTimes )

                  dataConcentrationDesignPlot = data.frame( time = dataConcentrationDesign$time,
                                                            response = dataConcentrationDesign[,responseName])

                  # plot responses
                  p <- ggplot() +

                    theme(legend.position = "none",plot.title = element_text(hjust = 0.5),
                          axis.title.x.top = element_text(color = "red"),
                          axis.text.x.top = element_text(angle = 90, hjust = 0,
                                                         color = "red" ) ) +

                    labs(y = paste0(responseName," ", "(",unitYAxis[[responseName]],") \n"),
                         x = paste0(paste0("Time"," ", "(",unitXAxis,")"),
                                    "\n \n Design: ",  sub("_", " ",nameDesign),
                                    "      Arm: ",  nameArm)) +

                    geom_line(dataConcentrationDesignPlot, mapping = aes(x = time, y = response))+

                    geom_point(dataConcentrationDesignSamplingTimeResponse,
                               mapping = aes(x = time, y = response  ), color = "red") +

                    coord_cartesian(xlim=c( minXAxis, maxXAxis ), ylim=c( minYAxis, maxYAxis[[responseName]] ))+

                    scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                                       sec.axis = sec_axis(~ . * 1,
                                                           breaks = dataConcentrationDesignSamplingTimeResponse$time,
                                                           name = c(namePlotSamingTime ,"\n") ) ) +

                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

                  plotResponses[[k]] <- p

                  k = k + 1
                }
              }
            }
            return( plotResponses )
          }) # end method

# -------------------------------------------------------------------------------------------------------------------
#' Plot the sensitivity indices of a model over time.
#'
#' @name plotSensitivity
#' @param object \code{PFIMProject} object.
#' @param plotOptions A list giving the plot options.
#' @return A list containing the plots of the sensitivity indices of a model over time.

setGeneric(
  "plotSensitivity",
  function( object, plotOptions ) {
    standardGeneric("plotSensitivity")
  })

setMethod(f="plotSensitivity",
          signature("PFIMProject"),

          function( object, plotOptions ){

            # for plot
            object@statistical_model@computeFIM = FALSE

            # evaluate FIM
            MyEvaluationPop <- EvaluatePopulationFIM( object )

            # model components
            designs = getDesign( MyEvaluationPop )

            statisticalModel = getStatisticalModel( object )
            modelEquations = getEquationsStatisticalModel( statisticalModel )
            responses = getResponsesStatisticalModel( statisticalModel )

            responseNames = names( responses )

            if ( class( modelEquations ) %in% c("ModelODEquations","ModelInfusionODEquations" ) )
            {
              parameterInEquations  = checkParameterInEquations( statisticalModel )
              parameters = names( parameterInEquations )
            }else{
              modelParameters = getModelParameters( statisticalModel )
              parameters = names( modelParameters )
            }

            # remove dose from parameters
            if (  "dose" %in% parameters )
            {
              parameters = parameters[ -grep( "dose", parameters ) ]
            }

            # options for plot
            if ( length ( plotOptions ) !=0 )
            {
              if( length( plotOptions$unitTime ) == 0 ){
                unitXAxis = c()
              }else{
                unitXAxis = plotOptions$unitTime
              }
            }else{
              unitXAxis = c()
            }

            plotResponses = list()

            k = 1
            concentrationDesign = list()
            sensitivityIndiceDesign = list()
            plotSensitivityIndices = list()

            for ( nameDesign in names( designs ) )
            {
              design = designs[[ nameDesign ]]

              arms = getArms( design )

              for ( arm in arms)
              {
                nameArm = getNameArm( arm )
                samplingsArms = getSamplings(arms[[nameArm]])
                concentrationDesign = design@concentration[[nameArm]]
                sensitivityIndiceDesign = design@sensitivityindices[[nameArm]]

                minXAxis = 0
                maxXAxis = max( concentrationDesign$time )

                minYAxis = lapply(sensitivityIndiceDesign,FUN = min)
                maxYAxis = lapply(sensitivityIndiceDesign,FUN = max)

                minYAxis[['time']] <- NULL
                maxYAxis[['time']] <- NULL

                for( responseName in responseNames )
                {
                  minY = minYAxis[[responseName]]
                  maxY = maxYAxis[[responseName]]

                  samplingTimeResponse = getSampleTime(samplingsArms[[responseName]])

                  tmp = data.frame( time = concentrationDesign$time,
                                    parameters = sensitivityIndiceDesign[[responseName]] )

                  colnames(tmp) = c("time", parameters)


                  tmp2 = tmp[tmp$time %in% samplingTimeResponse, ]

                  for( nameParameter in parameters ){

                    dataSensitivityIndicesDesignPlot = data.frame( time = tmp$time,
                                                                   parameter = tmp[,nameParameter])

                    dataSensitivityIndicesSamplingTimeResponse = data.frame( time = tmp2$time,
                                                                             parameter = tmp2[,nameParameter])

                    p <- ggplot() +

                      theme(legend.position = "none",plot.title = element_text(hjust = 0.5),
                            axis.title.x.top = element_text(color = "red"),
                            axis.text.x.top = element_text(angle = 90, hjust = 0,
                                                           color = "red" ) ) +

                      labs(y = paste("df/d", nameParameter, sep=""),
                           x = paste0(paste0("Time"," ", "(",unitXAxis,")"),
                                      "\n \n Design: ",  sub("_", " ",nameDesign),
                                      "      Arm: ",  nameArm,
                                      "      Response: ",  responseName,
                                      "      Parameter: ",  nameParameter))+

                      geom_line(dataSensitivityIndicesDesignPlot, mapping = aes(x = time, y = parameter)) +

                      geom_point(dataSensitivityIndicesSamplingTimeResponse,
                                 mapping = aes(x = time, y = parameter  ), color = "red") +

                      coord_cartesian(xlim=c( minXAxis, maxXAxis ),
                                      ylim=c( minYAxis[[responseName]], maxY = maxYAxis[[responseName]] )) +

                      scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                                         sec.axis = sec_axis(~ . * 1,
                                                             breaks = dataSensitivityIndicesSamplingTimeResponse$time,
                                                             name = "Sampling Times \n")) +

                      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

                    plotSensitivityIndices[[k]] <- p

                    k = k + 1

                  }
                }
              }
            }
            return( plotSensitivityIndices )
          })

# -------------------------------------------------------------------------------------------------------------------
#' Plot the relative standard errors RSE of the model parameters.
#'
#' @name plotRSE
#' @param object \code{PFIMProject} object.
#' @return A list containing the plots of the RSE of the model parameters.

setGeneric(
  "plotRSE",
  function(object) {
    standardGeneric("plotRSE")
  })

setMethod(f="plotRSE",
          signature("PFIMProject"),

          function(object){

            plotlist_se = list()
            rowNamesSigma = list()

            allDesignsObject = getDesign(object)
            statisticalModel = getStatisticalModel(object)
            responses = getResponsesStatisticalModel( statisticalModel )
            namesResponses = names(responses)
            modelParameters = getModelParameters( statisticalModel )
            namesModelParameters = names(modelParameters)

            # mu values of the parameters
            muValueParameters = unlist( lapply( modelParameters, slot, "mu" ) )
            omegaValueParameters = unlist( lapply( modelParameters, slot, "omega" ) )
            omegaValueParameters = omegaValueParameters **2

            # For optimization report: keep optimal designs
            if ( length( object@optimizationAlgorithm )!= 0 )
            {
              indexOptimalDesigns = which( lapply( allDesignsObject, slot, "isOptimalDesign" ) == TRUE )
              object@designs <- allDesignsObject[ indexOptimalDesigns ]
              allDesignsObject = getDesign( object )
            }

            # get parameters and value for sigma

            valueSigma = c()

            for ( nameResponse in namesResponses )
            {
              modelError = getModelError(  responses[[nameResponse]] )
              sigmaInter = getSigmaInter( modelError )
              sigmaSlope = getSigmaSlope( modelError )

              if (sigmaInter != 0)
              {
                rowNamesSigma  =  append( rowNamesSigma, paste0("inter ",nameResponse) )
                valueSigma = c( valueSigma, sigmaInter )
              }
              if ( sigmaSlope != 0)
              {
                rowNamesSigma  =  append( rowNamesSigma, paste0("slope ",nameResponse) )
                valueSigma = c( valueSigma, sigmaSlope )
              }
            }

            rowNamesSigma = unlist( rowNamesSigma )

            for ( iterDesign in 1:length( allDesignsObject ) ){

              design = allDesignsObject[[iterDesign]]

              nameDesign = getNameDesign(design)

              fim <- getFimOfDesign( design )

              nameFIM = class(fim)[1]

              SE = getSE( fim )

              parametersIndicesMuFIM = fim@parametersIndicesMuFIM
              parametersIndicesOmegaFIM = fim@parametersIndicesOmegaFIM

              numberOfparametersIndicesMuFIM = length( parametersIndicesMuFIM )
              numberOfparametersOmegaMuFIM = length( parametersIndicesOmegaFIM )
              numberOfparametersSigmaFIM = length( rowNamesSigma )

              parametersMu = namesModelParameters[parametersIndicesMuFIM]
              parametersOmega = namesModelParameters[parametersIndicesOmegaFIM]
              parametersSigma = rowNamesSigma

              # FIMs
              if ( nameFIM == "PopulationFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'RSE~mu',length( parametersIndicesMuFIM ) ),
                                          rep( 'RSE~omega^2',length( parametersIndicesOmegaFIM ) ),
                                          rep( 'RSE~sigma',length( rowNamesSigma ) ) )

                rowNamesParameters = c( parametersMu, parametersOmega, parametersSigma )

                RSE_mu = SE[1:numberOfparametersIndicesMuFIM]/muValueParameters[parametersIndicesMuFIM] * 100
                RSE_omega = SE[numberOfparametersIndicesMuFIM+c(1:numberOfparametersOmegaMuFIM)]/omegaValueParameters[parametersIndicesOmegaFIM]*100
                RSE_sigma = tail(SE, numberOfparametersSigmaFIM) / valueSigma * 100

                RSE = c( RSE_mu, RSE_omega, RSE_sigma )
              }
              else  if ( nameFIM == "IndividualFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'RSE~mu',length( parametersIndicesMuFIM ) ),
                                          rep( 'RSE~sigma',length( rowNamesSigma ) ) )

                rowNamesParameters = c( parametersMu, parametersSigma )

                RSE_mu = SE[1:numberOfparametersIndicesMuFIM]/muValueParameters[parametersIndicesMuFIM] * 100
                RSE_sigma = tail(SE, numberOfparametersSigmaFIM) / valueSigma * 100
                RSE = c( RSE_mu, RSE_sigma )

              }
              else  if ( nameFIM == "BayesianFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'RSE~mu',length( parametersIndicesMuFIM ) ) )

                rowNamesParameters = c( parametersMu )

                RSE_mu = SE[1:numberOfparametersIndicesMuFIM]/muValueParameters[parametersIndicesMuFIM] * 100
                RSE = c( RSE_mu )
              }

              se_rse = data.frame( rowNamesParameters,
                                   RSE,
                                   rowNamesMuOmegaSigma )

              colnames(se_rse) = c("Parameters","value","RSE")

              # for break yaxis
              indMinMAx = which(se_rse$value>100)

              if ( length( indMinMAx ) !=0){

                thresholdsCut = sort(c(100,se_rse$value[indMinMAx]))

                if ( length( thresholdsCut ) %% 2 !=0 )
                {
                  thresholdsCut = thresholdsCut[-length(thresholdsCut)]
                }
              }

              # ggplot
              p = ggplot(se_rse,
                         aes(Parameters, value)) +

                theme(legend.position = "none",
                      plot.title = element_text(size=12, hjust = 0.5),
                      axis.text.x = element_text(size=10, angle = 90, vjust = 0.5))+

                geom_bar(stat="identity",
                         width = 0.5,
                         position = position_dodge2(preserve = "single")) +

                scale_x_discrete( guide = guide_axis(check.overlap = TRUE ) ) +

                facet_grid(. ~ RSE,
                           labeller=label_parsed,
                           scales="free",
                           space = "free") +

                labs(y = "Values (%)") +

                {if ( length( indMinMAx ) !=0 ){
                scale_y_break( thresholdsCut, scales="free")}}+

                scale_y_continuous( limits =c(0,max(se_rse$value)),
                                    breaks = scales::pretty_breaks(n = 5) ) +

                ggtitle(paste0(nameDesign,": " , nameFIM))

              plotlist_se[[iterDesign]] <- p
            }

            return(plotlist_se)

          })

# -------------------------------------------------------------------------------------------------------------------
#' Plot the standard errors SE of the model parameters.
#'
#' @name plotSE
#' @param object \code{PFIMProject} object.
#' @return A list containing the plots of the standard errors SE of the model parameters.

setGeneric(
  "plotSE",
  function(object) {
    standardGeneric("plotSE")
  })

setMethod(f="plotSE",
          signature("PFIMProject"),

          function(object){

            plotlist_se = list()
            rowNamesSigma = list()

            allDesignsObject = getDesign(object)
            statisticalModel = getStatisticalModel(object)
            responses = getResponsesStatisticalModel( statisticalModel )
            namesResponses = names(responses)
            modelParameters = getModelParameters( statisticalModel )
            namesModelParameters = names(modelParameters)

            # mu values of the parameters
            muValueParameters = unlist( lapply( modelParameters, slot, "mu" ) )

            # For optimization report: keep optimal designs
            if ( length( object@optimizationAlgorithm )!= 0 )
            {
              indexOptimalDesigns = which( lapply( allDesignsObject, slot, "isOptimalDesign" ) == TRUE )
              object@designs <- allDesignsObject[ indexOptimalDesigns ]
              allDesignsObject = getDesign( object )
            }

            for ( nameResponse in namesResponses )
            {
              modelError = getModelError(  responses[[nameResponse]] )
              sigmaInter = getSigmaInter( modelError )
              sigmaSlope = getSigmaSlope( modelError )

              if (sigmaInter != 0)
              {
                rowNamesSigma  =  append( rowNamesSigma, paste0("inter ",nameResponse) )
              }
              if ( sigmaSlope != 0)
              {
                rowNamesSigma  =  append( rowNamesSigma, paste0("slope ",nameResponse) )
              }
            }

            rowNamesSigma = unlist( rowNamesSigma )

            for ( iterDesign in 1:length( allDesignsObject ) ){

              design = allDesignsObject[[iterDesign]]

              nameDesign = getNameDesign(design)

              fim <- getFimOfDesign( design )

              nameFIM = class(fim)[1]

              SE = getSE( fim )
              RSE = SE/muValueParameters*100

              parametersIndicesMuFIM = fim@parametersIndicesMuFIM
              parametersIndicesOmegaFIM = fim@parametersIndicesOmegaFIM

              parametersMu = namesModelParameters[parametersIndicesMuFIM]
              parametersOmega = namesModelParameters[parametersIndicesOmegaFIM]
              parametersSigma = rowNamesSigma

              # FIMs
              if ( nameFIM == "PopulationFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'SE~mu',length( parametersIndicesMuFIM ) ),
                                          rep( 'SE~omega^2',length( parametersIndicesOmegaFIM ) ),
                                          rep( 'SE~sigma',length( rowNamesSigma ) ) )

                rowNamesParameters = c( parametersMu, parametersOmega, parametersSigma )
              }
              else  if ( nameFIM == "IndividualFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'SE~mu',length( parametersIndicesMuFIM ) ),
                                          rep( 'SE~sigma',length( rowNamesSigma ) ) )

                rowNamesParameters = c( parametersMu, parametersSigma )
              }
              else  if ( nameFIM == "BayesianFim")
              {
                rowNamesMuOmegaSigma = c( rep( 'SE~mu',length( parametersIndicesMuFIM ) ) )

                rowNamesParameters = c( parametersMu )
              }

              se_rse = data.frame( rowNamesParameters,
                                   SE,
                                   rowNamesMuOmegaSigma )

              colnames(se_rse) = c("Parameters","value","SE")

              # ggplot
              p = ggplot(se_rse,
                         aes(Parameters, value)) +

                theme(legend.position = "none",
                      plot.title = element_text(size=12, hjust = 0.5),
                      axis.text.x = element_text(size=10, angle = 90, vjust = 0.5))+

                geom_bar(stat="identity",
                         width = 0.5,
                         position = position_dodge2(preserve = "single")) +

                scale_x_discrete( guide = guide_axis(check.overlap = TRUE ) ) +

                facet_grid(. ~ SE, labeller= label_parsed, scales="free", space = "free") +

                labs(y = "Values") +

                ggtitle(paste0(nameDesign,": " , nameFIM))

              plotlist_se[[iterDesign]] <- p

            }

            return(plotlist_se)

          })


# -------------------------------------------------------------------------------------------------------------------
#' Plot the shrinkage data.
#'
#' @name plotShrinkage
#' @param object \code{PFIMProject} object.
#' @return A list containing the plots of the shrinkage of the model parameters.

setGeneric(
  "plotShrinkage",
  function(object) {
    standardGeneric("plotShrinkage")
  })

setMethod(f="plotShrinkage",
          signature("PFIMProject"),

          function(object){

            plotlist_shrinkage = list()
            latexSigma = list()

            allDesignsObject = getDesign(object)
            statisticalModel = getStatisticalModel(object)

            modelParameters = getModelParameters( statisticalModel )
            modelEquations = getEquationsStatisticalModel( statisticalModel )
            namesParameters = names( modelParameters )

            # For optimization report: keep optimal designs
            if ( length(object@optimizationAlgorithm)!=0 ){

              indexOptimalDesigns = which( lapply( allDesignsObject, slot, "isOptimalDesign" ) == TRUE )

              object@designs <- allDesignsObject[ indexOptimalDesigns ]

              allDesignsObject = getDesign( object )
            }

            for ( iterDesign in 1:length( allDesignsObject ) )
            {
              design = allDesignsObject[[iterDesign]]

              nameDesign = getNameDesign(design)

              fim <- getFimOfDesign( design )

              parametersIndicesMuFIM = fim@parametersIndicesMuFIM

              nameFIM = is(fim)[1]

              if( nameFIM %in% c("IndividualFim","PopulationFim") )
              {
                plotlist_shrinkage = list()

                return(plotlist_shrinkage)

              } else {

                shrinkage <- getShrinkage( fim )
                shrinkage = round( shrinkage, 4 )
                namesParameters = namesParameters[parametersIndicesMuFIM]
                dataShrinkage = data.frame( namesParameters, shrinkage )
                colnames( dataShrinkage ) = c("Parameters","shrinkage")

                p = ggplot(dataShrinkage, aes(Parameters, shrinkage)) +

                  theme(legend.position = "none",
                        plot.title = element_text(size=12, hjust = 0.5),
                        axis.text.x = element_text(size=10, angle = 0, vjust = 0.5))+

                  geom_bar(stat="identity",
                           width = 0.5,
                           position = position_dodge2( preserve = "single" ) ) +

                  coord_cartesian( ylim = c( 0, 100 ) ) +

                  scale_x_discrete( guide = guide_axis( check.overlap = TRUE ) ) +

                  labs(y = "Values (%)") +

                  ggtitle(paste0(nameDesign,": " , nameFIM))

                plotlist_shrinkage[[iterDesign]] <- p
              }
              return(plotlist_shrinkage)
            }
          })

# -------------------------------------------------------------------------------------------------------------------
#' Generate a htmal report html.
#'
#' @name reportPFIMProject
#' @param object \code{PFIMProject} object.
#' @param  ... A list giving options for the report.
#' @return an html givin a report of the project for evaluation or optimization

setGeneric(
  "reportPFIMProject",
  function( object, ... ) {
    standardGeneric("reportPFIMProject")
  })

setMethod(f="reportPFIMProject",
          signature("PFIMProject"),

          function( object, ... ){

            optimizationAlgorithm = object@optimizationAlgorithm

            listOptions = list(...)
            plotOptions = listOptions$plotOptions

            inputPath = "inst/rmarkdown/templates/skeleton/"

            if ( is.null( outputPath ) )
            {
              outputPath = "inst/report/"
            }

            # Report for evaluation
            if ( length( optimizationAlgorithm ) == 0 )
            {
              PFIMProjectReportEvaluation( object,
                                           inputPath = inputPath,
                                           outputPath = outputPath,
                                           plotOptions = plotOptions )

              # Report for optimization
            }else if ( length( optimizationAlgorithm ) != 0){

              PFIMProjectReportOptimization( object,
                                             inputPath = inputPath,
                                             outputPath = outputPath,
                                             plotOptions = plotOptions )
            }

          })

#####################################################################################################
# END Class "PFIMProject"
#####################################################################################################



