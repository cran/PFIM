##################################################################################
##' Class "ModelInfusionEquations" representing a model with infusion equations
##'
##' @description
##' A class giving information on the infusion equations regarding the equations of the model.
##'
##' @name ModelInfusionEquations-class
##' @aliases ModelInfusionEquations
##' @include ModelEquations.R
##' @docType class
##' @exportClass ModelInfusionEquations
##'
##' @section Objects from the class: \code{{ModelInfusionEquations}} objects are typically created by calls to \code{{ModelInfusionEquations}} and contain the following slots:
##'
##' \describe{
##' \item{\code{object}:}{An object from the class \code{ModelEquations}}
##' }

ModelInfusionEquations<-setClass(
  Class = "ModelInfusionEquations",
  contains = "ModelEquations",
  representation = representation(),
  validity = function(object)
  {
    return(TRUE)
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the Infusion Equations.
#' @name getInfusionEquations
#' @param object A \code{ModelInfusionEquations} object.
#' @return A list \code{equations} of the expressions giving the infusion equations of the  \code{ModelInfusionEquations} object

setGeneric(
  "getInfusionEquations",
  function(object) {
    standardGeneric("getInfusionEquations")
  })

setMethod("getInfusionEquations",
          signature("ModelInfusionEquations"),
          function(object)
          {
            return(object@equations)
          })



setMethod(
  "getEquationsModelPKPD",
  signature("ModelInfusionEquations","ModelEquations"),
  function(modelEquationsPKmodel, modelEquationsPDmodel){

    # verifier type infusion

    equationPKmodel = getInfusionEquations(modelEquationsPKmodel)
    equationPDmodel = modelEquationsPDmodel@equations

    # names for the equations of the PDmodel with infusion
    equationInfusionPDModel = vector("list", length = 2)
    names(equationInfusionPDModel) = c("DuringInfusion_RespPD", "AfterInfusion_RespPD")

    # change the variables
    equationInfusionPDModel[[1]] = as.expression(do.call('substitute', list(equationPDmodel[[1]][[1]], list(RespPK = quote(DuringInfusion_RespPK)))))
    equationInfusionPDModel[[2]] = as.expression(do.call('substitute', list(equationPDmodel[[1]][[1]], list(RespPK = quote(AfterInfusion_RespPK)))))

    output = ModelInfusionEquations(c(equationPKmodel,equationInfusionPDModel))

    return(output)

  })

# -------------------------------------------------------------------------------------------------------------------
#' Evaluate an analytic model in infusion.
#'
#' @rdname EvaluateModelInfusion
#' @param object A \code{ModelInfusionEquations} object.
#' @param samplingTimesModel A vector giving the sampling times of the model
#' @param inputsModel A list containing the inputs used for the model evaluation.
#' @param computeFIM A boolean if the FIM is computed or not.
#' @return A list containing the evaluated responses and the gradients.

setGeneric("EvaluateModelInfusion",
           function(object, samplingTimesModel,inputsModel, computeFIM)
           {
             standardGeneric("EvaluateModelInfusion")
           }
)

setMethod(f = "EvaluateModelInfusion",
          signature = "ModelInfusionEquations",
          definition = function(object, samplingTimesModel, inputsModel, computeFIM)
          {

            evaluateModelInfusionEquations = function(object, samplingTimesModel, inputsModel, computeFIM )
            {
              variablesNames = inputsModel$variablesNames
              responsePKNames = inputsModel$responsePKNames
              responsePDNames = inputsModel$responsePDNames
              responseNames = c( responsePKNames, responsePDNames )
              modelParameters = inputsModel$model_parameters
              nameModelParameters = names( modelParameters )
              variablesNames = inputsModel$variablesNames

              numberOfParameters = length( names( modelParameters ) )
              numberOfResponses = length( responsePKNames ) + length( responsePDNames )
              numberOfVariables = length( variablesNames )
              numberOfEquations = length( inputsModel$modelEquations@equations )

              # lag tau
              for ( nameRespPK in responsePKNames )
              {
                tau = inputsModel$tau[[nameRespPK]]

                if ( tau !=0 )
                {
                  maxSamplingTimeResponse = max( inputsModel$sampleTimeResponse[[nameRespPK]] )
                  n = maxSamplingTimeResponse%/%tau

                  inputsModel$time_dose[[nameRespPK]] =  c(0:n)*tau

                  l = length( inputsModel$time_dose[[nameRespPK]] )

                  inputsModel$Tinf[[nameRespPK]] = rep( inputsModel$Tinf[[nameRespPK]], l )
                  inputsModel$dose[[nameRespPK]] = rep( inputsModel$dose[[nameRespPK]], l )

                }
              }

              # set initial conditions
              for( parameter in modelParameters )
              {
                name = getNameModelParameter( parameter )
                assign( name, getMu( parameter ) )
              }

              if ( computeFIM == FALSE )
              {
                for ( nameResponse in responseNames )
                {
                  inputsModel$sampleTimeResponse[[nameResponse]] = samplingTimesModel
                }
              }

              # results
              k = 1
              data_for_evaluation = data.frame()
              testAllGradAreOne = c()

              dC_during_infusion=list()
              dC_after_infusion=list()

              # model equations
              equations = inputsModel$modelEquations@equations
              indexDuringInfusion = which(lapply( names( equations ), function(x) grep("DuringInfusion_", x))!=0)
              indexAfterInfusion = which(lapply( names( equations ), function(x) grep("AfterInfusion_", x))!=0)

              dC_during_infusion = equations[indexDuringInfusion]
              dC_after_infusion  = equations[indexAfterInfusion]

              dC = matrix(0.0,length(samplingTimesModel),numberOfResponses)
              dC_gradient = matrix(0.0,length(samplingTimesModel),numberOfResponses)

              # set number of variables
              numberOfVariables = length(responsePKNames)+length(responsePDNames)

              # set initial conditions
              for( parameter in modelParameters )
              {
                name = getNameModelParameter( parameter )
                assign( name, getMu( parameter ) )
              }

              # infusion equations
              dC_eval_during_infusion = c()
              dC_eval_after_infusion = c()
              res = list()

              # set administration parameters
              dose = list()
              Tinfs = list()
              tau = list()
              time = list()
              time_dose = list()
              time_matrix  = list()
              index_time = list()
              k = 1

              # data for evaluation

              dataForEvaluation = list()
              responses = list()
              timeDoseInfusion = samplingTimesModel

              duringAndAfter = rep("After",length(samplingTimesModel))
              duringAndAfter[1] = "No calcul"

              for ( nameRespPK in responsePKNames )
              {
                time_dose[[nameRespPK]] = inputsModel$time_dose[[nameRespPK]]

                dose[[nameRespPK]] = inputsModel$dose[[nameRespPK]]

                Tinfs[[nameRespPK]] = inputsModel$Tinf[[nameRespPK]]

                assign( paste0("dose_DuringInfusion_",nameRespPK), dose[[nameRespPK]] )
                assign( paste0("dose_AfterInfusion_",nameRespPK), dose[[nameRespPK]] )
                assign( "Tinf", Tinfs[[nameRespPK]] )

                responses[[nameRespPK]] = rep( nameRespPK, length(samplingTimesModel))

                time_matrix[[nameRespPK]] = matrix( ( c( time_dose[[nameRespPK]],
                                                         time_dose[[nameRespPK]] + Tinfs[[nameRespPK]] ) ),
                                                    length( time_dose[[nameRespPK]] ), 2 )

                # indices for doses and tinf
                indicesDoses = c()
                indicesDosesForEvalInfusion = c()
                indicesDoses[1] = 1
                timeDoseInfusionEnd = c()
                indexLastInfusionToEnd = c( rep( 0.0,length( samplingTimesModel ) ) )

                vec = c( time_dose[[nameRespPK]], max( sort( unique( c( samplingTimesModel, Tinfs[[nameRespPK]] ) ) ) ) )

                for (k in 1:length(vec))
                {
                  indicesDoses[samplingTimesModel >= vec[k] & samplingTimesModel <=vec[k+1]] = k
                }

                # for infusion during + after
                for (k in 1:length( indicesDoses) )
                {
                  if ( indicesDoses[k] > 1){
                    indicesDosesForEvalInfusion[k] = indicesDoses[k]-1
                  }else{
                    indicesDosesForEvalInfusion[k] = indicesDoses[k]
                  }
                }

                k=1
                for ( t in samplingTimesModel )
                {
                  assign("t", t)

                  index_time[[nameRespPK]] = which( apply( time_matrix[[nameRespPK]], 1, findInterval, x = t ) == 1)

                  timeDoseInfusionEnd[k] = samplingTimesModel[k] - time_dose[[nameRespPK]][indicesDoses[k]]

                  if ( length( index_time[[nameRespPK]]  ) != 0  )
                  {
                    duringAndAfter[k] = "during"

                    timeDoseInfusion[k] = samplingTimesModel[k] - time_dose[[nameRespPK]][indicesDoses[k]]

                  }else{
                    duringAndAfter[k] = "after"
                    if ( indicesDoses[k]>1){
                      indexLastInfusionToEnd[k] = 1
                    }
                  }
                  k=k+1
                }

                dataForEvaluation[[nameRespPK]] = data.frame( samplingTimesModel = samplingTimesModel,
                                                              timeDoseInfusion = timeDoseInfusion,
                                                              duringAndAfter = duringAndAfter,
                                                              indicesDoses = indicesDoses,
                                                              indexLastInfusionToEnd = indexLastInfusionToEnd,
                                                              timeDoseInfusionEnd = timeDoseInfusionEnd,
                                                              indicesDosesForEvalInfusion = indicesDosesForEvalInfusion,
                                                              responses = responses )
              }

              dataForEvaluation = do.call( "rbind", dataForEvaluation )
              dataForEvaluation = dataForEvaluation[ order( dataForEvaluation$samplingTimesModel ), ]
              rownames( dataForEvaluation ) <- 1:dim( dataForEvaluation )[1]

              # indices for Before/After infusion when time = infusion time
              index = match( unique( dataForEvaluation$indicesDoses ), dataForEvaluation$indicesDoses)
              dataForEvaluation$indexChange=0
              dataForEvaluation$indexChange[index]=1
              tmp=c()

              # model evaluation
              for( k in 1:dim(dataForEvaluation)[1]){

                samplingTimes  = dataForEvaluation$samplingTimesModel[k]
                timeDoseInfusion  = dataForEvaluation$timeDoseInfusion[k]
                duringAndAfter  = dataForEvaluation$duringAndAfter[k]
                indicesDoses  = dataForEvaluation$indicesDoses[k]
                indicesDosesForEvalInfusion = dataForEvaluation$indicesDosesForEvalInfusion[k]
                nameRespPK  = dataForEvaluation$RespPK[k]
                indexLastInfusionToEnd = dataForEvaluation$indexLastInfusionToEnd[k]
                timeDoseInfusionEnd =  dataForEvaluation$timeDoseInfusionEnd[k]

                ## time : [0, last time infusion]
                # infusion during
                if( duringAndAfter == "during")
                {
                   if( dataForEvaluation$indexChange[k] == 1 ){

                     if ( k>1)
                     {
                       timeDoseInfusionEndBefore = dataForEvaluation$timeDoseInfusionEnd[k-1]
                     }else{
                       timeDoseInfusionEndBefore = dataForEvaluation$timeDoseInfusionEnd[1]

                     }
                     assign( "t", timeDoseInfusionEndBefore )
                     assign( paste0("dose_DuringInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                     assign( paste0("dose_AfterInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                     assign( "Tinf", Tinfs[[nameRespPK]][indicesDoses] )

                     for ( iter in 1:numberOfVariables )
                     {
                       tmp[iter] = c( eval( dC_after_infusion[[iter]] ) )
                     }
                   }

                  assign( "t", timeDoseInfusion )
                  assign( paste0("dose_DuringInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                  assign( paste0("dose_AfterInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                  assign( "Tinf", Tinfs[[nameRespPK]][indicesDoses] )

                  for ( iter in 1:numberOfVariables )
                  {
                    dC[k,iter]  = c( eval( dC_during_infusion[[iter]] ))
                  }

                  if ( time_dose[[nameRespPK]][indicesDoses] > 0 )
                  {
                    assign( "t", samplingTimes )
                    assign( paste0("dose_DuringInfusion_",nameRespPK), dose[[nameRespPK]][indicesDosesForEvalInfusion] )
                    assign( paste0("dose_AfterInfusion_",nameRespPK), dose[[nameRespPK]][indicesDosesForEvalInfusion] )
                    assign( "Tinf", Tinfs[[nameRespPK]][indicesDosesForEvalInfusion] )

                    # test for gradient all responses equal to 1
                    testAllGradAreOne = c(unlist(lapply(dC_during_infusion,function(x){is.numeric(x[[1]])})))

                    if ( all(testAllGradAreOne)==TRUE)
                    {
                      for ( iter in 1:numberOfVariables )
                      {
                        dC[k,iter]  =   c( eval( dC_after_infusion[[iter]] ) )
                      }
                    }
                    else
                    {
                      for ( iter in 1:numberOfVariables )
                      {
                        dC[k,iter]  =  dC[k,iter] + c( eval( dC_after_infusion[[iter]] ) ) + tmp[iter]
                      }
                    }
                  }
                }

                # infusion after
                else  if( duringAndAfter == "after")
                {
                  # from 0 to last time infusion
                  if (indexLastInfusionToEnd == 0){

                    assign("t", samplingTimes)
                    assign( paste0("dose_DuringInfusion_",nameRespPK), dose[[nameRespPK]][indicesDoses] )
                    assign( paste0("dose_AfterInfusion_",nameRespPK), dose[[nameRespPK]][indicesDoses] )
                    assign( "Tinf", Tinfs[[nameRespPK]][indicesDoses] )

                    for ( iter in 1:numberOfVariables )
                    {
                      dC[k,iter]  = c(eval( dC_after_infusion[[iter]] ) )
                    }
                  }
                  # from last time dose to end sampling time
                  else if  (indexLastInfusionToEnd == 1){

                    assign("t", timeDoseInfusionEnd)
                    assign( paste0("dose_DuringInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                    assign( paste0("dose_AfterInfusion_",nameRespPK ), dose[[nameRespPK]][indicesDoses] )
                    assign( "Tinf", Tinfs[[nameRespPK]][indicesDoses] )

                    for ( iter in 1:numberOfVariables )
                    {
                      dC[k,iter]  = c( eval( dC_after_infusion[[iter]] ) )
                    }

                    assign( "t", samplingTimes )
                    assign( paste0("dose_DuringInfusion_",nameRespPK), dose[[nameRespPK]][indicesDosesForEvalInfusion] )
                    assign( paste0("dose_AfterInfusion_",nameRespPK), dose[[nameRespPK]][indicesDosesForEvalInfusion] )
                    assign( "Tinf", Tinfs[[nameRespPK]][indicesDosesForEvalInfusion] )

                    if ( all(testAllGradAreOne)==TRUE)
                    {
                      for ( iter in 1:numberOfVariables )
                      {
                        dC[k,iter]  =   c( eval( dC_after_infusion[[iter]] ) )
                      }
                    }
                    else
                    {
                      for ( iter in 1:numberOfVariables )
                      {
                        dC[k,iter]  =  dC[k,iter] + c( eval( dC_after_infusion[[iter]] ) )
                      }
                    }
                  }
                }
              }

              dC = as.data.frame(dC)
              out_response = cbind(samplingTimesModel, dC)
              names_out = c("time",responsePKNames,responsePDNames)
              names( out_response ) = names_out[names_out!=""]

              return( list( out_response = out_response ))
            }

            evaluation = evaluateModelInfusionEquations(object,
                                                        samplingTimesModel,
                                                        inputsModel,
                                                        computeFIM)

            out_response = evaluation$out_response

            # ----------------------------------------------------------------------------------------
            # compute gradient for sensitivity indices
            # ----------------------------------------------------------------------------------------

            equations = inputsModel$modelEquations@equations
            out_response_gradient = list()

            modelParameters = inputsModel$model_parameters
            nameModelParameters = names( modelParameters )

            responsePKNames = inputsModel$responsePKNames
            responsePDNames = inputsModel$responsePDNames
            responseNames = c( responsePKNames, responsePDNames )

            # sampling time for plot response and sensitivity indices
            if ( computeFIM == FALSE )
            {
              for ( nameResponse in responseNames )
              {
                inputsModel$sampleTimeResponse[[nameResponse]] = samplingTimesModel
              }
            }


            for( nameParameter in nameModelParameters )
            {
              i=1
              for ( equation in equations )
              {
                gradientEquation = sapply( nameParameter, function(x){ D( equation, x ) } )
                if ( is.numeric( gradientEquation[[nameParameter]]) ){
                }
                inputsModel$modelEquations@equations[[i]] = as.expression( gradientEquation[[nameParameter]] )
                i=i+1
              }

              evaluationModel = evaluateModelInfusionEquations(object,
                                                               samplingTimesModel,
                                                               inputsModel,
                                                               computeFIM)

              out_response_gradient[[nameParameter]] = evaluationModel$out_response
            }

            responseGradient = list()

            for ( responseName in responseNames )
            {
              tmp = lapply( out_response_gradient, "[[" , responseName )

              responseGradient[[responseName]] = t( do.call( "rbind", tmp ) )

              indexSamplingTimes = which( samplingTimesModel %in% inputsModel$sampleTimeResponse[[responseName]] )

              responseGradient[[responseName]] = responseGradient[[responseName]][ indexSamplingTimes ,]

            }

            responseAllGradient = list()
            responseAllGradient = do.call( rbind, responseGradient )

            return( list( evaluationResponse = out_response,
                          responseGradient = responseGradient,
                          responseAllGradient = responseAllGradient) )

          }) # end method

# ########################################################################################################################
# END Class ModelInfusionEquations
# ########################################################################################################################


