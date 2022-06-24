##################################################################################
#' Class "ModelEquations" representing a the equations of a model.
#'
#' @description
#' A class storing information concerning the model equations of the models in the \linkS4class{LibraryOfModels}.
#'
#' @name ModelEquations-class
#' @aliases ModelEquations
#' @docType class
#' @exportClass ModelEquations
#' @section Objects from the class \code{ModelEquations}:
#' Objects form the Class \code{ModelEquations} can be created by calls of the form \code{ModelEquations(...)} where
#' (...) are the parameters for the \code{ModelEquations} objects.
#'
#' @section Slots for  \code{ModelEquations} objects:
#' \describe{
#' \item{\code{equations}:}{A list giving the equations of the model.}
#' \item{\code{allParameters}:}{A vector giving all the parameters of the model.}
#' }


ModelEquations <- setClass(Class = "ModelEquations",
                           representation = representation
                           (
                             equations = "list",
                             allParameters = "vector"
                           ))


# -------------------------------------------------------------------------------------------------------------------
#' Function to remplace a dose.
#' @name  remplaceDose
#' @param ex parameter ex
#' @param progression parameter progression
#' @param all parameter all
#' @return expression of the equation with dose expression.
#'
remplaceDose<-function( ex, progression, all )
{
  for( i in 1:length( ex ) )
  {
    if ( is.symbol( ex[[i]] ) )
      if ( ex[[ i ]] == "dose" )
      {
        progression=c(progression,i)
        all[[ length(all) + 1 ]] = progression
      }
    if ( !is.symbol( ex[[ i ]] ) && !is.double( ex[[ i ]] ) )
    {
      all<-remplaceDose( ex[[ i ]], c(progression,i), all )
    }
  }
  return(all)
}


setMethod(
  f="initialize",
  signature="ModelEquations",
  definition= function (.Object, equations )
  {

    validObject(.Object)

    allParams = list()
    replaceIn = list()
    exception = c( "t", "dose", "Tinf", "tau" )
    for( equationName in names( equations ) )
    {
      e = equations[[ equationName ]]

      ### Replace dose with "dose_RespI"

      all<-remplaceDose(e , c(), list())
      if ( length( all ) > 0 )
      {
        for( i in 1:length( all ))
        {
          l<-all[[ i ]]
          m="e[["
          for( j in 1:length( l ))
          {
            v=paste0("all[[",i,"]]")
            m=paste0(m, v, "[",j,"]]][[")
          }
          m=paste0(m,"]]")

          options(useFancyQuotes = FALSE)
          str=m
          eval(parse(text=paste(strtrim(str,nchar(str)-4),paste0("=\"dose_",equationName,"\"" ) )))
        }
        str = sQuote( e )
        withoutQuote = gsub( "\'", "", str )
        withoutQuote = gsub( "\"", "", withoutQuote )

        equations[[ equationName ]] = parse( text = withoutQuote )
      }

      ### Parse all parameters of the equation
      equationParams = all.vars( equations[[ equationName ]] )
      for( param in equationParams )
      {
        if ( param %in% names( equations ) )
        {
          replaceIn[[ equationName ]]<- param
        }
        else
          if ( !( param %in% exception ) )
          {
            allParams[[ param ]] = 1
          }
      }
    }

    # Recursive responses
    for( equationName in names( equations ) )
    {
      assign( equationName, equations[[ equationName ]] )
    }
    options(useFancyQuotes = FALSE)
    for( replace in names( replaceIn ) )
    {
      f = sQuote( equations[[ replace ]] )
      g = paste( "(", sQuote( equations[[ replaceIn[[ replace ]] ]] ), ")" )
      quoted = gsub( replaceIn[[ replace ]], g, f )
      withoutQuote = gsub( "\'", "", quoted )

      equations[[ replace ]]<-parse( text = withoutQuote )
    }


    .Object@allParameters = names( allParams )



    if(!missing(equations))
      .Object@equations <- equations


    return ( .Object )
  }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the parameters of a \code{ModelEquations} object.
#' @name getParameters
#' @param object A \code{ModelEquations} object.
#' @return A vector \code{allParameters} giving the parameters of the model.

setGeneric("getParameters",
           function(object)
           {
             standardGeneric("getParameters")
           }
)
setMethod("getParameters",
          "ModelEquations",
          function(object)
          {
            return( object@allParameters )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Get the number of parameters of a \code{ModelEquations} object.
#' @name getNumberOfParameters
#' @param object A \code{ModelEquations} object.
#' @return A numeric \code{allParameters} giving the number of parameters.


setGeneric("getNumberOfParameters",
           function(object)
           {
             standardGeneric("getNumberOfParameters")
           }
)
setMethod("getNumberOfParameters",
          "ModelEquations",
          function(object)
          {
            return( length( object@allParameters ) )
          }
)


# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of the PK and PD models of a \code{ModelEquations} object.
#' @name getEquationsModelPKPD
#' @param modelEquationsPKmodel An expression giving the equation of the PK model.
#' @param modelEquationsPDmodel An expression giving the equation of the PD model.
#' @return A list \code{output} giving:
#' \itemize{
#' \item{expressions for the equations of the PK and PD models}{, for an analytic PK and PD models.}
#' \item{expressions for the equations of the PK and the infusion equations PD models}{, for a PK model in infusion.}
#' \item{expressions for the equations of the PK and PD models}{, for an ODE PK and PD models.}
#'}

setGeneric(
  "getEquationsModelPKPD",
  function(modelEquationsPKmodel, modelEquationsPDmodel) {
    standardGeneric("getEquationsModelPKPD")
  })

# -------------------------------------------------------------------------------------------------------------------
#' Get the equation of a \code{ModelEquations} object with respect to its name.
#' @rdname getEquation
#' @param object A \code{ModelEquations} object.
#' @param equationName A character string giving the name of the response of the equations.
#' @return An expression  \code{equations} giving the equation of a model with respect to its name.

setMethod("getEquation",
          "ModelEquations",
          function(object, equationName)
          {
            return(object@equations[[ equationName ]])
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the equations of a \code{ModelEquations} object.
#' @rdname getEquations
#' @aliases getEquations
#' @param object A \code{ModelEquations} object.
#' @return A list of expression \code{equations} giving the equations of the model after the change of variable in the model.

setGeneric("getEquations",
           function(object)
           {
             standardGeneric("getEquations")
           })

setMethod("getEquations",
          "ModelEquations",
          function(object)
          {
            return(object@equations)
          })

# -------------------------------------------------------------------------------------------------------------------
#' Get the index of the response of a \code{ModelEquations} object.
#' @name getResponseIndice
#' @param object A \code{ModelEquations} object.
#' @param equationName A character string giving the name of the response of the equations.
#' @return A numeric giving the index of the equation in the model.

setGeneric("getResponseIndice",
           function(object, equationName)
           {
             standardGeneric("getResponseIndice")
           }
)
setMethod("getResponseIndice",
          "ModelEquations",
          function(object, equationName)
          {
            responseNames<-names( unlist( object@equations ) )

            return( which( responseNames == equationName ) )
          }
)

#' Get the derivate of an equation of a \code{ModelEquations} object.
#'
#' @param object A \code{ModelEquations} object.
#' @param equationName A character string giving the name of the response of the equations.
#' @param parameter An \code{ModelParameter} object.
#'
#' @return A list of expression of the derivate of an equation of a \code{ModelEquations}
#'
#' @rdname getDerivate
#'
#' @docType methods

setGeneric("getDerivate",
           function(object, equationName, parameter )
           {
             standardGeneric("getDerivate")
           }
)

#' @rdname getDerivate

setMethod("getDerivate",
          "ModelEquations",
          function(object, equationName, parameter )
          {
            return( D( object@equations[[ equationName ]], parameter ) )
          }
)

# -------------------------------------------------------------------------------------------------------------------
#' Convert an equation of a PD model of a \code{ModelEquations} object from analytic to ODE.
#' @name convertAnalyticToODE
#' @param object \code{ModelEquations} object.
#' @return A list of expression \code{output} giving the equations of the analytic PD model in ODE form.

setGeneric(
  "convertAnalyticToODE",

  function(object) {
    standardGeneric("convertAnalyticToODE")
  })

setMethod(
  "convertAnalyticToODE",
  signature("ModelEquations"),
  function(object){

    output = list()

    # get the equations of the PKmodel
    equationsModelPKmodel = object@equations

    # equation for the PKmodel is an ODE of the form : dC/dc+kC=B
    for (i in 1:length(equationsModelPKmodel)){

      B = expression(dtEquationPK + k*equationPK)

      equationPKsubstitute = equationsModelPKmodel[[i]][[1]]

      dtEquationPKsubstitute = D(equationPKsubstitute, "t")

      B = Simplify(do.call('substitute', list(B[[1]], list(equationPK = equationPKsubstitute,
                                                           k = quote(Cl/V),
                                                           dtEquationPK = dtEquationPKsubstitute))))

      expr = expression(dCdt - (Cl/V)*RespPK)

      dCdt = Simplify(do.call('substitute', list(expr[[1]], list(dCdt = B))))
      dCdt = as.expression(dCdt)

      output[[i]] = dCdt

    }

    return(output)

  })

# ########################################################################################################################

#' Evaluate an analytic model.
#' @rdname EvaluateModel
#' @param object An object \code{EvaluateModel}.
#' @param samplingTimesModel A vector containing the sampling times.
#' @param inputsModel A list containing the models input.
#' @param computeFIM A boolean for computing the FIM or not (plot or evaluation).
#' @return A list containing the evaluated responses and the gradients.

setGeneric("EvaluateModel",
           function(object, samplingTimesModel, inputsModel, computeFIM)
           {
             standardGeneric("EvaluateModel")
           })

setMethod(f = "EvaluateModel",
          signature="ModelEquations",
          definition= function(object, samplingTimesModel, inputsModel, computeFIM)
          {

            # ----------------------------------------------------------------------------------
            # evaluate responses
            # ----------------------------------------------------------------------------------

            evaluateModelEquations = function(object, samplingTimesModel, inputsModel, computeFIM, flagMultiDosesPD )
            {
              variablesNames = inputsModel$variablesNames
              responsePKNames = inputsModel$responsePKNames
              responsePDNames = inputsModel$responsePDNames
              responseNames = c( responsePKNames, responsePDNames )
              modelParameters = inputsModel$model_parameters
              nameModelParameters = names( modelParameters )

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

                  inputsModel$time_dose[[nameRespPK]] = c(0:n)*tau

                  l = length( inputsModel$time_dose[[nameRespPK]] )

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

              # set number of variables
              numberOfVariables = length(responsePKNames)+length(responsePDNames)

              # model equations
              dC = inputsModel$modelEquations@equations

              dC_eval_RespPK = matrix(0.0, length(samplingTimesModel), length(responsePKNames))
              dC_eval_RespPD = matrix(0.0, length(samplingTimesModel), length(responsePDNames))

              # set administration parameters
              time_dose = list()
              dose=list()
              indices_doses = c()

              for ( nameRespPK in responsePKNames )
              {
                time_dose[[nameRespPK]] = inputsModel$time_dose[[nameRespPK]]
                dose[[nameRespPK]] = inputsModel$dose[[nameRespPK]]

                # matrix to stack the data : sampling times, time doses and doses
                time_evaluation = matrix(samplingTimesModel, length(samplingTimesModel),length( time_dose[[nameRespPK]] ))

                for ( i in 1:dim(time_evaluation)[1] )
                {
                  for ( j in 1 :length( time_dose[[nameRespPK]]  ) )
                  {
                    if( time_evaluation[i,j]-time_dose[[nameRespPK]][j] > 0)
                    {
                      time_evaluation[i,j] = time_evaluation[i,j]-time_dose[[nameRespPK]][j]
                    }
                  }
                  indices_doses[i]  = length(unique(time_evaluation[i,]))
                }


                time_evaluation = cbind( time_evaluation, indices_doses )
                time_evaluation = as.data.frame(time_evaluation)

                for ( j in 1:dim(time_evaluation)[1]  )
                {
                  indicesDoses = time_evaluation$indices_doses[j]

                  # doses and timedoses
                  doses =  dose[[nameRespPK]][1:indicesDoses]
                  time = sort(unique(unlist(c(time_evaluation[j,1:length(dose[[nameRespPK]])]))),decreasing=TRUE)

                  # evaluation model equations PK
                  for ( iter in 1:length(responsePKNames))
                  {
                    tmp = c()

                    for ( i in 1:length(doses) ){

                      assign( "t", time[i] )
                      assign( paste0("dose_",nameRespPK), doses[i] )

                      tmp[i] =  eval(dC[[iter]])
                    }
                    dC_eval_RespPK[j,iter]  = sum(tmp)
                  }
                }
              }

              # for evaluation model equations PKPD multi-doses

              if ( length( responsePDNames ) >=1 & flagMultiDosesPD == TRUE )
              {
                stringRespPK = list()
                stringRespPD = list()
                RespPK = list()
                RespPD = list()

                for ( nameRespPK in responsePKNames )
                {
                  RespPK[[nameRespPK]] = dC[[nameRespPK]]
                  RespPK[[nameRespPK]] = gsub(" ", "", RespPK[[nameRespPK]])
                  stringRespPK[[nameRespPK]] = deparse(RespPK[[nameRespPK]][[1]])
                  stringRespPK[[nameRespPK]] = gsub("\"", "", stringRespPK)
                  stringRespPK[[nameRespPK]] = gsub("\"", "", stringRespPK)
                  stringRespPK[[nameRespPK]] = gsub("\'", "", stringRespPK)

                  for ( nameRespPD in responsePDNames )
                  {
                    RespPD[[nameRespPD]] = dC[[nameRespPD]]
                    RespPD[[nameRespPD]] = gsub(" ", "", RespPD[[nameRespPD]])
                    stringRespPD[[nameRespPD]] = deparse(RespPD[[nameRespPD]][[1]])
                    stringRespPD[[nameRespPD]] = gsub("\"", "", stringRespPD)
                    stringRespPD[[nameRespPD]] = gsub("\"", "", stringRespPD)
                    stringRespPD[[nameRespPD]] = gsub("\'", "", stringRespPD)

                    RespPD[[nameRespPD]] = gsub(stringRespPK[[nameRespPK]], "RespPK", RespPD[[nameRespPD]],fixed = TRUE)
                    RespPD[[nameRespPD]] = parse(text=RespPD[[nameRespPD]])
                  }
                }

                j=1
                for ( nameRespPK in responsePKNames )
                {
                  assign(nameRespPK, dC_eval_RespPK[,j])
                  j=j+1
                }

                for ( i in length(responsePDNames ))
                {
                  dC_eval_RespPD[,i] = eval(RespPD[[i]])
                }

                out_response = cbind( samplingTimesModel, dC_eval_RespPK, dC_eval_RespPD)
                out_response = as.data.frame( out_response )
                names(out_response) = c( "time", responseNames )

                # equations used for gradient evaluation
                for ( nameRespPD in responsePDNames )
                {
                  dC[[nameRespPD]] =  RespPD[[nameRespPD]]
                }
              }

              ##for evaluation model equations PK multi-doses
              else{
                out_response = cbind( samplingTimesModel, dC_eval_RespPK)
                out_response = as.data.frame( out_response )
                names(out_response) = c( "time", responsePKNames )
              }

              return( out_response = list ( out_response = out_response,
                                            time_evaluation = time_evaluation,
                                            dose = inputsModel$dose ) )
            }

            flagMultiDosesPD = TRUE
            evaluation = evaluateModelEquations(object,
                                                samplingTimesModel,
                                                inputsModel,
                                                computeFIM,
                                                flagMultiDosesPD )

            out_response = evaluation$out_response

            # ----------------------------------------------------------------------------------
            # evaluate gradients
            # ----------------------------------------------------------------------------------

            variablesNames = inputsModel$variablesNames
            responsePKNames = inputsModel$responsePKNames
            responsePDNames = inputsModel$responsePDNames
            responseNames = c( responsePKNames, responsePDNames )
            modelParameters = inputsModel$model_parameters
            nameModelParameters = names( modelParameters )

            numberOfParameters = length( names( modelParameters ) )
            numberOfResponses = length( responsePKNames ) + length( responsePDNames )
            numberOfVariables = length( variablesNames )
            numberOfEquations = length( inputsModel$modelEquations@equations )

            if ( computeFIM == FALSE )
            {
              for ( nameResponse in responseNames )
              {
                inputsModel$sampleTimeResponse[[nameResponse]] = samplingTimesModel
              }
            }

            sampleTimeResponse = inputsModel$sampleTimeResponse

            evaluationEquations = list()
            responseAllGradient = list()
            responseGradient = list()

            for( parameter in modelParameters )
            {
              name = getNameModelParameter( parameter )
              assign( name, getMu( parameter ) )
            }

            # ----------------------
            # gradient respPK
            # ----------------------

            flagMultiDosesPD = FALSE

            for ( responseName in responsePKNames )
            {
              samplingTimes =  sampleTimeResponse[[responseName]]

              equations = inputsModel$modelEquations@equations[[responseName]]

              grad_expression = sapply( names( modelParameters ), function(x){ D( equations, x ) } )

              for ( nameModelParameter in names( modelParameters ) )
              {
                if ( is.numeric( grad_expression[[nameModelParameter]] ) )
                {
                  evaluationEquations[[nameModelParameter]] =
                    data.frame( samplingTimes, eval( grad_expression[[nameModelParameter]] ) )

                  colnames( evaluationEquations[[nameModelParameter]] ) = c( "time", responseName )
                }
                else
                {
                  inputsModel$modelEquations@equations[[responseName]] = as.expression(grad_expression[[nameModelParameter]])

                  evaluation = evaluateModelEquations( object,
                                                       samplingTimes,
                                                       inputsModel,
                                                       computeFIM,
                                                       flagMultiDosesPD)


                  evaluationEquations[[nameModelParameter]] = evaluation$out_response
                }
              }

              tmp = lapply( evaluationEquations, function(x) { x["time"] <- NULL; x })

              responseGradient[[responseName]] = do.call( "cbind", tmp )

              colnames(  responseGradient[[responseName]] ) = nameModelParameters
            }

            names( responseGradient ) = responsePKNames

            # ----------------------
            # gradient respPD
            # ----------------------

            dC = inputsModel$modelEquations@equations

            for ( responseName in responsePDNames )
            {
              samplingTimes =  sampleTimeResponse[[responseName]]

              equations = dC[[responseName]]

              matrixGradient = matrix(0.0,length(samplingTimes),numberOfParameters)

              for( iter in 1:length( samplingTimes ) )
              {
                assign("t",samplingTimes[iter] )

                for ( responsePKName in responsePKNames )
                {
                  assign( responsePKName, out_response$out_response[iter,responsePKName] )

                indiceDose = evaluation$time_evaluation[iter,"indices_doses"]

                doseValues = evaluation$dose[[responsePKName]]

                doseValue = doseValues[indiceDose]

                nameDose = paste0("dose_",responsePKName)

                assign( nameDose, doseValue )

                }

                grad_expression = sapply( names( modelParameters ), function(x){ D( equations, x ) } )

                matrixGradient[iter,] = unlist( sapply( grad_expression, function(x){eval(x)} ) )

              }
              responseGradient[[responseName]] = matrixGradient
              colnames(  responseGradient[[responseName]] ) = nameModelParameters
            }

             responseAllGradient = do.call( "rbind", responseGradient )
             rownames( responseAllGradient )=NULL

            return( list( evaluationResponse = out_response,
                          responseGradient = responseGradient,
                          responseAllGradient = responseAllGradient) )

          })

# ########################################################################################################################
# END Class "ModelEquations"
# ########################################################################################################################


