#'
#'Library of the PK models
#'
#' @include Model.R
#' @export

LibraryOfPKModels = function(){

  # -------------------------------------------------------------------------------------------------------------------------
  # PK Models
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # Linear
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 1. One compartment
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 1.1 IV bolus
  # -------------------------------------------------------------------------------------------------------------------------

  # 1.1.1 Single dose

  Linear1BolusSingleDose_kV = ModelAnalyticBolus( name = "Linear1BolusSingleDose_kV",
                                                  description = list("Linear", "1","Bolus","Single dose"),
                                                  outcomes = list("RespPK"),
                                                  equations = list("RespPK" = "dose/V * (exp(-k* t))"),
                                                  modelError = list())

  Linear1BolusSingleDose_ClV = ModelAnalyticBolus( name = "Linear1BolusSingleDose_ClV",
                                                   description = list("Linear", "1","Bolus","Single dose"),
                                                   outcomes = list("RespPK"),
                                                   equations = list("RespPK" = "dose/V * (exp(-Cl/V* t))"),
                                                   modelError = list())
  # 1.1.2 Steady state

  Linear1BolusSteadyState_ClVtau = ModelAnalyticBolusSteadyState( name = "Linear1BolusSteadyState_ClVtau",
                                                                  description = list("Linear", "1","Bolus","Steady state"),
                                                                  outcomes = list("RespPK"),
                                                                  equations = list("RespPK" = "dose/V * ( exp(-Cl/V*t)/(1-exp(-Cl/V*tau)))"),
                                                                  modelError = list())


  Linear1BolusSteadyState_kVtau = ModelAnalyticBolusSteadyState( name = "Linear1BolusSteadyState_kVtau",
                                                                 description = list("Linear", "1","Bolus","Steady state"),
                                                                 outcomes = list("RespPK"),
                                                                 equations = list("RespPK" = " dose/V * ( exp( -k*t )/( 1-exp( -k*tau ) ) )"),
                                                                 modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 1.2 Infusion
  # -------------------------------------------------------------------------------------------------------------------------

  # 1.2.1 Single dose

  Linear1InfusionSingleDose_ClV = ModelAnalyticInfusion( name = "Linear1InfusionSingleDose_ClV",
                                                         outcomes = list("RespPK"),
                                                         description = list("Linear","1", "Infusion", "Single dose"),
                                                         equations = list( duringInfusion = list( "RespPK" = "dose/Tinf/Cl * (1 - exp(-Cl/V * t ) )" ) ,
                                                                           afterInfusion  = list( "RespPK" = "dose/Tinf/Cl * (1 - exp(-Cl/V * Tinf)) * (exp(-Cl/V * (t - Tinf)))")),
                                                         modelError = list())

  Linear1InfusionSingleDose_kV = ModelAnalyticInfusion( name = "Linear1InfusionSingleDose_kV",
                                                        outcomes = list("RespPK"),
                                                        description = list("Linear","1", "Infusion", "Single dose"),
                                                        equations = list( duringInfusion = list( "RespPK" = "dose/Tinf/(k*V) * (1 - exp(-k * t ) )" ) ,
                                                                          afterInfusion  = list( "RespPK" = "(dose/Tinf)/(k*V) * (1 - exp(-k * Tinf)) * (exp(-k * (t - Tinf)))")),
                                                        modelError = list())

  # 1.2.2 Steady state

  Linear1InfusionSteadyState_kVtau = ModelAnalyticInfusionSteadyState( name = "Linear1InfusionSteadyState_kVtau",
                                                                       outcomes = list("RespPK"),
                                                                       description = list("Linear","1", "Infusion", "Steady state"),
                                                                       equations = list( duringInfusion = list( "RespPK" = "dose/Tinf/(k*V) * ( (1 - exp(-k * t)) + exp(-k*tau) * ( (1 - exp(-k*Tinf)) * exp(-k*(t-Tinf)) / (1-exp(-k*tau) ) ) )" ) ,
                                                                                         afterInfusion  = list( "RespPK" = "dose/Tinf/(k*V) * ( (1 - exp(-k*Tinf ) ) * exp(-k*(t-Tinf)) / (1-exp(-k*tau ) ) )")),
                                                                       modelError = list())

  Linear1InfusionSteadyState_ClVtau = ModelAnalyticInfusionSteadyState( name = "Linear1InfusionSteadyState_ClVtau",
                                                                        outcomes = list("RespPK"),
                                                                        description = list("Linear","1", "Infusion", "Steady state"),
                                                                        equations = list( duringInfusion = list( "RespPK" = "dose/Tinf/((Cl/V)*V) * ( ( 1 - exp(-(Cl/V) * t)) + exp(-(Cl/V)*tau) * ( (1 - exp(-(Cl/V)*Tinf)) * exp(-(Cl/V)*(t-Tinf)) / (1-exp(-(Cl/V)*tau) ) ) )" ) ,
                                                                                          afterInfusion  = list( "RespPK" = "dose/Tinf/((Cl/V)*V) * ( ( 1 - exp(-(Cl/V)*Tinf ) ) * exp(-(Cl/V)*(t-Tinf)) / (1-exp(-(Cl/V)*tau ) ) )" ) ),
                                                                        modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 1.3 First order
  # -------------------------------------------------------------------------------------------------------------------------

  # 1.3.1 Single dose

  Linear1FirstOrderSingleDose_kaClV = ModelAnalytic( name = "Linear1FirstOrderSingleDose_kaClV",
                                                     description = list("Linear","1", "First order ", "Single dose"),
                                                     outcomes = list("RespPK"),
                                                     equations = list("RespPK" = "dose/V * ka/(ka - Cl/V) * (exp(-Cl/V * t) - exp(-ka * t))"),
                                                     modelError = list())

  Linear1FirstOrderSingleDose_kakV = ModelAnalytic( name = "Linear1FirstOrderSingleDose_kakV",
                                                    description = list("Linear","1", "First order ", "Single dose"),
                                                    outcomes = list("RespPK"),
                                                    equations = list("RespPK" = "dose/V * ka/(ka - k) * (exp(-k * t) - exp(-ka * t))"),
                                                    modelError = list())
  # 1.3.2 Steady state

  Linear1FirstOrderSteadyState_kaClVtau = ModelAnalyticSteadyState( name = "Linear1FirstOrderSteadyState_kaClVtau",
                                                                    description = list("Linear","1", "First order ", "Steady state"),
                                                                    outcomes = list("RespPK"),
                                                                    equations = list("RespPK" = "dose/V * ka/(ka - Cl/V) * (exp(-Cl/V * t)/(1-exp(-Cl/V * tau)) - exp(-ka * t)/(1-exp(-ka * tau)))"),
                                                                    modelError = list())

  Linear1FirstOrderSteadyState_kakVtau = ModelAnalyticSteadyState( name = "Linear1FirstOrderSteadyState_kakVtau",
                                                                   description = list("Linear","1", "First order ", "Steady state"),
                                                                   outcomes = list("RespPK"),
                                                                   equations = list("RespPK" = "dose/V * ka/(ka - k) * (exp(-k * t)/(1-exp(-k * tau)) - exp(-ka * t)/(1-exp(-ka * tau)))"),
                                                                   modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 2. Two compartments
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 2.1 Bolus
  # -------------------------------------------------------------------------------------------------------------------------

  # 2.1.1 Single dose

  # Linear2BolusSingleDose_ClQV1V2

  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V1 * (alpha-Q/V2)/(alpha-beta))
    B = expression(1/V1 * (beta-Q/V2)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    dose = expression(dose)

    model_expression = expression(dose * (A * exp(-alpha*t) + B * exp(-beta*t)))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))

    model_expression = paste0(deparse(model_expression),collapse="")

    return( model_expression )
  }

  model_expression = model_expression()

  Linear2BolusSingleDose_ClQV1V2 = ModelAnalyticBolus( name = "Linear2BolusSingleDose_ClQV1V2",
                                                       description = list("Linear","2", "Bolus", "Single dose"),
                                                       outcomes = list("RespPK"),
                                                       equations = list("RespPK" = model_expression),
                                                       modelError = list())

  # Linear2BolusSingleDose_kk12k21V

  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V * (alpha-k21)/(alpha-beta))
    B = expression(1/V * (beta-k21)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t) + B * exp(-beta*t)))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")

    return( model_expression )

  }

  model_expression = model_expression()

  Linear2BolusSingleDose_kk12k21V = ModelAnalyticBolus( name = "Linear2BolusSingleDose_kk12k21V",
                                                        description = list("Linear","2", "Bolus", "Single dose"),
                                                        outcomes = list("RespPK"),
                                                        equations = list("RespPK" = model_expression),
                                                        modelError = list())

  # 2.1.2 Steady state

  # Linear2BolusSteadyState_ClQV1V2tau

  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V1 * (alpha-Q/V2)/(alpha-beta))
    B = expression(1/V1 * (beta-Q/V2)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t)/exp(1-exp(-alpha*tau)) + B * exp(-beta*t)/exp(1-exp(-beta*tau))))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")

    return( model_expression )
  }

  model_expression = model_expression ()

  Linear2BolusSteadyState_ClQV1V2tau = ModelAnalyticBolusSteadyState( name = "Linear2BolusSteadyState_ClQV1V2tau",
                                                                      description = list("Linear","2", "Bolus", "Steady state"),
                                                                      outcomes = list("RespPK"),
                                                                      equations = list("RespPK" = model_expression ),
                                                                      modelError = list())

  # Linear2BolusSteadyState_kk12k21Vtau

  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V * (alpha-k21)/(alpha-beta))
    B = expression(1/V * (beta-k21)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t)/exp(1-exp(-alpha*tau)) + B * exp(-beta*t)/exp(1-exp(-beta*tau))))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")

  }

  model_expression = model_expression ()

  Linear2BolusSteadyState_kk12k21Vtau = ModelAnalyticBolusSteadyState( name = "Linear2BolusSteadyState_kk12k21Vtau",
                                                                       description = list("Linear","2", "Bolus", "Steady state"),
                                                                       outcomes = list("RespPK"),
                                                                       equations = list("RespPK" = model_expression ),
                                                                       modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 2.2 First order
  # -------------------------------------------------------------------------------------------------------------------------
  # 2.2.1 Single dose

  # Linear2First orderSingleDose_kaClQV1V2

  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(ka/V1 * (Q/V2-alpha)/(beta-alpha)/(ka-alpha))
    B = expression(ka/V1 * (Q/V2-beta)/(alpha-beta)(ka-beta))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t) + B * exp(-beta*t)) - (A+B)*exp(-ka*t))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")
  }

  model_expression = model_expression ()


  Linear2FirstOrderSingleDose_kaClQV1V2 = ModelAnalytic( name = "Linear2FirstOrderSingleDose_kaClQV1V2",
                                                         description  = list("Linear","2", "First order ", "Single dose"),
                                                         outcomes = list("RespPK"),
                                                         equations = list("RespPK" = model_expression ),
                                                         modelError = list())


  # Linear2First orderSingleDose_kakk12k21V
  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(ka/V1 * (Q/V2-alpha)/(beta-alpha)/(ka-alpha))
    B = expression(ka/V1 * (Q/V2-beta)/(alpha-beta)(ka-beta))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t) + B * exp(-beta*t)) - (A+B)*exp(-ka*t))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")
  }

  model_expression = model_expression ()

  Linear2FirstOrderSingleDose_kakk12k21V = ModelAnalytic( name = "Linear2FirstOrderSingleDose_kakk12k21V",
                                                          description = list("Linear","2", "First order ", "Single dose"),
                                                          outcomes = list("RespPK"),
                                                          equations = list("RespPK" = model_expression ),
                                                          modelError = list())

  # 2.2.2 Steady state
  # Linear2First orderSteadyState_kaClQV1V2tau
  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(ka/V1 * (Q/V2-alpha)/(beta-alpha)/(ka-alpha))
    B = expression(ka/V1 * (Q/V2-beta)/(alpha-beta)(ka-beta))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t)/(1-exp(-alpha*tau)) + B * exp(-beta*t))/(1-exp(-beta*tau)) - (A+B)*exp(-ka*t)/(1-exp(-ka*tau)))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")
  }


  model_expression = model_expression ()

  Linear2FirstOrderSteadyState_kaClQV1V2tau = ModelAnalyticSteadyState( name = "Linear2FirstOrderSteadyState_kaClQV1V2tau",
                                                                        description = list("Linear","2", "First order ", "Steady state"),
                                                                        outcomes = list("RespPK"),
                                                                        equations = list("RespPK" = model_expression ),
                                                                        modelError = list())


  # Linear2First orderSteadyState_kakk12k21V

  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(ka/V1 * (Q/V2-alpha)/(beta-alpha)/(ka-alpha))
    B = expression(ka/V1 * (Q/V2-beta)/(alpha-beta)(ka-beta))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    model_expression = expression(dose * (A * exp(-alpha*t)/(1-exp(-alpha*tau)) + B * exp(-beta*t))/(1-exp(-beta*tau)) - (A+B)*exp(-ka*t)/(1-exp(-ka*tau)))

    model_expression = do.call('substitute', list(model_expression[[1]], list(beta = beta_expression,
                                                                              alpha = alpha_substitute,
                                                                              A = A_substitute,
                                                                              B = B_substitute)))
    model_expression = paste0(deparse(model_expression),collapse="")
  }


  model_expression = model_expression ()

  Linear2FirstOrderSteadyState_kakk12k21Vtau = ModelAnalyticSteadyState( name = "Linear2FirstOrderSteadyState_kakk12k21Vtau",
                                                                         description = list("Linear","2", "First order ", "Steady state"),
                                                                         outcomes = list("RespPK"),
                                                                         equations = list("RespPK" = model_expression ),
                                                                         modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 2.3 Infusion
  # -------------------------------------------------------------------------------------------------------------------------

  # 2.3.1 Single dose

  # Linear2InfusionSingleDose_kk12k21V

  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V * (alpha-k21)/(alpha-beta))
    B = expression(1/V * (beta-k21)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    equation_during_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*t))  + B/beta(1-exp(-beta*t)) ))
    equation_during_infusion = do.call('substitute', list(equation_during_infusion[[1]], list(beta = beta_expression,
                                                                                              alpha = alpha_substitute,
                                                                                              A = A_substitute,
                                                                                              B = B_substitute)))

    equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))
                                                       + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))))

    equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                            alpha = alpha_substitute,
                                                                                            A = A_substitute,
                                                                                            B = B_substitute)))

    equation_during_infusion = paste0(deparse(equation_during_infusion),collapse="")
    equation_after_infusion = paste0(deparse(equation_after_infusion),collapse="")

    model_expression = list( equation_during_infusion = equation_during_infusion,
                             equation_after_infusion = equation_after_infusion)

    return( ( model_expression ) )
  }

  model_expression = model_expression()


  Linear2InfusionSingleDose_kk12k21V = ModelAnalyticInfusion( name = "Linear2InfusionSingleDose_kk12k21V",
                                                              outcomes = list("RespPK"),
                                                              description = list("Linear","2", "Infusion", "Single dose"),
                                                              equations = model_expression,
                                                              modelError = list())


  # Linear2InfusionSingleDose_ClQV1V2

  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V1 * (alpha-Q/V2)/(alpha-beta))
    B = expression(1/V1 * (beta-Q/V2)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    equation_during_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*t))  + B/beta(1-exp(-beta*t)) ))
    equation_during_infusion = do.call('substitute', list(equation_during_infusion[[1]], list(beta = beta_expression,
                                                                                              alpha = alpha_substitute,
                                                                                              A = A_substitute,
                                                                                              B = B_substitute)))
    equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))
                                                       + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))))

    equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                            alpha = alpha_substitute,
                                                                                            A = A_substitute,
                                                                                            B = B_substitute)))

    equation_during_infusion = paste0(deparse(equation_during_infusion),collapse="")
    equation_after_infusion = paste0(deparse(equation_after_infusion),collapse="")

    model_expression = list( equation_during_infusion = equation_during_infusion,
                             equation_after_infusion = equation_after_infusion)

    return( model_expression )
  }

  model_expression = model_expression()

  Linear2InfusionSingleDose_ClQV1V2 = ModelAnalyticInfusion( name = "Linear2InfusionSingleDose_ClQV1V2",
                                                             outcomes = list("RespPK"),
                                                             description = list("Linear","2", "Infusion", "Single dose"),
                                                             equations = model_expression,
                                                             modelError = list())


  # 2.3.2 Steady state

  # Linear2InfusionSteadyState_kk12k21Vtau

  model_expression = function(){

    alpha = expression((k21 * k)/beta)
    alpha_expression = quote((k21 * k)/beta)
    beta_expression = quote(0.5*(k12+k21+k-sqrt((k12 + k21 + k)**2 - 4* k21 * k )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V * (alpha-k21)/(alpha-beta))
    B = expression(1/V * (beta-k21)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    equation_during_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*t) + exp(-alpha*tau)*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau)))
                                                        + B/beta*(1-exp(-beta*t) + exp(-beta*tau)*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))) ))
    equation_during_infusion = do.call('substitute', list(equation_during_infusion[[1]], list(beta = beta_expression,
                                                                                              alpha = alpha_substitute,
                                                                                              A = A_substitute,
                                                                                              B = B_substitute)))

    equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau))
                                                       + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))))

    equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                            alpha = alpha_substitute,
                                                                                            A = A_substitute,
                                                                                            B = B_substitute)))
    equation_during_infusion = paste0(deparse(equation_during_infusion),collapse="")
    equation_after_infusion = paste0(deparse(equation_after_infusion),collapse="")

    model_expression = list( equation_during_infusion = equation_during_infusion,
                             equation_after_infusion = equation_after_infusion)

    return( ( model_expression ) )
  }

  model_expression = model_expression()

  Linear2InfusionSteadyState_kk12k21Vtau =  ModelAnalyticInfusionSteadyState( name = "Linear2InfusionSteadyState_kk12k21Vtau",
                                                                              outcomes = list("RespPK"),
                                                                              description = list("Linear","2", "Infusion", "Steady state"),
                                                                              equations = model_expression,
                                                                              modelError = list())

  # Linear2InfusionSteadyState_ClQV1V2tau
  model_expression = function(){

    alpha = expression((Q/V2 * Cl/V1)/beta)
    alpha_expression = quote((Q/V2 * Cl/V1)/beta)
    beta_expression = quote(0.5*(Q/V1+Q/V2+Cl/V1-sqrt((Q/V1 + Q/V2 + Cl/V1)**2 - 4* Q/V2 * Cl/V1 )))

    alpha_substitute = do.call('substitute', list(alpha[[1]],
                                                  list(beta=beta_expression)))

    A = expression(1/V1 * (alpha-Q/V2)/(alpha-beta))
    B = expression(1/V1 * (beta-Q/V2)/(beta-alpha))

    A_substitute = do.call('substitute', list(A[[1]], list(beta = beta_expression, alpha = alpha_substitute)))
    B_substitute = do.call('substitute', list(B[[1]], list(beta = beta_expression, alpha = alpha_substitute)))

    equation_during_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*t) + exp(-alpha*tau)*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau)))
                                                        + B/beta*(1-exp(-beta*t) + exp(-beta*tau)*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))) ))
    equation_during_infusion = do.call('substitute', list(equation_during_infusion[[1]], list(beta = beta_expression,
                                                                                              alpha = alpha_substitute,
                                                                                              A = A_substitute,
                                                                                              B = B_substitute)))

    equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau))
                                                       + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))))

    equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                            alpha = alpha_substitute,
                                                                                            A = A_substitute,
                                                                                            B = B_substitute)))

    equation_during_infusion = paste0(deparse(equation_during_infusion),collapse="")
    equation_after_infusion = paste0(deparse(equation_after_infusion),collapse="")

    model_expression = list( equation_during_infusion = equation_during_infusion,
                             equation_after_infusion = equation_after_infusion)

    return( ( model_expression ) )
  }

  model_expression = model_expression()

  Linear2InfusionSteadyState_ClQV1V2tau = ModelAnalyticInfusionSteadyState( name = "Linear2InfusionSteadyState_ClQV1V2tau",
                                                                            outcomes = list("RespPK"),
                                                                            description = list("Linear","2", "Infusion", "Steady state"),
                                                                            equations = model_expression,
                                                                            modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Michaelis-Menten elimination
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 1. One compartment
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 1.1 IV bolus
  # -------------------------------------------------------------------------------------------------------------------------

  MichaelisMenten1BolusSingleDose_VmKmV = ModelODEDoseNotInEquations( name = "MichaelisMenten1BolusSingleDose_VmKmV",
                                                                      description =  list("Michaelis-Menten", "1", "Bolus", "Single dose"),
                                                                      outcomes = list("RespPK" = "C1"),
                                                                      equations = list("Deriv_C1" = "-Vm*C1/(Km+C1)"),
                                                                      modelError = list())


  # -------------------------------------------------------------------------------------------------------------------------
  # 1.2 First order
  # -------------------------------------------------------------------------------------------------------------------------

  MichaelisMenten1FirstOrderSingleDose_kaVmKmV = ModelODEDoseInEquations( name = "MichaelisMenten1FirstOrderSingleDose_kaVmKmV",
                                                                          description =  list("Michaelis-Menten", "1", "First order", "Single dose"),
                                                                          outcomes = list("RespPK" = "C1"),
                                                                          equations = list("Deriv_C1" = "-Vm*C1/(Km+C1) + dose/V*ka*exp(-ka*t)"),
                                                                          modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 1.2 First order
  # -------------------------------------------------------------------------------------------------------------------------

  MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2 = ModelODEDoseInEquations( name = "MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2",
                                                                                   description =  list("Michaelis-Menten", "2", "First order", "Single dose"),
                                                                                   outcomes = list("RespPK1" = "C1", "RespPK2" = "C2"),
                                                                                   equations = list("Deriv_C1" = "-Vm*C1/(Km+C1) - k12*C1 + k21*V2/V*C2 + dose/V*ka*exp(-ka*t)",
                                                                                                    "Deriv_C2" = "k12*V/V2*C1 - k21*C2"),
                                                                                   modelError = list())

  # -------------------------------------------------------------------------------------------------------------------
  # 1.3 Infusion
  # -------------------------------------------------------------------------------------------------------------------

  MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2 =

    ModelODEInfusionDoseInEquations(

      name = "MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2",

      description = list("Michaelis-Menten","2", "Infusion", "Single dose"),

      outcomes = list("RespPK1" = "C1",
                      "RespPK2" = "C2"),

      equations = list( duringInfusion = list( "Deriv_C1" = "dose/Tinf/Cl * (1 - exp(-Cl/V * t ) )",
                                               "Deriv_C2" = "k12*V/V2*C1 - k21*C2"),

                        afterInfusion  = list( "Deriv_C1" = " -Vm*C1/(Km+C1)",
                                               "Deriv_C2" = "k12*V/V2*C1 - k21*C2") ),

      modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 2. Two compartments
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # 2.1 IV bolus
  # -------------------------------------------------------------------------------------------------------------------------

  MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2 = ModelODEDoseInEquations( name = "MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2",

                                                                            description =  list("Michaelis-Menten", "2", "Bolus", "Single dose"),

                                                                            outcomes = list("RespPK1" = "C1",
                                                                                            "RespPK2" = "C2"),

                                                                            equations = list("Deriv_C1" = "-Vm*C1/(Km+C1) - k12*C1+k21*V2/V*C2",
                                                                                             "Deriv_C2" = "k12*V/V2*C1 - k21*C2"),

                                                                            modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # 2.2 First order
  # -------------------------------------------------------------------------------------------------------------------------

  MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2 = ModelODEDoseInEquations( name = "MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2",

                                                                                   description =  list("Michaelis-Menten", "2", "First order", "Single dose"),

                                                                                   outcomes = list("RespPK1" = "C1",
                                                                                                   "RespPK2" = "C2"),

                                                                                   equations = list("Deriv_C1" = "-Vm*C1/(Km+C1) - k12*C1 + k21*V2/V*C2 + dose_RespPK1/V*ka*exp(-ka*t)",
                                                                                                    "Deriv_C2" = "k12*V/V2*C1 - k21*C2"),

                                                                                   modelError = list())

  # -------------------------------------------------------------------------------------------------------------------
  # 2.3 Infusion
  # -------------------------------------------------------------------------------------------------------------------

  MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2 =

    ModelODEInfusionDoseInEquations(

      name = "MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2",

      description = list("Michaelis-Menten","2", "Infusion", "Single dose"),

      outcomes = list("RespPK1" = "C1",
                      "RespPK2" = "C2"),

      equations = list( duringInfusion = list( "Deriv_C1" = "-Vm*C1/(Km+C1) + dose_RespPK1/V/Tinf_RespPK1",
                                               "Deriv_C2" = "k12*V/V2*C1 - k21*C2"),

                        afterInfusion  = list( "Deriv_C1" = "-Vm*C1/(Km+C1)",
                                               "Deriv_C2" = "k12*V/V2*C1 - k21*C2") ),

      modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Add PK to the library
  # -------------------------------------------------------------------------------------------------------------------------

  pkModels = list(   Linear1BolusSingleDose_kV,
                     Linear1BolusSingleDose_ClV,
                     Linear1BolusSteadyState_ClVtau,
                     Linear1BolusSteadyState_kVtau,

                     Linear1InfusionSingleDose_ClV,
                     Linear1InfusionSingleDose_kV,
                     Linear1InfusionSteadyState_kVtau,
                     Linear1InfusionSteadyState_ClVtau,

                     Linear1FirstOrderSingleDose_kaClV,
                     Linear1FirstOrderSingleDose_kakV,
                     Linear1FirstOrderSteadyState_kaClVtau,
                     Linear1FirstOrderSteadyState_kakVtau,

                     Linear2BolusSingleDose_ClQV1V2,
                     Linear2BolusSingleDose_kk12k21V,
                     Linear2BolusSteadyState_ClQV1V2tau,
                     Linear2BolusSteadyState_kk12k21Vtau,

                     Linear2FirstOrderSingleDose_kaClQV1V2,
                     Linear2FirstOrderSingleDose_kakk12k21V,
                     Linear2FirstOrderSteadyState_kaClQV1V2tau,
                     Linear2FirstOrderSteadyState_kakk12k21Vtau,

                     Linear2InfusionSingleDose_kk12k21V,
                     Linear2InfusionSingleDose_ClQV1V2,
                     Linear2InfusionSteadyState_kk12k21Vtau,
                     Linear2InfusionSteadyState_ClQV1V2tau,

                     MichaelisMenten1BolusSingleDose_VmKmV,
                     MichaelisMenten1FirstOrderSingleDose_kaVmKmV,
                     MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2,
                     MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2,
                     MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2,
                     MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2,
                     MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2 )

  return( pkModels )
}

##########################################################################################################
# END Class "LibraryOfPKModels"
##########################################################################################################

