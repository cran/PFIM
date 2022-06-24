##################################################################################
#' Function "FillLibraryOfModels"
#' @name FillLibraryOfModels
#' @description This function is used to define the models PK, PD and PKPD models and load these models in the library of models.
#'
#' @include ModelEquations.R
#' @include ModelODEquations.R
#' @include ModelInfusionEquations.R
#' @include ModelInfusionODEquations.R
#' @include LibraryOfModels.R
#' @include PDModel.R
#' @include PKModel.R

# ################################################################################################

# LibraryOfModels

# ################################################################################################

PFIMLibraryOfModels <- LibraryOfModels(nameLibraryOfModels = "Models PK & PD & PKPD")

# -------------------------------------------------------------------------------------------------------------------------
# PK Models
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# Linear elimination
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 1. One compartment
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 1.1 IV bolus
# -------------------------------------------------------------------------------------------------------------------------

# 1.1.1 Single dose
Linear1BolusSingleDose_kV = PKModel( nameModel = "Linear1BolusSingleDose_kV",
                                     descriptionModel = list("Linear Elimination", "1 Compartment","Bolus","Single Dose"),
                                     equationsModel = ModelEquations( list("RespPK" = expression( dose/V * (exp(-k* t))))))


Linear1BolusSingleDose_ClV = PKModel( nameModel = "Linear1BolusSingleDose_ClV",
                                      descriptionModel = list("Linear Elimination", "1 Compartment","Bolus","Single Dose"),
                                      equationsModel = ModelEquations( list("RespPK" = expression( dose/V * (exp(-Cl/V * t))))))

# 1.1.2 Steady state
Linear1BolusSteadyState_ClVtau = PKModel( nameModel = "Linear1BolusSteadyState_ClVtau",
                                          descriptionModel = list("Linear Elimination", "1 Compartment","Bolus","Steady State"),
                                          equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ( exp(-Cl/V*t)/(1-exp(-Cl/V*tau)))))))
Linear1BolusSteadyState_kVtau = PKModel( nameModel = "Linear1BolusSteadyState_kVtau",
                                         descriptionModel = list("Linear Elimination", "1 Compartment","Bolus","Steady State"),
                                         equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ( exp(-k*t)/(1-exp(-k*tau)))))))

# -------------------------------------------------------------------------------------------------------------------------
# 1.2 Infusion
# -------------------------------------------------------------------------------------------------------------------------

# 1.2.1 Single dose
Linear1InfusionSingleDose_ClV = PKModel( nameModel = "Linear1InfusionSingleDose_ClV",
                                         descriptionModel = list("Linear Elimination","1 Compartment", "Infusion", "Single Dose"),
                                         equationsModel = ModelInfusionEquations(list(
                                           "DuringInfusion_RespPK" = expression(dose/Tinf/Cl * (1 - exp(-Cl/V * t ) ) ) ,
                                           "AfterInfusion_RespPK"  = expression(dose/Tinf/Cl * (1 - exp(-Cl/V * Tinf)) * (exp(-Cl/V * (t - Tinf)))))))

Linear1InfusionSingleDose_kV = PKModel( nameModel = "Linear1InfusionSingleDose_kV",
                                        descriptionModel = list("Linear Elimination","1 Compartment", "Infusion", "Single Dose"),
                                        equationsModel = ModelInfusionEquations(list(
                                          "DuringInfusion_RespPK" = expression(dose/Tinf/(k*V) * (1 - exp(-k * t ) ) ) ,
                                          "AfterInfusion_RespPK"  = expression((dose/Tinf)/(k*V) * (1 - exp(-k * Tinf)) * (exp(-k * (t - Tinf)))))))

# 1.2.2 Steady State
Linear1InfusionSteadyState_kVtau = PKModel( nameModel = "Linear1InfusionSteadyState_kVtau",
                                            descriptionModel = list("Linear Elimination","1 Compartment", "Infusion", "Steady State"),
                                            equationsModel = ModelInfusionEquations(list(
                                              "DuringInfusion_RespPK" = expression(
                                                dose/Tinf/(k*V) *
                                                  ( (1 - exp(-k * t)) + exp(-k*tau) * ( (1 - exp(-k*Tinf)) * exp(-k*(t-Tinf)) / (1-exp(-k*tau))) )),
                                              "AfterInfusion_RespPK" = expression(
                                                dose/Tinf/(k*V) *
                                                  ( (1 - exp(-k*Tinf)) * exp(-k*(t-Tinf)) / (1-exp(-k*tau)))))))

Linear1InfusionSteadyState_ClVtau = PKModel( nameModel = "Linear1InfusionSteadyState_ClVtau",
                                             descriptionModel = list("Linear Elimination","1 Compartment", "Infusion", "Steady State"),
                                             equationsModel = ModelInfusionEquations(list(
                                               "DuringInfusion_RespPK" = expression(
                                                 dose/Tinf/Cl *
                                                   ( (1 - exp(-Cl/V * t)) + exp(-Cl/V*tau) * ( (1 - exp(-Cl/V*Tinf)) * exp(-Cl/V*(t-Tinf)) / (1-exp(-Cl/V*tau))) )),
                                               "AfterInfusion_RespPK" = expression(
                                                 dose/Tinf/Cl *
                                                   ( (1 - exp(-Cl/V*Tinf)) * exp(-Cl/V*(t-Tinf)) / (1-exp(-Cl/V*tau)))))))

# -------------------------------------------------------------------------------------------------------------------------
# 1.3 First order absorption
# -------------------------------------------------------------------------------------------------------------------------

# 1.3.1 Single dose
Linear1FirstOrderSingleDose_kaClV = PKModel( nameModel = "Linear1FirstOrderSingleDose_kaClV",
                                             descriptionModel = list("Linear Elimination","1 Compartment", "First Order Absorption", "Single Dose"),
                                             equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ka/(ka - Cl/V) * (exp(-Cl/V * t) - exp(-ka * t))))))

Linear1FirstOrderSingleDose_kakV = PKModel( nameModel = "Linear1FirstOrderSingleDose_kakV",
                                            descriptionModel = list("Linear Elimination","1 Compartment", "First Order Absorption", "Single Dose"),
                                            equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ka/(ka - k) * (exp(-k * t) - exp(-ka * t))))))

# 1.3.2 Steady State
Linear1FirstOrderSteadyState_kaClVtau = PKModel( nameModel = "Linear1FirstOrderSteadyState_kaClVtau",
                                                 descriptionModel = list("Linear Elimination","1 Compartment", "First Order Absorption", "Steady State"),
                                                 equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ka/(ka - Cl/V) * (exp(-Cl/V * t)/(1-exp(-Cl/V * tau)) - exp(-ka * t)/(1-exp(-ka * tau)))))))

Linear1FirstOrderSteadyState_kakVtau = PKModel( nameModel = "Linear1FirstOrderSteadyState_kakVtau",
                                                descriptionModel = list("Linear Elimination","1 Compartment", "First Order Absorption", "Steady State"),
                                                equationsModel = ModelEquations( list("RespPK" = expression( dose/V * ka/(ka - k) * (exp(-k * t)/(1-exp(-k * tau)) - exp(-ka * t)/(1-exp(-ka * tau)))))))


# -------------------------------------------------------------------------------------------------------------------------
# 2. Two compartments
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 2.1 Bolus
# -------------------------------------------------------------------------------------------------------------------------
# 2.1.1 Single Dose
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2BolusSingleDose_ClQV1V2 = PKModel( nameModel = "Linear2BolusSingleDose_ClQV1V2",
                                          descriptionModel = list("Linear Elimination","2 Compartments", "Bolus", "Single Dose"),
                                          equationsModel = ModelEquations( list("RespPK" = model_expression)))

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

  return( as.expression( model_expression ) )

}

model_expression = model_expression()

Linear2BolusSingleDose_kk12k21V = PKModel( nameModel = "Linear2BolusSingleDose_kk12k21V",
                                           descriptionModel = list("Linear Elimination","2 Compartments", "Bolus", "Single Dose"),
                                           equationsModel = ModelEquations( list("RespPK" = model_expression)))

# 2.1.2 Steady State
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2BolusSteadyState_ClQV1V2tau = PKModel( nameModel = "Linear2BolusSteadyState_ClQV1V2tau",
                                              descriptionModel = list("Linear Elimination","2 Compartments", "Bolus", "Steady State"),
                                              equationsModel = ModelEquations( list("RespPK" = model_expression)))

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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2BolusSteadyState_kk12k21Vtau = PKModel( nameModel = "Linear2BolusSteadyState_kk12k21Vtau",
                                               descriptionModel = list("Linear Elimination","2 Compartments", "Bolus", "Steady State"),
                                               equationsModel = ModelEquations( list("RespPK" = model_expression)))

# -------------------------------------------------------------------------------------------------------------------------
# 2.2 First Order Absorption
# -------------------------------------------------------------------------------------------------------------------------
# 2.2.1 Single Dose
# Linear2FirstOrderSingleDose_kaClQV1V2
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2FirstOrderSingleDose_kaClQV1V2 = PKModel( nameModel = "Linear2FirstOrderSingleDose_kaClQV1V2",
                                                 descriptionModel = list("Linear Elimination","2 Compartments", "First Order Absorption", "Single Dose"),
                                                 equationsModel = ModelEquations( list("RespPK" = model_expression)))

# Linear2FirstOrderSingleDose_kakk12k21V
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2FirstOrderSingleDose_kakk12k21V = PKModel( nameModel = "Linear2FirstOrderSingleDose_kakk12k21V",
                                                  descriptionModel = list("Linear Elimination","2 Compartments", "First Order Absorption", "Single Dose"),
                                                  equationsModel = ModelEquations( list("RespPK" = model_expression)))
# 2.2.2 Steady State
# Linear2FirstOrderSteadyState_kaClQV1V2tau
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2FirstOrderSteadyState_kaClQV1V2tau = PKModel( nameModel = "Linear2FirstOrderSteadyState_kaClQV1V2tau",
                                                     descriptionModel = list("Linear Elimination","2 Compartments", "First Order Absorption", "Steady State"),
                                                     equationsModel = ModelEquations( list("RespPK" = model_expression)))

# Linear2FirstOrderSteadyState_kakk12k21V
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
  return( as.expression( model_expression ) )

}

model_expression = model_expression ()

Linear2FirstOrderSteadyState_kakk12k21Vtau = PKModel( nameModel = "Linear2FirstOrderSteadyState_kakk12k21Vtau",
                                                      descriptionModel = list("Linear Elimination","2 Compartments", "First Order Absorption", "Steady State"),
                                                      equationsModel = ModelEquations( list("RespPK" = model_expression)))

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
  equation_during_infusion = as.expression( equation_during_infusion )

  equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))
                                                     + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))))

  equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                          alpha = alpha_substitute,
                                                                                          A = A_substitute,
                                                                                          B = B_substitute)))
  equation_after_infusion = as.expression( equation_after_infusion )


  model_expression = list( equation_during_infusion = equation_during_infusion,
                           equation_after_infusion = equation_after_infusion)

  return( ( model_expression ) )
}

model_expression = model_expression()

equation_during_infusion = model_expression$equation_during_infusion
equation_after_infusion = model_expression$equation_after_infusion

Linear2InfusionSingleDose_kk12k21V = PKModel( nameModel = "Linear2InfusionSingleDose_kk12k21V",
                                              descriptionModel = list("Linear Elimination","2 Compartments", "Infusion", "Single Dose"),
                                              equationsModel =  ModelInfusionEquations(list("DuringInfusion_RespPK" = equation_during_infusion,
                                                                                            "AfterInfusion_RespPK" = equation_after_infusion )))

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
  equation_during_infusion = as.expression( equation_during_infusion )

  equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))
                                                     + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))))

  equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                          alpha = alpha_substitute,
                                                                                          A = A_substitute,
                                                                                          B = B_substitute)))
  equation_after_infusion = as.expression( equation_after_infusion )


  model_expression = list( equation_during_infusion = equation_during_infusion,
                           equation_after_infusion = equation_after_infusion)

  return( ( model_expression ) )
}

model_expression = model_expression()

equation_during_infusion = model_expression$equation_during_infusion
equation_after_infusion = model_expression$equation_after_infusion

Linear2InfusionSingleDose_ClQV1V2 = PKModel( nameModel = "Linear2InfusionSingleDose_ClQV1V2",
                                             descriptionModel = list("Linear Elimination","2 Compartments", "Infusion", "Single Dose"),
                                             equationsModel =  ModelInfusionEquations(list("DuringInfusion_RespPK" = equation_during_infusion,
                                                                                           "AfterInfusion_RespPK" = equation_after_infusion )))

# 2.3.2 Steady State
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
  equation_during_infusion = as.expression( equation_during_infusion )

  equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau))
                                                     + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))))

  equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                          alpha = alpha_substitute,
                                                                                          A = A_substitute,
                                                                                          B = B_substitute)))
  equation_after_infusion = as.expression( equation_after_infusion )


  model_expression = list( equation_during_infusion = equation_during_infusion,
                           equation_after_infusion = equation_after_infusion)

  return( ( model_expression ) )
}

model_expression = model_expression()

equation_during_infusion = model_expression$equation_during_infusion
equation_after_infusion = model_expression$equation_after_infusion

Linear2InfusionSteadyState_kk12k21Vtau = PKModel( nameModel = "Linear2InfusionSteadyState_kk12k21Vtau",
                                                  descriptionModel = list("Linear Elimination","2 Compartments", "Infusion", "SteadyState"),
                                                  equationsModel =  ModelInfusionEquations(list("DuringInfusion_RespPK" = equation_during_infusion,
                                                                                                "AfterInfusion_RespPK" = equation_after_infusion )))
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
  equation_during_infusion = as.expression( equation_during_infusion )

  equation_after_infusion = expression(dose/Tinf * ( A/alpha*(1-exp(-alpha*Tinf))*exp(-alpha*(t-Tinf))/(1-exp(-alpha*tau))
                                                     + B/beta*(1-exp(-beta*Tinf))*exp(-beta*(t-Tinf))/(1-exp(-beta*tau))))

  equation_after_infusion = do.call('substitute', list(equation_after_infusion[[1]], list(beta = beta_expression,
                                                                                          alpha = alpha_substitute,
                                                                                          A = A_substitute,
                                                                                          B = B_substitute)))
  equation_after_infusion = as.expression( equation_after_infusion )


  model_expression = list( equation_during_infusion = equation_during_infusion,
                           equation_after_infusion = equation_after_infusion)

  return( ( model_expression ) )
}

model_expression = model_expression()

equation_during_infusion = model_expression$equation_during_infusion
equation_after_infusion = model_expression$equation_after_infusion

Linear2InfusionSteadyState_ClQV1V2tau = PKModel( nameModel = "Linear2InfusionSteadyState_ClQV1V2tau",
                                                 descriptionModel = list("Linear Elimination","2 Compartments", "Infusion", "SteadyState"),
                                                 equationsModel =  ModelInfusionEquations(list("DuringInfusion_RespPK" = equation_during_infusion,
                                                                                               "AfterInfusion_RespPK" = equation_after_infusion )))


# -------------------------------------------------------------------------------------------------------------------------
# Michaelis-Menten elimination
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 1. One compartment
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 1.1 IV bolus
# -------------------------------------------------------------------------------------------------------------------------
# mass ODE to include V in FIM calculation

MichaelisMenten1BolusSingleDose_VmKmV = PKModel( nameModel = "MichaelisMenten1BolusSingleDose_VmKmV",
                                                 descriptionModel =  list("Michaelis-Menten", "1 Compartment", "Bolus", "Single Dose"),
                                                 equationsModel = ModelODEquations(
                                                   list("RespPK" = expression(C1/V)),
                                                   list("Deriv_C1" = expression(-Vm*C1/(Km+C1)))))

# -------------------------------------------------------------------------------------------------------------------------
# 1.2 First Order Absorption
# -------------------------------------------------------------------------------------------------------------------------

MichaelisMenten1FirstOrderSingleDose_kaVmKmV = PKModel( nameModel = "MichaelisMenten1FirstOrderSingleDose_kaVmKmV",
                                                        descriptionModel =  list("Michaelis-Menten", "1 Compartment", "FirstOrder", "Single Dose"),
                                                        equationsModel = ModelODEquations(
                                                          list("RespPK" = expression(C1)),
                                                          list("Deriv_C1" = expression(-Vm*C1/(Km+C1) + dose_RespPK/V*ka*exp(-ka*t)))))

# -------------------------------------------------------------------------------------------------------------------
# 1.3 Infusion
# -------------------------------------------------------------------------------------------------------------------

MichaelisMenten1InfusionSingleDose_VmKmV = PKModel( nameModel = "MichaelisMenten1InfusionSingleDose_VmKmV",
                                                    descriptionModel = list("Michaelis-Menten","1 Compartment", "Infusion", "Single Dose"),
                                                    equationsModel = ModelInfusionODEquations(
                                                      list("DuringInfusion_RespPK" = expression(C1)),
                                                      list("AfterInfusion_RespPK" = expression(C1)),
                                                      list("Deriv_DuringInfusion_C1" = expression(-Vm*C1/(Km+C1) + dose_RespPK/(V*Tinf_RespPK))),
                                                      list("Deriv_AfterInfusion_C1" = expression(-Vm*C1/(Km+C1)))))


# -------------------------------------------------------------------------------------------------------------------------
# 2. Two compartments
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# 2.1 IV bolus
# -------------------------------------------------------------------------------------------------------------------------

MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2 = PKModel( nameModel = "MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2",
                                                          descriptionModel =  list("Michaelis-Menten", "2 Compartments", "Bolus", "Single Dose"),
                                                          equationsModel = ModelODEquations(
                                                            list("RespPK1" = expression(C1),
                                                                 "RespPK2" = expression(C2)),
                                                            list("Deriv_C1" = expression(-Vm*C1/(Km+C1) - k12*C1+k21*V2/V*C2),
                                                                 "Deriv_C2" = expression(k12*V/V2*C1 - k21*C2))))

# -------------------------------------------------------------------------------------------------------------------------
# 2.2 First Order Absorption
# -------------------------------------------------------------------------------------------------------------------------

MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2 = PKModel( nameModel = "MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2",
                                                                 descriptionModel =  list("Michaelis-Menten", "2 Compartments", "FirstOrder", "Single Dose"),
                                                                 equationsModel = ModelODEquations(
                                                                   list("RespPK1" = expression(C1),
                                                                        "RespPK2" = expression(C2)),
                                                                   list("Deriv_C1" = expression(-Vm*C1/(Km+C1) - k12*C1 + k21*V2/V*C2 + dose_RespPK1/V*ka*exp(-ka*t)),
                                                                        "Deriv_C2" = expression(k12*V/V2*C1 - k21*C2))))

# -------------------------------------------------------------------------------------------------------------------
# 2.3 Infusion
# -------------------------------------------------------------------------------------------------------------------

MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2 = PKModel( nameModel = "MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2",
                                                             descriptionModel = list("Michaelis-Menten","2 Compartments", "Infusion", "Single Dose"),
                                                             equationsModel = ModelInfusionODEquations(
                                                               list("DuringInfusion_RespPK1" = expression(C1),
                                                                    "DuringInfusion_RespPK2" = expression(C2)),
                                                               list("AfterInfusion_RespPK1" = expression(C1),
                                                                    "AfterInfusion_RespPK2" = expression(C2)),
                                                               list("Deriv_DuringInfusion_C1" = expression(-Vm*C1/(Km+C1) + dose_RespPK1/V/Tinf_RespPK1),
                                                                    "Deriv_DuringInfusion_C2" = expression(k12*V/V2*C1 - k21*C2)),
                                                               list("Deriv_AfterInfusion_C1" = expression(-Vm*C1/(Km+C1)),
                                                                    "Deriv_AfterInfusion_C2" = expression(k12*V/V2*C1 - k21*C2))))


# -------------------------------------------------------------------------------------------------------------------------
# PD Models
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# Immediate Response Models
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# Drug action models
# -------------------------------------------------------------------------------------------------------------------------

# linear
ImmediateDrugLinear_S0Alin = PDModel( nameModel = "ImmediateDrugLinear_S0Alin",
                                      descriptionModel = list("Immediate Response","Drug action ", "Linear"),
                                      equationsModel = ModelEquations( list("RespPD" = expression(S0 + RespPK * Alin ))))

# quadratic
ImmediateDrugImaxQuadratic_S0AlinAquad = PDModel( nameModel = "ImmediateDrugImaxQuadratic_S0AlinAquad",
                                                  descriptionModel = list("Immediate Response", "Drug action ", "Quadratic"),
                                                  equationsModel = ModelEquations( list("RespPD" = expression(S0 + RespPK * Alin + Alin * (RespPK)**2))))

# logarithmic
ImmediateDrugImaxLogarithmic_S0Alog = PDModel( nameModel = "ImmediateDrugImaxLogarithmic_S0Alog",
                                               descriptionModel = list("Immediate Response", "Drug action ", "Logarithmic"),
                                               equationsModel = ModelEquations( list("RespPD" = expression(S0 + Alog * log(RespPK)))))
# Emax
ImmediateDrugEmax_S0EmaxC50 = PDModel( nameModel = "ImmediateDrugEmax_S0EmaxC50",
                                       descriptionModel = list("Immediate Response", "Drug action ", "Emax"),
                                       equationsModel = ModelEquations(list("RespPD" = expression(S0 + Emax*RespPK/(RespPK+C50)))))

# Sigmoid Emax
ImmediateDrugSigmoidEmax_S0EmaxC50gamma = PDModel( nameModel = "ImmediateDrugSigmoidEmax_S0EmaxC50gamma",
                                                   descriptionModel = list("Immediate Response", "Drug action ", "Emax"),
                                                   equationsModel = ModelEquations(list("RespPD" = expression(S0 + Emax*(RespPK**gamma)/(RespPK**gamma+C50**gamma)))))

# Imax
ImmediateDrugImax_S0ImaxC50 = PDModel( nameModel = "ImmediateDrugImax_S0ImaxC50",
                                       descriptionModel = list("Immediate Response","Drug action", "Imax"),
                                       equationsModel = ModelEquations( list("RespPD" = expression( S0 * (1 - Imax * RespPK/( RespPK + C50 ))))))

# Sigmoid Imax
ImmediateDrugImax_S0ImaxC50_gamma = PDModel( nameModel = "ImmediateDrugImax_S0ImaxC50_gamma",
                                             descriptionModel = list("Immediate Response","Drug action", "Imax"),
                                             equationsModel = ModelEquations( list("RespPD" = expression(S0 * (1 - Imax * RespPK/(RespPK**gamma + C50**gamma ))))))

# -------------------------------------------------------------------------------------------------------------------------
# Baseline/disease models
# -------------------------------------------------------------------------------------------------------------------------

# Constant
ImmediateBaselineConstant_S0 = PDModel( nameModel = "ImmediateBaselineConstant_S0",
                                        descriptionModel = list("Immediate Response", "BaselineDisease", "Constant"),
                                        equationsModel = ModelEquations( list("RespPD" = expression(S0))))

# Linear
ImmediateBaselineLinear_S0kprog = PDModel( nameModel = "ImmediateBaselineLinear_S0kprog",
                                           descriptionModel = list("Immediate Response", "BaselineDisease", "Linear"),
                                           equationsModel = ModelEquations( list("RespPD" = expression(S0 + kprog*t))))

# Exponential disease increase
ImmediateBaselineExponentialincrease_S0kprog = PDModel( nameModel = "ImmediateBaselineExponentialincrease_S0kprog",
                                                        descriptionModel = list("Immediate Response", "BaselineDisease", "Exponential disease increase"),
                                                        equationsModel = ModelEquations( list("RespPD" = expression(S0*exp(-kprog*t)))))
# Exponential disease decrease
ImmediateBaselineExponentialdecrease_S0kprog = PDModel( nameModel = "ImmediateBaselineExponentialdecrease_S0kprog",
                                                        descriptionModel = list("Immediate Response", "BaselineDisease", "Exponential disease decrease"),
                                                        equationsModel = ModelEquations( list("RespPD" = expression(S0*(1-exp(-kprog*t))))))

# -------------------------------------------------------------------------------------------------------------------------
# Turnover Models
# -------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------
# Models with impact on the input (Rin)
# -------------------------------------------------------------------------------------------------------------------------

# Emax
TurnoverRinEmax_RinEmaxCC50koutE = PDModel( nameModel = "TurnoverRinEmax_RinEmaxCC50koutE",
                                            descriptionModel = list("Turnover", "Rin", "Emax"),
                                            equationsModel = ModelODEquations(
                                              list("RespPD" = expression( E )),
                                              list("Deriv_E" = expression(Rin*(1+(Emax*RespPK)/(RespPK+C50))-kout*E))))

# Sigmoid Emax
TurnoverRinSigmoidEmax_RinEmaxCC50koutE = PDModel(nameModel = "TurnoverRinSigmoidEmax_RinEmaxCC50koutE",
                                                  descriptionModel = list("Turnover", "Rin", "Sigmoid Emax"),
                                                  equationsModel = ModelODEquations(
                                                    list("RespPD" = expression( E )),
                                                    list("Deriv_E" = expression(Rin*(1+(Emax*RespPK**gamma)/(RespPK**gamma+C50**gamma))-kout*E))))

# Imax
TurnoverRinImax_RinImaxCC50koutE = PDModel(nameModel = "TurnoverRinImax_RinImaxCC50koutE",
                                           descriptionModel = list("Turnover", "Rin", "Imax"),
                                           equationsModel = ModelODEquations(
                                             list("RespPD" = expression( E )),
                                             list("Deriv_E" = expression(Rin*(1-(Imax*RespPK)/(RespPK+C50))-kout*E))))

# Sigmoid Imax
TurnoverRinSigmoidImax_RinImaxCC50koutE = PDModel(nameModel = "TurnoverRinSigmoidImax_RinImaxCC50koutE",
                                                  descriptionModel = list("Turnover", "Rin", "Sigmoid Imax"),
                                                  equationsModel = ModelODEquations(
                                                    list("RespPD" = expression( E )),
                                                    list("Deriv_E" = expression(Rin*(1-(Imax*RespPK**gamma)/(RespPK**gamma+C50**gamma))-kout*E))))

# Full Imax
TurnoverRinFullImax_RinCC50koutE = PDModel( nameModel = "TurnoverRinFullImax_RinCC50koutE",
                                            descriptionModel =  list("Turnover", "Rin", "Full Imax"),
                                            equationsModel = ModelODEquations(
                                              list("RespPD" = expression( E )),
                                              list("Deriv_E" = expression(Rin*(1-(RespPK)/(RespPK+C50))-kout*E))))

# Sigmoid Full Imax
TurnoverRinSigmoidFullImax_RinCC50koutE = PDModel( nameModel = "TurnoverRinSigmoidFullImax_RinCC50koutE",
                                                   descriptionModel = list("Turnover", "Rin", "Sigmoid Full Imax"),
                                                   equationsModel = ModelODEquations(
                                                     list("RespPD" = expression( E )),
                                                     list("Deriv_E" = expression(Rin*(1-RespPK**gamma/(RespPK**gamma+C50**gamma))-kout*E))))

# -------------------------------------------------------------------------------------------------------------------------
# Models with impact on the output (kout)
# -------------------------------------------------------------------------------------------------------------------------

# Emax
TurnoverkoutEmax_RinEmaxCC50koutE = PDModel( nameModel = "TurnoverRinEmax_RinEmaxCC50koutE",
                                             descriptionModel = list("Turnover", "Rin", "Emax"),
                                             equationsModel = ModelODEquations(
                                               list("RespPD" = expression( E )),
                                               list("Deriv_E" = expression( Rin*(1+(Emax*RespPK)/(RespPK+C50))-kout*E))))

# Sigmoid Emax
TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma = PDModel( nameModel = "TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma",
                                                         descriptionModel = list("Turnover", "kout", "Sigmoid Emax"),
                                                         equationsModel = ModelODEquations(
                                                           list("RespPD" = expression( E )),
                                                           list("Deriv_E" = expression(Rin-kout*(1+(Emax*RespPK**gamma)/(RespPK**gamma+C50**gamma))*E))))
# Imax
TurnoverkoutImax_RinImaxCC50koutE = PDModel( nameModel = "TurnoverkoutImax_RinImaxCC50koutE",
                                             descriptionModel = list("Turnover", "kout", "Imax"),
                                             equationsModel = ModelODEquations(
                                               list("RespPD" = expression( E )),
                                               list("Deriv_E" = expression(Rin-kout*(1-Imax*RespPK/(RespPK+C50))*E))))

# Sigmoid Imax
TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma = PDModel( nameModel = "TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma",
                                                         descriptionModel = list("Turnover", "kout", "Sigmoid Imax"),
                                                         equationsModel = ModelODEquations(
                                                           list("RespPD" = expression( E )),
                                                           list("Deriv_E" = expression(Rin-kout*(1-Imax*RespPK**gamma/(RespPK**gamma+C50**gamma))*E))))

# Full Imax
TurnoverkoutFullImax_RinCC50koutE = PDModel( nameModel = "TurnoverkoutFullImax_RinCC50koutE",
                                             descriptionModel = list("Turnover", "kout", "Full Imax"),
                                             equationsModel = ModelODEquations(
                                               list("RespPD" = expression( E )),
                                               list("Deriv_E" = expression(Rin-kout*(1-RespPK/(RespPK+C50))*E))))


# Sigmoid Full Imax
TurnoverkoutSigmoidFullImax_RinCC50koutE = PDModel( nameModel = "TurnoverkoutSigmoidFullImax_RinCC50koutE",
                                                    descriptionModel = list("Turnover", "kout", "Sigmoid Full Imax"),
                                                    equationsModel = ModelODEquations(
                                                      list("RespPD" = expression( E )),
                                                      list("Deriv_E" = expression(Rin-kout*(1-RespPK**gamma/(RespPK**gamma+C50**gamma))*E))))

# -------------------------------------------------------------------------------------------------------------------------
# Add PK and PD models to the library
# -------------------------------------------------------------------------------------------------------------------------

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1BolusSingleDose_kV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1BolusSingleDose_ClV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1BolusSteadyState_ClVtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1BolusSteadyState_kVtau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1InfusionSingleDose_kV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1InfusionSingleDose_ClV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1InfusionSteadyState_kVtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1InfusionSteadyState_ClVtau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1FirstOrderSingleDose_kaClV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1FirstOrderSingleDose_kakV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1FirstOrderSteadyState_kaClVtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, Linear1FirstOrderSteadyState_kakVtau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2BolusSingleDose_kk12k21V)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2BolusSingleDose_ClQV1V2)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2BolusSteadyState_kk12k21Vtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2BolusSteadyState_ClQV1V2tau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2FirstOrderSingleDose_kakk12k21V)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2FirstOrderSingleDose_kaClQV1V2)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2FirstOrderSteadyState_kakk12k21Vtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2FirstOrderSteadyState_kaClQV1V2tau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2InfusionSingleDose_kk12k21V)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2InfusionSingleDose_ClQV1V2)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2InfusionSteadyState_kk12k21Vtau)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  Linear2InfusionSteadyState_ClQV1V2tau)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten1BolusSingleDose_VmKmV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten1FirstOrderSingleDose_kaVmKmV)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten1InfusionSingleDose_VmKmV)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten2BolusSingleDose_VmKmk12k21V1V2)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten2FirstOrderSingleDose_kaVmKmk12k21V1V2)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels, MichaelisMenten2InfusionSingleDose_VmKmk12k21V1V2)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugLinear_S0Alin)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugImaxQuadratic_S0AlinAquad)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugImaxLogarithmic_S0Alog)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugEmax_S0EmaxC50)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugSigmoidEmax_S0EmaxC50gamma)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugImax_S0ImaxC50)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateDrugImax_S0ImaxC50_gamma)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateBaselineConstant_S0)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateBaselineLinear_S0kprog)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateBaselineExponentialincrease_S0kprog)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  ImmediateBaselineExponentialdecrease_S0kprog)

PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverRinSigmoidEmax_RinEmaxCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverRinImax_RinImaxCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverRinSigmoidImax_RinImaxCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverRinFullImax_RinCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverRinSigmoidFullImax_RinCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutEmax_RinEmaxCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutImax_RinImaxCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutFullImax_RinCC50koutE)
PFIMLibraryOfModels <- addModel( PFIMLibraryOfModels,  TurnoverkoutSigmoidFullImax_RinCC50koutE)

##########################################################################################################
# END Function "FillLibraryOfModels"
##########################################################################################################




