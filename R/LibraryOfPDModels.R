#' @include Model.R

# ======================================================================================================
# Library of the PK models
# ======================================================================================================

LibraryOfPDModels = function(){

  # -------------------------------------------------------------------------------------------------------------------------
  # Immediate Response Models
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # Drug action models
  # -------------------------------------------------------------------------------------------------------------------------

  # linear
  ImmediateDrugLinear_S0Alin = ModelAnalytic( name = "ImmediateDrugLinear_S0Alin",
                                              outcomes = list("RespPD"),
                                              description = list("Immediate Response","Drug action ", "Linear"),
                                              equations = list("RespPD" = "S0 + RespPK * Alin" ),
                                              modelError = list())

  # quadratic
  ImmediateDrugImaxQuadratic_S0AlinAquad = ModelAnalytic( name = "ImmediateDrugImaxQuadratic_S0AlinAquad",
                                                          outcomes = list("RespPD"),
                                                          description = list("Immediate Response","Drug action ", "Quadratic"),
                                                          equations = list("RespPD" = "S0 + RespPK * Alin + Alin * (RespPK)**2" ),
                                                          modelError = list())

  # logarithmic
  ImmediateDrugImaxLogarithmic_S0Alog = ModelAnalytic( name = "ImmediateDrugImaxLogarithmic_S0Alog",
                                                       outcomes = list("RespPD"),
                                                       description = list("Immediate Response","Drug action ", "Logarithmic"),
                                                       equations = list("RespPD" = "S0 + Alog * log(RespPK)" ),
                                                       modelError = list())
  # Emax
  ImmediateDrugEmax_S0EmaxC50 = ModelAnalytic( name = "ImmediateDrugEmax_S0EmaxC50",
                                               outcomes = list("RespPD"),
                                               description = list("Immediate Response","Drug action ", "Logarithmic"),
                                               equations = list("RespPD" = "S0 + Emax*RespPK/(RespPK+C50)" ),
                                               modelError = list())
  # Sigmoid Emax
  ImmediateDrugSigmoidEmax_S0EmaxC50gamma = ModelAnalytic( name = "ImmediateDrugSigmoidEmax_S0EmaxC50gamma",
                                                           outcomes = list("RespPD"),
                                                           description = list("Immediate Response", "Drug action ", "Emax"),
                                                           equations = list("RespPD" = "S0 + Emax*(RespPK**gamma)/(RespPK**gamma+C50**gamma)" ),
                                                           modelError = list())
  # Imax
  ImmediateDrugImax_S0ImaxC50 = ModelAnalytic( name = "ImmediateDrugImax_S0ImaxC50",
                                               description = list("Immediate Response","Drug action", "Imax"),
                                               outcomes = list("RespPD"),
                                               equations =  list("RespPD" = "S0 * (1 - Imax * RespPK/( RespPK + C50 ))"),
                                               modelError = list())
  # Sigmoid Imax
  ImmediateDrugImax_S0ImaxC50_gamma = ModelAnalytic( name = "ImmediateDrugImax_S0ImaxC50_gamma",
                                                     description = list("Immediate Response","Drug action", "Imax"),
                                                     outcomes = list("RespPD"),
                                                     equations =  list("RespPD" = "S0 * (1 - Imax * RespPK/(RespPK**gamma + C50**gamma ) )"),
                                                     modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Baseline/disease models
  # -------------------------------------------------------------------------------------------------------------------------

  # Constant
  ImmediateBaselineConstant_S0 = ModelAnalytic( name = "ImmediateBaselineConstant_S0",
                                                description = list("Immediate Response", "BaselineDisease", "Constant"),
                                                outcomes = list("RespPD"),
                                                equations =  list("RespPD" = "S0"),
                                                modelError = list())

  # Linear
  ImmediateBaselineLinear_S0kprog = ModelAnalytic( name = "ImmediateBaselineLinear_S0kprog",
                                                   description = list("Immediate Response", "BaselineDisease", "Linear"),
                                                   outcomes = list("RespPD"),
                                                   equations =  list("RespPD" = "S0 + kprog*t"),
                                                   modelError = list())
  # Exponential disease increase
  ImmediateBaselineExponentialincrease_S0kprog = ModelAnalytic( name = "ImmediateBaselineExponentialincrease_S0kprog",
                                                                description =  list("Immediate Response", "BaselineDisease", "Exponential disease increase"),
                                                                outcomes = list("RespPD"),
                                                                equations =  list("RespPD" = "S0*exp(-kprog*t)"),
                                                                modelError = list())
  # Exponential disease decrease
  ImmediateBaselineExponentialdecrease_S0kprog = ModelAnalytic( name = "ImmediateBaselineExponentialdecrease_S0kprog",
                                                                description =  list("Immediate Response", "BaselineDisease", "Exponential disease decrease"),
                                                                outcomes = list("RespPD"),
                                                                equations =  list("RespPD" = "S0*(1-exp(-kprog*t))"),
                                                                modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Turnover Models
  # -------------------------------------------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------------------------------------------
  # Models with impact on the input (Rin)
  # -------------------------------------------------------------------------------------------------------------------------

  # Emax
  TurnoverRinEmax_RinEmaxCC50koutE = ModelODE( name = "TurnoverRinEmax_RinEmaxCC50koutE",
                                               description = list("Turnover", "Rin", "Emax"),
                                               outcomes = list("RespPD" = "E"),
                                               equations = list("Deriv_E" = "Rin*(1+(Emax*RespPK)/(RespPK+C50))-kout*E"),
                                               modelError = list())

  # Sigmoid Emax
  TurnoverRinSigmoidEmax_RinEmaxCC50koutE = ModelODE( name = "TurnoverRinSigmoidEmax_RinEmaxCC50koutE",
                                                      description = list("Turnover", "Rin", "Sigmoid Emax"),
                                                      outcomes = list("RespPD" = "E"),
                                                      equations = list("Deriv_E" = "Rin*(1+(Emax*RespPK**gamma)/(RespPK**gamma+C50**gamma))-kout*E"),
                                                      modelError = list())

  # Imax
  TurnoverRinImax_RinImaxCC50koutE = ModelODE( name = "TurnoverRinImax_RinImaxCC50koutE",
                                               description = list("Turnover", "Rin", "Full Imax"),
                                               outcomes = list("RespPD" = "E"),
                                               equations = list("Deriv_E" = "Rin*(1-(Imax*RespPK)/(RespPK+C50))-kout*E"),
                                               modelError = list())

  # Sigmoid Imax
  TurnoverRinSigmoidImax_RinImaxCC50koutE = ModelODE( name = "TurnoverRinSigmoidImax_RinImaxCC50koutE",
                                                      description = list("Turnover", "Rin", "Sigmoid Imax"),
                                                      outcomes = list("RespPD" = "E"),
                                                      equations = list("Deriv_E" = "Rin*(1-(Imax*RespPK**gamma)/(RespPK**gamma+C50**gamma))-kout*E"),
                                                      modelError = list())

  # Full Imax
  TurnoverRinFullImax_RinCC50koutE = ModelODE( name = "TurnoverRinFullImax_RinCC50koutE",
                                               description = list("Turnover", "Rin", "Full Imax"),
                                               outcomes = list("RespPD" = "E"),
                                               equations = list("Deriv_E" = "(Rin*(1-(RespPK)/(RespPK+C50))-kout*E)"),
                                               modelError = list())
  # Sigmoid Full Imax
  TurnoverRinSigmoidFullImax_RinCC50koutE = ModelODE( name = "TurnoverRinSigmoidFullImax_RinCC50koutE",
                                                      description = list("Turnover", "Rin", "Sigmoid Full Imax"),
                                                      outcomes = list("RespPD" = "E"),
                                                      equations = list("Deriv_E" = "Rin*(1-RespPK**gamma/(RespPK**gamma+C50**gamma))-kout*E"),
                                                      modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Models with impact on the output (kout)
  # -------------------------------------------------------------------------------------------------------------------------

  # Emax
  TurnoverkoutEmax_RinEmaxCC50koutE = ModelODE( name = "TurnoverkoutEmax_RinEmaxCC50koutE",
                                                description = list("Turnover", "Rin", "Emax"),
                                                outcomes = list("RespPD" = "E"),
                                                equations = list("Deriv_E" = "Rin*(1+(Emax*RespPK)/(RespPK+C50))-kout*E"),
                                                modelError = list())

  # Sigmoid Emax
  TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma = ModelODE( name = "TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma",
                                                            description = list("Turnover", "Rin", "Sigmoid Emax"),
                                                            outcomes = list("RespPD" = "E"),
                                                            equations = list("Deriv_E" = "Rin-kout*(1+(Emax*RespPK**gamma)/(RespPK**gamma+C50**gamma))*E"),
                                                            modelError = list())

  # Imax
  TurnoverkoutImax_RinImaxCC50koutE = ModelODE( name = "TurnoverkoutImax_RinImaxCC50koutE",
                                                description = list("Turnover", "Rin", "Imax"),
                                                outcomes = list("RespPD" = "E"),
                                                equations = list("Deriv_E" = "Rin-kout*(1-Imax*RespPK/(RespPK+C50))*E"),
                                                modelError = list())

  # Sigmoid Imax
  TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma = ModelODE( name = "TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma",
                                                            description = list("Turnover", "kout", "Sigmoid Imax"),
                                                            outcomes = list("RespPD" = "E"),
                                                            equations = list("Deriv_E" = "Rin-kout*(1-Imax*RespPK**gamma/(RespPK**gamma+C50**gamma))*E"),
                                                            modelError = list())

  # Full Imax
  TurnoverkoutFullImax_RinCC50koutE = ModelODE( name = "TurnoverkoutFullImax_RinCC50koutE",
                                                description = list("Turnover", "kout", "Full Imax"),
                                                outcomes = list("RespPD" = "E"),
                                                equations = list("Deriv_E" = "Rin-kout*(1-RespPK/(RespPK+C50))*E"),
                                                modelError = list())

  # Sigmoid Full Imax
  TurnoverkoutSigmoidFullImax_RinCC50koutE = ModelODE( name = "TurnoverkoutSigmoidFullImax_RinCC50koutE",
                                                       description = list("Turnover", "kout", "Sigmoid Full Imax"),
                                                       outcomes = list("RespPD" = "E"),
                                                       equations = list("Deriv_E" = "Rin-kout*(1-RespPK**gamma/(RespPK**gamma+C50**gamma))*E"),
                                                       modelError = list())

  # -------------------------------------------------------------------------------------------------------------------------
  # Add models to the library
  # -------------------------------------------------------------------------------------------------------------------------

  pdModels = list(
    ImmediateDrugLinear_S0Alin,
    ImmediateDrugImaxQuadratic_S0AlinAquad,
    ImmediateDrugImaxLogarithmic_S0Alog,
    ImmediateDrugEmax_S0EmaxC50,
    ImmediateDrugSigmoidEmax_S0EmaxC50gamma,
    ImmediateDrugImax_S0ImaxC50,
    ImmediateDrugImax_S0ImaxC50_gamma,
    ImmediateBaselineConstant_S0,
    ImmediateBaselineLinear_S0kprog,
    ImmediateBaselineExponentialincrease_S0kprog,
    ImmediateBaselineExponentialdecrease_S0kprog,
    TurnoverRinEmax_RinEmaxCC50koutE,
    TurnoverRinSigmoidEmax_RinEmaxCC50koutE,
    TurnoverRinFullImax_RinCC50koutE,
    TurnoverRinImax_RinImaxCC50koutE,
    TurnoverRinSigmoidImax_RinImaxCC50koutE,

    TurnoverRinFullImax_RinCC50koutE,
    TurnoverRinSigmoidFullImax_RinCC50koutE,
    TurnoverkoutEmax_RinEmaxCC50koutE,
    TurnoverkoutSigmoidEmax_RinEmaxCC50koutEgamma,
    TurnoverkoutImax_RinImaxCC50koutE,
    TurnoverkoutSigmoidImax_RinImaxCC50koutEgamma,
    TurnoverkoutFullImax_RinCC50koutE,
    TurnoverkoutSigmoidFullImax_RinCC50koutE )

  return( pdModels )
}

##########################################################################################################
# END Class "LibraryOfPDModels"
##########################################################################################################
