############################################################################################################################

# PKPD MODELS

############################################################################################################################

# PK analytic & PD analytic

context("Model PKPD : PK Linear1FirstOrderSingleDose_kaClV & PD ImmediateDrugImax_S0ImaxC50")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK", "RespPD" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV",
                            "PDModel" = "ImmediateDrugImax_S0ImaxC50")
  )


  # model parameters
  modelParameters = list(
    ModelParameter( name = "V",    distribution = LogNormal( mu = 8, omega = 0.02 ) ),
    ModelParameter( name = "Cl",   distribution = LogNormal( mu = 0.13, omega = 0.06 ) ),
    ModelParameter( name = "S0",   distribution = LogNormal( mu = 100, omega = 0.1 ) ),
    ModelParameter( name = "C50",  distribution = LogNormal( mu = 0.17, omega = 0.7 ) ),
    ModelParameter( name = "ka",   distribution = LogNormal( mu = 1.6, omega = 0.1 ) ),
    ModelParameter( name = "Imax", distribution = LogNormal( mu = 0.73, omega = 0.3 ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  errorModelRespPD = Constant( outcome = "RespPD", sigmaInter = 2 )
  modelError = list( errorModelRespPK, errorModelRespPD )


  ## administration
  administrationRespPK = Administration( outcome = "RespPK", timeDose = c( 0 ), dose = c( 100 ) )

  ## sampling times
  samplingTimesRespPK = SamplingTimes( outcome = "RespPK", samplings = c( 0, 2, 3, 8, 12, 24, 36, 72, 120, 144 ) )
  samplingTimesRespPD = SamplingTimes( outcome = "RespPD", samplings = c( 0, 4, 8, 12, 24, 36, 72, 100, 120, 144 ) )

  ## arms
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations = list( administrationRespPK ) ,
              samplingTimes   = list( samplingTimesRespPK, samplingTimesRespPD ) )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "PKPD",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK", "RespPD"),
                              designs = list( design1 ),
                              fim = "individual",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )
  # show( evaluationFIM )

  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detIndividualFim = getDeterminant(fim)

  valueDetIndividualFim = 8.632751e+14

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)

}) # end test


############################################################################################################################

# PK Infusion analytic & PD analytic

context("Model PKPD : PK Linear1InfusionSingleDose_VCl & PD ImmediateDrugLinear_Alin")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK", "RespPD" ),
    modelFromLibrary = list("PKModel" = "Linear1BolusSingleDose_ClV",
                            "PDModel" = "ImmediateDrugLinear_S0Alin")
  )


  # model parameters
  modelParameters = list(
    ModelParameter( name = "V",    distribution = LogNormal( mu = 3.5, omega = sqrt( 0.09 ) ) ),
    ModelParameter( name = "Cl",   distribution = LogNormal( mu = 2, omega = sqrt( 0.09 ) ) ),
    ModelParameter( name = "S0",   distribution = LogNormal( mu = 0.1, omega = sqrt( 0.0 ) ) ),
    ModelParameter( name = "Alin",   distribution = LogNormal( mu = 10, omega = sqrt( 0.5 ) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.1, sigmaSlope = 0.1 )
  errorModelRespPD = Constant( outcome = "RespPD", sigmaInter = 0.8 )
  modelError = list( errorModelRespPK, errorModelRespPD )


  ## administration
  administrationRespPK = Administration( outcome = "RespPK", Tinf=c(2,2,3,3),
                                         timeDose = c(0,12,24,36),
                                         dose = c( 30,30,30,30 )
  )

  ## sampling times
  samplingTimesRespPK = SamplingTimes( outcome = "RespPK", samplings = c(0, 1,2,5,7,8, 10,12,14, 15, 16, 20, 21, 30 ) )
  samplingTimesRespPD = SamplingTimes( outcome = "RespPD", samplings = c( 0, 2, 10, 12, 14, 20, 30 ) )

  ## arms
  arm1 = Arm( name = "BrasTest",
              size = 40,
              administrations = list( administrationRespPK ) ,
              samplingTimes   = list( samplingTimesRespPK, samplingTimesRespPD ) )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "PKPD",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK", "RespPD"),
                              designs = list( design1 ),
                              fim = "individual",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )
  # show( evaluationFIM )

  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detIndividualFim = getDeterminant(fim)


  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "PKPD",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK", "RespPD"),
                              designs = list( design1 ),
                              fim = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )
  # show( evaluationFIM )

  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detPopulationFim = getDeterminant(fim)


  valueDetIndividualFim = 2.505716e+14
  valueDetPopulationFim = 1.619213e+26

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################

context("Model PKPD : PK Linear1FirstOrderSingleDose_kaClV & PD TurnoverRinFullImax_RinCC50koutE")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(

    outcomes = list( "RespPK" = "Cc",
                     "RespPD" = "E"),

    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV",
                            "PDModel" = "TurnoverRinFullImax_RinCC50koutE")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "ka",    distribution = LogNormal( mu = 1.6, omega = sqrt( 0.7 ) ) ),
    ModelParameter( name = "V",   distribution = LogNormal( mu = 8, omega = sqrt( 0.02 ) ) ),
    ModelParameter( name = "Cl",   distribution = LogNormal( mu = 0.13, omega = sqrt( 0.06 ) ) ),
    ModelParameter( name = "Rin",   distribution = LogNormal( mu = 5.4, omega = sqrt( 0.2 ) ) ),
    ModelParameter( name = "kout",   distribution = LogNormal( mu = 0.06, omega = sqrt( 0.02 ) ) ),
    ModelParameter( name = "C50",   distribution = LogNormal( mu = 1.2, omega = sqrt( 0.01 ) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  errorModelRespPD = Constant( outcome = "RespPD", sigmaInter = 0.4 )
  modelError = list( errorModelRespPK, errorModelRespPD )


  ## administration
  administrationRespPK = Administration( outcome = "RespPK",
                                         timeDose = c(0),
                                         dose = c( 100 )
  )

  ## sampling times
  samplingTimesRespPK = SamplingTimes( outcome = "RespPK",
                                       samplings = c(0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120) )
  samplingTimesRespPD = SamplingTimes( outcome = "RespPD",
                                       samplings = c(0, 24, 36, 48, 72, 96, 120, 144) )

  ## arms
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations = list( administrationRespPK ) ,
              samplingTimes   = list( samplingTimesRespPK, samplingTimesRespPD ),
              initialCondition = list( "Cc" = 0 , "E" = 90)
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "PKPD",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list(  "RespPK" = "Cc",
                                                "RespPD" = "E"),
                              designs = list( design1 ),
                              fim = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )
  # show( evaluationFIM )

  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detPopulationFim = getDeterminant(fim)

  valueDetPopulationFim = 3.868994e+46
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})






