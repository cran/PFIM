############################################################################################################################

# PK MODELS

############################################################################################################################

context(" Model PK 1cpt : Linear1BolusSingleDose_kV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1BolusSingleDose_kV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "k", distribution = LogNormal( mu = 0.25, omega = sqrt(0.25) ) ),
    ModelParameter( name = "V",  distribution = LogNormal( mu = 15, omega = sqrt(0.1) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK",
                                sigmaInter = 0.5, sigmaSlope = 0.15
                                )
  modelError = list( errorModelRespPK )

  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK",
                                 samplings = c( 0.33, 1.5, 5, 12 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1BolusSingleDose_kV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
                              designs = list( design1 ),
                              fim = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )
  #show( evaluationFIM )



  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detPopulationFim = getDeterminant(fim)

  valueDetPopulationFim = 1.456485e+18

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol)

})


############################################################################################################################


context(" Model PK 1cpt : Linear1BolusSingleDose_ClV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1BolusSingleDose_ClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "Cl", distribution = LogNormal( mu = 3.75, omega = sqrt(0.25) ) ),
    ModelParameter( name = "V",  distribution = LogNormal( mu = 15, omega = sqrt(0.1) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )

  # administration
  administration = Administration( outcome = "RespPK", timeDose = c( 0 ), dose = c( 100 ) )
  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.33, 1.5, 5, 12 ) )
  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1BolusSingleDose_ClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 7.292816e+15

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol )

})


############################################################################################################################


context(" Model PK 1cpt : Linear1InfusionSingleDose_kV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1InfusionSingleDose_kV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V",  distribution = LogNormal( mu = 3.5, omega = sqrt(0.09) ) ),
    ModelParameter( name = "k",  distribution = LogNormal( mu = 0.6, omega = sqrt(0.09) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.1, sigmaSlope = 0.1 )
  modelError = list( errorModelRespPK )

  # administration
  administration = Administration( outcome = "RespPK",  Tinf = c(2), timeDose = c( 0 ), dose = c( 30 ) )
  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 4, 8 ) )
  # arm
  arm1 = Arm( name = "BrasTest",
              size = 40,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1InfusionSingleDose_kV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 0 # 1.907967e+12 #3.414077e+13
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})


############################################################################################################################


context(" Model PK 1cpt : Linear1InfusionSingleDose_VCl")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1InfusionSingleDose_ClV") #Linear1InfusionSingleDose_VCl
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 3.5, omega = sqrt(0.09) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 2, omega = sqrt(0.09) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.1, sigmaSlope = 0.1 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   Tinf = c(2),
                                   timeDose = c( 0 ),
                                   dose = c( 30 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 4, 8 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 40,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1InfusionSingleDose_ClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
                              designs = list( design1 ),
                              fim = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )

  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detPopulationFim = getDeterminant(fim)

  valueDetPopulationFim = 1.558579e+17

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol)

})

############################################################################################################################


context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kakV")

test_that("", {

  ## --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list( "PKModel" = "Linear1FirstOrderSingleDose_kakV" ) )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 2, omega = sqrt(1) ) ),
    ModelParameter( name = "k", distribution = LogNormal( mu = 0.25, omega = sqrt(0.25) ) ),
    ModelParameter( name = "V",  distribution = LogNormal( mu = 15, omega = sqrt(0.1) ) ) )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.33, 1.5, 5, 12 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes ) )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kakV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
                              designs = list( design1 ),
                              fim = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )


  # get the evaluated designs
  designs = getDesigns( evaluationFIM )
  designsNames = getNames(designs)
  designName = designsNames[[1]]
  design = designs[[designName]]

  # get the fim of the design
  fim = getFim( design )

  # get the determinant of the Fisher matrix
  detPopulationFim = getDeterminant(fim)

  valueDetPopulationFim = 2.930397e+20
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 8, omega = sqrt(0.020) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.13, omega = sqrt(0.06) ) ),
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.6, omega = sqrt(0.7) ) )
  )




  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 7.503881e+22


  # Evaluate the Fisher Information Matrix for the individual FIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetIndividualFim =  1532105538

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol)
  expect_equal( detIndividualFim, valueDetIndividualFim, tol)


})

############################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV (BayesianFIM instead of PopulationFIM)")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 8, omega = sqrt(0.020) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.13, omega = sqrt(0.06) ) ),
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.6, omega = sqrt(0.7) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
                              designs = list( design1 ),
                              fim = "Bayesian",
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
  detBayesianFim = getDeterminant(fim)

  valueDetBayesianFim = 7349327
  tol = 1e-6
  expect_equal(valueDetBayesianFim,detBayesianFim, tol)

})

###################################################################################################################################

context("Model PK 1cpt : Linear1InfusionSingleDose_ClV ")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1InfusionSingleDose_ClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 3.5, omega = sqrt(0.09) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 2, omega = sqrt(0.09) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.1, sigmaSlope = 0.1 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   Tinf=c(2),
                                   tau=c(12),
                                   dose = c( 30,50,30,50 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0, 1,2,5,7,8, 10,12,14, 15, 16, 20, 21, 30 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 40,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1InfusionSingleDose_ClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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


  valueDetPopulationFim =  1.703046e+19

  tol = 1e-6
  expect_equal(valueDetPopulationFim,detPopulationFim, tol)

})

###################################################################################################################################

context("Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 8, omega = sqrt(0.020) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.13, omega = sqrt(0.06) ) ),
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.6, omega = sqrt(0.7) ) )
  )

  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0, 80, 160 ),
                                   dose = c( 100,100,100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c(  0.5, 1, 2, 6, 12, 48, 72, 120, 165, 220 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 2.304835e+22


  # Evaluate the Fisher Information Matrix for the individual FIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetIndividualFim = 618022401

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol)
  expect_equal( detIndividualFim, valueDetIndividualFim, tol)

})

###################################################################################################################################

context(" Model PK 2cpts : Linear2BolusSingleDose_ClQV1V2")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear2BolusSingleDose_ClQV1V2")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.4, omega = sqrt(0.2) ) ),
    ModelParameter( name = "V1", distribution = LogNormal( mu = 10, omega = sqrt(0.1) ) ),
    ModelParameter( name = "Q", distribution = LogNormal( mu = 2, omega = sqrt(0.05) ) ),
    ModelParameter( name = "V2", distribution = LogNormal( mu = 50, omega = sqrt(0.4) ) )

  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.6, sigmaSlope = 0.07 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 32,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear2BolusSingleDose_ClQV1V2",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 3.587146e+18

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 2cpts : Linear2BolusSingleDose_kk12k21V")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK"),
    modelFromLibrary = list("PKModel" = "Linear2BolusSingleDose_kk12k21V")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "k", distribution = LogNormal( mu = 0.25, omega = sqrt(0.25) ) ),
    ModelParameter( name = "V", distribution = LogNormal( mu = 15.00, omega = sqrt(0.10) ) ),
    ModelParameter( name = "k12",  distribution = LogNormal( mu = 1.00, omega = sqrt(0.40) ) ),
    ModelParameter( name = "k21",  distribution = LogNormal( mu = 0.80, omega = sqrt(0.30) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK",
                                 samplings = c(0.33, 1.5, 3, 5, 8, 12 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear2BolusSingleDose_kk12k21V",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK"),
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

  valueDetPopulationFim = 2.137813e+24

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)


})

###################################################################################################################################

context(" Model PK 1cpt : MichaelisMenten1FirstOrderSingleDose_kaVmKmV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" = "C1" ),
    modelFromLibrary = list("PKModel" = "MichaelisMenten1FirstOrderSingleDose_kaVmKmV") )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "ka", distribution = LogNormal( mu = 1.0, omega = sqrt(0.20) ) ),
    ModelParameter( name = "V", distribution = LogNormal( mu = 15.00, omega = sqrt(0.25) ) ),
    ModelParameter( name = "Vm",  distribution = LogNormal( mu = 0.08, omega = sqrt(0.10) ) ),
    ModelParameter( name = "Km",  distribution = LogNormal( mu = 0.40, omega = sqrt(0.30) ) ) )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK",
                                 samplings = c( 0, 0.33, 1.5, 3, 5, 8, 11, 12 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes ) ,
              initialCondition = list( "C1" = 0 ) )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "MichaelisMenten1FirstOrderSingleDose_kaVmKmV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" = "C1" ),
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


  valueDetPopulationFim = 594724488850

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : MichaelisMenten1BolusSingleDose_VmKmV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" = "C1" ),
    modelFromLibrary = list("PKModel" = "MichaelisMenten1BolusSingleDose_VmKmV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 15.00, omega = sqrt(0.25) ) ),
    ModelParameter( name = "Vm",  distribution = LogNormal( mu = 0.08, omega = sqrt(0.10) ) ),
    ModelParameter( name = "Km",  distribution = LogNormal( mu = 0.40, omega = sqrt(0.30) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "C1",
                                   timeDose = c( 0 ),
                                   dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "C1",
                                 samplings = c( 0, 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 200,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes ) ,
              initialCondition = list( "C1" = 0 )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "MichaelisMenten1BolusSingleDose_VmKmV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" = "C1" ),
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

  valueDetPopulationFim = 0

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})


###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 63.000, omega = 0 ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.513, omega = 0 ) ),
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.050, omega = sqrt(0.1) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0, sigmaSlope = 0.0676 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0 ),
                                   dose = c( 5500 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.01, 1, 3, 5, 7, 10, 13, 17, 24 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 25,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 8.835413e+13

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSingleDose_kaClV")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "V", distribution = LogNormal( mu = 3.5, omega =  sqrt( 0.09 ) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 2.0, omega =  sqrt( 0.09 ) ) ),
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.0, omega = sqrt(0.09) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0.1, sigmaSlope = 0.1 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   timeDose = c( 0,12,24,36,48 ),
                                   dose = c( 30,30,30,30,30 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.5, 1, 4, 8, 12.5, 13, 16, 20, 24.5, 25, 28, 32, 36.5, 37, 40, 44, 48.5, 49, 52, 56 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 40,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSingleDose_kaClV",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 2.835909e+24

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSteadyState_kaClVtau")

test_that("", {

  # --------------------------------------
  # model definition

  # model equations
  modelEquations = list(
    outcomes = list( "RespPK" ),
    modelFromLibrary = list("PKModel" = "Linear1FirstOrderSteadyState_kaClVtau")
  )

  # model parameters
  modelParameters = list(
    ModelParameter( name = "ka",  distribution = LogNormal( mu = 1.050, omega = sqrt(0.1) ) ),
    ModelParameter( name = "Cl",  distribution = LogNormal( mu = 0.513, omega =  0 ) ),
    ModelParameter( name = "V", distribution = LogNormal( mu = 63.000, omega =  0 ) )
  )


  # Error Model
  errorModelRespPK = Combined1( outcome = "RespPK", sigmaInter = 0, sigmaSlope = 0.0676 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "RespPK",
                                   tau = c(24),
                                   dose = c( 5500 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "RespPK", samplings = c( 0.01, 1, 3, 5, 7, 10, 13, 17, 24 ) )

  # arm
  arm1 = Arm( name = "BrasTest",
              size = 25,
              administrations  = list( administration ) ,
              samplingTimes    = list( samplingTimes )
  )

  # design
  design1 = Design( name = "design1",
                    arms = list( arm1 ) )

  # --------------------------------------
  # Evaluation

  # Evaluate the Fisher Information Matrix for the PopulationFIM
  evaluationFIM = Evaluation( name = "Linear1FirstOrderSteadyState_kaClVtau",
                              modelEquations = modelEquations,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outcomes = list( "RespPK" ),
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

  valueDetPopulationFim = 119307107145

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################
# END CODE
############################################################################################################################
