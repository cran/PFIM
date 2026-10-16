# ODE + finite-difference FIM: the determinant is reproducible to ~2e-4 only
# (the solver step sequence differs across platforms, e.g. arm64 macOS).
# CRAN 8.0 failed at tolerance 1e-6 on r-release/oldrel-macos-arm64: waldo
# reported "actual != expected but don't know how to show the difference"
# (relative error just above 1e-6). Compare relatively, with a readable
# failure message, at 1e-3.
odeDetTolerance = 1e-3

expectOdeDet = function( actual, expected, tolerance = odeDetTolerance ) {
  rel = if ( is.finite( actual ) && abs( expected ) > 0 ) {
    abs( actual - expected ) / abs( expected )
  } else {
    Inf
  }
  expect(
    is.finite( actual ) && actual > 0 && rel < tolerance,
    sprintf(
      "ODE FIM det not within relative tolerance %g: rel.err=%s actual=%.16g expected=%.16g",
      tolerance,
      format( rel, digits = 8, scientific = TRUE ),
      actual,
      expected
    )
  )
}

test_that("Model PK 1cpt : MichaelisMenten1FirstOrderSingleDose_kaVmKmV", {

  # --------------------------------------
  # model definition

  # model equations


  modelFromLibrary = list("PKModel" = "MichaelisMenten1FirstOrderSingleDose_kaVmKmV")

  # model modelParameters
  modelParameters = list(
    ModelParameter( name = "ka", distribution = LogNormal( mu = 1.0, omega = sqrt(0.20) ) ),
    ModelParameter( name = "V", distribution = LogNormal( mu = 15.00, omega = sqrt(0.25) ) ),
    ModelParameter( name = "Vm",  distribution = LogNormal( mu = 0.08, omega = sqrt(0.10) ) ),
    ModelParameter( name = "Km",  distribution = LogNormal( mu = 0.40, omega = sqrt(0.30) ) ) )


  # Error Model
  errorModelRespPK = Combined1( output = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
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
                              modelFromLibrary = modelFromLibrary,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outputs = list( "RespPK" = "C1" ),
                              designs = list( design1 ),
                              fimType = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )


  FisherMatrix = getFisherMatrix( evaluationFIM )
  detPopulationFim = det( FisherMatrix$fisherMatrix )
  expectOdeDet( detPopulationFim, 592523761240 )

})


test_that("Model PK 1cpt : MichaelisMenten1BolusSingleDose_VmKm", {

  # model equations

  modelFromLibrary = list("PKModel" = "MichaelisMenten1BolusSingleDose_VmKm")


  # model modelParameters
  modelParameters = list(
    ModelParameter( name = "Vm",  distribution = LogNormal( mu = 0.08, omega = sqrt(0.10) ) ),
    ModelParameter( name = "Km",  distribution = LogNormal( mu = 0.40, omega = sqrt(0.30) ) )
  )


  # Error Model
  errorModelRespPK = Combined1( output = "RespPK", sigmaInter = 0.5, sigmaSlope = 0.15 )
  modelError = list( errorModelRespPK )


  # administration
  administration = Administration( outcome = "C1", timeDose = c( 0 ), dose = c( 100 ) )

  # sampling times
  samplingTimes = SamplingTimes( outcome = "C1",
                                 samplings = c( 0, 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120 ) )

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
  evaluationFIM = Evaluation( name = "MichaelisMenten1BolusSingleDose_VmKm",
                              modelFromLibrary = modelFromLibrary,
                              modelParameters = modelParameters,
                              modelError = modelError,
                              outputs = list( "RespPK" = "C1" ),
                              designs = list( design1 ),
                              fimType = "population",
                              odeSolverParameters = list( atol = 1e-8, rtol = 1e-8 ) )

  evaluationFIM = run( evaluationFIM )


  FisherMatrix = getFisherMatrix( evaluationFIM )
  detPopulationFim = det( FisherMatrix$fisherMatrix )
  expectOdeDet( detPopulationFim, 0.015545625761308865323 )

})


###################################################################################################################################


