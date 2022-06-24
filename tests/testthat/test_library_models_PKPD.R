
############################################################################################################################

# PKPD MODELS

############################################################################################################################

# PK analytic & PD analytic

context("Model PKPD : PK Linear1FirstOrderSingleDose_kaClV & PD ImmediateDrugImax_S0ImaxC50")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "PKPD_analytic_1_dose_populationFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PKPD model
  MyPKPDModel = getModel(PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV", "ImmediateDrugImax_S0ImaxC50")

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, MyPKPDModel )

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 8,
                       omega = sqrt( 0.020 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.13,
                        omega = sqrt( 0.06 ),
                        distribution = LogNormalDistribution() )

  pS0 = ModelParameter( "S0", mu = 100,
                        omega = sqrt( 0.1 ),
                        distribution = LogNormalDistribution() )

  pC50 = ModelParameter( "C50", mu = 0.17,
                         omega = sqrt(0.7),
                         distribution = LogNormalDistribution() )

  pka = ModelParameter( "ka", mu = 1.6,
                        omega = sqrt( 0.1 ),
                        distribution = LogNormalDistribution() )

  pImax = ModelParameter( "Imax", mu = 0.73,
                          omega = sqrt(0.3),
                          distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pC50 )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pS0 )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pImax )

  ### Create and add the responses to the statistical model

  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPD", Constant( sigma_inter = 2 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design("MyDesign")
  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 32 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0, 2, 3, 8, 12, 24, 36, 50, 72, 120 ) ) )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPD", sample_time = c( 0, 4, 8, 12, 24, 36, 72, 100, 120 ) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK",
                                                           time_dose = c(0),
                                                           amount_dose = c( 100 ) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )
  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  # Evaluation of the Design
  individualFim <- EvaluateIndividualFIM( MyProject )

  matrixFisherIndividualFIM = getFim(individualFim)
  detIndividualFim = det(matrixFisherIndividualFIM)

  valueDetIndividualFim = 2.461834e+14

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)

}) # end test


############################################################################################################################

# PK Infusion analytic & PD analytic

context("Model PKPD : PK Linear1InfusionSingleDose_ClV & PD ImmediateDrugLinear_S0Alin")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PKPD model
  MyPKPDModel = getModel(PFIMLibraryOfModels, "Linear1InfusionSingleDose_ClV", "ImmediateDrugLinear_S0Alin")
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, MyPKPDModel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 3.5,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 2,
                        omega = sqrt( 0.09 ),
                        distribution = LogNormalDistribution() )

  pAlin = ModelParameter( "Alin", mu = 10,
                          omega = sqrt( 0.5 ),
                          distribution = LogNormalDistribution() )

  pS0 = ModelParameter( "S0", mu = 0,
                        omega = sqrt( 0.0 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pAlin )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pS0 )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.1, sigma_slope = 0.1 ) ) )
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPD", Constant( sigma_inter = 0.8 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 40 )

  #brasTest <- addSampling( brasTest, SamplingTimes( outcome = "Resp1", sample_time = c(0, 0.5, 1, 2, 4, 10, 12, 12.5, 13, 14, 16, 22, 24, 24.5, 25, 26, 28, 34, 36, 36.5, 37, 38, 40, 46, 48, 48.5, 49, 50, 52, 58) ) )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0, 1,2,5,7,8, 10,12,14, 15, 16, 20, 21, 30) ) )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPD", sample_time = c(0, 2, 10, 12, 14, 20, 30)  ) )

  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", Tinf=c(2,2,3,3),
                                                           time_dose = c(0,12,24,36) ,
                                                           amount_dose = c(30,30,30,30) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the IndividualFIM
  MyEvaluationInd <- EvaluateIndividualFIM( MyProject )
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  matrixFisherIndividualFIM = getFim(MyEvaluationInd)

  detPopulationFim = det(matrixFisherPopulationFIM)
  detIndividualFim = det(matrixFisherIndividualFIM)

  valueDetIndividualFim = 5.744219e+12
  valueDetPopulationFim = 7.064801e+23

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################

context("Model PKPD : PK Linear1FirstOrderSingleDose_kaClV & PD TurnoverRinFullImax_RinCC50koutE")

test_that("", {

  MyProject <- PFIMProject(name = "Test PFIM")

  ### Create the ODE model
  MyModel <- StatisticalModel()

  MyPKPDModel = getModel( PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV", "TurnoverRinFullImax_RinCC50koutE" )

  MyModel <- defineModelEquations( MyModel, MyPKPDModel )

  ### Define the variables of the ode model
  vC1 <- ModelVariable( "C1" )
  vC2 <- ModelVariable( "C2" )

  MyModel <- defineVariable( MyModel, vC1 )
  MyModel <- defineVariable( MyModel, vC2 )

  ### Set fixed effects (mu), standard deviation of random effects (omega) and distribution of each parameter
  pka <- ModelParameter( "ka", mu = 1.6,
                         omega = sqrt( 0.7 ),
                         distribution = LogNormalDistribution() )

  pV <- ModelParameter( "V", mu = 8,
                        omega = sqrt( 0.02 ),
                        distribution = LogNormalDistribution() )

  pCl <- ModelParameter( "Cl", mu = 0.13,
                         omega = sqrt( 0.06 ),
                         distribution = LogNormalDistribution() )

  pRin <- ModelParameter( "Rin", mu = 5.4,
                          omega = sqrt( 0.2 ),
                          distribution = LogNormalDistribution() )

  pkout <- ModelParameter( "kout", mu = 0.06,
                           omega = sqrt( 0.02 ),
                           distribution = LogNormalDistribution() )

  pC50 <- ModelParameter( "C50", mu = 1.2,
                          omega = sqrt( 0.01 ),
                          distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyModel <- defineParameter( MyModel, pka )
  MyModel <- defineParameter( MyModel, pV )
  MyModel <- defineParameter( MyModel, pCl )
  MyModel <- defineParameter( MyModel, pRin )
  MyModel <- defineParameter( MyModel, pkout )
  MyModel <- defineParameter( MyModel, pC50 )

  ### Error model (standard deviations)
  MyModel <- addResponse( MyModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )
  MyModel <- addResponse( MyModel, Response( "RespPD", Constant( sigma_inter = 4 ) ) )

  ### Assign the model to the project
  MyProject <- defineStatisticalModel( MyProject, MyModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  arm1 <- Arm( name="Bras test", arm_size = 32, cond_init=list( "C1"=0, "C2"= 90 ))

  arm1 <- addSampling( arm1, SamplingTimes( outcome = "RespPK", sample_time = c(0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120) , initialTime = 0 ) )
  arm1 <- addSampling( arm1, SamplingTimes( outcome = "RespPD", sample_time = c(0, 24, 36, 48, 72, 96, 120, 144), initialTime = 0 ) )

  ### Add administration
  arm1 <- addAdministration( arm1, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, arm1 )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 3.627313e+40
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})






