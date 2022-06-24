 # PK MODELS

context(" Model PK 1cpt : Linear1BolusSingleDose_kV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1BolusSingleDose_kV")

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 15,
                       omega = sqrt( 0.1 ),
                       distribution = LogNormalDistribution())

  pk = ModelParameter( "k", mu = 0.25,
                       omega = sqrt( 0.25 ),
                       distribution = LogNormalDistribution())

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 200 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0.33, 1.5, 5, 12) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )

  # show( MyEvaluationPop )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)

  detPopulationFim = det( matrixFisherPopulationFIM )

  valueDetPopulationFim = 1.456485e+18

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol)

})


############################################################################################################################


context(" Model PK 1cpt : Linear1BolusSingleDose_ClV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1BolusSingleDose_ClV")

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 15,
                       omega = sqrt( 0.10 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 3.75,
                        omega = sqrt( 0.25),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 200 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0.33, 1.5, 5, 12) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )

  # show( MyEvaluationPop )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)

  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 7.292816e+15

  tol = 1e-6

  expect_equal( detPopulationFim, valueDetPopulationFim, tol )

})

############################################################################################################################


context(" Model PK 1cpt : Linear1InfusionSingleDose_kV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1InfusionSingleDose_kV")

  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  #### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 3.5,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )

  pk = ModelParameter( "k", mu = 0.6,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )


  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.1, sigma_slope = 0.1 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 40 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.5, 1, 4, 8) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", Tinf=c(2), time_dose = c(0) , amount_dose = c(30) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)

  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 3.414077e+13
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})


############################################################################################################################


context(" Model PK 1cpt : Linear1InfusionSingleDose_ClV")

test_that("", {
  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1InfusionSingleDose_ClV")

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  #### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 3.5,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 2.0,
                        omega = sqrt( 0.09 ),
                        distribution = LogNormalDistribution() )


  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.1, sigma_slope = 0.1 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 40 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.5, 1, 4, 8) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", Tinf=c(2), time_dose = c(0) , amount_dose = c(30) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)

  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 1.558579e+17
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################


context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kakV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kakV")

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  #### Set mu and omega for each parameter
  pka = ModelParameter( "ka", mu = 2,
                        omega = sqrt( 1 ),
                        distribution = LogNormalDistribution() )

  pk = ModelParameter( "k", mu = 0.25,
                       omega = sqrt( 0.25 ),
                       distribution = LogNormalDistribution() )

  pV = ModelParameter( "V", mu = 15,
                       omega = sqrt( 0.1 ),
                       distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 200 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.33, 1.5, 5, 12) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0) , amount_dose = c(100) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 2.930397e+20
  tol = 1e-6
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV" )

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 8,
                       omega = sqrt( 0.020 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.13,
                        omega = sqrt( 0.06 ),
                        distribution = LogNormalDistribution() )

  pka = ModelParameter( "ka", mu = 1.6,
                        omega = sqrt( 0.7 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )


  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 32 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK",
                                                    sample_time = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120 ) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK",
                                                           time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  MyEvaluationInd <- EvaluateIndividualFIM( MyProject )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  matrixFisherIndividualFIM = getFim(MyEvaluationInd)

  detPopulationFim = det(matrixFisherPopulationFIM)
  detIndividualFim = det(matrixFisherIndividualFIM)

  valueDetIndividualFim = 1532105538
  valueDetPopulationFim = 7.503881e+22

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV (BayesianFIM instead of PopulationFIM)")

test_that("", {

  ### Create a project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ### Defineequations of the model
  # Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV" )

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 8,
                       omega = sqrt( 0.020 ),
                       distribution = NormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.13,
                        omega = sqrt( 0.06 ),
                        distribution = NormalDistribution() )

  pka = ModelParameter( "ka", mu = 1.6,
                        omega = sqrt( 0.7 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )

  ### Assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 1 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120 ) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  MyEvaluationInd <- EvaluateIndividualFIM( MyProject )
  MyEvaluationBayes <- EvaluateBayesianFIM( MyProject )

  matrixFisherIndividualFIM = getFim(MyEvaluationInd)
  matrixFisherBayesianFIM = getFim(MyEvaluationBayes)

  detIndividualFim = det(matrixFisherIndividualFIM)
  detBayesianFim = det(matrixFisherBayesianFIM)

  valueDetIndividualFim = 1532105538
  valueDetBayesianFim = 4428901

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(valueDetBayesianFim,detBayesianFim, tol)

})

###################################################################################################################################

context("Model PK 1cpt : Linear1InfusionSingleDose_ClV")

test_that("", {

  ### Create a project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Assign the equations to the statistical model
  Linear1InfusionSingleDose_kVCl <- getModel( PFIMLibraryOfModels, "Linear1InfusionSingleDose_ClV" )
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, Linear1InfusionSingleDose_kVCl)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 3.5,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 2,
                        omega = sqrt( 0.09 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model

  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )

  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model

  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.1, sigma_slope = 0.1 ) ) )

  ### Finaly assign the statistical model to the project

  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )


  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 40 )

  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0, 1,2,5,7,8, 10,12,14, 15, 16, 20, 21, 30) ) )

  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", Tinf=c(2), tau=c(12) , amount_dose = c(30,50,30,50) ) )     #brasTest <- addAdministration( brasTest, Administration( outcome = "Resp1", tinf=c(2,2,3,3), amount_dose = [100][c(30,30,50,50)] )

  ### Add the arm to the design

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project

  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
    MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 1.634513e+19

  tol = 1e-6
  expect_equal(valueDetPopulationFim,detPopulationFim, tol)

})

###################################################################################################################################

context("Model PK 1cpt :  Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV" )
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 8,
                       omega = sqrt( 0.020 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.13,
                        omega = sqrt( 0.06 ),
                        distribution = LogNormalDistribution() )

  pka = ModelParameter( "ka", mu = 1.6,
                        omega = sqrt( 0.7 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )


  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 32 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120 ) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  MyEvaluationInd <- EvaluateIndividualFIM( MyProject )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  matrixFisherIndividualFIM = getFim(MyEvaluationInd)

  detPopulationFim = det(matrixFisherPopulationFIM)
  detIndividualFim = det(matrixFisherIndividualFIM)

  valueDetIndividualFim = 1532105538
  valueDetPopulationFim =  7.503881e+22

  tol = 1e-6

  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})


###################################################################################################################################

context("Model PK 1cpt :Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV" )
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel)

  ### Set mu and omega for each parameter
  pV = ModelParameter( "V", mu = 8,
                       omega = sqrt( 0.020 ),
                       distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.13,
                        omega = sqrt( 0.06 ),
                        distribution = LogNormalDistribution() )

  pka = ModelParameter( "ka", mu = 1.6,
                        omega = sqrt( 0.7 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )

  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )

  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )

  ### Create and add the responses to the statistical model

  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )

  ### Finaly assign the statistical model to the project

  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )


  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 32 )

  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c( 0.5, 1, 2, 6, 12, 48, 72, 120, 165, 220 ) ) )

  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0, 80, 160), amount_dose = c(100,100,100) ) )

  ### Add the arm to the design

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project

  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  MyEvaluationInd <- EvaluateIndividualFIM( MyProject )

  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  matrixFisherIndividualFIM = getFim(MyEvaluationInd)

  detPopulationFim = det(matrixFisherPopulationFIM)
  detIndividualFim = det(matrixFisherIndividualFIM)

  valueDetIndividualFim = 618022401
  valueDetPopulationFim = 2.304835e+22

  tol = 1e-6
  expect_equal(detIndividualFim,valueDetIndividualFim, tol)
  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 2cpts : Linear2BolusSingleDose_ClQV1V2")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear2BolusSingleDose_ClQV1V2" )
  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel )

  ### Set mu and omega for each parameter
  pCl = ModelParameter( "Cl", mu = 0.4,
                        omega = sqrt( 0.2 ),
                        distribution = LogNormalDistribution() )

  pV1 = ModelParameter( "V1", mu = 10,
                        omega = sqrt( 0.1 ),
                        distribution = LogNormalDistribution() )

  pQ = ModelParameter( "Q", mu = 2,
                       omega = sqrt( 0.05 ),
                       distribution = LogNormalDistribution() )

  pV2 = ModelParameter( "V2", mu = 50,
                        omega = sqrt( 0.4 ),
                        distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV1 )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pQ )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV2 )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.6, sigma_slope = 0.07 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 32 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120)  ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 3.587146e+18

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 2cpts : Linear2BolusSingleDose_kk12k21V")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "Linear2BolusSingleDose_kk12k21V" )

  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel )

  ### Set mu and omega for each parameter
  pk = ModelParameter( "k", mu = 0.25,
                       omega = sqrt( 0.25 ),
                       distribution = LogNormalDistribution() )

  pV = ModelParameter( "V", mu = 15.00,
                       omega = sqrt( 0.10 ),
                       distribution = LogNormalDistribution() )

  pk12 = ModelParameter( "k12", mu = 1.00,
                         omega = sqrt( 0.4 ),
                         distribution = LogNormalDistribution() )

  pk21 = ModelParameter( "k21", mu = 0.80,
                         omega = sqrt( 0.3 ),
                         distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk12 )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pk21 )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 200 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.33, 1.5, 3, 5, 8, 12)  ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 2.137813e+24

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)


})

###################################################################################################################################

context(" Model PK 1cpt : MichaelisMenten1FirstOrderSingleDose_kaVmKmV")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel<-StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "MichaelisMenten1FirstOrderSingleDose_kaVmKmV")

  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel )

  ### Define the variables of the ode model
  vC1 <- ModelVariable( "C1" )
  MyModel = defineVariable( MyStatisticalModel, vC1 )

  ### Set fixed effects (mu), standard deviation of random effects (omega) and distribution of each parameter
  pka <- ModelParameter( "ka", mu = 1.00,
                         omega = sqrt( 0.20 ),
                         distribution = LogNormalDistribution() )

  pV <- ModelParameter( "V", mu = 15.00,
                        omega = sqrt( 0.25 ),
                        distribution = LogNormalDistribution() )

  pVm <- ModelParameter( "Vm", mu = 0.08,
                         omega = sqrt( 0.10 ),
                         distribution = LogNormalDistribution() )

  pKm <- ModelParameter( "Km", mu = 0.40,
                         omega = sqrt( 0.30 ),
                         distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pVm )
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pKm )

  ### Error model (standard deviations)
  MyModel <- addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Assign the model to the project
  MyProject <- defineStatisticalModel( MyProject, MyModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 200, cond_init=list( "C1"=0 ))
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK",
                                                    sample_time = c(0, 0.33, 1.5, 3, 5, 8, 11, 12),
                                                    initialTime = 0 ) )

  brasTest <- setInitialConditions( brasTest, list( "C1"=0.0) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 588037073988

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : MichaelisMenten1BolusSingleDose_VmKmV")

test_that("", {

  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model

  MyStatisticalModel <- StatisticalModel()

  ###  Get Equations models
  PKmodel <- getModel( PFIMLibraryOfModels, "MichaelisMenten1BolusSingleDose_VmKmV")

  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKmodel )

  ### Define the variables of the ode model
  vC1 <- ModelVariable( "C1" )

  MyStatisticalModel = defineVariable( MyStatisticalModel, vC1 )

  ### Set fixed effects (mu), standard deviation of random effects (omega) and distribution of each parameter
  pV <- ModelParameter( "V", mu = 15.00,
                        omega = sqrt( 0.25 ),
                        distribution = LogNormalDistribution() )

  pVm <- ModelParameter( "Vm", mu = 0.08,
                         omega = sqrt( 0.10 ),
                         distribution = LogNormalDistribution() )

  pKm <- ModelParameter( "Km", mu = 0.40,
                         omega = sqrt( 0.30 ),
                         distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pVm )
  MyStatisticalModel <- defineParameter( MyStatisticalModel, pKm )

  ### Error model (standard deviations)
  MyModel <- addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.5, sigma_slope = 0.15 ) ) )

  ### Assign the model to the project
  MyProject <- defineStatisticalModel( MyProject, MyModel )

  ### Create a design
  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response
  brasTest <- Arm( name="Bras test", arm_size = 200 )

  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK",
                                                    sample_time = c(0, 0.5, 1, 2, 6, 9, 12, 24, 36, 48, 72, 96, 120),
                                                    initialTime = 0 ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0), amount_dose = c(100) ) )

  brasTest <- setInitialConditions( brasTest, list( "C1"= expression( dose_RespPK/V) ) )

  ### Add the arm to the design
  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the PopulationFIM
  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 5.742082e+12

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV")
  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)


  #### Set mu and omega for each parameter
  pka = ModelParameter( "ka", mu = 1.050,
                        omega = sqrt( 0.1 ),
                        distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.513,
                        omega = 0,
                        distribution = LogNormalDistribution() )

  pV = ModelParameter( "V", mu = 63.000,
                       omega = 0,
                       distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl  )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0, sigma_slope = 0.0676 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 25 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.01, 1, 3, 5, 7, 10, 13, 17, 24) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK", time_dose = c(0) , amount_dose = c(5500) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 8.835413e+13

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSingleDose_kaClV")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1FirstOrderSingleDose_kaClV")
  ### Assign the equations to the statistical model
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  #### Set mu and omega for each parameter
  pka = ModelParameter( "ka", mu = 1.0,
                        omega = sqrt( 0.09 ),
                        distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 2.0,
                        omega = sqrt( 0.09 ),
                        distribution = LogNormalDistribution() )

  pV = ModelParameter( "V", mu = 3.5,
                       omega = sqrt( 0.09 ),
                       distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl  )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.1, sigma_slope = 0.1 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 40 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK",
                                                    sample_time = c(0.5, 1, 4, 8, 12.5, 13, 16, 20, 24.5, 25, 28, 32, 36.5, 37, 40, 44, 48.5, 49, 52, 56) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK",
                                                           time_dose = c(0,12,24,36,48) , amount_dose = c(30,30,30,30,30) ) )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 2.835909e+24

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

###################################################################################################################################

context(" Model PK 1cpt : Linear1FirstOrderSteadyState_kaClVtau")

test_that("", {

  ### Create PFIM project
  MyProject<-PFIMProject(name = "Test PFIM")

  ### Create the statistical model
  MyStatisticalModel<-StatisticalModel()

  ### Create PK model
  PKModel = getModel(PFIMLibraryOfModels, "Linear1FirstOrderSteadyState_kaClVtau")
  MyStatisticalModel = defineModelEquations( MyStatisticalModel, PKModel)

  ### Set mu and omega for each parameter
  pka = ModelParameter( "ka", mu = 1.050,
                        omega = sqrt( 0.1 ),
                        distribution = LogNormalDistribution() )

  pCl = ModelParameter( "Cl", mu = 0.513,
                        omega = 0,
                        distribution = LogNormalDistribution() )

  pV = ModelParameter( "V", mu = 63.000,
                       omega = 0,
                       distribution = LogNormalDistribution() )

  ### Assign the parameters to the statistical model
  MyStatisticalModel = defineParameter( MyStatisticalModel, pka )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pCl  )
  MyStatisticalModel = defineParameter( MyStatisticalModel, pV )

  ### Create and add the responses to the statistical model
  MyStatisticalModel = addResponse( MyStatisticalModel, Response( "RespPK", Combined1( sigma_inter = 0.0, sigma_slope = 0.0676 ) ) )

  ### Finaly assign the statistical model to the project
  MyProject = defineStatisticalModel( MyProject, MyStatisticalModel )

  ### Create a design

  MyDesign<- Design()

  ### For each arm create and add the sampling times for each response

  brasTest <- Arm( name="Bras test", arm_size = 25 )
  brasTest <- addSampling( brasTest, SamplingTimes( outcome = "RespPK", sample_time = c(0.01, 1, 3, 5, 7, 10, 13, 17, 24) ) )
  brasTest <- addAdministration( brasTest, Administration( outcome = "RespPK",
                                                           tau = c(24),
                                                           amount_dose = c(5500 ) )   )

  MyDesign <- addArm( MyDesign, brasTest )

  ### Add the design to the project
  MyProject <- addDesign( MyProject, MyDesign )

  ### Evaluate the Fisher Information Matrix for the IndividualFIM

  MyEvaluationPop <- EvaluatePopulationFIM( MyProject )
  # show( MyEvaluationPop )
  matrixFisherPopulationFIM = getFim(MyEvaluationPop)
  detPopulationFim = det(matrixFisherPopulationFIM)

  valueDetPopulationFim = 119307107189

  tol = 1e-6

  expect_equal(detPopulationFim,valueDetPopulationFim, tol)

})

############################################################################################################################
# END CODE
############################################################################################################################
