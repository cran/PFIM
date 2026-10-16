# Vignette_04_tim_waterhouse_pediatric: evaluation, PSO optimizations, plots.
utils = system.file("vignette-scripts", "pfim-vignette-utils.R", package = "PFIM")
if (!nzchar(utils)) stop("pfim-vignette-utils.R not found.", call. = FALSE)
source(utils, local = TRUE)
library(PFIM)

paths = pfimVignetteSetupPaths()
plotOptions = list(unitTime = c("hour"), unitOutcomes = c("mcg/mL"))

plotInitialEvaluation = NULL

modelEquations = list(
  RespPK = paste(
    "dose_RespPK/(V*(WT/70)^(beta_V_WT)) *",
    "ka/(ka - (Cl*(WT/70)^(beta_Cl_WT))/(V*(WT/70)^(beta_V_WT))) *",
    "(exp(-(Cl*(WT/70)^(beta_Cl_WT))/(V*(WT/70)^(beta_V_WT)) * t)",
    "- exp(-ka * t))"
  )
)
modelParameters = list(
  ModelParameter(name = "V",  distribution = LogNormal(mu = 100,  omega = sqrt(0.1))),
  ModelParameter(name = "Cl", distribution = LogNormal(mu = 10,   omega = sqrt(0.08))),
  ModelParameter(name = "ka", distribution = LogNormal(mu = 0.25, omega = sqrt(0.2))),
  ModelParameter(
    name = "beta_V_WT", distribution = Normal(mu = 1, omega = 0),
    fixedMu = TRUE, fixedOmega = TRUE
  ),
  ModelParameter(
    name = "beta_Cl_WT", distribution = Normal(mu = 0.75, omega = 0),
    fixedMu = TRUE, fixedOmega = TRUE
  )
)
errorModelRespPK = Combined2(
  output = "RespPK",
  sigmaInter = 1,
  sigmaSlope = sqrt(0.05)
)
modelError = list(errorModelRespPK)

WT = 32
week_h = 24 * 7
lastDose = week_h * 24
dose_times = seq(0, lastDose, by = 24)
administrationRespPK = Administration(
  outcome = "RespPK",
  timeDose = dose_times,
  dose = rep(10 * 1000, times = length(dose_times))
)
observation_times = c(
  5,
  week_h * 8 - 1,
  week_h * c(12, 24),
  lastDose + 168
)
samplingTimesRespPK = SamplingTimes(
  outcome = "RespPK",
  samplings = observation_times
)
armInitial = Arm(
  name = "Initial arm",
  size = 12,
  administrations = list(administrationRespPK),
  samplingTimes = list(samplingTimesRespPK)
)
designInitial = Design(
  name = "designInitial",
  arms = list(armInitial)
)

evaluationPopFimInitial = pfimRunOrLoad(
  file.path(paths$data, "vignette4_waterhouse_evaluation_populationFIM.RDS"),
  function() {
    run(Evaluation(
      name = "FIM design initial",
      modelParameters = modelParameters,
      modelEquations = modelEquations,
      modelError = modelError,
      designs = list(designInitial),
      outputs = list("RespPK"),
      fimType = "population"
    ))
  }
)

plotInitialEvaluation = pfimSafePlot(
  plotEvaluation(evaluationPopFimInitial, plotOptions)
)

samplingConstraintsRespPK5 = SamplingTimeConstraints(
  outcome = "RespPK",
  initialSamplings = observation_times,
  samplingsWindows = list(
    c(0, 6),
    c(week_h * 8 - 1, week_h * 8),
    c(week_h * 12 - 1, week_h * 12),
    c(week_h * 24 - 1, week_h * 24),
    c(lastDose + 96, lastDose + 168)
  ),
  numberOfTimesByWindows = c(1, 1, 1, 1, 1),
  minSampling = c(1, 1, 1, 1, 1)
)
armOptim5 = Arm(
  name = "ArmOptim5",
  size = 12,
  administrations = list(administrationRespPK),
  samplingTimes = list(samplingTimesRespPK),
  samplingTimesConstraints = list(samplingConstraintsRespPK5)
)
designOptim5 = Design(
  name = "designOptim5",
  arms = list(armOptim5)
)
psoParams = list(
  maxIteration = 10,
  populationSize = 50,
  personalLearningCoefficient = 2.05,
  globalLearningCoefficient = 2.05,
  seed = 1234,
  showProcess = FALSE
)

optimizationPopFIM5 = pfimRunOrLoad(
  file.path(paths$data, "vignette4_waterhouse_optimization_pso5.RDS"),
  function() {
    run(Optimization(
      name = "Optimization 5 samples",
      modelEquations = modelEquations,
      modelParameters = modelParameters,
      modelError = modelError,
      optimizer = "PSOAlgorithm",
      optimizerParameters = psoParams,
      designs = list(designOptim5),
      fimType = "population",
      outputs = list("RespPK")
    ))
  }
)

observation_times_6 = c(
  5,
  week_h * 8 - 1,
  week_h * c(12, 24),
  lastDose + 72,
  lastDose + 168
)
samplingConstraintsRespPK6 = SamplingTimeConstraints(
  outcome = "RespPK",
  initialSamplings = observation_times_6,
  samplingsWindows = list(
    c(0, 6),
    c(week_h * 8 - 1, week_h * 8),
    c(week_h * 12 - 1, week_h * 12),
    c(week_h * 24 - 1, week_h * 24),
    c(lastDose, lastDose + 168),
    c(lastDose + 167.5, lastDose + 168)
  ),
  numberOfTimesByWindows = c(1, 1, 1, 1, 1, 1),
  minSampling = c(1, 1, 1, 1, 1, 1)
)
armOptim6 = Arm(
  name = "ArmOptim6",
  size = 12,
  administrations = list(administrationRespPK),
  samplingTimes = list(
    SamplingTimes(outcome = "RespPK", samplings = observation_times_6)
  ),
  samplingTimesConstraints = list(samplingConstraintsRespPK6)
)
designOptim6 = Design(
  name = "designOptim6",
  arms = list(armOptim6)
)

optimizationPopFIM6 = pfimRunOrLoad(
  file.path(paths$data, "vignette4_waterhouse_optimization_pso6.RDS"),
  function() {
    run(Optimization(
      name = "Optimization 6 samples",
      modelEquations = modelEquations,
      modelParameters = modelParameters,
      modelError = modelError,
      optimizer = "PSOAlgorithm",
      optimizerParameters = psoParams,
      designs = list(designOptim6),
      fimType = "population",
      outputs = list("RespPK")
    ))
  }
)

optArms = prop(optimizationPopFIM6, "optimisationAlgorithmOutputs")$optimalArms
optSamplings = prop(prop(optArms[[1]], "samplingTimes")[[1]], "samplings")

invisible(list(
  evaluationPopFimInitial = evaluationPopFimInitial,
  optimizationPopFIM5 = optimizationPopFIM5,
  optimizationPopFIM6 = optimizationPopFIM6,
  optSamplings = optSamplings,
  plotInitialEvaluation = plotInitialEvaluation
))
