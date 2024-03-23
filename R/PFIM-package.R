#' @name PFIM-package
#' @aliases PFIM, package-PFIM
#' @docType package
#' @encoding UTF-8
#' @title Fisher Information matrix for design evaluation/optimization for  nonlinear mixed effects models.
#' @section Description:
#' Nonlinear mixed effects models (NLMEM) are widely used in model-based drug development and use to analyze
#' longitudinal data. The use of the "population" Fisher Information Matrix (FIM) is a good alternative to
#' clinical trial simulation to optimize the design of these studies. PFIM 6.0 was released in 2023. The present version, \pkg{PFIM} 6.0, is an R package that uses the S4 object system  for evaluating and/or
#' optimizing population designs based on FIM in NLMEMs.
#'
#' This version of \pkg{PFIM} now includes a library of models implemented also using the object oriented system S4 of R.
#' This library contains two libraries of pharmacokinetic (PK) and/or pharmacodynamic (PD) models. The PK library  includes model with different
#' administration routes (bolus, infusion, first-order absorption), different number of compartments (from 1 to 3), and different types of eliminations
#' (linear or Michaelis-Menten). The PD model library, contains direct immediate models (e.g. Emax and Imax) with various baseline models, and turnover
#' response models. The PK/PD models are obtained with combination of the models from the PK and PD model libraries. \pkg{PFIM} handles both analytical
#' and ODE models and offers the possibility to the user to define his/her own model(s).
#' In \pkg{PFIM 6.0}, the FIM is evaluated by first order linearization of the model assuming a block diagonal FIM as in [3]. The Bayesian FIM is also
#' available to give shrinkage predictions [4].
#' \pkg{PFIM 6.0} includes several algorithms to conduct design optimization based on the D-criterion, given design constraints : the simplex algorithm (Nelder-Mead) [5], the
#' multiplicative algorithm [6], the Fedorov-Wynn algorithm [7], PSO (\emph{Particle Swarm Optimization}) and PGBO (\emph{Population Genetics Based
#' Optimizer}) [9].
#'
#' @section Documentation:
#' Documentation and user guide are available at \url{http://www.pfim.biostat.fr/}
#'
#' @section Validation:
#' \pkg{PFIM 6.0} also provides quality control with tests and validation using the evaluated FIM to assess the validity of the new version  and its new
#' features. Finally, \pkg{PFIM 6.0} displays all the results with both clear graphical form and a data summary, while ensuring their easy manipulation in
#' R. The standard data visualization package ggplot2 for R is used to display all the results with clear graphical form [10]. A quality control using the
#' D-criterion is also provided.
#'
#' @references
#' [1] Dumont C, Lestini G, Le Nagard H, Mentré F, Comets E, Nguyen TT, et al. PFIM 4.0, an extended R program for design evaluation and optimization in
#' nonlinear mixed-effect models. Comput Methods Programs Biomed. 2018;156:217-29.
#' @references [2] Chambers JM. Object-Oriented Programming, Functional Programming and R. Stat Sci. 2014;29:167-80.
#' @references [3] Mentré F, Mallet A, Baccar D. Optimal Design in Random-Effects Regression Models. Biometrika. 1997;84:429-42.
#' @references [4] Combes FP, Retout S, Frey N, Mentré F. Prediction of shrinkage of individual parameters using the Bayesian information matrix in nonlinear mixed effect models with evaluation in pharmacokinetics. Pharm Res. 2013;30:2355-67.
#' @references [5] Nelder JA, Mead R. A simplex method for function minimization. Comput J. 1965;7:308-13.
#' @references [6] Seurat J, Tang Y, Mentré F, Nguyen, TT. Finding optimal design in nonlinear mixed effect models using multiplicative algorithms. Computer Methods and Programs in Biomedicine, 2021.
#' @references [7] Fedorov VV. Theory of Optimal Experiments. Academic Press, New York, 1972.
#' @references [8] Eberhart RC, Kennedy J. A new optimizer using particle swarm theory. Proc. of the Sixth International Symposium on Micro Machine and Human Science, Nagoya, 4-6 October 1995, 39-43.
#' @references [9] Le Nagard H, Chao L, Tenaillon O. The emergence of complexity and restricted pleiotropy in adapting networks. BMC Evol Biol. 2011;11:326.
#' @references [10] Wickham H. ggplot2: Elegant Graphics for Data Analysis, Springer-Verlag New York, 2016.
#'
#' @section Organization of the source files in the \code{/R} folder:
#' \pkg{PFIM 6.0} contains a hierarchy of S4 classes with corresponding methods and functions serving as constructors.
#' All of the source code related to the specification of a certain class is contained in a file named \code{[Name_of_the_class]-Class.R}.
#' These classes include:
#' \itemize{
#' \item{1.} all roxygen \code{@@include} to insure the correctly generated collate for the DESCRIPTION file,
#' \item{2.} \code{\\setClass} preceded by a roxygen documentation that describes the purpose and slots of the class,
#' \item{3.} specification of an initialize method,
#' \item{4.} all getter and setter, respectively returning attributes of the object and associated objects.
#' }
#'
#' @section Content of the source code and files in the \code{/R} folder:
#'
#' Class \code{\link{Administration}}
#'
#' \itemize{
#'    \item{} \code{\link{getOutcome}}
#'    \item{} \code{\link{setOutcome}}
#'    \item{} \code{\link{getTimeDose}}
#'    \item{} \code{\link{setTimeDose}}
#'    \item{} \code{\link{getDose}}
#'    \item{} \code{\link{setDose}}
#'    \item{} \code{\link{getTinf}}
#'    \item{} \code{\link{setTinf}}
#'    \item{} \code{\link{getTau}}
#'    \item{} \code{\link{setTau}}
#'}
#'
#'
#' Class \code{\link{AdministrationConstraints}}
#'
#' \itemize{
#'      \item{} \code{\link{getOutcome}}
#'      \item{} \code{\link{getDose}}
#'}
#'
#' Class \code{\link{Arm}}
#'
#' \itemize{
#'      \item{} \code{\link{getName}}
#'      \item{} \code{\link{setName}}
#'      \item{} \code{\link{getSize}}
#'      \item{} \code{\link{setSize}}
#'      \item{} \code{\link{getAdministrations}}
#'      \item{} \code{\link{setAdministrations}}
#'      \item{} \code{\link{getSamplingTimes}}
#'      \item{} \code{\link{setSamplingTimes}}
#'      \item{} \code{\link{getInitialConditions}}
#'      \item{} \code{\link{setInitialConditions}}
#'      \item{} \code{\link{getAdministrationsConstraints}}
#'      \item{} \code{\link{getSamplingTimesConstraints}}
#'      \item{} \code{\link{getSamplingTime}}
#'      \item{} \code{\link{getSamplingTimeConstraint}}
#'      \item{} \code{\link{setSamplingTimesConstraints}}
#'      \item{} \code{\link{setSamplingTime}}
#'      \item{} \code{\link{getAdministration}}
#'      \item{} \code{\link{getAdministrationConstraint}}
#'      \item{} \code{\link{EvaluateArm}}
#'}
#'
#' Class \code{\link{BayesianFim}}
#'
#'   \itemize{
#'     \item{} \code{\link{EvaluateFisherMatrix}}
#'     \item{} \code{\link{getRSE}}
#'     \item{} \code{\link{getConditionNumberVarianceEffects}}
#'     \item{} \code{\link{getShrinkage}}
#'     \item{} \code{\link{setShrinkage}}
#'     \item{} \code{\link{reportTablesFIM}}
#'     \item{} \code{\link{generateReportEvaluation}}
#'  }
#'
#' Class \code{\link{Combined1}}
#'
#'   \itemize{
#'     \item{} See class \code{\link{ModelError}}
#'  }
#'
#' Class \code{\link{Constant}}
#'
#'   \itemize{
#'     \item{} See class \code{\link{ModelError}}
#'  }
#'
#' Class \code{\link{Design}}
#'
#'   \itemize{
#'     \item{} \code{\link{getName}}
#'     \item{} \code{\link{setName}}
#'     \item{} \code{\link{getSize}}
#'     \item{} \code{\link{setSize}}
#'     \item{} \code{\link{setArms}}
#'     \item{} \code{\link{getOutcomesEvaluation}}
#'     \item{} \code{\link{setOutcomesEvaluation}}
#'     \item{} \code{\link{getOutcomesGradient}}
#'     \item{} \code{\link{setOutcomesGradient}}
#'     \item{} \code{\link{getFim}}
#'     \item{} \code{\link{setFim}}
#'     \item{} \code{\link{getNumberOfArms}}
#'     \item{} \code{\link{setNumberOfArms}}
#'     \item{} \code{\link{setArm}}
#'     \item{} \code{\link{EvaluateDesign}}
#'     \item{} \code{\link{plotOutcomesEvaluation}}
#'     \item{} \code{\link{plotOutcomesGradient}}
#     \item{} \code{\link{show}}
#'     \item{} \code{\link{reportTablesAdministration}}
#'     \item{} \code{\link{reportTablesDesign}}
#' }
#'
#' Class \code{\link{Distribution}}
#'
#'   \itemize{
#'     \item{}\code{\link{getParameters}}
#'     \item{}\code{\link{setParameters}}
#'     \item{}\code{\link{getMu}}
#'     \item{}\code{\link{setMu}}
#'     \item{}\code{\link{getOmega}}
#'     \item{}\code{\link{setOmega}}
#'     \item{}\code{\link{getAdjustedGradient}}
#'  }
#'
#'
#'
#' Class \code{\link{Evaluation}}
#'
#'   \itemize{
#'     \item{}\code{\link{run}}
#     \item{}\code{\link{show}}
#'     \item{}\code{\link{reportTablesPlot}}
#'     \item{}\code{\link{generateTables}}
#'     \item{}\code{\link{Report}}
#'  }
#'
#'
#' Class \code{\link{FedorovWynnAlgorithm}}
#'
#'   \itemize{
#'     \item{}\code{\link{FedorovWynnAlgorithm_Rcpp}}
#'     \item{}\code{\link{resizeFisherMatrix}}
#'     \item{}\code{\link{setParameters}}
#'     \item{}\code{\link{optimize}}
#     \item{}\code{\link{show}}
#'     \item{}\code{\link{generateReportOptimization}}
#'  }
#'
#'
#'
#' Class \code{\link{FedorovWynnAlgorithm}}
#'
#'   \itemize{
#'     \item{}\code{\link{FedorovWynnAlgorithm_Rcpp}}
#'     \item{}\code{\link{resizeFisherMatrix}}
#'     \item{}\code{\link{setParameters}}
#'     \item{}\code{\link{optimize}}
#    \item{}\code{\link{show}}
#'     \item{}\code{\link{generateReportOptimization}}
#'  }
#'
#'
#'
#' Class \code{\link{Fim}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateFisherMatrix}}
#'     \item{}\code{\link{EvaluateVarianceFIM}}
#'     \item{}\code{\link{getFisherMatrix}}
#'     \item{}\code{\link{setFisherMatrix}}
#'     \item{}\code{\link{getFixedEffects}}
#'     \item{}\code{\link{setFixedEffects}}
#'     \item{}\code{\link{getVarianceEffects}}
#'     \item{}\code{\link{setVarianceEffects}}
#'     \item{}\code{\link{getDeterminant}}
#'     \item{}\code{\link{getDcriterion}}
#'     \item{}\code{\link{getCorrelationMatrix}}
#'     \item{}\code{\link{getSE}}
#'     \item{}\code{\link{getRSE}}
#'     \item{}\code{\link{getShrinkage}}
#'     \item{}\code{\link{getEigenValues}}
#'     \item{}\code{\link{getConditionNumberFixedEffects}}
#'     \item{}\code{\link{getConditionNumberVarianceEffects}}
#'     \item{}\code{\link{getColumnAndParametersNamesFIM}}
#'     \item{}\code{\link{getColumnAndParametersNamesFIMInLatex}}
#'     \item{}\code{\link{reportTablesFIM}}
#'     \item{}\code{\link{generateReportEvaluation}}
#'     \item{}\code{\link{setFimTypeToString}}
#'  }
#'
#' Class \code{GenericMethods}
#'
#'   \itemize{
#'     \item{}\code{\link{getName}}
#'     \item{}\code{\link{getNames}}
#'     \item{}\code{\link{getSize}}
#'     \item{}\code{\link{setSize}}
#'     \item{}\code{\link{getOutcome}}
#'     \item{}\code{\link{setOutcome}}
#'     \item{}\code{\link{getFim}}
#'     \item{}\code{\link{getOdeSolverParameters}}
#'     \item{}\code{\link{getMu}}
#'     \item{}\code{\link{setMu}}
#'     \item{}\code{\link{getOmega}}
#'     \item{}\code{\link{setOmega}}
#'     \item{}\code{\link{getParameters}}
#'     \item{}\code{\link{setParameters}}
#'     \item{}\code{\link{getModelError}}
#'     \item{}\code{\link{getSamplings}}
#'     \item{}\code{\link{getFim}}
#'     \item{}\code{\link{setName}}
#'     \item{}\code{\link{setArms}}
#'     \item{}\code{\link{getArms}}
#    \item{}\code{\link{show}}
#'  }
#'
#' Class \code{\link{IndividualFim}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateFisherMatrix}}
#'     \item{}\code{\link{EvaluateVarianceFIM}}
#'     \item{}\code{\link{getRSE}}
#'     \item{}\code{\link{getShrinkage}}
#'     \item{}\code{\link{setShrinkage}}
#'     \item{}\code{\link{reportTablesFIM}}
#'     \item{}\code{\link{generateReportEvaluation}}
#'  }
#'
#'
#'
#' Class \code{\link{LibraryOfModels}}
#'
#'   \itemize{
#'     \item{}\code{\link{getName}}
#'     \item{}\code{\link{getContent}}
#'     \item{}\code{\link{setContent}}
#'     \item{}\code{\link{addModel}}
#'     \item{}\code{\link{addModels}}
#'     \item{}\code{\link{getLibraryPKModels}}
#'     \item{}\code{\link{getLibraryPDModels}}
#'  }
#'
#'
#'
#' Class \code{\link{LibraryOfPKPDModels}}
#'
#'   \itemize{
#'     \item{}\code{\link{getPKModel}}
#'     \item{}\code{\link{getPDModel}}
#'     \item{}\code{\link{getPKPDModel}}
#'  }
#'
#'
#'
#' Class \code{\link{LogNormal}}
#'
#'   \itemize{
#'     \item{}\code{\link{getAdjustedGradient}}
#'  }
#'
#'
#'
#' Class \code{\link{Model}}
#'
#'   \itemize{
#'     \item{}\code{\link{getName}}
#'     \item{}\code{\link{setName}}
#'     \item{}\code{\link{getDescription}}
#'     \item{}\code{\link{setDescription}}
#'     \item{}\code{\link{getEquations}}
#'     \item{}\code{\link{setEquations}}
#'     \item{}\code{\link{setModelFromLibrary}}
#'     \item{}\code{\link{getOutcomes}}
#'     \item{}\code{\link{setOutcomes}}
#'     \item{}\code{\link{getOutcomesForEvaluation}}
#'     \item{}\code{\link{setOutcomesForEvaluation}}
#'     \item{}\code{\link{getParameters}}
#'     \item{}\code{\link{setParameters}}
#'     \item{}\code{\link{getModelError}}
#'     \item{}\code{\link{setModelError}}
#'     \item{}\code{\link{getInitialConditions}}
#'     \item{}\code{\link{setInitialConditions}}
#'     \item{}\code{\link{getOdeSolverParameters}}
#'     \item{}\code{\link{setOdeSolverParameters}}
#'     \item{}\code{\link{getModelFromLibrary}}
#'     \item{}\code{\link{convertPKModelAnalyticToPKModelODE}}
#'     \item{}\code{\link{getNumberOfParameters}}
#'     \item{}\code{\link{isModelODE}}
#'     \item{}\code{\link{isModelAnalytic}}
#'     \item{}\code{\link{isDoseInEquations}}
#'     \item{}\code{\link{isModelInfusion}}
#'     \item{}\code{\link{isModelSteadyState}}
#'     \item{}\code{\link{isModelBolus}}
#'     \item{}\code{\link{definePKPDModel}}
#'     \item{}\code{\link{definePKModel}}
#'     \item{}\code{\link{defineModel}}
#'     \item{}\code{\link{defineModelFromLibraryOfModels}}
#'     \item{}\code{\link{defineModelUserDefined}}
#'     \item{}\code{\link{defineModelType}}
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{parametersForComputingGradient}}
#'     \item{}\code{\link{EvaluateVarianceModel}}
#'     \item{}\code{\link{getFixedParameters}}
#'     \item{}\code{\link{getModelErrorParametersValues}}
#'     \item{}\code{\link{reportTablesModelParameters}}
#'     \item{}\code{\link{reportTablesModelError}}
#'  }
#'
#'
#' Class \code{\link{ModelAnalytic}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{definePKModel}}
#'     \item{}\code{\link{definePKPDModel}}
#'     \item{}\code{\link{convertPKModelAnalyticToPKModelODE}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelAnalyticBolus}}
#'
#'   \itemize{
#'     \item{}See class \code{\link{ModelAnalytic}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelAnalyticBolusSteadyState}}
#'
#'   \itemize{
#'     \item{}See class \code{\link{ModelAnalyticBolus}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelBolus}}
#'
#'   \itemize{
#'     \item{}See class \code{\link{Model}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelError}}
#'
#'   \itemize{
#'     \item{}\code{\link{getOutcome}}
#'     \item{}\code{\link{getEquation}}
#'     \item{}\code{\link{setEquation}}
#'     \item{}\code{\link{getDerivatives}}
#'     \item{}\code{\link{setDerivatives}}
#'     \item{}\code{\link{getSigmaInter}}
#'     \item{}\code{\link{setSigmaInter}}
#'     \item{}\code{\link{getSigmaSlope}}
#'     \item{}\code{\link{setSigmaSlope}}
#'     \item{}\code{\link{getcError}}
#'     \item{}\code{\link{setcError}}
#'     \item{}\code{\link{getParameters}}
#'     \item{}\code{\link{EvaluateErrorModelDerivatives}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelInfusion}}
#'
#'   \itemize{
#'     \item{}\code{\link{getEquationsDuringInfusion}}
#'     \item{}\code{\link{getEquationsAfterInfusion}}
#'     \item{}\code{\link{setEquationsAfterInfusion}}
#'     \item{}\code{\link{setEquationsDuringInfusion}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODE}}
#'
#'   \itemize{
#'     \item{}See class \code{\link{Model}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODEBolus}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{definePKPDModel}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODEDoseInEquations}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{definePKModel}}
#'     \item{}\code{\link{definePKPDModel}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODEDoseNotInEquations}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{definePKModel}}
#'     \item{}\code{\link{definePKPDModel}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODEInfusion}}
#'
#'   \itemize{
#'     \item{}See class \code{\link{ModelInfusion}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelODEInfusionDoseInEquations}}
#'
#'   \itemize{
#'     \item{}\code{\link{EvaluateModel}}
#'     \item{}\code{\link{definePKModel}}
#'     \item{}\code{\link{definePKPDModel}}
#'  }
#'
#'
#'
#' Class \code{\link{ModelParameter}}
#'
#'   \itemize{
#'      \item{} \code{\link{getName}}
#'      \item{} \code{\link{getDistribution}}
#'      \item{} \code{\link{setDistribution}}
#'      \item{} \code{\link{getFixedMu}}
#'      \item{} \code{\link{setFixedMu}}
#'      \item{} \code{\link{getFixedOmega}}
#'      \item{} \code{\link{setFixedOmega}}
#'      \item{} \code{\link{getMu}}
#'      \item{} \code{\link{setMu}}
#'      \item{} \code{\link{getOmega}}
#'      \item{} \code{\link{setOmega}}
#'  }
#'
#'
#' Class \code{\link{MultiplicativeAlgorithm}}
#'
#'   \itemize{
#'      \item{} \code{\link{MultiplicativeAlgorithm_Rcpp}}
#'      \item{} \code{\link{getLambda}}
#'      \item{} \code{\link{getDelta}}
#'      \item{} \code{\link{getNumberOfIterations}}
#'      \item{} \code{\link{getOptimalWeights}}
#'      \item{} \code{\link{setOptimalWeights}}
#'      \item{} \code{\link{setParameters}}
#'      \item{} \code{\link{optimize}}
#'      \item{} \code{\link{getDataFrameResults}}
#'      \item{} \code{\link{plotWeights}}
#      \item{} \code{\link{show}}
#'      \item{} \code{\link{generateReportOptimization}}
#'  }
#'
#'
#'
#' Class \code{\link{Normal}}
#'
#'   \itemize{
#'      \item{} \code{\link{getAdjustedGradient}}
#'  }
#'
#'
#'
#' Class \code{\link{Optimization}}
#'
#'   \itemize{
#'      \item{} \code{\link{getProportionsOfSubjects}}
#'      \item{} \code{\link{getOptimizationResults}}
#'      \item{} \code{\link{setOptimizationResults}}
#'      \item{} \code{\link{getEvaluationFIMResults}}
#'      \item{} \code{\link{setEvaluationFIMResults}}
#'      \item{} \code{\link{setEvaluationInitialDesignResults}}
#'      \item{} \code{\link{getEvaluationInitialDesignResults}}
#'      \item{} \code{\link{getElementaryProtocols}}
#'      \item{} \code{\link{generateFimsFromConstraints}}
#'      \item{} \code{\link{run}}
#      \item{} \code{\link{show}}
#'      \item{} \code{\link{plotWeights}}
#'      \item{} \code{\link{Report}}
#'  }
#'
#'
#'
#' Class \code{\link{PFIMProject}}
#'
#'   \itemize{
#'      \item{}\code{\link{getName}}
#'      \item{}\code{\link{setModel}}
#'      \item{}\code{\link{getModel}}
#'      \item{}\code{\link{getModelEquations}}
#'      \item{}\code{\link{getModelParameters}}
#'      \item{}\code{\link{getModelError}}
#'      \item{}\code{\link{getDesigns}}
#'      \item{}\code{\link{getFim}}
#'      \item{}\code{\link{getOdeSolverParameters}}
#'      \item{}\code{\link{getOutcomes}}
#'      \item{}\code{\link{getOptimizer}}
#'      \item{}\code{\link{getOptimizerParameters}}
#'      \item{}\code{\link{run}}
#'      \item{}\code{\link{generateTables}}
#'      \item{}\code{\link{Report}}
#'  }
#'
#'
#'
#' Class \code{\link{PGBOAlgorithm}}
#'
#'   \itemize{
#'      \item{} \code{\link{setParameters}}
#'      \item{} \code{\link{optimize}}
#    \item{} \code{\link{show}}
#'      \item{} \code{\link{generateReportOptimization}}
#'  }
#'
#'
#'
#' Class \code{\link{PlotEvaluation}}
#'
#'   \itemize{
#'      \item{} \code{\link{plot}}
#'      \item{} \code{\link{plotSE}}
#'      \item{} \code{\link{plotRSE}}
#'      \item{} \code{\link{plotShrinkage}}
#'  }
#'
#'
#' Class \code{\link{PopulationFim}}
#'
#'   \itemize{
#'      \item{} \code{\link{EvaluateFisherMatrix}}
#'      \item{} \code{\link{EvaluateVarianceFIM}}
#'      \item{} \code{\link{getRSE}}
#'      \item{} \code{\link{getShrinkage}}
#'      \item{} \code{\link{setShrinkage}}
#'      \item{} \code{\link{reportTablesFIM}}
#'      \item{} \code{\link{generateReportEvaluation}}
#'  }
#'
#'
#'
#' Class \code{\link{Proportional}}
#'
#'   \itemize{
#'      \item{} See class \code{\linkS4class{ModelError}}
#'  }
#'
#'
#'
#' Class \code{\link{PSOAlgorithm}}
#'
#'   \itemize{
#'      \item{} \code{\link{setParameters}}
#'      \item{} \code{\link{optimize}}
#      \item{} \code{\link{show}}
#'      \item{} \code{\link{generateReportOptimization}}
#'  }
#'
#'
#'
#' Class \code{\link{SamplingTimeConstraints}}
#'
#'   \itemize{
#'      \item{} \code{\link{getOutcome}}
#'      \item{} \code{\link{getSamplings}}
#'      \item{} \code{\link{getFixedTimes}}
#'      \item{} \code{\link{getNumberOfTimesByWindows}}
#'      \item{} \code{\link{getMinSampling}}
#'      \item{} \code{\link{getSamplingsWindows}}
#'      \item{} \code{\link{getNumberOfsamplingsOptimisable}}
#'      \item{} \code{\link{checkSamplingTimeConstraintsForContinuousOptimization}}
#'      \item{} \code{\link{generateSamplingsFromSamplingConstraints}}
#'  }
#'
#'
#'
#'Class \code{\link{SamplingTimes}}
#'
#'  \itemize{
#'  \item{}\code{\link{getOutcome}}
#'  \item{}\code{\link{setOutcome}}
#'  \item{}\code{\link{getSamplings}}
#'  \item{}\code{\link{setSamplings}}
#' }
#'
#'
#' Class \code{\link{SimplexAlgorithm}}
#'
#'   \itemize{
#'      \item{} \code{\link{setParameters}}
#'      \item{} \code{\link{fun.amoeba}}
#'      \item{} \code{\link{fisher.simplex}}
#'      \item{} \code{\link{optimize}}
#     \item{} \code{\link{show}}
#'      \item{} \code{\link{generateReportOptimization}}
#'  }
#'
"_PACKAGE"
