#' @name PFIM-package
#' @aliases PFIM, package-PFIM
#' @docType package
#' @title Fisher Information matrix for design evaluation/optimization for  nonlinear mixed effects models.
#'
#' @section Description :
#' Nonlinear mixed effects models (NLMEM) are widely used in model-based drug development and use to analyze
#' longitudinal data. The use of the "population" Fisher Information Matrix (FIM) is a good alternative to
#' clinical trial simulation to optimize the design of these studies. PFIM 4.0 was released in 2018 as a list of
#' R functions [1]. The present version, \pkg{PFIM} 5.0, is an R package that uses the S4 object system  for evaluating and/or
#' optimizing population designs based on FIM in NLMEMs.
#'
#'
#' This new version of \pkg{PFIM} now includes a library of models implemented also using the object oriented system S4 of R.
#' This new library contains two libraries of pharmacokinetic (PK) and/or pharmacodynamic (PD) models. The PK library  includes model with different
#' administration routes (bolus, infusion, first-order absorption), different number of compartments (from 1 to 3), and different types of eliminations
#' (linear or Michaelis-Menten). The PD model library, contains direct immediate models (e.g. Emax and Imax) with various baseline models, and turnover
#' response models. The PK/PD models are obtained with combination of the models from the PK and PD model libraries. \pkg{PFIM}  handles both analytical
#' and ODE models and offers the possibility to the user to define his/her own model(s).
#'
#'
#' In \pkg{PFIM 5.0}, the FIM is evaluated by first order linearization of the model assuming a block diagonal FIM as in [3]. The Bayesian FIM is also
#' available to give shrinkage predictions [4].
#' \pkg{PFIM 5.0} includes several algorithms to conduct design optimization based on the D-criterion, given design constraints : the simplex algorithm (Nelder-Mead) [5], the
#' multiplicative algorithm [6], the Fedorov-Wynn algorithm [7], PSO (\emph{Particle Swarm Optimization}) and PGBO (\emph{Population Genetics Based
#' Optimizer}) [9].
#' @section Validation:
#' \pkg{PFIM 5.0} also provides quality control with tests and validation using the evaluated FIM to assess the validity of the new version  and its new
#' features. Finally, \pkg{PFIM 5.0} displays all the results with both clear graphical form and a data summary, while ensuring their easy manipulation in
#' R. The standard data visualization package ggplot2 for R is used to display all the results with clear graphical form [10]. A quality control using the
#' D-criterion is also provided.
#' @references
#' [1] Dumont C, Lestini G, Le Nagard H, Mentré F, Comets E, Nguyen TT, et al. PFIM 4.0, an extended R program for design evaluation and optimization in
#' nonlinear mixed-effect models. Comput Methods Programs Biomed. 2018;156:217-29.
#' @references [2] Chambers JM. Object-Oriented Programming, Functional Programming and R. Stat Sci. 2014;29:167-80.
#' @references [3] Mentré F, Mallet A, Baccar D. Optimal Design in Random-Effects Regression Models. Biometrika. 1997;84:429-42.
#' @references [4] Combes FP, Retout S, Frey N, Mentré F. Prediction of shrinkage of individual parameters using the bayesian information matrix in nonlinear mixed effect models with evaluation in pharmacokinetics. Pharm Res. 2013;30:2355-67.
#' @references [5] Nelder JA, Mead R. A simplex method for function minimization. Comput J. 1965;7:308–13.
#' @references [6] Seurat J, Tang Y, Mentré F, Nguyen, TT. Finding optimal design in nonlinear mixed effect models using multiplicative algorithms. Computer Methods and Programs in Biomedicine, 2021.
#' @references [7] Fedorov VV. Theory of Optimal Experiments. Academic Press, New York, 1972.
#' @references [8] Eberhart RC, Kennedy J. A new optimizer using particle swarm theory. Proc. of the Sixth International Symposium on Micro Machine and Human Science, Nagoya, 4-6 October 1995, 39-43.
#' @references [9] Le Nagard H, Chao L, Tenaillon O. The emergence of complexity and restricted pleiotropy in adapting networks. BMC Evol Biol. 2011;11:326.
#' @references [10] Wickham H. ggplot2: Elegant Graphics for Data Analysis, Springer-Verlag New York, 2016.
#' @section Organization of the source code / files in the \code{/R} folder:
#' \pkg{PFIM 5.0} contains a hierarchy of S4 classes with corresponding methods and functions serving as constructors.
#' All of the source code related to the specification of a certain class is contained in a file named
#' \code{[Name_of_the_class]-Class.R}. These classes include:
#'
#' \itemize{
#' \item{1.} all roxygen \code{@@include} to insure the correctly generated collate for the DESCRIPTION file,
#' \item{2.} \code{\\setClass} preceded by a roxygen documentation that describes the purpose and slots of the class,
#' \item{3.} specification of an initialize method,
#' \item{4.} all getter and setter, respectively returning attributes of the object and associated objects.
#' }
#'
#' The following class diagrams provide an overview on the structure of the package.
#\if{html}{\figure{PFIMClassDiagram.png}}
# \if{latex}{
#   \out{\begin{center}}\if{latex}{\figure{PFIMClassDiagram.png}{options: width=0.5in}}\out{\end{center}}
#
# }
#' \if{html}{ \figure{PFIMClassDiagram.png}{options: width=960} }
#' \if{latex}{\figure{PFIMClassDiagram.pdf}{options: width=6cm} }
#'
#' @section Content of the source code and files in the \code{/R} folder:
#'
#' \itemize{
#'
#' \item {Class \code{\link{Administration}}}{
#'
#'   \itemize{
#'      \item \code{\link{getAllowedDose}}
#'      \item \code{\link{getAllowedTime}}
#'      \item \code{\link{getAllowedTinf}}
#'      \item \code{\link{getAmountDose}}
#'      \item \code{\link{getNameAdministration}}
#'      \item \code{\link{getTau}}
#'      \item \code{\link{getTimeDose}}
#'      \item \code{\link{getTinf}}
#'      \item \code{\link{is.multidose}}
#'      \item \code{\link{setAllowedDose}}
#'      \item \code{\link{setAllowedTime<-}}
#'      \item \code{\link{setAllowedTinf<-}}
#'      \item \code{\link{setAmountDose}}
#'      \item \code{\link{setTau}}
#'      \item \code{\link{setTimeDose<-}}
#'      \item \code{\link{setTinf}}
#'    }
#'}
#'
#'\item{Class \code{\link{AdministrationConstraint}}}{
#'
#'   \itemize{
#'    \item   \code{\link{AllowedDoses}}
#'    \item   \code{\link{fixedDoses}}
#'    \item   \code{\link{getAllowedDoses}}
#'    \item   \code{\link{getDoseOptimisability}}
#'    \item   \code{\link{getNumberOfDoses}}
#'    \item   \code{\link{getResponseName}}
#'    }
#'}
#'
#'\item{Class \code{\link{Arm}} } {
#'
#'   \itemize{
#'    \item   \code{\link{addAdministration}}
#'    \item   \code{\link{addSampling}}
#'    \item   \code{\link{addSamplings}}
#'    \item   \code{\link{EvaluateStatisticalModel}}
#'    \item   \code{\link{getAdministration}}
#'    \item   \code{\link{getAdministrationByOutcome}}
#'    \item   \code{\link{getArmSize}}
#'    \item   \code{\link{getNameArm}}
#'    \item   \code{\link{getCondInit}}
#'    \item   \code{\link{getSamplings}}
#'    \item   \code{\link{setArmSize}}
#'    \item   \code{\link{setInitialConditions}}
#'    \item   \code{\link{setSamplings<-}}
#'    \item   \code{\link{getResponseNameByIndice}}
#'    \item   \code{\link{addSamplingConstraints}}
#'    \item   \code{\link{getSamplingConstraintsInArm}}
#'    \item   \code{\link{modifySamplingTimes}}
#'    \item   \code{\link{getNumberOfSamplings}}
#'    }
#'}
#'
#'\item{Class \code{\link{BayesianFim}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getDescription}}
#'    \item   \code{\link{getShrinkage}}
#'    }
#'}
#'
#'\item{Classes \code{\link{Combined1}}, \code{\link{Combined1c}}, \code{\link{Combined2}}, \code{\link{Combined2c}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getSigmaNames}}
#'    \item   \code{\link{getSigmaValues}}
#'    \item   \code{\link{show}}
#'    }
#'}
#'
#'\item{Class \code{\link{Constant}}} {
#'
#'   \itemize{
#'    \item   \code{\link{getSigmaNames}}
#'    \item   \code{\link{getSigmaValues}}
#'    \item   \code{\link{show}}
#'    }
#'}
#'
#'
#'
#'\item{Class \code{\link{Constraint}}}
#'
#'\item{Class \code{\link{ContinuousConstraint}}} {
#'
#'   \itemize{
#'    \item   \code{\link{getRange}}
#'    \item   \code{\link{setRange<-}}
#'    }
#'}
#'
#'\item{Class \code{\link{Design}}}{
#'
#'   \itemize{
#'    \item   \code{\link{addArm}}
#'    \item   \code{\link{addArms}}
#'    \item   \code{\link{EvaluateDesignForEachArm}}
#'    \item   \code{\link{getAmountOfArms}}
#'    \item   \code{\link{getArms}}
#'    \item   \code{\link{getEvaluationDesign}}
#'    \item   \code{\link{getFimOfDesign}}
#'    \item   \code{\link{getNameDesign}}
#'    \item   \code{\link{getNumberSamples}}
#'    \item   \code{\link{setNumberSamples<-}}
#'    \item   \code{\link{getOptimizationResult}}
#'    \item   \code{\link{getTotalSize}}
#'    \item   \code{\link{modifyArm}}
#'    \item   \code{\link{setAmountOfArms}}
#'    \item   \code{\link{setArms}}
#'    \item   \code{\link{setNameDesign}}
#'    \item   \code{\link{setTotalSize<-}}
#'    \item   \code{\link{show}}
#'    \item   \code{\link{showArmData}}
#'    \item   \code{\link{summary}}
#'    \item   \code{\link{summaryArmData}}
#'    }
#'}
#'
#'\item{Class  \code{\link{DesignConstraint}}}{
#'
#'   \itemize{
#'    \item   \code{\link{addAdministrationConstraint}}
#'    \item   \code{\link{addDesignConstraints}}
#'    \item   \code{\link{addSamplingConstraint}}
#'    \item   \code{\link{getAdministrationConstraint}}
#'    \item   \code{\link{getNameDesignConstraint}}
#'    \item   \code{\link{getTotalNumberOfIndividuals}}
#'    \item   \code{\link{getSamplingConstraints}}
#'    \item   \code{\link{setAmountOfArmsAim}}
#'    \item   \code{\link{setPossibleArms}}
#'    \item   \code{\link{setTotalNumberOfIndividuals}}
#'    \item   \code{\link{show}}
#'    }
#'}
#'
#'\item{Class DiscreteConstraint}{
#'
#'   \itemize{
#'    \item   \code{\link{getDiscret}}
#'    \item   \code{\link{setDiscret<-}}
#'    }
#'}
#'
#'\item{Class \code{\link{Distribution}}}
#'
#'\item{Class \code{\link{FedorovWynnAlgorithm}}}{
#'   \itemize{
#'  \item \code{\link{FedorovWynnAlgorithm_Rcpp}}
#'    \item \code{\link{resizeFisherMatrix}}
#    \item \code{\link{indicesVectorsInElementaryProtocols}}
#'    \item \code{\link{PrepareFIMs}}
#'    \item \code{\link{Optimize}}
#'   }
#'}
#'
#'\item{Class \code{\link{Fim}}}{
#'
#'   \itemize{
#'    \item   \code{\link{FinalizeFIMForOneElementaryDesign}}
#'    \item   \code{\link{getConditionNumberMatrix}}
#'    \item   \code{\link{getCorr}}
#'    \item   \code{\link{getDcriterion}}
#'    \item   \code{\link{getDescription}}
#'    \item   \code{\link{getDeterminant}}
#'    \item   \code{\link{getEigenValue}}
#  \item   \code{\link{getFimComputMethod}}
#'    \item   \code{\link{getMfisher}}
#'    \item   \code{\link{getSE}}
#'    \item   \code{\link{getStatisticalModelStandardErrors}}
#    \item   \code{\link{setFimComputMethod<-}}
#'    \item   \code{\link{setMfisher<-}}
#'    \item   \code{\link{setMu}}
#'    \item   \code{\link{setOmega}}
#'    \item   \code{\link{show}}
#'    \item   \code{\link{showStatisticalModelStandardErrors}}
#'    }
#'}
#'
#'\item{Class \code{\link{IndividualFim}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getDescription}}
#'    \item   \code{\link{getStatisticalModelStandardErrors}}
#'    \item   \code{\link{show}}
#'    \item   \code{\link{showStatisticalModelStandardErrors}}
#'   }
#'}
#'
#'\item{Class \code{\link{LibraryOfModels}}}{
#'
#'   \itemize{
#'    \item   \code{\link{addModel}}
#'    \item   \code{\link{getContentsLibraryOfModels}}
#'    \item   \code{\link{getModel}}
#'    \item   \code{\link{getModelNameList}}
#'    \item   \code{\link{getPKPDModel}}
#'   }
#'}
#'
#'\item{Class \code{\link{LogNormalDistribution}}} {
#'
#'   \itemize{
#'    \item   \code{\link{AdjustLogNormalDistribution}}
#'   }
#'}
#'
#'\item{Class \code{\link{Model}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getEquations}}
#'    \item   \code{\link{getEquationsModel}}
#'    \item   \code{\link{getModelName}}
#'    \item   \code{\link{setParametersModel}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelEquations}}}{
#'
#'   \itemize{
#'    \item   \code{\link{convertAnalyticToODE}}
#'    \item   \code{\link{EvaluateModel}}
#'    \item   \code{\link{getDerivate}}
#'    \item   \code{\link{getEquation}}
#'    \item   \code{\link{getEquations}}
#'    \item   \code{\link{getNumberOfParameters}}
#'    \item   \code{\link{getParameters}}
#'    \item   \code{\link{getResponseIndice}}
#'    \item   \code{\link{remplaceDose}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelError}}} {
#'
#'   \itemize{
#'    \item   \code{\link{g}}
#'    \item   \code{\link{getCError}}
#'    \item   \code{\link{getDVSigma}}
#'    \item   \code{\link{getEquation}}
#'    \item   \code{\link{getErrorModelParameters}}
#'    \item   \code{\link{getNumberOfParameter}}
#'    \item   \code{\link{getSig}}
#'    \item   \code{\link{getSigmaInter}}
#'    \item   \code{\link{getSigmaNames}}
#'    \item   \code{\link{getSigmaSlope}}
#'    \item   \code{\link{getSigmaValues}}
#'    \item   \code{\link{setCError<-}}
#'    \item   \code{\link{setSigmaInter<-}}
#'    \item   \code{\link{setSigmaSlope<-}}
#'    \item   \code{\link{show}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelInfusionEquations}}}{
#'
#'   \itemize{
#'    \item \code{\link{getInfusionEquations}}
#'    \item \code{\link{getEquationsModelPKPD}}
#'    \item \code{\link{EvaluateModelInfusion}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelInfusionODEquations}}}{
#'
#'   \itemize{
#'    \item \code{\link{getResponseIndice}}
#'    \item \code{\link{scaleResponsesEvaluationODEInfusion}}
#'    \item \code{\link{EvaluateModelODEInfusion}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelODEquations}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getDerivatives}}
#'    \item   \code{\link{scaleResponsesEvaluationODE}}
#'    \item   \code{\link{getEquationsModelPKPD}}
#'    \item   \code{\link{EvaluateModelODE}}
#'   }
#'}
#'
#'
#'\item{Class \code{\link{ModelParameter}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getDerivatesAdjustedByDistribution}}
#'    \item   \code{\link{getDistribution}}
#'    \item   \code{\link{getMu}}
#'    \item   \code{\link{getNameModelParameter}}
#'    \item   \code{\link{getOmega}}
#'    \item   \code{\link{isFixed}}
#'    \item   \code{\link{isFixedMu}}
#'    \item   \code{\link{isNotFixed}}
#'    \item   \code{\link{isNotFixedMu}}
#'   }
#'}
#'
#'\item{Class \code{\link{ModelVariable}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getNameModelVariable}}
#'   }
#'}
#'
#'\item{Class \code{\link{MultiplicativeAlgorithm}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getWeightFrame}}
#'    \item   \code{\link{MultiplicativeAlgorithm_Rcpp}}
#'    \item   \code{\link{Optimize}}
#'    \item   \code{\link{PrepareFIMs}}
#'    \item   \code{\link{setDelta}}
#'    \item   \code{\link{setIteration}}
#'    \item   \code{\link{setShowProcess}}
#'    \item   \code{\link{show}}
#'   }
#'}
#'
#'\item{Class \code{\link{NormalDistribution}}}{
#'
#'   \itemize{
#'    \item   \code{\link{AdjustNormalDistribution}}
#'   }
#'}
#'
#'
#'\item{Class \code{\link{Optimization}}}{
#'
#'   \itemize{
#'    \item   \code{\link{Combinaison}}
#'    \item   \code{\link{EvaluateFIMsAndDesigns}}
#'    \item   \code{\link{getElementaryProtocols}}
#'    \item   \code{\link{getOptimalDesign}}
#'    \item   \code{\link{setOptimalDesign}}
#'    \item   \code{\link{Optimize}}
#'    \item   \code{\link{PrepareFIMs}}
#'    \item   \code{\link{setShowProcess}}
#'    \item   \code{\link{show}}
#'   }
#'}
#'
#'\item{Class \code{\link{PDModel}}}
#'
#'\item{Class \code{\link{PFIMProject}}}{
#'
#'   \itemize{
#'    \item   \code{\link{addDesign}}
#'    \item   \code{\link{addDesigns}}
#'    \item   \code{\link{defineStatisticalModel}}
#'    \item   \code{\link{EvaluateBayesianFIM}}
#'    \item   \code{\link{EvaluateDesign}}
#'    \item   \code{\link{EvaluateIndividualFIM}}
#'    \item   \code{\link{EvaluatePopulationFIM}}
#'    \item   \code{\link{getDesign}}
#'    \item   \code{\link{getEvaluationResponses}}
#'    \item   \code{\link{getFim}}
#'    \item   \code{\link{getFims}}
#'    \item   \code{\link{getFisherMatrices}}
#'    \item   \code{\link{getNamePFIMProject}}
#'    \item   \code{\link{setNamePFIMProject}}
#'    \item   \code{\link{getParametersOdeSolver}}
#'    \item   \code{\link{setParametersOdeSolver}}
#'    \item   \code{\link{getStatisticalModel}}
#'    \item   \code{\link{getWeights}}
#'    \item   \code{\link{OptimizeDesign}}
#'    \item   \code{\link{plotCriteria}}
#'    \item   \code{\link{plotResponse}}
#'    \item   \code{\link{plotRSE}}
#'    \item   \code{\link{plotSE}}
#'    \item   \code{\link{plotSensitivity}}
#'    \item   \code{\link{plotWeightOptimisation}}
#'    \item   \code{\link{plotFrequenciesOptimisation}}
#'    \item   \code{\link{plotShrinkage}}
#    \item   \code{\link{resultsFedorovWynnAlgorithm}}
#'    \item   \code{\link{setConstraint}}
#'    \item   \code{\link{setDesign}}
#    \item   \code{\link{setGraphOptions<-}}
#   \item   \code{\link{setNamePFIMProject<-}}
#    \item   \code{\link{setPreviousFim<-}}
#'    \item   \code{\link{show}}
#'    \item   \code{\link{showConstraints}}
#'    \item   \code{\link{showDesigns}}
#'    \item   \code{\link{showFims}}
#'    \item   \code{\link{summary}}
#'    \item   \code{\link{reportPFIMProject}}
#'   }
#'}
#'
#'\item{Class \code{\link{PKModel}}}{
#'
#'   \itemize{
#'    \item   \code{\link{changeVariablePKModel}}
#'    \item   \code{\link{getEquations}}
#'   }
#'}
#'
#'\item{Class \code{\link{PKPDModel}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getEquations}}
#'    \item   \code{\link{getPDModel}}
#'    \item   \code{\link{getPKModel}}
#'   }
#'}
#'
#'\item{Class \code{\link{PopulationFim}}}{
#'
#'   \itemize{
#'    \item   \code{\link{FinalizeFIMForOneElementaryDesign}}
#'    \item   \code{\link{getDescription}}
#'    \item   \code{\link{getStatisticalModelStandardErrors}}
#'    \item   \code{\link{showStatisticalModelStandardErrors}}
#'   }
#'}
#'
#'\item{Class \code{\link{ReportAndPlots}}}{
#'   \itemize{
#'   \item   \code{\link{knitrModelEquations}}
#'   \item   \code{\link{knitrModelError}}
#'   \item   \code{\link{knitrModelParameters}}
#'   \item   \code{\link{knitrAdministrationParameters}}
#'   \item   \code{\link{knitrInitialDesigns}}
#'   \item   \code{\link{knitrFIM}}
#'   \item   \code{\link{knitrOptimalDesign}}
#'   \item   \code{\link{PFIMProjectReportEvaluation}}
#'   \item   \code{\link{PFIMProjectReportOptimization}}
#'   }
#'}
#'
#'
#'\item{Class \code{\link{Response}}}{
#'
#'   \itemize{
#'    \item   \code{\link{EvaluateErrorModelDerivatives}}
#'    \item   \code{\link{EvaluateODEErrorModelDerivatives}}
#'    \item   \code{\link{getModelError}}
#'    \item   \code{\link{getNameResponse}}
#'    \item   \code{\link{getSigmaNames}}
#'    \item   \code{\link{IndividualFIMEvaluateVariance}}
#'    \item   \code{\link{PopulationFIMEvaluateVariance}}
#'    \item   \code{\link{setModelError<-}}
#'   }
#'}
#'
#'\item{Class \code{\link{SamplingConstraint}}}{
#'
#'   \itemize{
#'    \item   \code{\link{allowedContinuousSamplingTimes}}
#'    \item   \code{\link{allowedDiscretSamplingTimes}}
#'    \item   \code{\link{FixTimeValues}}
#'    \item   \code{\link{getallowedDiscretSamplingTimes}}
#'    \item   \code{\link{getfixedTimes}}
#'    \item   \code{\link{getnumberOfSamplingTimes}}
#'    \item   \code{\link{getOptimisability}}
#'    \item   \code{\link{getResponseName}}
#'    \item   \code{\link{isLessThanDelay}}
#'    \item   \code{\link{isTimeInBetweenBounds}}
#'    \item   \code{\link{numberOfSamplingTimesIsOptimisable}}
#'   }
#'}
#'
#'\item{Class \code{\link{SamplingTimes}}}{
#'
#'   \itemize{
#'    \item   \code{\link{getNameSampleTime}}
#'    \item   \code{\link{getSampleTime}}
#'    \item   \code{\link{setSampleTime}}
#'    \item   \code{\link{getNumberTime}}
#'    \item   \code{\link{getInitialTime}}
#'   }
#'}
#'
#'\item{Class \code{\link{StandardDistribution}}}
#'
#'\item{Class \code{\link{StatisticalModel}}}{
#'
#'   \itemize{
#'    \item   \code{\link{addResponse}}
#'    \item   \code{\link{addResponses}}
#'    \item   \code{\link{CalculatedResidualVariance}}
#'    \item   \code{\link{defineCorrelation}}
#'    \item   \code{\link{defineModelEquations}}
#'    \item   \code{\link{defineParameter}}
#'    \item   \code{\link{Evaluate}}
#'    \item   \code{\link{EvaluationModel}}
#'    \item   \code{\link{getEquationsStatisticalModel}}
#'    \item   \code{\link{getErrorModelStandardErrors}}
#'    \item   \code{\link{getModelParameters}}
#'     \item \code{\link{getFixedParameters}}
#'    \item   \code{\link{getResponsesStatisticalModel}}
#    \item   \code{\link{getStatisticalModelParameters}}
#'    \item   \code{\link{show}}
#    \item   \code{\link{showErrorModelStandardErrors}}
#'    \item   \code{\link{checkParameterInEquations}}
#'    \item   \code{\link{setParametersForEvaluateModel}}
#'    \item   \code{\link{parametersForComputingGradient}}
#'    \item   \code{\link{defineVariable}}
#'    \item   \code{\link{defineVariables}}
#'   }
#'}
#'
#'}

"_PACKAGE"





