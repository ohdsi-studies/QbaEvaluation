cat("

      ######### Creating estimation specifications #########

    ")

# Get cmData ===================================================================

getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  excludeDrugsFromCovariates = NULL,
  firstExposureOnly = FALSE,
  removeDuplicateSubjects = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 0,
  maxCohortSize = 0,
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings())

# Define study populations =====================================================

createStudyPopArgs183d <- CohortMethod::createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = TRUE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep first",
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 183,
  censorAtNewRiskWindow = FALSE)

createStudyPopArgs365d <- CohortMethod::createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = TRUE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep first",
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 365,
  censorAtNewRiskWindow = FALSE)

createStudyPopArgs730d <- CohortMethod::createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = TRUE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep first",
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 730,
  censorAtNewRiskWindow = FALSE)

# PS specifications ============================================================

createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE,
  prior = Cyclops::createPrior(
    priorType = "laplace",
    exclude = c(0),
    useCrossValidation = TRUE),
  control = Cyclops::createControl(
    noiseLevel = "silent",
    cvType = "auto",
    tolerance = 2e-07,
    fold = 5,
    cvRepetitions = 1,
    startingVariance = 0.01,
    seed = 123))

matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  allowReverseMatch = FALSE,
  maxRatio = 1)

# outcome model specifications =================================================

fitOutcomeModelArgsMarginal <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "logistic",
  stratified = FALSE)

fitOutcomeModelArgsConditional <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "logistic",
  stratified = TRUE)

# Analysis specifications ======================================================

# 183d =========================================================================
cmAnalysis1 <- CohortMethod::createCmAnalysis(
  analysisId = 1,
  description = "Unadjusted, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis2 <- CohortMethod::createCmAnalysis(
  analysisId = 2,
  description = "QBA, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis3 <- CohortMethod::createCmAnalysis(
  analysisId = 3,
  description = "PS matched, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional) # use marginal model when 1:1 matched?

cmAnalysis4 <- CohortMethod::createCmAnalysis(
  analysisId = 4,
  description = "PS matched, QBA, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)


# 365d =========================================================================

cmAnalysis5 <- CohortMethod::createCmAnalysis(
  analysisId = 5,
  description = "Unadjusted, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis6 <- CohortMethod::createCmAnalysis(
  analysisId = 6,
  description = "QBA, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis7 <- CohortMethod::createCmAnalysis(
  analysisId = 7,
  description = "PS matched, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional) # use marginal model when 1:1 matched?

cmAnalysis8 <- CohortMethod::createCmAnalysis(
  analysisId = 8,
  description = "PS matched, QBA, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)

# 730d =========================================================================

cmAnalysis9 <- CohortMethod::createCmAnalysis(
  analysisId = 9,
  description = "Unadjusted, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis10 <- CohortMethod::createCmAnalysis(
  analysisId = 10,
  description = "QBA, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis11 <- CohortMethod::createCmAnalysis(
  analysisId = 11,
  description = "PS matched, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional) # use marginal model when 1:1 matched?

cmAnalysis12 <- CohortMethod::createCmAnalysis(
  analysisId = 12,
  description = "PS matched, QBA, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)


# differential misclassification ===============================================

cmAnalysis13 <- CohortMethod::createCmAnalysis(
  analysisId = 13,
  description = "diff QBA, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis14 <- CohortMethod::createCmAnalysis(
  analysisId = 14,
  description = "PS matched, diff QBA, 183d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs183d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)

cmAnalysis15 <- CohortMethod::createCmAnalysis(
  analysisId = 15,
  description = "diff QBA, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis16 <- CohortMethod::createCmAnalysis(
  analysisId = 16,
  description = "PS matched, diff QBA, 365d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs365d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)

cmAnalysis17 <- CohortMethod::createCmAnalysis(
  analysisId = 17,
  description = "diff QBA, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = FALSE,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsMarginal)

cmAnalysis18 <- CohortMethod::createCmAnalysis(
  analysisId = 18,
  description = "PS matched, diff QBA, 730d",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs730d,
  createPs = TRUE,
  createPsArgs = createPsArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fitOutcomeModelArgsConditional)

# Analysis list ================================================================

CohortMethod::saveCmAnalysisList(list(cmAnalysis1,
                                      cmAnalysis2,
                                      cmAnalysis3,
                                      cmAnalysis4,
                                      cmAnalysis5,
                                      cmAnalysis6,
                                      cmAnalysis7,
                                      cmAnalysis8,
                                      cmAnalysis9,
                                      cmAnalysis10,
                                      cmAnalysis11,
                                      cmAnalysis12,
                                      cmAnalysis13,
                                      cmAnalysis14,
                                      cmAnalysis15,
                                      cmAnalysis16,
                                      cmAnalysis17,
                                      cmAnalysis18),
                                 file = "inst/settings/cmAnalysisList.json")
