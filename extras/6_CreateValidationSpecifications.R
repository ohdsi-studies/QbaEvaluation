cat("

      ######### Creating validation specifications #########

    ")

tcoRef <- readr::read_csv("inst/settings/tcoRefFull.csv", show_col_types = FALSE)
validationRef <- readr::read_csv("inst/settings/validationCohortRef.csv", show_col_types = FALSE)
outcomeRef <- readr::read_csv("inst/settings/outcomeRef.csv", show_col_types = FALSE)

outcomeNames <- unique(tcoRef$outcomeName)
for (outcomeName in outcomeNames) { # outcomeName <- outcomeNames[1]

  tcRef <- tcoRef[tcoRef$outcomeName == outcomeName, ]
  for (i in 1:nrow(tcRef)) { # i=1
    outcomeCohortIds <- as.numeric(strsplit(tcRef$outcomeCohortIds[i], split = ";")[[1]])

    xSpecCohortId <- tcRef$xSpecCohortId[i]
    xSpecCohortName <- tcRef$xSpecCohortName[i]

    xSensCohortId <- tcRef$xSensCohortId[i]
    xSensCohortName <- tcRef$xSensCohortName[i]

    prevCohortId <- tcRef$prevCohortId[i]
    prevCohortName <- tcRef$prevCohortName[i]

    evalCohortId <- tcRef$evalCohortId[i]
    evalCohortName <- tcRef$evalCohortName[i]

    targetEvalCohortId <- tcRef$targetEvalCohortId[i]
    targetEvalCohortName <- tcRef$targetEvalCohortName[i]
    comparatorEvalCohortId <- tcRef$comparatorEvalCohortId[i]
    comparatorEvalCohortName <- tcRef$comparatorEvalCohortName[i]

    targetIndEvalCohortId <- tcRef$targetIndEvalCohortId[i]
    targetIndEvalCohortName <- tcRef$targetIndEvalCohortName[i]
    comparatorIndEvalCohortId <- tcRef$comparatorIndEvalCohortId[i]
    comparatorIndEvalCohortName <- tcRef$comparatorIndEvalCohortName[i]

    targetConceptSetId <- tcRef$targetConceptSetId[i]
    targetConceptSet <- ROhdsiWebApi::getConceptSetDefinition(targetConceptSetId, baseUrl)
    targetConceptIds <- c()
    for (j in 1:length(targetConceptSet$expression$items)) { #j=1
      targetConceptIds <- c(targetConceptIds, targetConceptSet$expression$items[[j]]$concept$CONCEPT_ID)
    }

    comparatorConceptSetId <- tcRef$comparatorConceptSetId[i]
    comparatorConceptSet <- ROhdsiWebApi::getConceptSetDefinition(comparatorConceptSetId, baseUrl)
    comparatorConceptIds <- c()
    for (k in 1:length(comparatorConceptSet$expression$items)) { #k=1
      comparatorConceptIds <- c(comparatorConceptIds, comparatorConceptSet$expression$items[[k]]$concept$CONCEPT_ID)
    }


    # db validation settings ===================================================
    evalCovariateSettings <- PheValuator::createDefaultAcuteCovariateSettings(
      excludedCovariateConceptIds = c(),
      addDescendantsToExclude = TRUE,
      startDayWindow1 = 0,
      endDayWindow1 = 10,
      startDayWindow2 = 11,
      endDayWindow2 = 20,
      startDayWindow3 = 21,
      endDayWindow3 = 30)

    evalCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecCohortId,
      xSensCohortId = xSensCohortId,
      prevalenceCohortId = prevCohortId,
      evaluationPopulationCohortId = evalCohortId,
      covariateSettings = PheValuator::createDefaultAcuteCovariateSettings(
        excludedCovariateConceptIds = c(),
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30),
      lowerAgeLimit = 0,
      upperAgeLimit = 120,
      startDate = "20100101",
      endDate = "21000101")

    evalAnalysisList <- list()
    evalAnalysisList[[1]] <- PheValuator::createPheValuatorAnalysis(
      analysisId = 1,
      description = xSpecCohortName,
      createEvaluationCohortArgs = evalCohortArgs,
      testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(
        phenotypeCohortId = xSpecCohortId))

    evalAnalysisList[[2]] <- PheValuator::createPheValuatorAnalysis(
      analysisId = length(evalAnalysisList) + 1,
      description = xSensCohortName,
      createEvaluationCohortArgs = evalCohortArgs,
      testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(
        phenotypeCohortId = xSensCohortId))

    evalAnalysisList[[3]] <- PheValuator::createPheValuatorAnalysis(
      analysisId = length(evalAnalysisList) + 1,
      description = prevCohortName,
      createEvaluationCohortArgs = evalCohortArgs,
      testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(
        phenotypeCohortId = prevCohortId))

    for (outcomeCohortId in outcomeCohortIds) {  # outcomeCohortId = 4008
      evalAnalysisList[[length(evalAnalysisList) + 1]] <- PheValuator::createPheValuatorAnalysis(
        analysisId = length(evalAnalysisList) + 1,
        description = outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId],
        createEvaluationCohortArgs = evalCohortArgs,
        testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(phenotypeCohortId = outcomeCohortId))
    }
    PheValuator::savePheValuatorAnalysisList(evalAnalysisList, sprintf("inst/validation/%s.json", evalCohortName))


    # target validation settings ===============================================
    targetEvalCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecCohortId,
      xSensCohortId = xSensCohortId,
      prevalenceCohortId = prevCohortId,
      evaluationPopulationCohortId = targetEvalCohortId,
      covariateSettings = PheValuator::createDefaultAcuteCovariateSettings(
        excludedCovariateConceptIds = targetConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30),
      lowerAgeLimit = 0,
      upperAgeLimit = 120,
      startDate = "20100101",
      endDate = "21000101")

    targetEvalAnalysisList <- list()
    for (outcomeCohortId in outcomeCohortIds) {  # outcomeCohortId = 4008
      targetEvalAnalysisList[[length(targetEvalAnalysisList) + 1]] <- PheValuator::createPheValuatorAnalysis(
        analysisId = length(targetEvalAnalysisList) + 1,
        description = outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId],
        createEvaluationCohortArgs = targetEvalCohortArgs,
        testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(phenotypeCohortId = outcomeCohortId))
    }
    PheValuator::savePheValuatorAnalysisList(targetEvalAnalysisList, sprintf("inst/validation/%s.json", targetEvalCohortName))


    # comparator validation settings ===========================================
    comparatorEvalCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecCohortId,
      xSensCohortId = xSensCohortId,
      prevalenceCohortId = prevCohortId,
      evaluationPopulationCohortId = comparatorEvalCohortId,
      covariateSettings = PheValuator::createDefaultAcuteCovariateSettings(
        excludedCovariateConceptIds = comparatorConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30),
      lowerAgeLimit = 0,
      upperAgeLimit = 120,
      startDate = "20100101",
      endDate = "21000101")

    comparatorEvalAnalysisList <- list()
    for (outcomeCohortId in outcomeCohortIds) {  # outcomeCohortId = 4008
      comparatorEvalAnalysisList[[length(comparatorEvalAnalysisList) + 1]] <- PheValuator::createPheValuatorAnalysis(
        analysisId = length(comparatorEvalAnalysisList) + 1,
        description = outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId],
        createEvaluationCohortArgs = targetEvalCohortArgs,
        testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(phenotypeCohortId = outcomeCohortId))
    }
    PheValuator::savePheValuatorAnalysisList(comparatorEvalAnalysisList, sprintf("inst/validation/%s.json", comparatorEvalCohortName))


    # target/indication validation settings ====================================
    targetIndEvalCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecCohortId,
      xSensCohortId = xSensCohortId,
      prevalenceCohortId = prevCohortId,
      evaluationPopulationCohortId = targetIndEvalCohortId,
      covariateSettings = PheValuator::createDefaultAcuteCovariateSettings(
        excludedCovariateConceptIds = targetConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30),
      lowerAgeLimit = 0,
      upperAgeLimit = 120,
      startDate = "20100101",
      endDate = "21000101")

    targetIndEvalAnalysisList <- list()
    for (outcomeCohortId in outcomeCohortIds) {  # outcomeCohortId = 4008
      targetIndEvalAnalysisList[[length(targetIndEvalAnalysisList) + 1]] <- PheValuator::createPheValuatorAnalysis(
        analysisId = length(targetIndEvalAnalysisList) + 1,
        description = outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId],
        createEvaluationCohortArgs = targetIndEvalCohortArgs,
        testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(phenotypeCohortId = outcomeCohortId))
    }
    PheValuator::savePheValuatorAnalysisList(targetIndEvalAnalysisList, sprintf("inst/validation/%s.json", targetIndEvalCohortName))


    # comparator/indication validation settings ================================
    comparatorIndEvalCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecCohortId,
      xSensCohortId = xSensCohortId,
      prevalenceCohortId = prevCohortId,
      evaluationPopulationCohortId = comparatorIndEvalCohortId,
      covariateSettings = PheValuator::createDefaultAcuteCovariateSettings(
        excludedCovariateConceptIds = comparatorConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30),
      lowerAgeLimit = 0,
      upperAgeLimit = 120,
      startDate = "20100101",
      endDate = "21000101")

    comparatorIndEvalAnalysisList <- list()
    for (outcomeCohortId in outcomeCohortIds) {  # outcomeCohortId = 4008
      comparatorIndEvalAnalysisList[[length(comparatorIndEvalAnalysisList) + 1]] <- PheValuator::createPheValuatorAnalysis(
        analysisId = length(comparatorIndEvalAnalysisList) + 1,
        description = outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId],
        createEvaluationCohortArgs = comparatorIndEvalCohortArgs,
        testPhenotypeAlgorithmArgs = PheValuator::createTestPhenotypeAlgorithmArgs(phenotypeCohortId = outcomeCohortId))
    }
    PheValuator::savePheValuatorAnalysisList(comparatorIndEvalAnalysisList, sprintf("inst/validation/%s.json", comparatorIndEvalCohortName))
  }
}
