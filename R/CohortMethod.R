#' @export
runCohortMethod <- function(connectionDetails,
                            cdmDatabaseSchema,
                            cohortDatabaseSchema,
                            cohortTable,
                            outputFolder,
                            databaseId,
                            maxCores) {

  cmOutputFolder <- file.path(outputFolder, "cmOutput")
  if (!file.exists(cmOutputFolder)) {
    dir.create(cmOutputFolder)
  }

  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "QbaEvaluation")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
  tcosList <- createTcos()
  outcomeIdsOfInterest <- tcosList[[1]]$outcomeIds # FIX THIS

  results <- CohortMethod::runCmAnalyses(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         exposureDatabaseSchema = cohortDatabaseSchema,
                                         exposureTable = cohortTable,
                                         outcomeDatabaseSchema = cohortDatabaseSchema,
                                         outcomeTable = cohortTable,
                                         outputFolder = cmOutputFolder,
                                         cmAnalysisList = cmAnalysisList,
                                         targetComparatorOutcomesList = tcosList,
                                         prefilterCovariates = FALSE,
                                         getDbCohortMethodDataThreads = 1,
                                         createStudyPopThreads = min(3, maxCores),
                                         createPsThreads = max(1, round(maxCores/10)),
                                         psCvThreads = min(10, maxCores),
                                         trimMatchStratifyThreads = min(10, maxCores),
                                         fitOutcomeModelThreads = max(1, round(maxCores/4)),
                                         outcomeCvThreads = min(4, maxCores),
                                         refitPsForEveryOutcome = FALSE,
                                         outcomeIdsOfInterest = outcomeIdsOfInterest)

  analysisSummary <- CohortMethod::summarizeAnalyses(referenceTable = results,
                                                     outputFolder = cmOutputFolder)
  analysisSummary <- addCohortNames(analysisSummary, "targetId", "targetName")
  analysisSummary <- addCohortNames(analysisSummary, "comparatorId", "comparatorName")
  analysisSummary <- addCohortNames(analysisSummary, "outcomeId", "outcomeName")
  analysisSummary <- addAnalysisDescription(analysisSummary, "analysisId", "analysisDescription")
  write.csv(analysisSummary, file.path(outputFolder, "cmAnalysisSummary.csv"), row.names = FALSE)

  ParallelLogger::logInfo(" ---- Computing covariate balance ----")
  balanceFolder <- file.path(cmOutputFolder, "balance")
  if (!file.exists(balanceFolder)) {
    dir.create(balanceFolder)
  }

  subset <- results[results$outcomeId %in% outcomeIdsOfInterest, ]
  #subset <- subset[subset$strataFile != "", ]
  if (nrow(subset) > 0) {
    subset <- split(subset, seq(nrow(subset)))
    # cluster <- ParallelLogger::makeCluster(min(3, maxCores))
    # ParallelLogger::clusterApply(cluster, subset, computeCovBal, cmOutputFolder = cmOutputFolder, balanceFolder = balanceFolder)
    # ParallelLogger::stopCluster(cluster)
    dummy <- lapply(subset, computeCovBal, cmOutputFolder, balanceFolder)
  }
}

createTcos <- function() {
  path <- system.file("settings", "tcoRefFull.csv", package = "QbaEvaluation")
  tcoRef <- read.csv(path, stringsAsFactors = FALSE)
  tcs <- unique(tcoRef[, c("targetCohortId", "comparatorCohortId")])
  createTco <- function(i) { # i = 2
    targetCohortId <- tcs$targetCohortId[i]
    comparatorCohortId <- tcs$comparatorCohortId[i]
    outcomeCohortIds <- as.character(tcoRef$outcomeCohortIds[tcoRef$targetCohortId == targetCohortId & tcoRef$comparatorCohortId == comparatorCohortId])
    outcomeCohortIds <- as.numeric(strsplit(outcomeCohortIds, split = ";")[[1]])
    excludedConceptIds <- paste(tcoRef$targetConceptIds[tcoRef$targetCohortId == targetCohortId][1],
                                tcoRef$comparatorConceptIds[tcoRef$comparatorCohortId == comparatorCohortId][1], sep = ";")
    excludedConceptIds <- as.numeric(strsplit(excludedConceptIds, split = ";")[[1]])
    tco <- CohortMethod::createTargetComparatorOutcomes(targetId = targetCohortId,
                                                        comparatorId = comparatorCohortId,
                                                        outcomeIds = outcomeCohortIds,
                                                        excludedCovariateConceptIds = excludedConceptIds)
    return(tco)
  }
  tcosList <- lapply(1:nrow(tcs), createTco)
  return(tcosList)
}

addCohortNames <- function(data,
                           IdColumnName = "cohortDefinitionId",
                           nameColumnName = "cohortName") {
  exposureRef <- read.csv(system.file("settings", "exposureRefFull.csv", package = "QbaEvaluation"))
  outcomeRef <- read.csv(system.file("settings", "outcomeRef.csv", package = "QbaEvaluation"))
  idToName <- data.frame(cohortId = c(exposureRef$exposureCohortId, outcomeRef$cohortId),
                         cohortName = c(exposureRef$exposureCohortName, outcomeRef$cohortName))
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data) , (idCol+1):(ncol(data)-1))]
  }
  return(data)
}

addAnalysisDescription <- function(data,
                                   IdColumnName = "analysisId",
                                   nameColumnName = "analysisDescription") {
  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "QbaEvaluation")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
  idToName <- lapply(cmAnalysisList, function(x) data.frame(analysisId = x$analysisId, description = as.character(x$description)))
  idToName <- do.call("rbind", idToName)
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data) , (idCol+1):(ncol(data)-1))]
  }
  return(data)
}

# updated functions ============================================================
computeCovBal <- function(row, cmOutputFolder, balanceFolder) {  # row <- subset[[1]]
  outputFileName <- file.path(balanceFolder,
                              sprintf("bal_t%s_c%s_o%s_a%s.rds",
                                      row$targetId,
                                      row$comparatorId,
                                      row$outcomeId,
                                      row$analysisId))
  if (!file.exists(outputFileName)) {
    ParallelLogger::logInfo("Creating covariate balance file ", outputFileName)
    cohortMethodDataFile <- file.path(cmOutputFolder, row$cohortMethodDataFile)
    cohortMethodData <- CohortMethod::loadCohortMethodData(cohortMethodDataFile)
    if (row$strataFile == "") {
      studyPopFile <- file.path(cmOutputFolder, row$studyPopFile)
      studyPop <- readRDS(studyPopFile)
      balance <- computeBeforeMatchingCovariateBalance(population = studyPop, cohortMethodData = cohortMethodData)
      saveRDS(balance, outputFileName)
    } else {
      strataFile <- file.path(cmOutputFolder, row$strataFile)
      strataPop <- readRDS(strataFile)
      balance <- CohortMethod::computeCovariateBalance(population = strataPop, cohortMethodData = cohortMethodData)
      saveRDS(balance, outputFileName)
    }
  }
}

computeBeforeMatchingCovariateBalance <- function(population, # population = studyPop
                                                  cohortMethodData,
                                                  maxCohortSize = 250000) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()

  cohortMethodData$tempCohorts <- cohortMethodData$cohorts %>%
    dplyr::select(.data$rowId, .data$treatment) %>%
    sampleCohortsAndromeda(maxCohortSize = maxCohortSize, label = "before matching")
  on.exit(cohortMethodData$tempCohorts <- NULL)

  #beforeMatching <- computeBeforeMatchinMeansPerGroup(cohortMethodData$cohorts, cohortMethodData)
  beforeMatching <- computeBeforeMatchinMeansPerGroup(cohortMethodData$tempCohorts, cohortMethodData)
  colnames(beforeMatching)[colnames(beforeMatching) == "meanTarget"] <- "beforeMatchingMeanTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTarget"] <- "beforeMatchingSumTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"

  balance <- beforeMatching %>%
    dplyr::inner_join(dplyr::collect(cohortMethodData$covariateRef), by = "covariateId") %>%
    dplyr::mutate(beforeMatchingStdDiff = (.data$beforeMatchingMeanTarget - .data$beforeMatchingMeanComparator)/.data$beforeMatchingSd)
  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
  return(balance)
}

computeBeforeMatchinMeansPerGroup <- function(cohorts,  cohortMethodData) {
  hasStrata <- "stratumId" %in% colnames(cohorts)
  if (hasStrata) {
    stratumSize <- cohorts %>%
      dplyr::group_by(.data$stratumId, .data$treatment) %>%
      dplyr::count() %>%
      dplyr::ungroup()
  }
  if (hasStrata && any(stratumSize %>% pull(.data$n) > 1)) {
    # Variable strata sizes detected: weigh by size of strata set
    w <- stratumSize %>%
      dplyr::mutate(weight = 1/.data$n) %>%
      dplyr::inner_join(cohorts, by = c("stratumId", "treatment")) %>%
      dplyr::select(.data$rowId, .data$treatment, .data$weight)

    # Normalize so sum(weight) == 1 per treatment arm:
    wSum <- w %>%
      dplyr::group_by(.data$treatment) %>%
      dplyr::summarise(wSum = sum(.data$weight, na.rm = TRUE)) %>%
      dplyr::ungroup()

    cohortMethodData$w <- w %>%
      dplyr::inner_join(wSum, by = "treatment") %>%
      dplyr::mutate(weight = .data$weight / .data$wSum) %>%
      dplyr::select(.data$rowId, .data$treatment, .data$weight)

    # By definition:
    sumW <- 1

    # Note: using abs() because due to rounding to machine precision number can become slightly negative:
    result <- cohortMethodData$covariates %>%
      dplyr::inner_join(cohortMethodData$w, by = c("rowId")) %>%
      dplyr::group_by(.data$covariateId, .data$treatment) %>%
      dplyr::summarise(sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
                       mean = sum(.data$weight * as.numeric(.data$covariateValue), na.rm = TRUE),
                       sumSqr = sum(.data$weight * as.numeric(.data$covariateValue)^2, na.rm = TRUE),
                       sumWSqr = sum(.data$weight^2, na.rm = TRUE)) %>%
      dplyr::mutate(sd = sqrt(abs(.data$sumSqr - .data$mean^2) * sumW/(sumW^2 - .data$sumWSqr))) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$covariateId, .data$treatment, .data$sum, .data$mean, .data$sd) %>%
      dplyr::collect()

    cohortMethodData$w <- NULL
  } else {
    cohortCounts <- cohorts %>%
      dplyr::group_by(.data$treatment) %>%
      dplyr::count()

    result <- cohortMethodData$covariates %>%
      dplyr::inner_join(dplyr::select(cohorts, .data$rowId, .data$treatment), by = "rowId") %>%
      dplyr::group_by(.data$covariateId, .data$treatment) %>%
      dplyr::summarise(sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
                       sumSqr = sum(as.numeric(.data$covariateValue)^2, na.rm = TRUE)) %>%
      dplyr::inner_join(cohortCounts, by = "treatment") %>%
      dplyr::mutate(sd = sqrt((.data$sumSqr - (.data$sum^2/.data$n))/.data$n),
                    mean = .data$sum/.data$n) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$covariateId, .data$treatment, .data$sum, .data$mean, .data$sd) %>%
      dplyr::collect()
  }
  target <- result %>%
    dplyr::filter(.data$treatment == 1) %>%
    dplyr::select(.data$covariateId, sumTarget = .data$sum, meanTarget = .data$mean, sdTarget = .data$sd)

  comparator <- result %>%
    dplyr::filter(.data$treatment == 0) %>%
    dplyr::select(.data$covariateId, sumComparator = .data$sum, meanComparator = .data$mean, sdComparator = .data$sd)

  result <- target %>%
    dplyr::full_join(comparator, by = "covariateId") %>%
    dplyr::mutate(sd = sqrt((.data$sdTarget^2 + .data$sdComparator^2)/2)) %>%
    dplyr::select(!c(.data$sdTarget, .data$sdComparator))

  return(result)
}

sampleCohortsAndromeda <- function(cohorts, maxCohortSize, label) {
  if (maxCohortSize <= 0) {
    return(cohorts)
  }
  cohortCounts <- cohorts %>%
    dplyr::group_by(.data$treatment) %>%
    dplyr::count() %>%
    dplyr::collect()
  if (any(cohortCounts$n > maxCohortSize)) {
    return(sampleCohorts(dplyr::collect(cohorts), maxCohortSize, label))
  } else {
    return(cohorts)
  }
}

sampleCohorts <- function(cohorts, maxCohortSize, label) {
  if (maxCohortSize <= 0) {
    return(cohorts)
  }
  sampled <- FALSE
  targetIdx <- which(cohorts$treatment == 1)
  comparatorIdx <- which(cohorts$treatment == 0)
  targetCount <- length(targetIdx)
  comparatorCount <- length(comparatorIdx)
  if (targetCount > maxCohortSize) {
    targetIdx <- sampleSingleCohort(cohorts, 1, maxCohortSize)
    ParallelLogger::logInfo("Downsampling target cohort ",
                            label,
                            " from ",
                            targetCount,
                            " to ",
                            length(targetIdx),
                            " before computing covariate balance")
    sampled <- TRUE
  }
  if (comparatorCount > maxCohortSize) {
    comparatorIdx <- sampleSingleCohort(cohorts, 0, maxCohortSize)
    ParallelLogger::logInfo("Downsampling comparator cohort ",
                            label,
                            " from ",
                            comparatorCount,
                            " to ",
                            length(targetIdx),
                            " before computing covariate balance")
    sampled <- TRUE
  }
  if (sampled) {
    return(cohorts[c(targetIdx, comparatorIdx), ])
  } else {
    return(cohorts)
  }
}

sampleSingleCohort <- function(cohorts, treatment, maxCohortSize) {
  variableStrata <- FALSE
  if ("stratumId" %in% colnames(cohorts)) {
    strataSizes <- cohorts %>%
      filter(.data$treatment == !!treatment) %>%
      group_by(.data$stratumId) %>%
      summarise(count = n())
    variableStrata <- nrow(strataSizes) > 20 && any(strataSizes$count > 1)
  }
  if (variableStrata) {
    # If there are many small and large strata (variable ratio matching), small strata are more likely
    # to be completely deleted by row-based sampling than larger strata, causing bias. Hence we're sampling
    # entire strata to achieve the desired number of rows:
    strataSizes <- strataSizes[sample.int(nrow(strataSizes), replace = F), ]
    stratumIdsToKeep <- strataSizes$stratumId[cumsum(strataSizes$count) <= maxCohortSize]
    idx <- which(cohorts$treatment == !!treatment & cohorts$stratumId %in% stratumIdsToKeep)
  } else {
    idx <- which(cohorts$treatment == !!treatment)
    idx <- sample(idx, maxCohortSize)
  }
  return(idx)
}




