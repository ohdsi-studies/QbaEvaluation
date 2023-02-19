#' @export
exportResults <- function(outputFolder,
                          databaseId,
                          databaseName) {

  exportFolder <- file.path(outputFolder, "export")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  exportAnalyses(outputFolder, exportFolder)
  exportExposures(outputFolder, exportFolder)
  exportOutcomes(outcomeFolder, exportFolder)
  exportMetadata(outputFolder, exportFolder, databaseId, databaseId)
  exportMainResults(outputFolder, exportFolder, databaseId)
  exportDiagnostics(outputFolder, exportFolder, databaseId)
}


exportAnalyses <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo(" ==== Exporting analyses ============================")

  ParallelLogger::logInfo(" ---- cohort_method_analysis table ------------------")
  tempFileName <- tempfile()
  cmAnalysisListFile <- system.file("settings", "cmAnalysisList.json", package = "QbaEvaluation")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

  cmAnalysisToRow <- function(cmAnalysis) {  # cmAnalysis <- cmAnalysisList[[1]]
    ParallelLogger::saveSettingsToJson(cmAnalysis, tempFileName)
    row <- tibble::tibble(analysisId = cmAnalysis$analysisId,
                          description = cmAnalysis$description)
    return(row)
  }
  cohortMethodAnalysis <- lapply(cmAnalysisList, cmAnalysisToRow)
  cohortMethodAnalysis <- do.call("rbind", cohortMethodAnalysis)
  cohortMethodAnalysis <- unique(cohortMethodAnalysis)
  unlink(tempFileName)
  colnames(cohortMethodAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(cohortMethodAnalysis))
  fileName <- file.path(exportFolder, sprintf("cohort_method_analysis_%s.csv", databaseId))
  readr::write_csv(cohortMethodAnalysis, fileName)

  # ParallelLogger::logInfo(" ---- covariate_analysis table ----------------------")
  # reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  #
  # getCovariateAnalyses <- function(cmAnalysis) {
  #   cmDataFile <- reference$cohortMethodDataFile[reference$analysisId == cmAnalysis$analysisId][1]
  #   cmData <- CohortMethod::loadCohortMethodData(file.path(outputFolder, "cmOutput", cmDataFile))
  #   covariateAnalysis <- dplyr::collect(cmData$analysisRef)
  #   covariateAnalysis <- covariateAnalysis[, c("analysisId", "analysisName")]
  #   colnames(covariateAnalysis) <- c("covariate_analysis_id", "covariate_analysis_name")
  #   covariateAnalysis$analysis_id <- cmAnalysis$analysisId
  #   return(covariateAnalysis)
  # }
  # covariateAnalysis <- lapply(cmAnalysisList, getCovariateAnalyses)
  # covariateAnalysis <- do.call("rbind", covariateAnalysis)
  # fileName <- file.path(exportFolder, sprintf("covariate_analysis_%s.csv", databaseId))
  # readr::write_csv(covariateAnalysis, fileName)
}


exportExposures <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo(" ==== Exporting exposures ===========================")

  ParallelLogger::logInfo(" ---- exposure_of_interest table --------------------")
  tcoRefFile <- system.file("settings", "tcoRefFull.csv", package = "QbaEvaluation")
  tcosRef <- read.csv(tcoRefFile, stringsAsFactors = FALSE)
  exposureRefFile <- system.file("settings", "exposureRefFull.csv", package = "QbaEvaluation")
  exposureRef <- read.csv(exposureRefFile)

  createExposureRow <- function(exposureCohortId) { # exposureCohortId=1
    exposureCohortName <- as.character(exposureRef$exposureCohortName[exposureRef$exposureCohortId == exposureCohortId])
    return(tibble::tibble(exposureId = exposureCohortId,
                          exposureName = exposureCohortName))
  }
  exposureOfInterest <- unique(c(tcosRef$targetCohortId, tcosRef$comparatorCohortId))
  exposureOfInterest <- lapply(exposureOfInterest, createExposureRow)
  exposureOfInterest <- do.call("rbind", exposureOfInterest)
  colnames(exposureOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(exposureOfInterest))
  fileName <- file.path(exportFolder, sprintf("exposure_of_interest_%s.csv", databaseId))
  readr::write_csv(exposureOfInterest, fileName)
}


exportOutcomes <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo(" ==== Exporting outcomes ============================")

  ParallelLogger::logInfo(" ---- outcome_of_interest table ---------------------")
  outcomeRefFile <- system.file("settings", "outcomeRef.csv", package = "QbaEvaluation")
  outcomeRef <- read.csv(outcomeRefFile)

  createOutcomeRow <- function(outcomeCohortId) { # outcomeCohortId = 4008
    outcomeCohortName <- as.character(outcomeRef$cohortName[outcomeRef$cohortId == outcomeCohortId])
    return(tibble::tibble(outcomeId = outcomeCohortId,
                          outcomeName = outcomeCohortName))
  }
  outcomeOfInterest <- lapply(outcomeRef$cohortId, createOutcomeRow)
  outcomeOfInterest <- do.call("rbind", outcomeOfInterest)
  colnames(outcomeOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(outcomeOfInterest))
  fileName <- file.path(exportFolder, sprintf("outcome_of_interest_%s.csv", databaseId))
  readr::write_csv(outcomeOfInterest, fileName)
}


exportMetadata <- function(outputFolder,
                           exportFolder, # exportFolder <- file.path(outputFolder, "export")
                           databaseId,
                           databaseName) {
  ParallelLogger::logInfo(" ==== Exporting metadata ============================")

  ParallelLogger::logInfo(" ---- database table --------------------------------")
  database <- tibble::tibble(database_id = databaseId,
                             database_name = databaseId,
                             is_meta_analysis = 0)
  fileName <- file.path(exportFolder, sprintf("database_%s.csv", databaseId))
  readr::write_csv(database, fileName)

  ParallelLogger::logInfo("---- attrition table --------------------------------")
  fileName <- file.path(exportFolder, sprintf("attrition_%s.csv", databaseId))
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  outcomeRefFile <- system.file("settings", "outcomeRef.csv", package = "QbaEvaluation")
  outcomeRef <- read.csv(outcomeRefFile)
  outcomesOfInterest <- outcomeRef$cohortId
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  first <- !file.exists(fileName)
  pb <- txtProgressBar(style = 3)
  for (i in 1:nrow(reference)) { # i=1
    outcomeModel <- readRDS(file.path(outputFolder,
                                      "cmOutput",
                                      reference$outcomeModelFile[i]))
    attrition <- outcomeModel$attrition[, c("description", "targetPersons", "comparatorPersons")]
    attrition$sequenceNumber <- 1:nrow(attrition)
    attrition1 <- attrition[, c("sequenceNumber", "description", "targetPersons")]
    colnames(attrition1)[3] <- "subjects"
    attrition1$exposureId <- reference$targetId[i]
    attrition2 <- attrition[, c("sequenceNumber", "description", "comparatorPersons")]
    colnames(attrition2)[3] <- "subjects"
    attrition2$exposureId <- reference$comparatorId[i]
    attrition <- rbind(attrition1, attrition2)
    attrition$targetId <- reference$targetId[i]
    attrition$comparatorId <- reference$comparatorId[i]
    attrition$analysisId <- reference$analysisId[i]
    attrition$outcomeId <- reference$outcomeId[i]
    attrition$databaseId <- databaseId
    attrition <- attrition[, c("databaseId",
                               "exposureId",
                               "targetId",
                               "comparatorId",
                               "outcomeId",
                               "analysisId",
                               "sequenceNumber",
                               "description",
                               "subjects")]
    #attrition <- enforceMinCellValue(attrition, "subjects", minCellCount, silent = TRUE)

    colnames(attrition) <- SqlRender::camelCaseToSnakeCase(colnames(attrition))
    write.table(x = attrition,
                file = fileName,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
    first <- FALSE
    if (i %% 100 == 10) {
      setTxtProgressBar(pb, i/nrow(reference))
    }
  }
  setTxtProgressBar(pb, 1)
  close(pb)

  # ParallelLogger::logInfo(" ---- covariate table -------------------------------")
  # reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  #
  # getCovariates <- function(analysisId) { # analysisId=1
  #   cmDataFile <- reference$cohortMethodDataFile[reference$analysisId == analysisId][1]
  #   cmData <- CohortMethod::loadCohortMethodData(file.path(outputFolder, "cmOutput", cmDataFile))
  #   covariateRef <- dplyr::collect(cmData$covariateRef)
  #   covariateRef <- covariateRef[, c("covariateId", "covariateName", "analysisId")]
  #   colnames(covariateRef) <- c("covariateId", "covariateName", "covariateAnalysisId")
  #   covariateRef$analysisId <- analysisId
  #   return(covariateRef)
  # }
  # covariates <- lapply(unique(reference$analysisId), getCovariates)
  # covariates <- do.call("rbind", covariates)
  # covariates$databaseId <- databaseId
  # colnames(covariates) <- SqlRender::camelCaseToSnakeCase(colnames(covariates))
  # fileName <- file.path(exportFolder, sprintf("covariate_%s.csv", databaseId))
  # readr::write_csv(covariates, fileName)
  # rm(covariates)
}


exportMainResults <- function(outputFolder,
                              exportFolder,
                              databaseId) {
  ParallelLogger::logInfo(" ==== Exporting main results ========================")

  ParallelLogger::logInfo(" ---- cohort_method_result table --------------------")
  results <- readr::read_csv(file.path(outputFolder, "misclassificationCorrectedCmSummary.csv"), col_types = readr::cols())
  results$databaseId <- databaseId
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <- file.path(exportFolder, sprintf("cohort_method_result_%s.csv", databaseId))
  readr::write_csv(results, fileName)
  rm(results)

  ParallelLogger::logInfo(" ---- validation results table ----------------------")
  results <- readr::read_csv(file.path(outputFolder, "validationResultsSummary.csv"), col_types = readr::cols())
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <- file.path(exportFolder, sprintf("validation_result_%s.csv", databaseId))
  readr::write_csv(results, fileName)
  rm(results)

  ParallelLogger::logInfo(" ---- likelihood_profile table ----------------------")
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  fileName <- file.path(exportFolder, sprintf("likelihood_profile_%s.csv", databaseId))
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  pb <- txtProgressBar(style = 3)
  for (i in 1:nrow(reference)) { # i=1
    if (reference$outcomeModelFile[i] != "") {
      outcomeModel <- readRDS(file.path(outputFolder, "cmOutput", reference$outcomeModelFile[i]))
      profile <- outcomeModel$logLikelihoodProfile
      if (!is.null(profile)) {
        profile <- data.frame(targetId = reference$targetId[i],
                              comparatorId = reference$comparatorId[i],
                              outcomeId = reference$outcomeId[i],
                              analysisId = reference$analysisId[i],
                              logRelativeRisk = as.numeric(names(profile)),
                              logLikelihood = profile - max(profile))
        colnames(profile) <- SqlRender::camelCaseToSnakeCase(colnames(profile))
        write.table(x = profile,
                    file = fileName,
                    row.names = FALSE,
                    col.names = first,
                    sep = ",",
                    dec = ".",
                    qmethod = "double",
                    append = !first)
        first <- FALSE
      }
    }
    setTxtProgressBar(pb, i/nrow(reference))
  }
  close(pb)
}


exportDiagnostics <- function(outputFolder,
                              exportFolder,
                              databaseId){
  ParallelLogger::logInfo(" ==== Exporting diagnostics =========================")

  ParallelLogger::logInfo(" ---- covariate_balance table -----------------------")
  fileName <- file.path(exportFolder, sprintf("covariate_balance_%s.csv", databaseId))
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  balanceFolder <- file.path(outputFolder, "cmOutput", "balance")
  files <- list.files(balanceFolder, pattern = "bal_.*.rds", full.names = TRUE)
  pb <- txtProgressBar(style = 3)

  for (i in 1:length(files)) { # i=1
    ids <- gsub("^.*bal_t", "", files[i])
    targetId <- as.numeric(gsub("_c.*", "", ids))
    ids <- gsub("^.*_c", "", ids)
    comparatorId <- as.numeric(gsub("_[aso].*$", "", ids))
    if (grepl("_o", ids)) {
      outcomeId <- as.numeric(gsub("^.*_o", "", gsub("_a[0-9]*.rds", "", ids)))
    } else {
      outcomeId <- NA
    }
    ids <- gsub("^.*_a", "", ids)
    analysisId <- as.numeric(gsub(".rds", "", ids))

    balance <- readRDS(files[i])
    balance$databaseId <- databaseId
    balance$targetId <- targetId
    balance$comparatorId <- comparatorId
    balance$outcomeId <- outcomeId
    balance$analysisId <- analysisId

    if (!("afterMatchingSumTarget" %in% names(balance))) {
      balance$afterMatchingSumTarget <- NA
      balance$afterMatchingMeanTarget <- NA
      balance$afterMatchingSumComparator <- NA
      balance$afterMatchingMeanComparator <- NA
      balance$afterMatchingStdDiff <- NA
    }

    balance <- balance[, c("databaseId",
                           "targetId",
                           "comparatorId",
                           "outcomeId",
                           "analysisId",
                           "covariateId",
                           "beforeMatchingMeanTarget",
                           "beforeMatchingMeanComparator",
                           "beforeMatchingStdDiff",
                           "afterMatchingMeanTarget",
                           "afterMatchingMeanComparator",
                           "afterMatchingStdDiff")]
    colnames(balance) <- c("databaseId",
                           "targetId",
                           "comparatorId",
                           "outcomeId",
                           "analysisId",
                           "covariateId",
                           "targetMeanBefore",
                           "comparatorMeanBefore",
                           "stdDiffBefore",
                           "targetMeanAfter",
                           "comparatorMeanAfter",
                           "stdDiffAfter")

    balance$targetMeanBefore[is.na(balance$targetMeanBefore)] <- 0
    balance$comparatorMeanBefore[is.na(balance$comparatorMeanBefore)] <- 0
    balance$stdDiffBefore[is.na(balance$stdDiffBefore)] <- 0
    balance$targetMeanAfter[is.na(balance$targetMeanAfter)] <- 0
    balance$comparatorMeanAfter[is.na(balance$comparatorMeanAfter)] <- 0
    balance$stdDiffAfter[is.na(balance$stdDiffAfter)] <- 0

    balance <- balance[!(round(balance$targetMeanBefore, 3) == 0 &
                           round(balance$comparatorMeanBefore, 3) == 0 &
                           round(balance$targetMeanAfter, 3) == 0 &
                           round(balance$comparatorMeanAfter, 3) == 0 &
                           round(balance$stdDiffBefore, 3) == 0 &
                           round(balance$stdDiffAfter, 3) == 0), ]

    balance$targetMeanBefore <- round(balance$targetMeanBefore, 3)
    balance$comparatorMeanBefore <- round(balance$comparatorMeanBefore, 3)
    balance$stdDiffBefore <- round(balance$stdDiffBefore, 3)
    balance$targetMeanAfter <- round(balance$targetMeanAfter, 3)
    balance$comparatorMeanAfter <- round(balance$comparatorMeanAfter, 3)
    balance$stdDiffAfter <- round(balance$stdDiffAfter, 3)

    balance <- balance[!is.na(balance$targetId), ]
    colnames(balance) <- SqlRender::camelCaseToSnakeCase(colnames(balance))
    write.table(x = balance,
                file = fileName,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
    first <- FALSE
    setTxtProgressBar(pb, i/length(files))
  }
  close(pb)

  ParallelLogger::logInfo(" ---- preference_score_dist table -------------------")
  reference <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))

  preparePlotData <- function(row, reference) { # row <- subset[[1]]
    idx <- reference$analysisId == row$analysisId &
      reference$targetId == row$targetId &
      reference$comparatorId == row$comparatorId
    psFileName <- file.path(outputFolder, "cmOutput", reference$sharedPsFile[idx][1])

    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      if (min(ps$propensityScore) < max(ps$propensityScore)) {
        ps <- CohortMethod:::computePreferenceScore(ps)
        d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
        d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)
        result <- tibble::tibble(databaseId = databaseId,
                                 targetId = row$targetId,
                                 comparatorId = row$comparatorId,
                                 analysisId = row$analysisId,
                                 preferenceScore = d1$x,
                                 targetDensity = d1$y,
                                 comparatorDensity = d0$y)
        return(result)
      }
    }
    return(NULL)
  }
  subset <- unique(reference[reference$sharedPsFile != "", c("targetId", "comparatorId", "analysisId")])
  subset <- split(subset, 1:nrow(subset))
  data <- plyr::llply(subset,
                      preparePlotData,
                      reference = reference,
                      .progress = "text")
  data <- do.call("rbind", data)
  fileName <- file.path(exportFolder, sprintf("preference_score_dist_%s.csv", databaseId))
  if (!is.null(data)) {
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  }
  readr::write_csv(data, fileName)


  ParallelLogger::logInfo(" ---- propensity_model table ------------------------")
  getPsModel <- function(row, reference) {
    idx <- reference$analysisId == row$analysisId &
      reference$targetId == row$targetId &
      reference$comparatorId == row$comparatorId
    psFileName <- file.path(outputFolder,
                            "cmOutput",
                            reference$sharedPsFile[idx][1])
    if (file.exists(psFileName)) {
      ps <- readRDS(psFileName)
      metaData <- attr(ps, "metaData")
      if (is.null(metaData$psError)) {
        cmDataFile <- file.path(outputFolder,
                                "cmOutput",
                                reference$cohortMethodDataFile[idx][1])
        cmData <- CohortMethod::loadCohortMethodData(cmDataFile)
        model <- CohortMethod::getPsModel(ps, cmData)
        model$covariateId[is.na(model$covariateId)] <- 0
        Andromeda::close(cmData)
        model$databaseId <- databaseId
        model$targetId <- row$targetId
        model$comparatorId <- row$comparatorId
        model$analysisId <- row$analysisId
        model <- model[, c("databaseId", "targetId", "comparatorId", "analysisId", "covariateId", "coefficient")]
        return(model)
      }
    }
    return(NULL)
  }
  subset <- unique(reference[reference$sharedPsFile != "", c("targetId", "comparatorId", "analysisId")])
  subset <- split(subset, 1:nrow(subset))
  data <- plyr::llply(subset,
                      getPsModel,
                      reference = reference,
                      .progress = "text")
  data <- do.call("rbind", data)
  fileName <- file.path(exportFolder, sprintf("propensity_model_%s.csv", databaseId))
  if (!is.null(data)) {
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  }
  readr::write_csv(data, fileName)
}
