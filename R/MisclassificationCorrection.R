#' @export
runMisclassificationCorrection <- function(outputFolder,
                                           databaseId) {

  # create bias parameter object ===============================================
  path <- system.file("settings", "outcomeRef.csv", package = "QbaEvaluation")
  outcomeRef <- readr::read_csv(path, show_col_types = FALSE)
  outcomeRef <- outcomeRef[, c("cohortId", "cohortName")]
  names(outcomeRef) <- c("outcomeCohortId", "outcomeCohortName")

  validationFolders <- list.dirs(outputFolder, full.names = TRUE, recursive = FALSE)
  validationFolders <- grep("- Eval -", validationFolders, value = TRUE)

  validationResults <- tibble::tibble()
  for (i in 1:length(validationFolders)) { # i=1
    path <- file.path(validationFolders[i], "validationResults.csv")
    evalCohort <- sub(".* - Eval - ", "", validationFolders[i])
    exposureCohort <- sub(".*- ", "", evalCohort)

    validationResult <- readr::read_csv(path, show_col_types = FALSE)
    validationResult <- validationResult[validationResult$description %in% outcomeRef$outcomeCohortName, ]

    ## increase validation result precision and drop CIs =======================
    validationResult$sens <- validationResult$truePositives / (validationResult$truePositives + validationResult$falseNegatives)
    validationResult$spec <- validationResult$trueNegatives / (validationResult$trueNegatives + validationResult$falsePositives)
    validationResult$ppv <- validationResult$truePositives / (validationResult$truePositives + validationResult$falsePositives)
    validationResult$npv <- validationResult$trueNegatives / (validationResult$trueNegatives + validationResult$falseNegatives)

    validationResult <- dplyr::inner_join(validationResult,
                                          outcomeRef,
                                          by = c("description" = "outcomeCohortName"))
    validationResult$evalCohort <- evalCohort

    if (i == 1) { # for validation in full db, i.e., not stratified by exposure
      validationResult$exposureCohort <- ""
    } else {
      validationResult$exposureCohort <- exposureCohort
    }
    validationResult$analysisId <- NULL
    validationResults <- dplyr::bind_rows(validationResults, validationResult)
  }
  rm(validationResult)
  validationResults <- validationResults[, c("outcomeCohortId", "evalCohort", "exposureCohort", "estimatedPrevalence",
                                             "truePositives", "trueNegatives", "falsePositives", "falseNegatives",
                                             "sens", "spec", "ppv", "npv")]
  names(validationResults)[5:8] <- c("TP", "TN", "FP", "FN")
  validationResults$databaseId <- databaseId
  validationResults <- validationResults[!(validationResults$exposureCohort %in% c("ACEI", "ARB", "dCCB", "ndCCB", "THZ")), ]
  readr::write_csv(validationResults, file.path(outputFolder, "validationResultsSummary.csv"))


  # misclassification correction analyses ======================================
  path <- file.path(outputFolder, "cmAnalysisSummary.csv")


  ## uncorrected ===============================================================
  uncorrectedAnalysisIds <- c(1, 3, 5, 7, 9, 11)
  uncorrectedCmAnalysisSummary <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::filter(analysisId %in% uncorrectedAnalysisIds) %>%
    dplyr::arrange(analysisId, outcomeId, targetId, comparatorId)


  ## non-differential QBA analysis =============================================
  nonDiffAnalysisIds <- c(2, 4, 6, 8, 10, 12)
  nonDiffCmAnalysisSummary <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::filter(analysisId %in% nonDiffAnalysisIds) %>%
    dplyr::arrange(analysisId, outcomeId, targetId, comparatorId)

  nonDiffValidationResults <- validationResults[validationResults$exposureCohort == "", ] %>%
    dplyr::select(-c(evalCohort, databaseId))

  nonDiffCmAnalysisSummary <- dplyr::inner_join(nonDiffCmAnalysisSummary,
                                                nonDiffValidationResults,
                                                by = c("outcomeId" = "outcomeCohortId"))

  nonDiffCmAnalysisSummaryList <- split(nonDiffCmAnalysisSummary, 1:nrow(nonDiffCmAnalysisSummary))
  nonDiffCmAnalysisSummary <- lapply(nonDiffCmAnalysisSummaryList, runNonDiffQba)
  nonDiffCmAnalysisSummary <- dplyr::bind_rows(nonDiffCmAnalysisSummary)
  nonDiffCmAnalysisSummary <- nonDiffCmAnalysisSummary %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 7))


  ## differential QBA analysis =================================================
  diffAnalysisIds <- 13:18
  diffCmAnalysisSummary <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::filter(analysisId %in% diffAnalysisIds) %>%
    dplyr::arrange(analysisId, outcomeId, targetId, comparatorId)

  diffValidationResults <- validationResults[grepl("w hypertension", validationResults$exposureCohort), ] %>%
    dplyr::select(-c(evalCohort, databaseId))

  diffCmAnalysisSummary <- dplyr::inner_join(diffCmAnalysisSummary,
                                            diffValidationResults,
                                            by = c("outcomeId" = "outcomeCohortId",
                                                   "targetName" = "exposureCohort"))
  diffCmAnalysisSummary <- diffCmAnalysisSummary %>%
    dplyr::rename(estimatedPrevalence_t = estimatedPrevalence,
                  TP_t = TP,
                  TN_t = TN,
                  FP_t = FP,
                  FN_t = FN,
                  sens_t = sens,
                  spec_t = spec,
                  ppv_t = ppv,
                  npv_t = npv)
  diffCmAnalysisSummary <- dplyr::inner_join(diffCmAnalysisSummary,
                                             diffValidationResults,
                                             by = c("outcomeId" = "outcomeCohortId",
                                                    "comparatorName" = "exposureCohort"))
  diffCmAnalysisSummary <- diffCmAnalysisSummary %>%
    dplyr::rename(estimatedPrevalence_c = estimatedPrevalence,
                  TP_c = TP,
                  TN_c = TN,
                  FP_c = FP,
                  FN_c = FN,
                  sens_c = sens,
                  spec_c = spec,
                  ppv_c = ppv,
                  npv_c = npv)

  diffCmAnalysisSummaryList <- split(diffCmAnalysisSummary, 1:nrow(diffCmAnalysisSummary))
  diffCmAnalysisSummary <- lapply(diffCmAnalysisSummaryList, runDiffQba)
  diffCmAnalysisSummary <- dplyr::bind_rows(diffCmAnalysisSummary)
  diffCmAnalysisSummary <- diffCmAnalysisSummary %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 7))


  # combine ===================================================================
  cmAnalysisSummary <- dplyr::bind_rows(uncorrectedCmAnalysisSummary,
                                        nonDiffCmAnalysisSummary,
                                        diffCmAnalysisSummary) %>%
    dplyr::select(-exposureCohort)
  readr::write_csv(cmAnalysisSummary, file.path(outputFolder, "misclassificationCorrectedCmSummary.csv"))
}

runNonDiffQba <- function(row) { # row <- nonDiffCmAnalysisSummaryList[[1]]
  result <- outcomeMisclassificationQba(a = row$eventsTarget,
                                        b = row$eventsComparator,
                                        c = row$target - row$eventsTarget,
                                        d = row$comparator - row$eventsComparator,
                                        outcomeName = row$outcomeName,
                                        targetName = row$targetName,
                                        comparatorName = row$comparatorName,
                                        sens1 = row$sens, # outcome sens in target
                                        sens0 = row$sens, # outcome sens in comparator
                                        spec1 = row$spec, # outcome spec in target
                                        spec0 = row$spec, # outcome spec in comparator
                                        alpha = 0.05)
  estimates <- result$estimates
  correctedTable <- result$correctedTable

  row$rr <- estimates$or[estimates$method == "simpleQba"]
  row$ci95lb <- estimates$ciLb[estimates$method == "simpleQba"]
  row$ci95ub <- estimates$ciUb[estimates$method == "simpleQba"]
  row$seLogRr <- estimates$seLogOr[estimates$method == "simpleQba"]
  z <- log(row$rr)/ row$seLogRr
  if (is.na(z)) {
    row$p <- NA
  } else {
    row$p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
  }
  row$target <- correctedTable$target[correctedTable$outcome == "totals"]
  row$comparator <- correctedTable$comparator[correctedTable$outcome == "totals"]
  row$targetDays <- NA
  row$comparatorDays <- NA
  row$eventsTarget <- correctedTable$target[correctedTable$outcome == paste(row$outcomeName, "[+]")]
  row$eventsComparator <- correctedTable$comparator[correctedTable$outcome == paste(row$outcomeName, "[+]")]
  row$logRr <- log(row$rr)
  row$llr <- NA # get LLR from QBA outcome model
  return(row)
}


runDiffQba <- function(row) { # row <- diffCmAnalysisSummaryList[[1]]
  result <- outcomeMisclassificationQba(a = row$eventsTarget,
                                        b = row$eventsComparator,
                                        c = row$target - row$eventsTarget,
                                        d = row$comparator - row$eventsComparator,
                                        outcomeName = row$outcomeName,
                                        targetName = row$targetName,
                                        comparatorName = row$comparatorName,
                                        sens1 = row$sens_t, # outcome sens in target
                                        sens0 = row$sens_c, # outcome sens in comparator
                                        spec1 = row$spec_t, # outcome spec in target
                                        spec0 = row$spec_c, # outcome spec in comparator
                                        alpha = 0.05)
  estimates <- result$estimates
  correctedTable <- result$correctedTable

  row$rr <- estimates$or[estimates$method == "simpleQba"]
  row$ci95lb <- estimates$ciLb[estimates$method == "simpleQba"]
  row$ci95ub <- estimates$ciUb[estimates$method == "simpleQba"]
  row$seLogRr <- estimates$seLogOr[estimates$method == "simpleQba"]
  z <- log(row$rr)/ row$seLogRr
  if (is.na(z)) {
    row$p <- NA
  } else {
    row$p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
  }
  row$target <- correctedTable$target[correctedTable$outcome == "totals"]
  row$comparator <- correctedTable$comparator[correctedTable$outcome == "totals"]
  row$targetDays <- NA
  row$comparatorDays <- NA
  row$eventsTarget <- correctedTable$target[correctedTable$outcome == paste(row$outcomeName, "[+]")]
  row$eventsComparator <- correctedTable$comparator[correctedTable$outcome == paste(row$outcomeName, "[+]")]
  row$logRr <- log(row$rr)
  row$llr <- NA # get LLR from QBA outcome model
  return(row)
}

