
#' @export
createTablesAndFigures <- function(createTable2 = FALSE,
                                   createTable3 = FALSE,
                                   createTable4 = FALSE,
                                   createFigure1 = FALSE,
                                   createFigure2 = FALSE,
                                   createFigure3 = FALSE,
                                   outputFolder) {

  source("S:/Git/GitHub/ohdsi-studies/QbaEvaluation/inst/shiny/global.R")
  rm(covariate, covariateAnalysis, propensityModel, attrition, shinySettings, tcos)
  # keep ace, arb, thz only in DataClean.R, remove filters below


  # Table 2: Database- and exposure-level phenotype errors =====================

  if (createTable2) {
    validationResult$exposureCohort[is.na(validationResult$exposureCohort)] <- "Database"
    table2 <- validationResult %>%
      dplyr::filter(outcomeCohortId == 4008 & # ischemic stroke PL
                      (is.na(exposureCohort) |
                         exposureCohort %in% c("Database",
                                               "ACEI w hypertension",
                                               "ARB w hypertension",
                                               "THZ w hypertension"))) %>%
      # make outcome and exposure filters in DataClean.R

      dplyr::mutate(dplyr::across(c(sens,
                                    spec,
                                    ppv,
                                    npv), round, 4),
                    exposureCohort = sub(" w hypertension", "", exposureCohort),
                    databaseId = dplyr::recode(databaseId,
                                               "optum_ehr" = "Optum EHR",
                                               "optum_extended_dod" = "Optum DOD",
                                               "truven_ccae" = "CCAE",
                                               "truven_mdcd" = "MDCD",
                                               "truven_mdcr" = "MDCR")) %>%
      dplyr::select(Database = databaseId,
                    Validation = exposureCohort,
                    Prevalence = estimatedPrevalence,
                    TP,
                    TN,
                    FP,
                    FN,
                    Sensitivity = sens,
                    Specificity = spec,
                    PPV = ppv,
                    NPV = npv)

    readr::write_csv(table2, file.path(outputFolder, "table2.csv"))
  }


  # Table 3: Empirical example QBA performance metrics =========================

  if (createTable3) {
    ## analysis comparisons ref ================================================
    refAnalysisId <- c(1,1,2,3,3,4,5,5,6,7,7,8,9,9,10,11,11,12)
    compareAnalysisId <- c(2,13,13,4,14,14,6,15,15,8,16,16,10,17,17,12,18,18)

    analysisComparisons <- tibble::tibble(refAnalysisId, compareAnalysisId) %>%
      dplyr::inner_join(cohortMethodAnalysis, dplyr::join_by(refAnalysisId == analysisId)) %>%
      dplyr::rename(refDescription = description) %>%
      dplyr::inner_join(cohortMethodAnalysis, dplyr::join_by(compareAnalysisId == analysisId)) %>%
      dplyr::rename(compareDescription = description) %>%
      dplyr::mutate(compareDescription = sub("183d, ", "", compareDescription),
                    compareDescription = sub("365d, ", "", compareDescription),
                    compareDescription = sub("730d, ", "", compareDescription)) %>%
      dplyr::mutate(comparison = paste(refDescription, "vs", compareDescription)) %>%
      dplyr::select(-c(refDescription, compareDescription))

    ## calculate refAnalysis vs compareAnalysis metrics ========================
    calculateMetrics <- function(row, cohortMethodResult) { # row <- analysisComparisons[1, ]
      refAnalysisId <- row$refAnalysisId
      compareAnalysisId <- row$compareAnalysisId

      result <- cohortMethodResult %>%
        dplyr::filter(analysisId %in% c(refAnalysisId, compareAnalysisId),
                      outcomeId == 4008 &
                        targetName == "ACEI w hypertension" & comparatorName %in% c("ARB w hypertension", "THZ w hypertension")) %>%
                        # make outcome and exposure filters in DataClean.R
        dplyr::reframe(refLogRr = logRr[analysisId == refAnalysisId],
                       compareLogRr = logRr[analysisId == compareAnalysisId],
                       biasDifference = logRr[analysisId == refAnalysisId] - logRr[analysisId == compareAnalysisId],
                       refRr = rr[analysisId == refAnalysisId],
                       compareRr = rr[analysisId == compareAnalysisId],
                       relativeBias = (rr[analysisId == refAnalysisId] - rr[analysisId == compareAnalysisId]) / rr[analysisId == refAnalysisId] * 100,
                       squaredError = (logRr[analysisId == refAnalysisId] - logRr[analysisId == compareAnalysisId])^2,
                       relativePrecision = ((1 / (seLogRr[analysisId == refAnalysisId]^2)) - (1 / (seLogRr[analysisId == compareAnalysisId])^2)) / (1 / (seLogRr[analysisId == refAnalysisId]^2)) * 100,
                       precisionDifference = (1 / (seLogRr[analysisId == refAnalysisId]^2)) - (1 / (seLogRr[analysisId == compareAnalysisId])^2),
                       .by = c(databaseId,
                               targetId,
                               comparatorId)) %>%
        dplyr::mutate(refAnalysisId = refAnalysisId,
                      compareAnalysisId = compareAnalysisId) %>%
        dplyr::select(databaseId,
                      targetId,
                      comparatorId,
                      refAnalysisId,
                      compareAnalysisId,
                      refLogRr,
                      compareLogRr,
                      refRr,
                      compareRr,
                      biasDifference,
                      relativeBias,
                      precisionDifference,
                      relativePrecision,
                      squaredError)
      return(result)
    }

    addExposureNames <- function(resultTable, exposuureOfInterest) {
      resultTable <- resultTable %>%
        dplyr::inner_join(exposureOfInterest, dplyr::join_by(targetId == exposureId)) %>%
        dplyr::rename(targetName = exposureName) %>%
        dplyr::inner_join(exposureOfInterest, dplyr::join_by(comparatorId == exposureId)) %>%
        dplyr::rename(comparatorName = exposureName)
      return(resultTable)
    }

    addAnalysisComparisonName <- function(resultTable, analysisComparisons) {
      resultTable <- resultTable %>%
        dplyr::inner_join(analysisComparisons) %>% # add join_by?
        dplyr::rename(comparisonName = comparison)
      return(resultTable)
    }

    table3Long <- plyr::adply(analysisComparisons,
                              calculateMetrics,
                              .margins = 1,
                              cohortMethodResult) %>%
      addExposureNames(exposureOfInterest) %>%
      addAnalysisComparisonName(analysisComparisons) %>%
      dplyr::mutate(n = NA) %>%
      dplyr::select(databaseId,
                    targetId,
                    targetName,
                    comparatorId,
                    comparatorName,
                    comparisonName,
                    refAnalysisId,
                    compareAnalysisId,
                    refLogRr,
                    compareLogRr,
                    refRr,
                    compareRr,
                    n,
                    biasDifference,
                    relativeBias,
                    precisionDifference,
                    relativePrecision,
                    squaredError)

    ## aggregate refAnalysis vs compareAnalysis metrics across databases =======
    table3aggregated <- table3Long %>%
      dplyr::group_by(comparisonName, targetId, comparatorId) %>%
      dplyr::summarise(n = sum(!is.na(relativeBias)),
                       relativeBias = mean(relativeBias, na.rm = TRUE),
                       biasDifference = mean(biasDifference, na.rm = TRUE),
                       squaredError = mean(squaredError, na.rm = TRUE),
                       relativePrecision = mean(relativePrecision, na.rm = TRUE),
                       precisionDifference = mean(precisionDifference, na.rm = TRUE),
                       .groups = "drop") %>%
      addExposureNames(exposureOfInterest)

    table3 <- dplyr::bind_rows(table3Long, table3aggregated) %>%
      dplyr::select(targetName,
                    comparatorName,
                    comparisonName,
                    databaseId,
                    n,
                    relativeBias,
                    biasDifference,
                    relativePrecision,
                    precisionDifference,
                    squaredError)

    table3$comparisonOrder <- match(table3$comparison, analysisComparisons$comparison)
    table3$databaseOrder <- match(table3$databaseId, unique(table3$databaseId))
    table3 <- table3[order(table3$targetName,
                           table3$comparatorName,
                           table3$comparisonOrder,
                           table3$databaseOrder), ]
    table3$comparisonOrder <- NULL
    table3$databaseOrder <- NULL
    table3$databaseId[is.na(table3$databaseId)] <- "aggregated"
    table3$targetName <- sub(" w hypertension", "", table3$targetName)
    table3$comparatorName <- sub(" w hypertension", "", table3$comparatorName)

    names(table3) <- c("Target",
                       "Comparator",
                       "Analysis comparison",
                       "Database",
                       "N",
                       "Relative Bias",
                       "Bias difference",
                       "Relative precision",
                       "Precision difference",
                       "Squared error")
    table3Manuscript <- table3[grepl("730d", table3$`Analysis comparison`), ]
    table3Appendix <- table3[!grepl("730d", table3$`Analysis comparison`), ] ## DROP 183d results

    readr::write_csv(table3Manuscript, file.path(outputFolder, "table3.csv"), na = "")
    readr::write_csv(table3Appendix, file.path(outputFolder, "sTable3.csv"), na = "")
  }


  # Table 4: Grid space simulation QBA performance metrics =====================
  if (createTable4) {

    prepareContourData <- function(gridSpaceResults,
                                   incidence,
                                   or) {
      dat <- gridSpaceResults[gridSpaceResults$incidence %in% incidence & gridSpaceResults$or %in% or, ]
      return(dat)
    }

    incidences <- unique(gridSpaceResults$incidence)
    ors <- unique(gridSpaceResults$or)

    gridSpaceEvalMetrics <- tibble::tibble()
    for (incidence in incidences) { # incidence <- incidences[1]
      for (or in ors) {             # or <- ors[1]
        dat <- prepareContourData(gridSpaceResults,
                                  incidence = incidence,
                                  or = or)
        breaks <- as.numeric(quantile(dat$correctedOr,
                                      na.rm = TRUE,
                                      type = 3))
        pointData <- dat[dat$correctedOr %in% breaks, c("or", "correctedOr", "sens", "spec")] %>%
          dplyr::group_by(or, correctedOr) %>%
          dplyr::summarize(n = nrow(dat),
                           valid = sum(!is.na(dat$correctedOr)),
                           estimable = valid / n,
                           nonEstimable = 1 - estimable,
                           sens = mean(sens),
                           spec = mean(spec),
                           .groups = "drop") %>% # avg sens and spec by corrected OR if >1 row per corrected OR (doesnt happen)
          dplyr::mutate(incidence = incidence,
                        biasDifference = log(or) - log(correctedOr),
                        relativeBias = (or - correctedOr) / or) %>%
          dplyr::bind_cols(dist = c("min", "25%ile", "50%ile", "75%ile", "max")) %>%
          dplyr::select(incidence,
                        or,
                        dist,
                        correctedOr,
                        estimable,
                        sens,
                        spec,
                        biasDifference,
                        relativeBias)
        gridSpaceEvalMetrics <- dplyr::bind_rows(gridSpaceEvalMetrics, pointData)
      }
    }
    table4 <- gridSpaceEvalMetrics %>%
      dplyr::filter(dist == "50%ile") %>%
      dplyr::select(-dist)
    readr::write_csv(table4, file.path(outputFolder, "table4.csv"), na = "")

    sTable4 <- gridSpaceEvalMetrics
    readr::write_csv(sTable4, file.path(outputFolder, "sTable4.csv"), na = "")
  }


  # Figure 1: Empirical example forest plots ===================================
  if (createFigure1) {

    figure1AnalysisIds <- c(9, 10, 17, 11, 12, 18)
    sfigure1AnalysisIds <- c(5, 6, 15, 7, 8, 16)
    figureAnalysisIds <- list(figure1AnalysisIds, sfigure1AnalysisIds)
    comparatorIds <- c(2, 3)

    forestPlots <- list()
    for (analysisIds in figureAnalysisIds) {
      for (comparatorId in comparatorIds) {
        forestPlotData <- getForestPlotData(connection = NULL, # filter exposureOfInterest and outcomeOfInterest in DataClean.R
                                            targetIds = 1,
                                            comparatorIds = comparatorId,
                                            outcomeIds = 4008,
                                            databaseIds = database$databaseId,
                                            analysisIds = analysisIds)
        forestPlot <- plotForest(forestPlotData)
        forestPlots[[length(forestPlots) + 1]] <- forestPlot
      }
    }

    row1 <- grid::textGrob("ACEI vs ARB", rot = 90, gp = grid::gpar(fontsize = 12))
    row2 <- grid::textGrob("ACEI vs THZ", rot = 90, gp = grid::gpar(fontsize = 12))
    col0 <- grid::textGrob("")

    # 730d TAR
    plotGrob <- gridExtra::arrangeGrob(row1, forestPlots[[1]],
                                       row2, forestPlots[[2]],
                                       heights = c(3, 3),
                                       widths = c(0.5, 7),
                                       nrow = 2)
    figure1 <- gridExtra::grid.arrange(plotGrob,
                                       nrow = 1,
                                       heights = 6)
    ggplot2::ggsave(file.path(outputFolder, "figure1.png"),
                    figure1,
                    height = 7,
                    width = 22)

    # 365d TAR
    sPlotGrob <- gridExtra::arrangeGrob(row1, forestPlots[[3]],
                                        row2, forestPlots[[4]],
                                        heights = c(3, 3),
                                        widths = c(0.5, 7),
                                        nrow = 2)
    sFigure1 <- gridExtra::grid.arrange(sPlotGrob,
                                        nrow = 1,
                                        heights = 6)
    ggplot2::ggsave(file.path(outputFolder, "sFigure1.png"),
                    sFigure1,
                    height = 7,
                    width = 22)
  }


  # Figure 2: Grid space simulation results ====================================
  if (createFigure2) {
    prepareContourData <- function(gridSpaceResults,
                                   incidence,
                                   or) {
      dat <- gridSpaceResults[gridSpaceResults$incidence %in% incidence & gridSpaceResults$or %in% or, ]
      return(dat)
    }

    drawContourPlot <- function(dat) {
      incidence <- unique(dat$incidence)
      minSens <- min(dat$sens)
      maxSens <- max(dat$sens)
      minSpec <- min(dat$spec)
      maxSpec <- max(dat$spec)
      breaks <- as.numeric(quantile(dat$correctedOr,
                                    na.rm = TRUE,
                                    type = 3))
      if (length(unique(breaks)) == 1) {
        pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")] # subset with %>%
        pointData <- pointData[pointData$sens == 1 & pointData$spec == 1, ]
        pointData$text <- sprintf("OR=%.3f", pointData$correctedOr)
      } else {
        pointData <- dat[dat$correctedOr %in% breaks, c("correctedOr", "sens", "spec")] %>%
          dplyr::group_by(correctedOr) %>%
          dplyr::summarize(sens = mean(sens),
                           spec = mean(spec)) %>% # if >1 row per corrected OR
          dplyr::mutate(text = sprintf("OR=%.3f", correctedOr))
      }
      contourPlot <- ggplot2::ggplot(data = dat,
                                     mapping = ggplot2::aes(x = sens,
                                                            y = spec,
                                                            z = correctedOr)) +
        ggplot2::geom_contour(breaks = breaks) +
        ggplot2::geom_point(data = pointData[1:nrow(pointData), ]) +
        ggplot2::geom_text(data = pointData,
                           ggplot2::aes(label = text),
                           size = 3,
                           check_overlap = TRUE,
                           vjust = 1.25) +
        ggplot2::scale_shape_manual(values = 21:25) +
        ggplot2::coord_cartesian(xlim = c(0 - 0.1, 1 + 0.1),
                                 ylim = c(minSpec - (minSpec * incidence * 0.05), maxSpec), expand = TRUE) +
        ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0,0)) +
        ggplot2::scale_colour_gradient(guide = 'none') +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      return(contourPlot)
    }

    incidences <- unique(gridSpaceResults$incidence)
    ors <- unique(gridSpaceResults$or)

    contourPlots <- list()
    for (incidence in incidences) { # incidence <- incidences[1]
      for (or in ors) {             # or <- ors[1]
        dat <- prepareContourData(gridSpaceResults,
                                  incidence = incidence,
                                  or = or)
        contourPlot <- drawContourPlot(dat)
        contourPlots[[length(contourPlots) + 1]] <- contourPlot
      }
    }

    row1 <- grid::textGrob("IP=0.1", rot = 90, gp = grid::gpar(fontsize = 12))
    row2 <- grid::textGrob("IP=0.01", rot = 90, gp = grid::gpar(fontsize = 12))
    row3 <- grid::textGrob("IP=0.001", rot = 90, gp = grid::gpar(fontsize = 12))
    row4 <- grid::textGrob("IP=0.0001", rot = 90, gp = grid::gpar(fontsize = 12))
    row5 <- grid::textGrob("IP=0.00001", rot = 90, gp = grid::gpar(fontsize = 12))
    col0 <- grid::textGrob("")
    col1 <- grid::textGrob("OR=1.001", gp = grid::gpar(fontsize = 12))
    col2 <- grid::textGrob("OR=1.25", gp = grid::gpar(fontsize = 12))
    col3 <- grid::textGrob("OR=1.5", gp = grid::gpar(fontsize = 12))
    col4 <- grid::textGrob("OR=2", gp = grid::gpar(fontsize = 12))
    col5 <- grid::textGrob("OR=4", gp = grid::gpar(fontsize = 12))
    col6 <- grid::textGrob("OR=10", gp = grid::gpar(fontsize = 12))
    yLabel <- grid::textGrob("Specificity", rot = 90, gp = grid::gpar(fontsize = 13))
    xLabel <- grid::textGrob("Sensitivity", gp = grid::gpar(fontsize = 13))

    plotGrob <- gridExtra::arrangeGrob(row1, contourPlots[[1]], contourPlots[[2]], contourPlots[[3]], contourPlots[[4]], contourPlots[[5]], contourPlots[[6]],
                                       row2, contourPlots[[7]], contourPlots[[8]], contourPlots[[9]], contourPlots[[10]], contourPlots[[11]], contourPlots[[12]],
                                       row3, contourPlots[[13]], contourPlots[[14]], contourPlots[[15]], contourPlots[[16]], contourPlots[[17]], contourPlots[[18]],
                                       row4, contourPlots[[19]], contourPlots[[20]], contourPlots[[21]], contourPlots[[22]], contourPlots[[23]], contourPlots[[24]],
                                       row5, contourPlots[[25]], contourPlots[[26]], contourPlots[[27]], contourPlots[[28]], contourPlots[[29]], contourPlots[[30]],
                                       col0, col1, col2, col3, col4, col5, col6,
                                       heights = c(7, 7, 7, 7, 7, 0.5),
                                       widths = c(0.5, 7, 7, 7, 7, 7, 7),
                                       nrow = 6)
    figure2 <- gridExtra::grid.arrange(plotGrob,
                                       nrow = 2,
                                       left = yLabel,
                                       bottom = xLabel,
                                       heights=c(14, 0.1))
    ggplot2::ggsave(file.path(outputFolder, "figure2.png"),
                    figure2,
                    height = 14,
                    width = 22)
  }


  # Figure 3: Valid QBA analytic space stratified by IP-uncorrected OR =========
  if (createFigure3) {
    prepareContourData <- function(gridSpaceResults,
                                   incidence,
                                   or) {
      dat <- gridSpaceResults[gridSpaceResults$incidence %in% incidence & gridSpaceResults$or %in% or, ]
      return(dat)
    }

    incidences <- unique(gridSpaceResults$incidence)
    ors <- unique(gridSpaceResults$or)

    validQbaEstimates <- tibble::tibble()
    for (incidence in incidences) {
      for (or in ors) {
        dat <- prepareContourData(gridSpaceResults,
                                  incidence = incidence,
                                  or = or)
        breaks <- as.numeric(quantile(dat$correctedOr,
                                      na.rm = TRUE,
                                      type = 3))
        pointData <- dat %>%
          dplyr::filter(correctedOr %in% breaks) %>%
          dplyr::select(incidence, or, correctedOr) %>%
          dplyr::group_by(incidence, or) %>%
          dplyr::summarize(n = nrow(dat),
                           valid = sum(!is.na(dat$correctedOr)),
                           Estimable = valid / n,
                           nonEstimable = 1 - Estimable,
                           .groups = "drop")
        validQbaEstimates <- dplyr::bind_rows(validQbaEstimates, pointData)
      }
    }

    validQbaEstimates <- validQbaEstimates %>%
      dplyr::mutate(incidence = factor(validQbaEstimates$incidence, levels = incidences),
                    OR = factor(validQbaEstimates$or, levels = ors))

    figure3 <- ggplot2::ggplot(data = validQbaEstimates,
                               ggplot2::aes(x = OR,
                                            y = Estimable,
                                            group = 1)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_text(data = validQbaEstimates,
                         ggplot2::aes(label = Estimable),
                         size = 5,
                         check_overlap = TRUE,
                         vjust = 1.25) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.x = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 15)) +
      ggplot2::facet_grid(~ incidence) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 20))

    ggplot2::ggsave(file.path(outputFolder, "figure3.png"),
                    figure3,
                    height = 7,
                    width = 22)
  }
}
