#' @export
runGridSpaceAnalysis <- function(createGridSpace = FALSE,
                                 oddsRatios = c(1.01, 1.25, 1.50, 2, 4, 10), # 1.0001
                                 sens = seq(0.05, 1, 0.05),
                                 incidences = 10^(-1:-5),
                                 runQba = FALSE,
                                 outputFolder) { # outputFolder <- gridSpaceFolder


  gridSpaceFile <- file.path(outputFolder, "grid_space.csv")
  gridSpaceResultsFile <- file.path(outputFolder, "grid_space_results.csv")

  # create grid space ==========================================================

  if (createGridSpace) {

    if (!file.exists(gridSpaceFile)) {

      start <- Sys.time()

      fullSpace <- tibble::tibble()
      for (incidence in incidences) {
        min <- 1 - incidence
        spec <- seq(min, 1, length = 20)
        sensSpec <- tibble::as_tibble(expand.grid(sens = sens, spec = spec))
        space <- merge(oddsRatios, sensSpec) %>%
          dplyr::rename(or = x) %>%
          dplyr::mutate(incidence = incidence) %>%
          dplyr::arrange(or, sens, spec)
        fullSpace <- dplyr::bind_rows(fullSpace, space) %>%
          dplyr::relocate(incidence, or, sens, spec)
      }

      gridSpace <- plyr::adply(fullSpace, 1, getCellCounts)
      readr::write_csv(gridSpace, file.path(outputFolder, "grid_space.csv"))

      delta <- Sys.time() - start
      message("Creating grid space took ", signif(delta, 3), attr(delta, "units"))

    } else {
      message(gridSpaceFile, "is already created.")
    }
  }


  # apply qba ==================================================================

  if (runQba) {

    if (!file.exists(gridSpaceResultsFile)) {

      start <- Sys.time()

      gridSpaceResults <- plyr::adply(gridSpace, 1, QbaEvaluation::getQbaResults)
      # gridSpaceResults <- gridSpaceResults %>% dplyr::filter(!is.na(correctedOr))
      delta <- Sys.time() - start
      readr::write_csv(gridSpaceResults, file.path(outputFolder, "grid_space_results.csv"))

      delta <- Sys.time() - start
      message("Applying QBA took ", signif(delta, 3), attr(delta, "units"))

    } else {
      message(gridSpaceResultsFile, "is already created.")
    }
  }
}

getCellCounts <- function(row) {
  values <- QbaEvaluation::createTable(or = row$or, incidence = row$incidence)
  row$a <- values$a
  row$b <- values$b
  row$c <- values$c
  row$d <- values$d
  return(row)
}


