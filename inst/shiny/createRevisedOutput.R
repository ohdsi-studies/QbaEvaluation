# create new output per examiner feedback ======================================
library(magrittr)
source("global.R")
rm(covariate, covariateAnalysis, propensityModel, attrition, shinySettings, tcos, gridSpaceResults)

# manuscript output ============================================================
unadj730Id <- 9
psAdj730Id <- 11

cmResult <- cohortMethodResult %>%
  dplyr::filter(
    analysisId %in% c(unadj730Id, psAdj730Id) &
      targetId %in% exposureOfInterest$exposureId &
      outcomeId == 4008
  ) %>%
  dplyr::select(
    databaseId,
    analysisId,
    analysisDescription,
    targetId,
    targetName,
    comparatorId,
    comparatorName,
    target,
    comparator,
    eventsTarget,
    eventsComparator
  )

valResult <- validationResult %>%
  dplyr::filter(
    outcomeCohortId == 4008
  ) %>%
  dplyr::select(
    databaseId,
    exposureCohort,
    sens,
    spec
  ) %>%
  dplyr::mutate(
    exposureCohort = ifelse(is.na(exposureCohort), "non-differential", exposureCohort)
  )

errorMetrics <- tibble::tibble()
errorCategories <- unique(valResult$exposureCohort)

for (errorCategory in errorCategories) { # errorCategory <- errorCategories[1]
  errors <- valResult %>% 
    dplyr::filter(exposureCohort == errorCategory) %>%
    dplyr::select(
      databaseId,
      errorCategory = exposureCohort,
      sens,
      spec
    )
  errors <- tibble::tibble(
    errorCategory = errors$errorCategory[1],
    sens = paste(round(sort(errors$sens), 5), collapse = ";"),
    spec = paste(round(sort(errors$spec), 5), collapse = ";")
  )
  errorMetrics <- dplyr::bind_rows(
    errorMetrics,
    errors
  )
}

cmResult <- cmResult %>%
  dplyr::mutate(
    sensNd = rep(errorMetrics$sens[errorMetrics$errorCategory == "non-differential"], nrow(cmResult)),
    specNd = rep(errorMetrics$spec[errorMetrics$errorCategory == "non-differential"], nrow(cmResult)),
    sensT = rep(errorMetrics$spec[errorMetrics$errorCategory == "ACEI w hypertension"], nrow(cmResult))
  )

cmResult <- dplyr::left_join(
  x = cmResult,
  y = errorMetrics,
  by = c("comparatorName" = "errorCategory")
) %>%
  dplyr::mutate(
    sensC = sens,
    specC = spec
  )





  


# supplement output ============================================================
unadj365Id <- 6
psAdj365Id <- 7



