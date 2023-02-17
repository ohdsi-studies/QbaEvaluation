cat("

      ######### Creating TCO reference #########

    ")

tcoRef <- readr::read_csv("inst/settings/tcoRef.csv", show_col_types = FALSE)
exposureRef <- readr::read_csv("inst/settings/exposureRefFull.csv", show_col_types = FALSE)

# add target info --------------------------------------------------------------
tcoRef <- dplyr::inner_join(tcoRef,
                            exposureRef[, c("exposureCohortId",
                                            "exposureName",
                                            "exposureCohortName",
                                            "exposureConceptSetId",
                                            "exposureConceptIds")],
                            by = c("targetCohortId" = "exposureCohortId"))
names(tcoRef)[names(tcoRef) == "exposureName"] <- "targetExposureName"
names(tcoRef)[names(tcoRef) == "exposureCohortName"] <- "targetCohortName"
names(tcoRef)[names(tcoRef) == "exposureConceptSetId"] <- "targetConceptSetId"
names(tcoRef)[names(tcoRef) == "exposureConceptIds"] <- "targetConceptIds"

# add comparator info ----------------------------------------------------------
tcoRef <- dplyr::inner_join(tcoRef,
                            exposureRef[, c("exposureCohortId",
                                            "exposureName",
                                            "exposureCohortName",
                                            "exposureConceptSetId",
                                            "exposureConceptIds")],
                            by = c("comparatorCohortId" = "exposureCohortId"))
names(tcoRef)[names(tcoRef) == "exposureName"] <- "comparatorExposureName"
names(tcoRef)[names(tcoRef) == "exposureCohortName"] <- "comparatorCohortName"
names(tcoRef)[names(tcoRef) == "exposureConceptSetId"] <- "comparatorConceptSetId"
names(tcoRef)[names(tcoRef) == "exposureConceptIds"] <- "comparatorConceptIds"

# add indication info ----------------------------------------------------------
tcoRef <- dplyr::inner_join(tcoRef,
                            exposureRef[, c("exposureCohortId",
                                            "indicationName",
                                            "indicationConceptSetId",
                                            "indicationDrugsConceptSetId")],
                            by = c("targetCohortId" = "exposureCohortId"))


# add validation cohort info ---------------------------------------------------
tcoRef$xSpecCohortId <- tcoRef$outcomeConceptSetId * 1000 + 1
tcoRef$xSpecCohortName <- sprintf("xSpec - %s", tcoRef$outcomeName)
tcoRef$xSensCohortId <- tcoRef$outcomeConceptSetId * 1000 + 2
tcoRef$xSensCohortName <- sprintf("xSens - %s", tcoRef$outcomeName)
tcoRef$prevCohortId <- tcoRef$outcomeConceptSetId * 1000 + 3
tcoRef$prevCohortName <- sprintf("Prev - %s", tcoRef$outcomeName)

# add db validation cohort info ------------------------------------------------
tcoRef$evalCohortId <- tcoRef$outcomeConceptSetId * 1000 + 4
tcoRef$evalCohortName <- sprintf("Eval - %s", tcoRef$outcomeName)

# exposure evaluation cohorts --------------------------------------------------
tcoRef$targetEvalCohortId <- as.numeric(paste0(tcoRef$targetCohortId, tcoRef$outcomeConceptSetId))
tcoRef$targetEvalCohortName <- sprintf("Eval - %s - %s", tcoRef$outcomeName, tcoRef$targetExposureName)
tcoRef$comparatorEvalCohortId <- as.numeric(paste0(tcoRef$comparatorCohortId, tcoRef$outcomeConceptSetId))
tcoRef$comparatorEvalCohortName <- sprintf("Eval - %s - %s", tcoRef$outcomeName, tcoRef$comparatorExposureName)

# exposure/indication evaluation cohorts ---------------------------------------
tcoRef$targetIndEvalCohortId <- as.numeric(paste0(tcoRef$targetCohortId, tcoRef$indicationConceptSetId, tcoRef$outcomeConceptSetId))
tcoRef$targetIndEvalCohortName <- sprintf("Eval - %s - %s", tcoRef$outcomeName, tcoRef$targetCohortName)
tcoRef$comparatorIndEvalCohortId <- as.numeric(paste0(tcoRef$comparatorCohortId, tcoRef$indicationConceptSetId, tcoRef$outcomeConceptSetId))
tcoRef$comparatorIndEvalCohortName <- sprintf("Eval - %s - %s", tcoRef$outcomeName, tcoRef$comparatorCohortName)

readr::write_csv(tcoRef, "inst/settings/tcoRefFull.csv")

# Apixaban vs warfarin among Afib patients for GI bleeding
# LEGEND exposure comparisons for heart failure
