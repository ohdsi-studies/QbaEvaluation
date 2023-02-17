cat("

      ######### Creating reference for cohort diagnostics #########

    ")

exposureRefCd <- readr::read_csv("inst/settings/exposureRefFull.csv", show_col_types = FALSE)
exposureRefCd <- exposureRefCd[, c("exposureCohortId", "exposureCohortName")]
names(exposureRefCd) <- c("cohortId", "cohortName")

outcomeRefCd <- readr::read_csv("inst/settings/outcomeRef.csv", show_col_types = FALSE)
outcomeRefCd <- outcomeRefCd[, c("cohortId", "name")]
names(outcomeRefCd) <- c("cohortId", "cohortName")

cohortRefCd <- dplyr::bind_rows(exposureRefCd, outcomeRefCd)
readr::write_csv(cohortRefCd, "inst/settings/cohortRefCd.csv")
