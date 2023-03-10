# Global settings ==============================================================

library(QbaEvaluation)
library(magrittr)

options(andromedaTempFolder = "G:/andromedaTemp", spipen = 999)
maxCores <- parallel::detectCores()
studyFolder <- "G:/OutcomeMisclassificationEval" # move to "G:/QbaEvaluation"
baseUrl <- Sys.getenv("BASE_URL")
maxCores <- parallel::detectCores()


# Optum DoD settings ===========================================================

databaseId <- "optum_extended_dod"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_optum_extended_dod_v1825"
cohortDatabaseSchema = "scratch_jweave17"
cohortTable = "qba_eval_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
        extraSettings = Sys.getenv("EXTRA_SETTINGS"),
        port = Sys.getenv("port"),
        user = Sys.getenv("OHDA_USER"),
        password = Sys.getenv("OHDA_PASSWORD"))

QbaEvaluation::execute(connectionDetails = connectionDetails,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       createCohortTable = FALSE, # TRUE will delete existing cohorts
                       createExposureCohorts = FALSE,
                       createOutcomeCohorts = FALSE,
                       createValidationCohorts = FALSE,
                       runOutcomeValidation = FALSE,
                       runCohortMethod = FALSE,
                       runMisclassificationCorrection = FALSE,
                       exportResults = TRUE)


# Optum EHR settings ===========================================================

databaseId <- "optum_ehr"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_optum_ehr_v1821"
cohortDatabaseSchema = "scratch_jweave17"
cohortTable = "qba_eval_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
        extraSettings = Sys.getenv("EXTRA_SETTINGS"),
        port = Sys.getenv("port"),
        user = Sys.getenv("OHDA_USER"),
        password = Sys.getenv("OHDA_PASSWORD"))

QbaEvaluation::execute(connectionDetails = connectionDetails,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       createCohortTable = FALSE, # TRUE will delete existing cohorts
                       createExposureCohorts = FALSE,
                       createOutcomeCohorts = FALSE,
                       createValidationCohorts = FALSE,
                       runOutcomeValidation = FALSE,
                       runCohortMethod = FALSE,
                       runMisclassificationCorrection = FALSE,
                       exportResults = TRUE)


# MDCR settings ================================================================

databaseId <- "truven_mdcr"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_truven_mdcr_v1838"
cohortDatabaseSchema = "scratch_jweave17"
cohortTable = "qba_eval_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
        extraSettings = Sys.getenv("EXTRA_SETTINGS"),
        port = Sys.getenv("port"),
        user = Sys.getenv("OHDA_USER"),
        password = Sys.getenv("OHDA_PASSWORD"))

QbaEvaluation::execute(connectionDetails = connectionDetails,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       createCohortTable = FALSE, # TRUE will delete existing cohorts
                       createExposureCohorts = FALSE,
                       createOutcomeCohorts = FALSE,
                       createValidationCohorts = FALSE,
                       runOutcomeValidation = FALSE,
                       runCohortMethod = FALSE,
                       runMisclassificationCorrection = FALSE,
                       exportResults = TRUE)


# CCAE settings ================================================================

databaseId <- "truven_ccae"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_truven_ccae_v1831"
cohortDatabaseSchema = "scratch_jweave17"
cohortTable = "qba_eval_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
        extraSettings = Sys.getenv("EXTRA_SETTINGS"),
        port = Sys.getenv("port"),
        user = Sys.getenv("OHDA_USER"),
        password = Sys.getenv("OHDA_PASSWORD"))

QbaEvaluation::execute(connectionDetails = connectionDetails,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       createCohortTable = FALSE,
                       createExposureCohorts = FALSE,
                       createOutcomeCohorts = FALSE,
                       createValidationCohorts = FALSE,
                       runOutcomeValidation = FALSE,
                       runCohortMethod = FALSE,
                       runMisclassificationCorrection = FALSE,
                       exportResults = TRUE)


# MDCD settings ================================================================

databaseId <- "truven_mdcd"
outputFolder <- file.path(studyFolder, databaseId)
cdmDatabaseSchema <- "cdm_truven_mdcd_v1734  "
cohortDatabaseSchema = "scratch_jweave17"
cohortTable = "qba_eval_cohort"

connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        server = paste0(Sys.getenv("OHDA_SERVER"), databaseId),
        extraSettings = Sys.getenv("EXTRA_SETTINGS"),
        port = Sys.getenv("port"),
        user = Sys.getenv("OHDA_USER"),
        password = Sys.getenv("OHDA_PASSWORD"))

QbaEvaluation::execute(connectionDetails = connectionDetails,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTable = cohortTable,
                       createCohortTable = FALSE,
                       createExposureCohorts = FALSE,
                       createOutcomeCohorts = FALSE,
                       createValidationCohorts = FALSE,
                       runOutcomeValidation = FALSE,
                       runCohortMethod = FALSE,
                       runMisclassificationCorrection = FALSE,
                       exportResults = TRUE)


# Prepare estimation for results viewer ========================================

shinyDataFolder <- file.path(studyFolder, "shinyData")
if (!file.exists(shinyDataFolder)) {
  dir.create(shinyDataFolder)
}
file.copy(from = c(list.files(file.path(studyFolder, "optum_ehr", "export"), full.names = TRUE),
                   list.files(file.path(studyFolder, "optum_extended_dod", "export"), full.names = TRUE),
                   list.files(file.path(studyFolder, "truven_ccae", "export"), full.names = TRUE),
                   list.files(file.path(studyFolder, "truven_mdcd", "export"), full.names = TRUE),
                   list.files(file.path(studyFolder, "truven_mdcr", "export"), full.names = TRUE)),
          to = shinyDataFolder,
          overwrite = TRUE)

mergeShinyData(shinyDataFolder = shinyDataFolder,
               mergedShinyDataFolder = file.path(studyFolder, "mergedShinyDataFolder"))


# Grid space simulation analysis ===============================================

gridSpaceFolder <- file.path(studyFolder, "gridSpace")
if (!file.exists(gridSpaceFolder)) {
  dir.create(gridSpaceFolder)
}


QbaEvaluation::runGridSpaceAnalysis(createGridSpace = TRUE,
                                    oddsRatios = c(1.001, 1.25, 1.50, 2, 4, 10),
                                    sens = seq(0.05, 1, 0.05),
                                    incidences = 10^(-1:-5),
                                    runQba = TRUE,
                                    outputFolder = gridSpaceFolder)

mergedShinyDataFolder = file.path(studyFolder, "mergedShinyDataFolder")
file.copy(from = file.path(gridSpaceFolder, "grid_space_results.rds"),
          to = mergedShinyDataFolder,
          overwrite = TRUE)


# Manuscript tables and figures ================================================


manuscriptFolder <- file.path(studyFolder, "manuscript")
if (!file.exists(manuscriptFolder)) {
  dir.create(manuscriptFolder)
}

QbaEvaluation::createTablesAndFigures(createTable2 = TRUE,
                                      createTable3 = TRUE,
                                      createFigure1 = TRUE,
                                      createFigure2 = TRUE,
                                      createFigure3 = TRUE,
                                      outputFolder = manuscriptFolder)


