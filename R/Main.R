#' @export
execute <- function(connectionDetails,
                    outputFolder,
                    databaseId,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable = "qba_eval_cohort",
                    createCohortTable = FALSE, # TRUE will delete existing cohorts
                    createExposureCohorts = FALSE,
                    createOutcomeCohorts = FALSE,
                    createValidationCohorts = FALSE,
                    runOutcomeValidation = FALSE,
                    runCohortMethod = FALSE,
                    runMisclassificationCorrection = FALSE,
                    exportResults = FALSE) {

  start <- Sys.time()

  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  if (createCohortTable) {
    ParallelLogger::logInfo(" ==== Creating cohort table ======================= ")
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                             packageName = "QbaEvaluation",
                                             dbms = attr(connection, "dbms"),
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  }

  if (createExposureCohorts) {
    ParallelLogger::logInfo(" ==== Creating exposure cohorts =================== ")
    createExposureCohorts(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          vocabularyDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          outputFolder = outputFolder,
                          databaseId = databaseId)
  }

  if (createOutcomeCohorts) {
    ParallelLogger::logInfo(" ==== Creating outcome cohorts ==================== ")
    createOutcomeCohorts(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         outputFolder = outputFolder,
                         databaseId = databaseId)
  }

  if (createValidationCohorts) {
    ParallelLogger::logInfo(" ==== Creating validation evaluation cohorts ======")
    createValidationCohorts(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable,
                            outputFolder = outputFolder,
                            databaseId = databaseId)
  }

  if (runOutcomeValidation) {
    ParallelLogger::logInfo(" ==== Running outcome validation ==================")
    runOutcomeValidation(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         outputFolder = outputFolder)
  }

  if (runCohortMethod) {
    ParallelLogger::logInfo(" ==== Running CohortMethod ========================")
    runCohortMethod(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    outputFolder = outputFolder,
                    databaseId = databaseId,
                    maxCores = maxCores)
  }

  if (runMisclassificationCorrection) {
    ParallelLogger::logInfo(" ==== Running misclassification correction ========")
    runMisclassificationCorrection(outputFolder = outputFolder,
                                   databaseId = databaseId)
  }

  if (exportResults) {
    exportResults(outputFolder = outputFolder,
                  databaseId = databaseId,
                  databaseName = databaseId)
  }

  delta <- Sys.time() - start
  sprintf("Executing study took %f %s", signif(delta, 3), attr(delta, "units"))
}
