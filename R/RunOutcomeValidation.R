#' @export
runOutcomeValidation <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 outputFolder) {

  path <- system.file("settings", "tcoRefFull.csv", package = "OutcomeMisclassificationEval")
  tcoRef <- readr::read_csv(path, show_col_types = FALSE)

  for (i in 1:nrow(tcoRef)) { # i=1

    # validation in database population ----------------------------------------
    evalCohortName <- tcoRef$evalCohortName[i]
    path <- file.path("inst/validation", sprintf("%s.json", evalCohortName))
    evalAnalysisList <- PheValuator::loadPheValuatorAnalysisList(path)
    evalExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", evalCohortName))
    referenceTable <- PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          workDatabaseSchema = cohortDatabaseSchema,
                                                          outputFolder = evalExportFolder,
                                                          pheValuatorAnalysisList = evalAnalysisList)
    validationResults <- PheValuator::summarizePheValuatorAnalyses(referenceTable, evalExportFolder)
    readr::write_csv(validationResults, file.path(evalExportFolder, "validationResults.csv"))

    # validation in target population ------------------------------------------
    targetEvalCohortName <- tcoRef$targetEvalCohortName[i]
    path <- file.path("inst/validation", sprintf("%s.json", targetEvalCohortName))
    targetAnalysisList <- PheValuator::loadPheValuatorAnalysisList(path)
    targetExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", targetEvalCohortName))
    referenceTable <- PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          workDatabaseSchema = cohortDatabaseSchema,
                                                          outputFolder = targetExportFolder,
                                                          pheValuatorAnalysisList = targetAnalysisList)
    validationResults <- PheValuator::summarizePheValuatorAnalyses(referenceTable, targetExportFolder)
    readr::write_csv(validationResults, file.path(targetExportFolder, "validationResults.csv"))


    # FILE TO COPY OVER: model_main.rds

    # # copy target pop model to comparator folder and rerun comparator analysis
    # comparatorExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", comparatorEvalCohortName))
    # if (!file.exists(comparatorExportFolder)) {
    #   dir.create(comparatorExportFolder, recursive = TRUE)
    # }
    #
    # comparatorExportEvalFolder <- file.path(comparatorExportFolder, "EvaluationCohort_e1")
    # dir.create(comparatorExportEvalFolder, recursive = TRUE)
    #
    #
    # file.copy(from = file.path(targetExportFolder, "EvaluationCohort_e1", "model_main.rds"), # copied everything. or just copy model_main.rds?
    #           to = comparatorExportEvalFolder,
    #           recursive = TRUE,
    #           overwrite = TRUE)

    # validation in comparator population --------------------------------------
    comparatorEvalCohortName <- tcoRef$comparatorEvalCohortName[i]
    comparatorExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", comparatorEvalCohortName))
    path <- file.path("inst/validation", sprintf("%s.json", comparatorEvalCohortName))
    comparatorAnalysisList <- PheValuator::loadPheValuatorAnalysisList(path)
    referenceTable <- PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          workDatabaseSchema = cohortDatabaseSchema,
                                                          outputFolder = comparatorExportFolder,
                                                          pheValuatorAnalysisList = comparatorAnalysisList)
    validationResults <- PheValuator::summarizePheValuatorAnalyses(referenceTable, comparatorExportFolder)
    readr::write_csv(validationResults, file.path(comparatorExportFolder, "validationResults.csv"))


    # validation in target/indication population -------------------------------
    targetIndEvalCohortName <- tcoRef$targetIndEvalCohortName[i]
    path <- file.path("inst/validation", sprintf("%s.json", targetIndEvalCohortName))
    targetIndAnalysisList <- PheValuator::loadPheValuatorAnalysisList(path)
    targetIndExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", targetIndEvalCohortName))
    referenceTable <- PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          workDatabaseSchema = cohortDatabaseSchema,
                                                          outputFolder = targetIndExportFolder,
                                                          pheValuatorAnalysisList = targetIndAnalysisList)
    validationResults <- PheValuator::summarizePheValuatorAnalyses(referenceTable, targetIndExportFolder)
    readr::write_csv(validationResults, file.path(targetIndExportFolder, "validationResults.csv"))


    # validation in comparator/indication population -------------------------------
    comparatorIndEvalCohortName <- tcoRef$comparatorIndEvalCohortName[i]
    path <- file.path("inst/validation", sprintf("%s.json", comparatorIndEvalCohortName))
    comparatorIndAnalysisList <- PheValuator::loadPheValuatorAnalysisList(path)
    comparatorIndExportFolder <- file.path(outputFolder, sprintf("validationResults - %s", comparatorIndEvalCohortName))
    referenceTable <- PheValuator::runPheValuatorAnalyses(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          workDatabaseSchema = cohortDatabaseSchema,
                                                          outputFolder = comparatorIndExportFolder,
                                                          pheValuatorAnalysisList = comparatorIndAnalysisList)
    validationResults <- PheValuator::summarizePheValuatorAnalyses(referenceTable, comparatorIndExportFolder)
    readr::write_csv(validationResults, file.path(comparatorIndExportFolder, "validationResults.csv"))
  }
}
