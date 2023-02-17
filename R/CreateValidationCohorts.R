# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of OutcomeMisclassificationEval
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#'
#' @export
createValidationCohorts <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    outputFolder,
                                    databaseId) {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  path <- system.file("settings", "validationCohortRef.csv", package = "OutcomeMisclassificationEval")
  validationCohortRef <- readr::read_csv(path, show_col_types = FALSE)

  # build validation evaluation cohorts ========================================
  for (i in 1:nrow(validationCohortRef)) { # i=4

    validationCohortId <- validationCohortRef$cohortId[i]
    validationCohortName <- validationCohortRef$cohortName[i]
    sqlFileName <- sprintf("%s.sql", validationCohortRef$cohortName[i])

    ParallelLogger::logInfo(sprintf("  ---- Building %s", validationCohortName))
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename =  sqlFileName,
                                             packageName = "OutcomeMisclassificationEval",
                                             dbms = attr(connection, "dbms"),
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = validationCohortId)
    DatabaseConnector::executeSql(connection, sql)
  }

  # get validation evaluation cohort counts -------------------------------------------------
  ParallelLogger::logInfo(sprintf("  ---- Getting validation evaluation cohort counts"))

  validationCohortIds <- validationCohortRef$cohortId
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetCohortCounts.sql",
                                           packageName = "OutcomeMisclassificationEval",
                                           dbms = attr(connection, "dbms"),
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cohort_ids = validationCohortIds)
  validationCohortCounts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  validationCohortCounts <- dplyr::inner_join(validationCohortRef[, c("cohortId", "cohortName")],
                                              validationCohortCounts,
                                              by = c("cohortId" = "cohortDefinitionId"))
  validationCohortCounts$databaseId <- rep(databaseId, nrow(validationCohortCounts))
  validationCohortCounts <- validationCohortCounts[order(validationCohortCounts$cohortName), ]
  readr::write_csv(validationCohortCounts, file.path(outputFolder, "validationCohortCounts.csv"))
}
