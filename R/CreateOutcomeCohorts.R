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
createOutcomeCohorts <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 vocabularyDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 outputFolder,
                                 databaseId) {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  path <- system.file("settings", "outcomeRef.csv", package = "QbaEvaluation")
  outcomeRef <- readr::read_csv(path, show_col_types = FALSE)

  # build exposure cohorts =====================================================
  ParallelLogger::logInfo("  ---- Building outcome cohorts")

  for (i in 1:nrow(outcomeRef)) { # i=1

    cohortId <- outcomeRef$cohortId[i]
    cohortName <- outcomeRef$cohortName[i]
    sqlFileName <- sprintf("%s.sql", outcomeRef$cohortName[i])

    ParallelLogger::logInfo(sprintf("  ---- Building %s", cohortName))
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename =  sqlFileName,
                                             packageName = "QbaEvaluation",
                                             dbms = attr(connection, "dbms"),
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = cohortId)
    DatabaseConnector::executeSql(connection, sql)
  }

  # get outcome cohort counts ==================================================
  outcomeCohortIds <- outcomeRef$cohortId
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetCohortCounts.sql",
                                           packageName = "QbaEvaluation",
                                           dbms = attr(connection, "dbms"),
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cohort_ids = outcomeCohortIds)
  outcomeCohortCounts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  outcomeCohortCounts <- dplyr::inner_join(outcomeRef[, c("cohortId", "cohortName")],
                                           outcomeCohortCounts,
                                           by = c("cohortId" = "cohortDefinitionId"))
  names(outcomeCohortCounts)[1:2] <- c("outcomeCohortId", "outcomeCohortName")
  outcomeCohortCounts$databaseId <- rep(databaseId, nrow(outcomeCohortCounts))
  readr::write_csv(outcomeCohortCounts, file.path(outputFolder, "outcomeCohortCounts.csv"))
}
