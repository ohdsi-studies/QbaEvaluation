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
createExposureCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  vocabularyDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  outputFolder,
                                  databaseId) {

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  path <- system.file("settings", "exposureRefFull.csv", package = "QbaEvaluation")
  exposureRef <- readr::read_csv(path, show_col_types = FALSE)

  # build exposure cohorts -----------------------------------------------------
  ParallelLogger::logInfo("  ---- Building exposure cohorts")

  for (i in 1:nrow(exposureRef)) { # i=1

    cohortId <- exposureRef$exposureCohortId[i]
    cohortName <- exposureRef$exposureCohortName[i]
    sqlFileName <- sprintf("%s.sql", exposureRef$exposureCohortName[i])

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

  # get exposure cohort counts -------------------------------------------------
  ParallelLogger::logInfo(sprintf("  ---- Getting exposure cohort counts"))

  exposureCohortIds <- exposureRef$exposureCohortId
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetCohortCounts.sql",
                                           packageName = "QbaEvaluation",
                                           dbms = attr(connection, "dbms"),
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cohort_ids = exposureCohortIds)
  exposureCounts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  exposureCounts <- dplyr::inner_join(exposureRef[, c("exposureCohortId", "exposureCohortName", "indicationName")],
                                      exposureCounts,
                                      by = c("exposureCohortId" = "cohortDefinitionId"))
  names(exposureCounts)[1:2] <- c("exposureCohortId", "exposureCohortName")
  exposureCounts$databaseId <- rep(databaseId, nrow(exposureCounts))
  exposureCounts <- exposureCounts[order(exposureCounts$indicationName, exposureCounts$exposureCohortName), ]
  readr::write_csv(exposureCounts, file.path(outputFolder, "exposureCohortCounts.csv"))
}
