cat("

      ######### Creating validation cohort specifications #########

    ")

tcoRef <- readr::read_csv("inst/settings/tcoRefFull.csv", show_col_types = FALSE)

getCohortJson <- function(file) {
  path <- sprintf("inst/cohorts/%s", file)
  cohortJson <- SqlRender::readSql(path)
  cohortJson <- RJSONIO::fromJSON(cohortJson)
  return(cohortJson)
}
xSpecCohort <- getCohortJson("BuildXspecCohort.json")
xSensCohort <- getCohortJson("BuildXsensCohort.json")
prevCohort <- getCohortJson("BuildPrevCohort.json")
evalCohort <- getCohortJson("BuildEvalCohort.json")
expEvalCohort <- getCohortJson("BuildExpEvalCohort.json")
expIndEvalCohort <- getCohortJson("BuildExpIndEvalCohort.json")


# DB validation ================================================================
dbEvalCohortRef <- tcoRef[, c("outcomeName",
                              "outcomeConceptSetId",
                              "xSpecCohortId",
                              "xSpecCohortName",
                              "xSensCohortId",
                              "xSensCohortName",
                              "prevCohortId",
                              "prevCohortName",
                              "evalCohortId",
                              "evalCohortName")]
dbEvalCohortRef <- unique(dbEvalCohortRef)

dbEvalCohortsToCreate <- data.frame(cohortId = c(dbEvalCohortRef$xSpecCohortId,
                                                 dbEvalCohortRef$xSensCohortId,
                                                 dbEvalCohortRef$prevCohortId,
                                                 dbEvalCohortRef$evalCohortId),
                                    cohortName = c(dbEvalCohortRef$xSpecCohortName,
                                                   dbEvalCohortRef$xSensCohortName,
                                                   dbEvalCohortRef$prevCohortName,
                                                   dbEvalCohortRef$evalCohortName))

createInsertValidationCohorts <- function(baseUrl,
                                          validationCohortId,
                                          validationCohortName,
                                          outcomeConceptSetId,
                                          validationCohort,
                                          csRef) {

  validationCohort$id <- validationCohortId
  validationCohort$name <- validationCohortName

  outcomeConceptSet <- ROhdsiWebApi::getConceptSetDefinition(outcomeConceptSetId, baseUrl)
  validationCohort$ConceptSets[[csRef]]$name <- outcomeConceptSet$name
  validationCohort$ConceptSets[[csRef]]$expression <- outcomeConceptSet$expression

  validationCohortJson <- RJSONIO::toJSON(validationCohort, digits = 50, pretty = TRUE)
  write(validationCohortJson, file = sprintf("inst/cohorts/%s.json", validationCohortName))
  options <- CirceR::createGenerateOptions(generateStats = FALSE)
  expression <- CirceR::cohortExpressionFromJson(validationCohortJson)
  sql <- CirceR::buildCohortQuery(expression = expression, options = options)
  SqlRender::writeSql(sql, sprintf("inst/sql/sql_server/%s.sql", validationCohortName))
}

for (i in 1:nrow(dbEvalCohortRef)) { # i = 1
  # xSpec
  createInsertValidationCohorts(baseUrl = baseUrl,
                                validationCohortId = dbEvalCohortRef$xSpecCohortId[i],
                                validationCohortName = dbEvalCohortRef$xSpecCohortName[i],
                                outcomeConceptSetId = dbEvalCohortRef$outcomeConceptSetId[i],
                                validationCohort = xSpecCohort,
                                csRef = 1)
  # xSens
  createInsertValidationCohorts(baseUrl = baseUrl,
                                validationCohortId = dbEvalCohortRef$xSensCohortId[i],
                                validationCohortName = dbEvalCohortRef$xSensCohortName[i],
                                outcomeConceptSetId = dbEvalCohortRef$outcomeConceptSetId[i],
                                validationCohort = xSensCohort,
                                csRef = 1)
  # prev
  createInsertValidationCohorts(baseUrl = baseUrl,
                                validationCohortId = dbEvalCohortRef$prevCohortId[i],
                                validationCohortName = dbEvalCohortRef$prevCohortName[i],
                                outcomeConceptSetId = dbEvalCohortRef$outcomeConceptSetId[i],
                                validationCohort = prevCohort,
                                csRef = 1)
  # eval db
  createInsertValidationCohorts(baseUrl = baseUrl,
                                validationCohortId = dbEvalCohortRef$evalCohortId[i],
                                validationCohortName = dbEvalCohortRef$evalCohortName[i],
                                outcomeConceptSetId = dbEvalCohortRef$outcomeConceptSetId[i],
                                validationCohort = evalCohort,
                                csRef = 2)
}


# Exposure validation ==========================================================
expEvalCohortsToCreate <- data.frame(cohortId = c(tcoRef$targetEvalCohortId, tcoRef$comparatorEvalCohortId),
                                     cohortName = c(tcoRef$targetEvalCohortName, tcoRef$comparatorEvalCohortName))
expEvalCohortsToCreate <- unique(expEvalCohortsToCreate)

createInsertExpEvalCohorts <- function(baseUrl,
                                       validationCohortId,
                                       validationCohortName,
                                       outcomeConceptSetId,
                                       exposureConceptSetId,
                                       validationCohort) {

  validationCohort$id <- validationCohortId
  validationCohort$name <- validationCohortName

  outcomeConceptSet <- ROhdsiWebApi::getConceptSetDefinition(outcomeConceptSetId, baseUrl)
  validationCohort$ConceptSets[[2]]$name <- outcomeConceptSet$name
  validationCohort$ConceptSets[[2]]$expression <- outcomeConceptSet$expression

  exposureConceptSet <- ROhdsiWebApi::getConceptSetDefinition(exposureConceptSetId, baseUrl)
  validationCohort$ConceptSets[[3]]$name <- exposureConceptSet$name
  validationCohort$ConceptSets[[3]]$expression <- exposureConceptSet$expression

  validationCohortJson <- RJSONIO::toJSON(validationCohort, digits = 50, pretty = TRUE)
  write(validationCohortJson, file = file.path("inst", "cohorts", sprintf("%s.json", validationCohortName)))
  options <- CirceR::createGenerateOptions(generateStats = FALSE)
  expression <- CirceR::cohortExpressionFromJson(validationCohortJson)
  sql <- CirceR::buildCohortQuery(expression = expression, options = options)
  SqlRender::writeSql(sql, sprintf("inst/sql/sql_server/%s.sql", validationCohortName))
}

for (i in 1:nrow(tcoRef)) { # i = 1
  # target
  createInsertExpEvalCohorts(baseUrl = baseUrl,
                             validationCohortId = tcoRef$targetEvalCohortId[i],
                             validationCohortName = tcoRef$targetEvalCohortName[i],
                             outcomeConceptSetId = tcoRef$outcomeConceptSetId[i],
                             exposureConceptSetId = tcoRef$targetConceptSetId[i],
                             validationCohort = expEvalCohort)
  # comparator
  createInsertExpEvalCohorts(baseUrl = baseUrl,
                             validationCohortId = tcoRef$comparatorEvalCohortId[i],
                             validationCohortName = tcoRef$comparatorEvalCohortName[i],
                             outcomeConceptSetId = tcoRef$outcomeConceptSetId[i],
                             exposureConceptSetId = tcoRef$comparatorConceptSetId[i],
                             validationCohort = expEvalCohort)
}


# Exposure/indication validation ===============================================
expIndEvalCohortsToCreate <- data.frame(cohortId = c(tcoRef$targetIndEvalCohortId, tcoRef$comparatorIndEvalCohortId),
                                        cohortName = c(tcoRef$targetIndEvalCohortName, tcoRef$comparatorIndEvalCohortName))
expIndEvalCohortsToCreate <- unique(expIndEvalCohortsToCreate)

createInsertExpIndEvalCohorts <- function(baseUrl,
                                          validationCohortId,
                                          validationCohortName,
                                          outcomeConceptSetId,
                                          exposureConceptSetId,
                                          indicationConceptSetId,
                                          validationCohort) {

  validationCohort$id <- validationCohortId
  validationCohort$name <- validationCohortName

  outcomeConceptSet <- ROhdsiWebApi::getConceptSetDefinition(outcomeConceptSetId, baseUrl)
  validationCohort$ConceptSets[[2]]$name <- outcomeConceptSet$name
  validationCohort$ConceptSets[[2]]$expression <- outcomeConceptSet$expression

  exposureConceptSet <- ROhdsiWebApi::getConceptSetDefinition(exposureConceptSetId, baseUrl)
  validationCohort$ConceptSets[[3]]$name <- exposureConceptSet$name
  validationCohort$ConceptSets[[3]]$expression <- exposureConceptSet$expression

  indicationConceptSet <- ROhdsiWebApi::getConceptSetDefinition(indicationConceptSetId, baseUrl)
  validationCohort$ConceptSets[[4]]$name <- indicationConceptSet$name
  validationCohort$ConceptSets[[4]]$expression <- indicationConceptSet$expression

  validationCohortJson <- RJSONIO::toJSON(validationCohort, digits = 50, pretty = TRUE)
  write(validationCohortJson, file = sprintf("inst/cohorts/%s", validationCohortName))

  options <- CirceR::createGenerateOptions(generateStats = FALSE)
  expression <- CirceR::cohortExpressionFromJson(validationCohortJson)
  sql <- CirceR::buildCohortQuery(expression = expression, options = options)
  SqlRender::writeSql(sql, sprintf("inst/sql/sql_server/%s.sql", validationCohortName))
}

for (i in 1:nrow(tcoRef)) { # i=1
  # target
  createInsertExpIndEvalCohorts(baseUrl = baseUrl,
                                validationCohortId = tcoRef$targetIndEvalCohortId[i],
                                validationCohortName = tcoRef$targetIndEvalCohortName[i],
                                outcomeConceptSetId = tcoRef$outcomeConceptSetId[i],
                                exposureConceptSetId = tcoRef$targetConceptSetId[i],
                                indicationConceptSetId = tcoRef$indicationConceptSetId[i],
                                validationCohort = expIndEvalCohort)
  #comparator
  createInsertExpIndEvalCohorts(baseUrl = baseUrl,
                                validationCohortId = tcoRef$comparatorIndEvalCohortId[i],
                                validationCohortName = tcoRef$comparatorIndEvalCohortName[i],
                                outcomeConceptSetId = tcoRef$outcomeConceptSetId[i],
                                exposureConceptSetId = tcoRef$comparatorConceptSetId[i],
                                indicationConceptSetId = tcoRef$indicationConceptSetId[i],
                                validationCohort = expIndEvalCohort)
}

validationCohortRef <- dplyr::bind_rows(dbEvalCohortsToCreate,
                                        expEvalCohortsToCreate,
                                        expIndEvalCohortsToCreate)
readr::write_csv(validationCohortRef, "inst/settings/validationCohortRef.csv")
