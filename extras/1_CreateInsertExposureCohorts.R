cat("

      ######### Creating exposure cohort specifications #########

    ")

exposureRef <- readr::read_csv("inst/settings/exposureRef.csv", show_col_types = FALSE)
exposureRef$exposureCohortName <- sprintf("%s w %s", exposureRef$exposureName, exposureRef$indicationName)
exposureRef$exposureConceptIds <- ""

createInsertExposureCohorts <- function(baseUrl,
                                        exposureCohortId,
                                        exposureCohortName,
                                        exposureConceptSetId,
                                        indicationConceptSetId,
                                        indicationDrugsConceptSetId) {

  exposureCohortJson <- SqlRender::readSql("inst/cohorts/BuildExposureCohort.json")
  exposureCohort <- RJSONIO::fromJSON(exposureCohortJson)

  indicationConceptSet <- ROhdsiWebApi::getConceptSetDefinition(indicationConceptSetId, baseUrl)
  exposureCohort$ConceptSets[[1]]$name <- indicationConceptSet$name
  exposureCohort$ConceptSets[[1]]$expression <- indicationConceptSet$expression

  indicationDrugsConceptSet <- ROhdsiWebApi::getConceptSetDefinition(indicationDrugsConceptSetId, baseUrl)
  exposureCohort$ConceptSets[[2]]$name <- indicationDrugsConceptSet$name
  exposureCohort$ConceptSets[[2]]$expression <- indicationDrugsConceptSet$expression

  exposureConceptSet <- ROhdsiWebApi::getConceptSetDefinition(exposureConceptSetId, baseUrl)
  exposureCohort$ConceptSets[[3]]$name <- exposureConceptSet$name
  exposureCohort$ConceptSets[[3]]$expression <- exposureConceptSet$expression

  exposureConceptIds <- c()
  for (j in 1:length(exposureConceptSet$expression$items)) { #j=1
    exposureConceptIds <- c(exposureConceptIds, exposureConceptSet$expression$items[[j]]$concept$CONCEPT_ID)
  }
  exposureConceptIds <- paste(exposureConceptIds, collapse = ";")

  exposureCohortJson <- RJSONIO::toJSON(exposureCohort, digits = 50, pretty = TRUE)
  write(exposureCohortJson, file = sprintf("inst/cohorts/%s.json", exposureCohortName))

  options <- CirceR::createGenerateOptions(generateStats = FALSE)
  expression <- CirceR::cohortExpressionFromJson(exposureCohortJson)
  sql <- CirceR::buildCohortQuery(expression = expression, options = options)
  SqlRender::writeSql(sql, sprintf("inst/sql/sql_server/%s.sql", exposureCohortName))

  return(exposureConceptIds)
}

for (i in 1:nrow(exposureRef)) { # i=1
  exposureConceptIds <- createInsertExposureCohorts(baseUrl = baseUrl,
                                                    exposureCohortId = exposureRef$exposureCohortId[i],
                                                    exposureCohortName = exposureRef$exposureCohortName[i],
                                                    exposureConceptSetId = exposureRef$exposureConceptSetId[i],
                                                    indicationConceptSetId = exposureRef$indicationConceptSetId[i],
                                                    indicationDrugsConceptSetId = exposureRef$indicationDrugsConceptSetId[i])
  exposureRef$exposureConceptIds[i] <- exposureConceptIds
}

readr::write_csv(exposureRef, "inst/settings/exposureRefFull.csv")










