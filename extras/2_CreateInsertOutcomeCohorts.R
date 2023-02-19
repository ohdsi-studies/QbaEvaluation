cat("

      ######### Creating outcome cohort specifications #########

    ")

ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "inst/settings/outcomeRef.csv",
                                                 baseUrl = baseUrl,
                                                 insertTableSql = FALSE,
                                                 insertCohortCreationR = FALSE,
                                                 generateStats = FALSE,
                                                 packageName = "QbaEvaluation")

