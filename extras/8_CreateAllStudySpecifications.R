library(magrittr)
options(scipen = 999)
baseUrl <- Sys.getenv("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

specificationFiles <- list.files("extras", full.names = TRUE)
specificationFiles <- specificationFiles[!(specificationFiles %in% c("extras/CodeToRun.R",
                                                                     "extras/3_CreateCohortRefForCd.R",
                                                                     "extras/8_CreateAllStudySpecifications.R"))]

dummy <- lapply(specificationFiles, source)

# build/reload =================================================================
