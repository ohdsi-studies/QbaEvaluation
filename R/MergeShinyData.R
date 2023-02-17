#' @export
mergeShinyData <- function(shinyDataFolder,
                           mergedShinyDataFolder) {

  if (!file.exists(mergedShinyDataFolder)) {
    dir.create(mergedShinyDataFolder)
  }

  files <- list.files(shinyDataFolder, pattern = ".csv")
  splittableTables <- c("covariate_balance", "preference_score_dist") # dont need pref as split table, could probably merge

  databaseFileName <- files[grepl("^database", files)]
  removeParts <- paste0(gsub("database", "", databaseFileName), "$")

  # load/merge files ===========================================================
  loadFile <- function(file, removePart) {
    tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file))
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    if (!(tableName %in% splittableTables)) {
      newData <- readr::read_csv(file.path(shinyDataFolder, file), show_col_types = FALSE)
      colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
      if (exists(camelCaseName, envir = .GlobalEnv)) {
        existingData <- get(camelCaseName, envir = .GlobalEnv)
        newData <- rbind(existingData, newData)
        newData <- unique(newData)
      }
      assign(camelCaseName, newData, envir = .GlobalEnv)
    }
    invisible(NULL)
  }
  for (removePart in removeParts) {
    dummy <- lapply(files[grepl(removePart, files)], loadFile, removePart)
  }

  # write merged files to new folder ===========================================
  removePart <- removeParts[1]
  tableNames <- gsub("_t([0-9]|NA)+_c([0-9]|NA)+$", "", gsub(removePart, "", files[grepl(removePart, files)]))
  tableNames <- unique(tableNames)
  tableNames <- tableNames[!(tableNames %in% splittableTables)] # probably dont need

  saveTable <- function(tableName) { # tableName <- tableNames[1]
    fileName <- file.path(mergedShinyDataFolder, sprintf("%s.rds", tableName))
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    data <- get(camelCaseName, envir = .GlobalEnv)
    if (tableName == "covariate") {
      data$covariateName <- as.factor(data$covariateName)
    }
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    print(fileName)
    saveRDS(data, fileName)
  }
  dummy <- lapply(tableNames, saveTable)

  # convert then copy split tables to new folder ===============================
  toCopy <- files[grepl(paste(splittableTables, collapse = "|"), files)]
  for (file in toCopy) { # file=toCopy[1]
    data <- readr::read_csv(file.path(shinyDataFolder, file), show_col_types = FALSE)
    newFile <- sub("csv", "rds", file)
    saveRDS(data, file.path(mergedShinyDataFolder, newFile))
  }
}


