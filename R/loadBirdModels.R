loadBirdModels <- function(birdsList = sim$birdsList,
                           folderUrl = extractURL("urlModels"),
                           cloudFolderID = sim$cloudFolderID,
                           pathData = dataPath(sim),
                           version){
  reproducible::Require("googledrive")
  filesToDownload <- Cache(googledrive::drive_ls, path = as_id(folderUrl), pattern = paste0("brt", version, ".R"))
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
  modelsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"), x = filesToDownload$name)]
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(modelsPath, modelFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% modelFile, ]$id), #modelFile,
            path = file.path(modelsPath, modelFile), overwrite = TRUE)
    }
    return(get(load(file.path(modelsPath, modelFile))))
  })

  names(downloadedModels) <- birdsList
  return(downloadedModels)
}
