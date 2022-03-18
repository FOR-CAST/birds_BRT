downloadBirdModelsBAM <- function(folderUrl,
                               version,
                               birdsList,
                               modelsPath,
                               returnPath = FALSE){

  filesToDownload <- googledrive::drive_ls(path = googledrive::as_id(folderUrl[Species == birdsList, folderID]))

  modelsForBirdList <- filesToDownload$name[grepl(pattern = folderUrl[Species == birdsList, modelUsed],
                                                  x = filesToDownload$name)]
  if (length(modelsForBirdList) == 0){
    message(crayon::red(paste0("No model available for ", birdsList,
                               " for models V", version)))
    return(NA)
  }
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(modelsPath, modelFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% modelFile, ]$id),
                                  path = file.path(modelsPath, modelFile), overwrite = TRUE)
    }
    if (returnPath){
      return(file.path(modelsPath, modelFile))
    } else {
      return(qs::qread(file.path(modelsPath, modelFile)))
    }
  })
  names(downloadedModels) <- birdsList

  return(downloadedModels)
}
