loadBirdModels <- function(birdsList,
                           folderUrl,
                           cloudFolderID,
                           pathData,
                           version,
                           quickLoad = FALSE){
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
if (quickLoad){
  bdAvailable <- list.files(path = modelsPath, 
                            pattern = paste0("brt", version, ".R"), full.names = TRUE, 
                            recursive = FALSE)
  bdAvailable <- unlist(lapply(X = birdsList, FUN = function(bird){
    mod <- grepMulti(x = bdAvailable, patterns = bird)
  }))
  downloadedModels <- lapply(X = bdAvailable, FUN = function(modelFile){
    modelFile <- basename(modelFile)
    return(get(load(file.path(modelsPath, modelFile))))
  })
} else {
  reproducible::Require("googledrive")
  filesToDownload <- Cache(googledrive::drive_ls, path = as_id(folderUrl), pattern = paste0("brt", version, ".R"))
  modelsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"), x = filesToDownload$name)]
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(modelsPath, modelFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% modelFile, ]$id), #modelFile,
                                  path = file.path(modelsPath, modelFile), overwrite = TRUE)
    }
    return(get(load(file.path(modelsPath, modelFile))))
  })
}
  names(downloadedModels) <- usefun::substrBoth(strng = modelsForBirdList, howManyCharacters = 4, fromEnd = FALSE)
  return(downloadedModels)
}
