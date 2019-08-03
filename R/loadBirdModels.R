loadBirdModels <- function(birdsList = sim$birdsList,
                           folderUrl = extractURL("urlModels"),
                           cloudFolderID = sim$cloudFolderID,
                           pathData = dataPath(sim),
                           version,
                           quickLoad = FALSE){
if (quickLoad){
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
  bdAvailable <- list.files(path = file.path(getwd(), "modules/birdsNWT/data/models"), 
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
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
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
