loadBirdModels <- function(birdsList,
                           folderUrl,
                           cloudFolderID,
                           pathData,
                           version,
                           quickLoad = FALSE){
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE) # [ FIX ] This function doesn't deal with one model (species) at a time.
if (quickLoad){
  bdAvailable <- list.files(path = modelsPath, 
                            pattern = paste0("brt", version, ".R"), full.names = TRUE, 
                            recursive = FALSE)
  if (length(bdAvailable) == 0){
    message(crayon::red(paste0("quickLoad (to load the models you have available without checking the GDrive) is TRUE, ", 
                               "\nbut you don't have the models yet. \nWill try to download the models now...")))
    downloadedModels <- downloadBirdModels(folderUrl = folderUrl, version = version, 
                       birdsList = birdsList, modelsPath = modelsPath)
  } else {
    bdAvailable <- unlist(lapply(X = birdsList, FUN = function(bird){
      mod <- grepMulti(x = bdAvailable, patterns = bird)
    }))
    downloadedModels <- lapply(X = bdAvailable, FUN = function(modelFile){
      modelFile <- basename(modelFile)
      return(get(load(file.path(modelsPath, modelFile))))
    })
  }
} else {
  downloadedModels <- downloadBirdModels(folderUrl = folderUrl, version = version, 
                     birdsList = birdsList, modelsPath = modelsPath)
}
  names(downloadedModels) <- usefun::substrBoth(strng = reproducible::basename2(bdAvailable), howManyCharacters = 4, fromEnd = FALSE)
  return(downloadedModels)
}
