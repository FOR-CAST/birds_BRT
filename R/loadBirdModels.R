loadBirdModels <- function(birdsList,
                           folderUrl,
                           pathData,
                           version){
  
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
  allModels <- lapply(X = birdsList, FUN = function(bird){
    modAvailable <- grepMulti(x = list.files(modelsPath, full.names = TRUE), 
                              patterns = c(bird, version))
    if (length(modAvailable) == 0){
      # If model is not available, download and return path
      message(paste0("Model for ", bird, 
                     " is not available. Trying download..."))
      downloadedModels <- downloadBirdModels(folderUrl = folderUrl, 
                                             version = version,
                                             birdsList = bird, 
                                             modelsPath = modelsPath,
                                             returnPath = TRUE)
        return(downloadedModels[[1]])
    } else {
      # If model is available, return the path
      return(modAvailable)
    }
  })
  # Cleanup any non existing models:
  allModels <- allModels[!is.na(allModels)]
  
  downloadedModels <- lapply(X = allModels, FUN = function(modelFile){
    modVec <- unlist(allModels)
    modelFile <- basename(modelFile)
    done <- which(basename(modVec) == modelFile)
    percentDone <- round(((done-1)/length(modVec))*100, 2)
    message(paste0("Loading model: ", crayon::magenta(modelFile), ". ", percentDone, "% completed."))
    return(get(load(file.path(modelsPath, modelFile))))
  })
  names(downloadedModels) <- usefulFuns::substrBoth(strng = reproducible::basename2(unlist(allModels)), 
                                                    howManyCharacters = 4, fromEnd = FALSE)
  return(downloadedModels)
}
