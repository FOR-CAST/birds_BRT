loadBirdModels <- function(birdsList,
                           folderUrl,
                           pathData,
                           version) {

  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
  if (version != "reducedBAM") {
    allModels <- lapply(X = birdsList, FUN = function(bird) {
      modAvailable <- grepMulti(x = list.files(modelsPath, full.names = TRUE),
                                patterns = c(bird, version))
      if (length(modAvailable) == 0) {
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
  } else {
    allModels <- lapply(X = birdsList, FUN = function(bird) {
      # What if the model exists? Need to make sure that I get the
      # correct replicate. Match bird with folderUrl's modelUsed
      modUsed <- folderUrl[Species == bird, modelUsed]
      modAvailable <- grepMulti(list.files(modelsPath, full.names = TRUE),
                                           patterns = c(bird, modUsed))
      if (length(modAvailable) == 0) {
        # If model is not available, download and return path
        message(paste0("Model for ", bird, "(", which(folderUrl[["Species"]] == bird),
                       " of ", NROW(folderUrl), ") is not available. Trying download..."))
        downloadedModels <- downloadBirdModelsBAM(folderUrl = folderUrl,
                                               version = version,
                                               birdsList = bird,
                                               modelsPath = modelsPath,
                                               returnPath = TRUE)
        return(downloadedModels[[bird]])
      } else {
        # If model is available, return the path
        return(modAvailable)
      }
    })
  }
  # Cleanup any non existing models:
  allModels <- allModels[!is.na(allModels)]
  downloadedModels <- lapply(X = allModels, FUN = function(modelFile) {
    modVec <- unlist(allModels)
    modelFile <- basename(modelFile)
    done <- which(basename(modVec) == modelFile)
    percentDone <- round(((done - 1)/length(modVec))*100, 2)
    message(paste0("Loading model: ", crayon::magenta(modelFile), ". ", percentDone, "% completed."))
    if (version == "reducedBAM") {
      return(qs::qread(file.path(modelsPath, modelFile)))
    } else {
      return(get(load(file.path(modelsPath, modelFile))))
    }
  })
  if (version == "reducedBAM") {
    listModName <- strsplit(usefulFuns::substrBoth(strng = reproducible::basename2(unlist(allModels)),
                                                   howManyCharacters = 7, fromEnd = FALSE), split = "WB-")
    names(downloadedModels) <- unlist(lapply(listModName, '[[', 2))
  } else {
    names(downloadedModels) <- usefulFuns::substrBoth(strng = reproducible::basename2(unlist(allModels)),
                                                      howManyCharacters = 4, fromEnd = FALSE)
  }
  return(downloadedModels)
}
