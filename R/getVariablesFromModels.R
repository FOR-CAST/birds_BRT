getVariablesFromModels <- function(birdModels){
  allPredNames <- unique(unlist(lapply(names(birdModels), function(bird){
    preds <- birdModels[[bird]][["gbm.call"]][["predictor.names"]]
    return(preds)
  })))
  return(allPredNames)
}