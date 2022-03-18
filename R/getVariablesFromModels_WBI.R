getVariablesFromModels_WBI <- function(birdModels){
  allPredNames <- unique(unlist(lapply(names(birdModels), function(bird){
    preds <- birdModels[[bird]][["RES"]][["gbm"]][["var.names"]]
    return(preds)
  })))
  return(allPredNames)
}
