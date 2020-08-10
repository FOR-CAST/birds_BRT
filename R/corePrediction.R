corePrediction <- function(bird, model, birdDensityRas,
                           predictedName,
                           disturbanceRas = NULL,
                           successionStaticLayers = NULL,
                           currentTime,
                           pathData,
                           overwritePredictions = FALSE,
                           savePredVectors = TRUE){
  

  message(crayon::yellow(paste0("Predicting for ", bird , ". Prediction for time ", currentTime)))
  if ("glmerMod" %in% class(model)){
    nameStackRas1 <- names(model@frame)[2]
    nameStackRas2 <- names(model@frame)[3]
  } else {
    if ("glm" %in% class(model)){
      nameStackRas1 <- names(model$coefficients)[2]
      nameStackRas2 <- names(model$coefficients)[3]
    } else {
      if ("gbm" %in% class(model)){ # If gbm, do everything in here, else, do outside
        if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
          message(crayon::yellow(paste0(" Starting prediction raster for ", bird, ". This might take some time... [", Sys.time(),"]")))
          startTime <- Sys.time()
          predicted <- gbm::predict.gbm(object = model, 
                                        newdata = successionStaticLayers,
                                        type = "response",
                                        n.trees = model$n.trees)
          attr(predicted, "prediction") <- paste0(bird, currentTime)
          message(crayon::green(paste0("Prediction finalized for ", bird, ". [", Sys.time(),"]. Total time elapsed: ", 
                                       Sys.time() - startTime)))
        }
        return(predicted)
      } else {
        return(predictedName)
      }
    }
  }
  
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  predictedNameVec <- paste0(predictedName, "VEC.rds")
  if (isTRUE(overwritePredictions)||!file.exists(predictedName)||!file.exists(predictedNameVec)){
    birdDensityRas <- log(birdDensityRas) # log the value of densities so it is the same of the original model
    birdDensityRas[birdDensityRas < -0.99] <- -1 
    # Why did I do this? Maybe because we were not supposed to have values smaller than -0.99?
    vecDF <- data.frame(disturbanceRas, birdDensityRas)
    colnames(vecDF) <- c(nameStackRas1, nameStackRas2)
    predicted <- suppressWarnings(fitModel(inRas = vecDF, 
                                           inputModel = model, 
                                           x = bird, 
                                           tileYear = currentTime))
    if (savePredVectors){
      qs::qsave(x = predicted, file = predictedNameVec)
      predicted <- predictedNameVec
    }
  } else {
    if (savePredVectors){
      message(crayon::blue("Prediction vector exists for ", bird, ". Returning ", predictedNameVec))
      predicted <- predictedNameVec
    } else {
      message(crayon::blue("Prediction already exists for ", bird, ". Returning ", predictedName))
      predicted <- raster(predictedName)
    }
  }
  gc()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}