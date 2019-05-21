corePrediction <- function(bird, model, birdDensityRas = NULL,
                           predictedName,
                           disturbanceRas = NULL,
                           successionStaticLayers = NULL,
                           currentTime,
                           pathData,
                           overwritePredictions = FALSE){
  
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
          predicted <- gbm::predict.gbm(object = model, newdata = successionStaticLayers,
                                        type = "response",
                                        n.trees = model$n.trees)
          attr(predicted, "prediction") <- paste0(bird, currentTime)
          message(crayon::green(paste0("Prediction finalized for ", bird, ". [", Sys.time(),"]. Total time elapsed: ", 
                                       Sys.time() - startTime)))
          return(predicted)
        } else {
          return(predictedName)
        }
      }
    }
  }
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  
  if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
    valsD <- log(raster::getValues(birdDensityRas)) # log the value of densities so it is the same of the original model
    valsD[valsD < -0.99] <- -1
    birdDensityRas <- raster::setValues(birdDensityRas, valsD)
    rm(valsD)
    gc()
    tryCatch({
      stkLays <- raster::stack(disturbanceRas, birdDensityRas)
    }, error = function(e){
      tryCatch({
        problem <- if (unique(raster::res(birdDensityRas[[1]])) != unique(raster::res(disturbanceRas))) 
          " different resolution" else if (raster::extent(birdDensityRas[[1]]) != raster::extent(disturbanceRas)) 
            " different extents" else " possibly alignment"
        message("At least one birdDensityRaster can't be stacked with disturbanceRaster. The probably problem is: ", problem)
        warning("The density raster used has originally a resolution of ", raster::res(birdDensityRas[[1]]), 
                "")
        birdDensityRas <- reproducible::postProcess(birdDensityRas, rasterToMatch = disturbanceRas, 
                                                    destinationPath = tempdir(), filename2 = NULL)
        raster::extent(birdDensityRas) <- raster::alignExtent(extent = raster::extent(birdDensityRas),
                                                              object = disturbanceRas,
                                                              snap = "near")
        stkLays <- raster::stack(disturbanceRas, birdDensityRas)
      }, error = function(e){
        stop("Please make sure the disturbanceRaster has the same crs, projection and alignment of the birdDensityRaster")
      })
    })
    names(stkLays)[1] <- nameStackRas1
    names(stkLays)[2] <- nameStackRas2
    predicted <- suppressWarnings(fitModel(inRas = stkLays, 
                                           inputModel = model, 
                                           x = bird, 
                                           tileYear = currentTime))
  } else {
    message("Prediction already exists for ", bird, ". Returning ", predictedName)
    predicted <- raster(predictedName)
  }
  gc()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}