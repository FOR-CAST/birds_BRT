corePrediction <- function(bird, successionLayers,
                           uplandsRaster,
                           staticLayers,
                           currentTime,
                           modelList,
                           overwritePredictions,
                           pathData = pathData,
                           studyArea = studyArea,
                           rasterToMatch = rasterToMatch){
  
  successionLayersNames <- names(successionLayers)
  staticLayersNames <- names(staticLayers)
  message(crayon::yellow(paste0("Predicting for ", bird , ". Prediction for time ", currentTime)))
  suppressWarnings(dir.create(file.path(pathData, "predicted")))
  
  models <- modelList[[bird]]
  if ("glmerMod" %in% class(models)){
    nameStackRas1 <- names(models@frame)[2]
    nameStackRas2 <- names(models@frame)[3]
  } else {
    if ("glm" %in% class(models)){
      nameStackRas1 <- names(models$coefficients)[2]
      nameStackRas2 <- names(models$coefficients)[3]
    } else {
      if ("gbm" %in% class(models)){ # If gbm, do everything in here, else, do outside
          tryCatch({
            stkLays <- raster::stack(successionLayers, staticLayers)
          }, error = function(e){
            message("crs and or extents don't align. Trying postProcessing succession layers")
            successionLayers <- lapply(X = seq_len(nlayers(successionLayers)), FUN = function(layer){
              lay <- postProcess(successionLayers[[layer]], studyArea = studyArea, rasterToMatch = staticLayers[[1]],
                                 destinationPath = tempdir(), filename2 = NULL)
              return(lay)
          })
            stkLays <- raster::stack(successionLayers, staticLayers)
            names(stkLays) <- c(successionLayersNames, staticLayersNames)
          })
        predictedName <- file.path(pathData, paste0("predicted/predicted", bird, "Year", currentTime, ".tif"))
        if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
          message(crayon::yellow(paste0(" Starting prediction raster for ", bird, ". This might take some time... [", Sys.time(),"]")))
          startTime <- Sys.time()
          predicted <- gbm::predict.gbm(object = models, newdata = raster::as.data.frame(stkLays, row.names = TRUE),
                                        type = "response",
                                        n.trees = models$n.trees)
          message(crayon::green(paste0("Prediction finalized for ", bird, ". [", Sys.time(),"]. Total time elapsed: ", 
                                       Sys.time() - startTime)))
          startTime <- Sys.time()
          message(crayon::green("Masking ", bird , " prediction to ", crayon::red("studyArea"), " for time ", currentTime))
          basePlot <- stkLays[[1]]
          basePlot <- setValues(basePlot, predicted)
          basePlot <- reproducible::fastMask(basePlot, y = studyArea)
          
          names(basePlot) <- paste0("predicted", bird)
          message(crayon::green("Masking ", bird , " prediction to ", crayon::red("uplands"), " for time ", currentTime))
          predictedMasked <- reproducible::postProcess(x = basePlot, rasterToMatch = uplandsRaster, 
                                                       maskWithRTM = TRUE, destinationPath = pathData, filename2 = NULL)
          raster::writeRaster(x = predictedMasked, filename = predictedName,
                              format = "GTiff", overwrite = TRUE)
          
        }
        predicted <- raster(predictedName)
        gc()
        return(predicted)
      }
    }
  }
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  predictedName <- file.path(pathData, paste0("predicted/predictedFocal", focDis, "m", bird, currentTime, ".tif"))
  if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
    birdD <- raster::raster(birdDensityRasters[[bird]])
    valsD <- log(raster::getValues(birdD)) # log the value of densities so it is the same of the original model
    valsD[valsD < -0.99] <- -1
    birdD <- raster::setValues(birdD, valsD)
    rm(valsD)
    gc()
    if (any(!identical(round(raster::extent(x = disturbanceRas), 10^-100), round(raster::extent(birdD), 10^-100)),
            !identical(raster::res(x = disturbanceRas), raster::res(birdD)),
            !identical(raster::crs(x = disturbanceRas), raster::crs(birdD)))){
      disturbanceRas <- postProcess(x = disturbanceRas, rasterToMatch = birdD,
                                    maskWithRTM = TRUE, 
                                    filename2 = file.path(pathData, "predicted", paste0(disturbanceRas@data@names, "Fixed")),
                                    format = "GTiff", overwrite = TRUE, useCache = FALSE)
    }
    stackRas <- raster::stack(disturbanceRas, birdD) # Might need to remove individual rasters here
    names(stackRas)[1] <- nameStackRas1
    names(stackRas)[2] <- nameStackRas2
    suppressWarnings(predicted <- fitModel(inRas = stackRas, 
                                           inputModel = models, 
                                           x = bird, 
                                           tileYear = currentTime))
    raster::writeRaster(x = predicted, filename = predictedName, 
                        format = "GTiff", overwrite = TRUE)
  }
  predicted <- raster(predictedName)
  gc()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}