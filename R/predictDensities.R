predictDensities <- function(birdSpecies,
                             uplandsRaster,
                             successionLayers,
                             staticLayers,
                             currentTime,
                             modelList,
                             overwritePredictions,
                             pathData,
                             useParallel = FALSE,
                             nCores = 1,
                             studyArea,
                             rasterToMatch) {
  tryCatch({
    stkLays <- raster::stack(successionLayers, staticLayers)
    stkLays[] <- stkLays[]
  }, error = function(e){
    stop("crs and or extents don't align. Check you layers have the same crs and projection before this call")
  })
  predictedName <- as.list(file.path(pathData, paste0("predicted", birdSpecies, "Year", currentTime, ".tif")))
  names(predictedName) <- birdSpecies
  
  allPredictionsExist <- all(unlist(lapply(predictedName, FUN = function(yearSpPrediction){
    fileExists <- file.exists(yearSpPrediction)
    return(fileExists)
  })))
  
  if (allPredictionsExist){
    predictionPerSpecies <- lapply(X = birdSpecies, FUN = function(bird){
      ras <- raster::raster(predictedName[[bird]])
    })
  } else {
  
    if (nCores == "auto") {
      nCores <- pemisc::optimalClusterNum(40000, maxNumClusters = length(birdSpecies))
    }
    if (all(.Platform$OS.type != "windows", isTRUE(useParallel))) {
      cl <- parallel::makeForkCluster(nCores, outfile = file.path(pathData, "logParallelBirdPrediction")) # Tried, works, too slow
      # cl <- parallel::makePSOCKcluster(sim$nCores, outfile = file.path(dataPath(sim), "logParallelBirdPrediction")) # Tried, also works, also slow
      
      on.exit(try(parallel::stopCluster(cl), silent = TRUE))
    } else {
      cl <- NULL
    }
  
    stackVectors <- raster::as.data.frame(stkLays)
    
    if (!is.null(cl)){
      message(crayon::red(paste0("Paralellizing for:\n", paste(birdSpecies, collapse = "\n"),
                                 "\nUsing ", nCores, " cores \n",
                                 "\nMessages will be suppressed until done")))
      predictVec <- clusterApplyLB(seq_along(birdSpecies),
                                   cl = cl, function(index) {
                                     corePrediction(bird = birdSpecies[[index]],
                                                    model = modelList[[index]],
                                                    predictedName = predictedName[[index]],
                                                    successionStaticLayers = stackVectors,
                                                    pathData = pathData,
                                                    currentTime = currentTime)
                                   })
    } else {
      predictVec <- lapply(seq_along(birdSpecies),
                           function(index) {
                             corePrediction(bird = birdSpecies[[index]],
                                            model = modelList[[index]],
                                            predictedName = predictedName[[index]],
                                            successionStaticLayers = stackVectors,
                                            pathData = pathData,
                                            currentTime = currentTime)
                           })
    }
    # Reconvert vectors into rasters
    rm(stackVectors)
    invisible(gc())
    predictionPerSpecies <- lapply(predictVec, FUN = function(spVec){
      bird <- substr(attributes(spVec)[["prediction"]], 1, 4)
      rasName <- paste0("prediction", attributes(spVec)[["prediction"]])
      birdRas <- raster(rasterToMatch) # Using the first as a template. All should be the same.
      birdRas <- raster::setValues(x = birdRas, values = as.numeric(spVec))
      
      # re-Mask study area and for uplands
      message(crayon::green("Masking ", bird ,
                            " prediction to ", crayon::red("uplands"), " for time ", currentTime))
      predictedMasked <- reproducible::postProcess(x = birdRas, rasterToMatch = uplandsRaster, 
                                                   maskWithRTM = TRUE, destinationPath = pathData, filename2 = NULL)
       raster::writeRaster(predictedMasked, 
                    filename = predictedName[[bird]], format = "GTiff")
    
        return(raster(predictedName[[bird]]))
    })
    names(predictionPerSpecies) <- birdSpecies      
    rm(predictVec)
    invisible(gc())
  }
  
  names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}

