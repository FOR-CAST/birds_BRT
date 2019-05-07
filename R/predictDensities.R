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

  if (useParallel == FALSE){
    
    predictionPerSpecies <- lapply(X = birdSpecies, FUN = corePrediction,
                                    uplandsRaster = uplandsRaster,
                                    successionLayers = successionLayers,
                                    staticLayers = staticLayers,
                                    currentTime = currentTime,
                                    modelList = modelList,
                                    overwritePredictions = overwritePredictions,
                                    pathData = pathData,
                                   studyArea = studyArea,
                                   rasterToMatch = rasterToMatch)
    
  } else {
    if (nCores == "auto") {
      nCores <- pemisc::optimalClusterNum(45000, maxNumClusters = length(birdSpecies))
    }
    if (.Platform$OS.type != "windows") {
      cl <- parallel::makeForkCluster(nCores, outfile = file.path(pathData, "logParallelBirdPrediction")) # Not working. DT?
      on.exit(try(parallel::stopCluster(cl), silent = TRUE))
    } else {
      cl <- NULL
    }
    if (!is.null(cl))
      message(crayon::red(paste0("Paralellizing for:\n", paste(birdSpecies, collapse = "\n"),
                                 "\nUsing ", nCores, " cores \n",
                                 "\nMessages will be suppressed until done")))
    browser() 
    uplandsRaster[] <- uplandsRaster[]
    successionLayers[] <- successionLayers[]
    staticLayers[] <- staticLayers[]
    rasterToMatch[] <- rasterToMatch[]

    # I should bring everything out of this function and really only pass birdSpecues, modelList and all the layers as a DF. 
    # NO WRITING!! Bring it all back out and lapply through for writing the layers.
    # After corePrediction (that should really just be the prediction function), rebuild the rasters and write each one at a time, 
    # returning the paths
    predictionPerSpecies <-  pemisc::Map2(cl = cl, f = corePrediction, bird = birdSpecies, modelList = modelList,
                                        MoreArgs = list(
                                          successionLayers = successionLayers,
                                          uplandsRaster = uplandsRaster,
                                          staticLayers = staticLayers,
                                          currentTime = currentTime,
                                          overwritePredictions = overwritePredictions,
                                          pathData = pathData,
                                          studyArea = studyArea))
  }
  names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}
