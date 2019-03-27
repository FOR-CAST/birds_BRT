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

    uplandsRaster[] <- uplandsRaster[]
    successionLayers[] <- successionLayers[]
    staticLayers[] <- staticLayers[]
    predictionPerSpecies <-  pemisc::Map2(cl = cl, f = corePrediction, bird = birdSpecies,
                                        MoreArgs = list(
                                          successionLayers = successionLayers,
                                          uplandsRaster = uplandsRaster,
                                          staticLayers = staticLayers,
                                          currentTime = currentTime,
                                          modelList = modelList,
                                          overwritePredictions = overwritePredictions,
                                          pathData = pathData,
                                          studyArea = studyArea,
                                          rasterToMatch = rasterToMatch))
  }
  names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}
