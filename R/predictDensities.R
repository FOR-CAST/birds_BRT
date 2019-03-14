predictDensities <- function(birdSpecies = sim$birdsList,
                             uplandsRaster = sim$uplandsRaster,
                             successionLayers = sim$successionLayers,
                             staticLayers = sim$staticLayers,
                             currentTime = time(sim),
                             modelList = sim$birdModels,
                             overwritePredictions = P(sim)$overwritePredictions,
                             pathData = dataPath(sim),
                             useParallel = FALSE,
                             nCores = 1,
                             studyArea = sim$studyArea,
                             rasterToMatch = sim$rasterToMatch) {

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
      nCores <- min(data.table::getDTthreads()*0.9, length(birdSpecies), 20) # The birds' prediction takes about 45Gb of RAM. I shoud not use more than 20 cores or <puff>
    }
    if (.Platform$OS.type != "windows") {
      browser() # Check the arguments for DT
      cl <- parallel::makeForkCluster(nCores, outfile = file.path(pathData, "logParallelBirdPrediction")) # Not working. DT?
      # cl <- parallel::makeCluster(spec = "", nnodes = nCores, outfile = file.path(pathData, "logParallelBirdPrediction")) # TESTING THIS ONE NOW. IF DOESN"T WORK, TEST PEMISC
      # cl <- pemisc::makeOptimalCluster(MBper = 8000, maxNumClusters = nCores, outfile = "logParallelBirdPrediction")

      on.exit(try(parallel::stopCluster(cl), silent = TRUE))
    } else {
      cl <- NULL
    }
    if (!is.null(cl))
      message(crayon::red(paste0("Paralellizing for:\n", paste(birdSpecies, collapse = "\n"),
                                 "\nUsing ", nCores, " cores \n",
                                 "\nMessages will be suppressed until done")))

    predictionPerSpecies <-  #parallel::clusterApplyLB(cl = cl, x = birdSpecies, fun = corePrediction, # 
                              pemisc::Map2(cl = cl, f = corePrediction, birdSpecies, # didn't work. got stuck
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
