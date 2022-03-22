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
                             rasterToMatch,
                             waterRaster,
                             rastersShowingNA,
                             scenario,
                             # memUsedByEachProcess = 31000,
                             lowMem = FALSE) {

  tryCatch({
    stkLays <- raster::stack(successionLayers, staticLayers)
    namesLays <- names(stkLays)
    stkLays <- raster::stack(lapply(namesLays, function(lay){
      message(crayon::blue("Bringing layer ", lay, " to memory..."))
      stkLays[[lay]][] <- stkLays[[lay]][]
      return(stkLays[[lay]])
    }))
    names(stkLays) <- namesLays
    message(crayon::green("All layers in memory"))

  }, error = function(e){
    stop("crs and or extents don't align. Check you layers have the same crs and projection before this call")
  })

  predictedName <- as.list(file.path(pathData, paste0(scenario, "_predicted_", birdSpecies, "_Year", currentTime, ".tif")))
  names(predictedName) <- birdSpecies
  message(crayon::yellow("Checking if predictions exist"))
  allPredictionsExist <- all(unlist(lapply(predictedName, FUN = function(yearSpPrediction){
    fileExists <- file.exists(yearSpPrediction)
    return(fileExists)
  })))

  if (allPredictionsExist){
    message(crayon::green("All predictions exist. Returning existing predictions"))
    predictionPerSpecies <- lapply(X = birdSpecies, FUN = function(bird){
      if (lowMem){
        return(predictedName[[bird]])
      } else {
        return(raster(predictedName[[bird]]))
      }
    })
    whichDontExist <- birdSpecies
  } else {
    # Which rasters we still don't have

    whichDontExist <- unlist(lapply(names(predictedName), FUN = function(yearSpPrediction){
      fileExists <- file.exists(predictedName[[yearSpPrediction]])
      if(fileExists){
        return(NA)
      } else {
        return(yearSpPrediction)
      }
    }))

    whichDontExist <- whichDontExist[!is.na(whichDontExist)]
    if (!is.null(whichDontExist))
      message(crayon::yellow(paste0("Rasters not found for ", length(whichDontExist)," birds: ",
                                    paste(whichDontExist, collapse = ", "),". Starting predictions...")))

    stackVectors <- data.table(getValues(stkLays))

    # localCores <- FALSE
    if (any(all(nCores == "auto", length(whichDontExist) > 1),
            all(is.numeric(nCores), nCores > 1))) {
      # if nCores is numeric or auto: local parallel
      # if (nCores == "auto") { # NOT FUNCTIONAL --> Not passing it to the future call
      #   nCores <- pemisc::optimalClusterNum(memUsedByEachProcess,
      #                                       maxNumClusters = length(birdSpecies))
      # }
      useParallel <- TRUE
      localParallel <- TRUE
      message("Cores = ", nCores, "; ",
              length(whichDontExist), " species to run for. Using localParallel")
      # nCores <- rep("localhost", nCores)
    } else { # If nCores == 1, no parallel
      if (any(length(whichDontExist) == 1,
              all(is.numeric(nCores), nCores == 1))){
        useParallel <- FALSE
        localParallel <- FALSE
        message("Cores = ", nCores, "; ", length(whichDontExist),
                " species to run for. Not using parallel")
      } else { # If nCores specifies the workers: parallel across machines
        useParallel <- TRUE
        localParallel <- FALSE
        message("Cores = ", nCores, "; ", length(whichDontExist),
                " species to run for. Using across machine's parallel")
      }
    }

    if (useParallel){
      if (localParallel){

# options(future.globals.onReference = "error") # Try to debug what is going on

        if (Sys.getenv("RSTUDIO") != 1) {
          if (packageVersion("pemisc") < "0.0.3.9004") {
            nCoresNeeded <- length(whichDontExist)
            nCoresAvail <- min(nCoresNeeded, 120) ## R cannot exceed 125 connections; use fewer to be safe
            nBatches <- ceiling(nCoresNeeded / nCoresAvail)
            nCores2Use <- ceiling(nCoresNeeded / nBatches)
            if (all(nCores2Use > nCoresAvail, nCores2Use/2 < nCoresAvail))
              nCores2Use <- nCores2Use/2
          } else {
            nCores2Use <- optimalClusterNumGeneralized(6000, 115, 96) ## TODO: adjust RAM req.
          }
          plan("multicore", workers = nCores2Use)
          # plan("multisession", workers = nCores2Use) # Extremely slow and high RAM usage! Not usable!
        } else {
          plan("sequential", workers = 1)
        }

        message(crayon::red(paste0("Paralellizing for ", nCores2Use," species for year ", currentTime,": ",
                                   crayon::white(paste0(paste(whichDontExist, collapse = "; ")),
                                                 "(", length(whichDontExist)," species)"),
                                   " Using future package with plan ",
                                   paste0(attributes(plan())[["class"]][2]),
                                   " Messages will be suppressed until done")))

        t1 <- Sys.time()
        predictVec <- future_lapply(whichDontExist,
                                    function(index) {
                                      corePrediction(bird = index,
                                                     model = modelList[[index]],
                                                     predictedName = predictedName[[index]],
                                                     pathData = pathData,
                                                     currentTime = currentTime,
                                                     successionStaticLayers = stackVectors)
                                    })
        plan("sequential")
        t2 <- Sys.time()
        print(t2-t1)

      } else {
        # NOT WORKING!
        # For across machines, we need reverse tunnels
        revtunnel <- if (all(nCores == "localhost")) FALSE else TRUE

        # Making a 1 core per machine cluster to pass the libraries/objects we need
        st <- system.time(cl <- future::makeClusterPSOCK(unique(nCores),
                                                         revtunnel = revtunnel))
        message("Starting ", paste(paste(names(table(nCores))),
                                   "x", table(nCores), collapse = ", "), " clusters")
        logPath <- pathData
        logPath <- file.path(logPath, paste0("birdPrediction_log",
                                             Sys.getpid()))
        message(crayon::blurred(paste0("Starting parallel prediction for ",
                                       "boreal birds. Log: ", logPath)))

        clusterExport(cl, list("logPath"), envir = environment())
        parallel::clusterEvalQ(cl, {
          reproducible::checkPath(dirname(logPath), create = TRUE)
          print(paste0(logPath, " created in ", Sys.getpid()))
        })
        stopCluster(cl)

        # ~~~~~~~~~~~~~~~ Passing the objects and loading libraries
        browser()
        st <- system.time(cl <- future::makeClusterPSOCK(workers = nCores,
                                                         revtunnel = revtunnel,
                                                         outfile = logPath))

        on.exit(stopCluster(cl))
        message("it took ", round(st[3], 2), "s to start ",
                paste(paste(names(table(nCores))), "x", table(nCores), collapse = ", "),
                " threads")

        objsNeeded <- list("pathData",
                           "currentTime",
                           "predictedName") # "stackVectors", "modelList"

        clusterExport(cl, objsNeeded, envir = environment())

        parallel::clusterEvalQ(cl, {
          for (i in c("gbm",
                      "crayon"
          ))
            library(i, character.only = TRUE)
          print(paste0("Libraries loaded in PID: ",
                       Sys.getpid()))
        })

        # ~~~~~~~~~~~~~~~~~ Starting predictions
        plan(cluster, workers = nCores) # Doesn't work!!
        browser()
        # TEST
        fun <- function(x){
          paste0("This is cluster ", Sys.getpid(), ". X is ", x)
          return(x^2)
        }

        ret <- future_lapply(X = 1:90, FUN = fun)

      }
    } else {
      t1 <- Sys.time()
      predictVec <- lapply(whichDontExist,
                           function(index) {
                             corePrediction(bird = index,
                                            model = modelList[[index]],
                                            predictedName = predictedName[[index]],
                                            successionStaticLayers = stackVectors,
                                            pathData = pathData,
                                            currentTime = currentTime)
                             # The returned prediction is in density! So for abundance need to * 6.25
                           })
      t2 <- Sys.time()
      print(t2-t1)
    }
    # Reconvert vectors into rasters
    rm(stackVectors)
    invisible(gc())
    predictionPerSpecies <- lapply(seq_along(predictVec), FUN = function(spVecIndex){
      #spVecIndex is the index so I can convert to NA in the big vector list for memory
      if (class(predictVec[[spVecIndex]]) == "numeric"){
        bird <- substr(attributes(predictVec[[spVecIndex]])[["prediction"]], 1, 4)
        rasName <- paste0("prediction", attributes(predictVec[[spVecIndex]])[["prediction"]])
        birdRas <- raster(rasterToMatch) # Using the first as a template. All should be the same.
        birdRas <- raster::setValues(x = birdRas, values = as.numeric(predictVec[[spVecIndex]]))
        predictVec[[spVecIndex]] <- NA # Remove it, should lower memory usage
        gc()

        # re-Mask study area and/or for uplands
        if (rastersShowingNA){
          uplandsRaster <- Cache(convertToNA, ras = uplandsRaster,
                                 userTags = "goal:uplandsRasterFromOneToNA")
          uplandsPostProcessed <- reproducible::postProcess(x = uplandsRaster,
                                                            rasterToMatch = rasterToMatch,
                                                            maskWithRTM = TRUE,
                                                            destinationPath = pathData,
                                                            filename2 = NULL)
          message(crayon::green("Masking ", bird ,
                                " prediction to ", crayon::red("uplands"), " for time ", currentTime))
          predictedMasked <- reproducible::postProcess(x = birdRas,
                                                       rasterToMatch = uplandsPostProcessed,
                                                       maskWithRTM = TRUE,
                                                       destinationPath = pathData,
                                                       filename2 = NULL)
        } else {
          waterRaster <- Cache(convertToNA, ras = waterRaster,
                               userTags = "goal:waterRasterFromOneToNA")
          waterPostProcessed <- reproducible::postProcess(x = waterRaster,
                                                          rasterToMatch = rasterToMatch,
                                                          maskWithRTM = TRUE,
                                                          destinationPath = pathData,
                                                          filename2 = NULL)
          message(crayon::green("Masking ", bird ,
                                " prediction to ", crayon::red("water"), " for time ", currentTime))
          predictedMasked <- reproducible::postProcess(x = birdRas,
                                                       rasterToMatch = waterPostProcessed,
                                                       maskWithRTM = TRUE,
                                                       destinationPath = pathData,
                                                       filename2 = NULL)
        }

        raster::writeRaster(predictedMasked,
                            filename = predictedName[[bird]], format = "GTiff", overwrite = TRUE)
        rm(predictedMasked)
      } else {
        browser()
        # birdFull <- strsplit(spVecIndex, "Year") # Was erroring. Maybe will fail with other models?!
        birdFull <- strsplit(predictVec[[spVecIndex]], "_Year")
        bird <- usefulFuns::substrBoth(strng = birdFull[[1]][1], howManyCharacters = 4, fromEnd = TRUE)
      }
      if (lowMem){
        return(predictedName[[bird]])
      } else {
        return(raster(predictedName[[bird]]))
      }
    })
    names(predictionPerSpecies) <- whichDontExist
    rm(predictVec)
    invisible(gc())
  }
  message(crayon::green(paste0("Predictions finalized for ", currentTime, " for ",
                               paste(whichDontExist, collapse = "; "))))
  names(predictionPerSpecies) <- whichDontExist
  return(predictionPerSpecies)
}

