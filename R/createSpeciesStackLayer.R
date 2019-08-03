createSpeciesStackLayer <- function(modelList,
                                    simulatedBiomassMap,
                                    urlStaticLayer,
                                    cohortData, # Should also have age
                                    sppEquiv,
                                    sppEquivCol,
                                    staticLayers,
                                    pixelGroupMap,
                                    pathData,
                                    forestOnly,
                                    uplandsRaster,
                                    rasterToMatch,
                                    useOnlyUplandsForPrediction,
                                    useStaticPredictionsForNonForest){
reproducible::Require("data.table")
reproducible::Require("plyr")
reproducible::Require("raster")
  
  message("Biomass data was simulated, using it for prediction")

  # Create layer names based on the model
  predictors <- modelList[[1]]$gbm.call$predictor.names
  speciesNames <- unique(na.omit(sppEquiv[, ..sppEquivCol])[[1]])
  speciesLayerNames <- rbindlist(lapply(X = speciesNames, FUN = function(sp){
    speciesLayerNames <- data.table::data.table(modelLayer = predictors[grepl(sp, predictors)], 
                                                speciesName = sp)
  })
  )
  
  # Here is where birds get masked to upland forested sites only. We should not do that IF LandR is predicting well for 
  # lower biomass sites
if(useOnlyUplandsForPrediction){
  if (!all(unlist(lapply(list(uplandsRaster, forestOnly, rasterToMatch), FUN = is, class2 = "RasterLayer"))))
    stop("At least one of your layers (sim$uplandRaster, sim$forestOnly, sim$rasterToMatch) is NULL. Please debug.")
  forestUplandRTM <- uplandsRaster * forestOnly * rasterToMatch
  if (any(!names(table(forestUplandRTM[], useNA = "ifany")) %in% c("0", "1", NA)))
    forestUplandRTM[forestUplandRTM > 0 | forestUplandRTM < 0] <- 1
  if (any(!names(table(forestUplandRTM[], useNA = "ifany")) %in% c("0", "1", NA)))
    stop("One or more of your rasters (sim$uplandRaster, sim$forestOnly, sim$rasterToMatch)",
         " is not binary even after converting. Please debug.")
    
  pixelGroupMap <- postProcess(x = pixelGroupMap, rasterToMatch = forestUplandRTM, destinationPath = tempdir(),
                               filename2 = NULL, maskWithRTM = TRUE, useCache = FALSE)
}
  # Iterate through species and for each species, plot the B in the `pixelGroupMap`
  names(speciesNames) <- speciesLayerNames$modelLayer[
    match(speciesLayerNames$speciesName, speciesNames)]
  zeroedMap <- raster(pixelGroupMap)
  pixelGroupMapRed <- data.table(pixelGroup = unique(raster::getValues(pixelGroupMap)))
  pixelGroupMapRed <- na.omit(pixelGroupMapRed)
  cohortData <- cohortData[pixelGroupMapRed, on = "pixelGroup"]
  cohortData$pixelGroup[is.na(cohortData$ecoregionGroup)] <- NA
  cohortData <- na.omit(cohortData)
  
  speciesRasters <- lapply(X = speciesNames, FUN = function(sp){
    subsCohort <- cohortData[speciesCode == sp, ]
    zeroedMap[] <- getValues(pixelGroupMap)
    vals <- getValues(x = zeroedMap)
    vals[!is.na(vals)] <- 0
    zeroedMap <- setValues(x = zeroedMap, values = vals)
    
    if (NROW(subsCohort) == 0){
      assign(x = sp, value = zeroedMap)
      names(zeroedMap) <- speciesLayerNames[speciesName == sp, modelLayer]
      return(zeroedMap)
    } else {
      valsCoho <- data.table(pixelID = 1:ncell(pixelGroupMap), 
                             pixelGroup = getValues(x = pixelGroupMap))
      joinOn <- c("speciesCode", "pixelGroup")
      newCohoVals <- valsCoho[subsCohort[, list(sumBiomass = sum(B)), by = joinOn], 
                              on = "pixelGroup"]
      zeroedMap[newCohoVals$pixelID] <- newCohoVals$sumBiomass
      assign(x = sp, value = zeroedMap)
      names(zeroedMap) <- speciesLayerNames[speciesName == sp, modelLayer]
      return(zeroedMap)
    }
  })
  # Rename biomass
  biomassLayerName <- predictors[grepl(x = predictors, pattern = "Biomass")]
  biomass <- simulatedBiomassMap
  if (length(biomassLayerName) ==0){
    biomass <- NULL
  } else {
    names(biomass) <- biomassLayerName
  }
  
  # Creat age map
  ageLayerName <- predictors[grepl(x = predictors, pattern = "Age")]
  ageMap <- raster(pixelGroupMap)
  valsAge <- data.table(pixelID = 1: ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- valsAge[cohortData[, list(age = max(age)), by = "pixelGroup"], on = "pixelGroup"]
  ageMap[newAgeVals$pixelID] <- newAgeVals$age
  names(ageMap) <- ageLayerName
  
  speciesStack <- raster::stack(speciesRasters) %>%
    raster::stack(biomass) %>%
    raster::stack(ageMap)
  
  if (useStaticPredictionsForNonForest){
    if (useOnlyUplandsForPrediction){
      message(paste0("Both useStaticPredictionsForNonForest and useOnlyUplandsForPrediction are TRUE.",
                     "\nBiomass maps from simulation will be masked for only uplands using DU layer and \n",
                     "all NA pixels in the map after that will be forecasted using the original biomass \n",
                     "layers used to fit the model (kNN, Beaudoin et al., 2014)."))
    }
    
    originalSpeciesLayers <- Cache(prepInputStack, url = urlStaticLayer, # This is probably the correct file with non holes ras
                                   alsoExtract = "similar",
                                   destinationPath = pathData, 
                                   studyArea = studyArea,
                                   rasterToMatch = rasterToMatch)
    message("Original species layers contain:")
    message(paste(names(originalSpeciesLayers), sep = '\n'))
    
    #Matching the rasters names that I have, to mask the NA's
    nameStack <- names(speciesStack)
    matchedLays <- data.frame(toMask = seq(1:length(names(speciesStack))), 
                            original = match(names(speciesStack), names(originalSpeciesLayers)))
    matched <- split(matchedLays, seq(nrow(matchedLays)))
    
    speciesStack <- raster::stack(lapply(X = matched, FUN = function(matching){
      if (names(speciesStack[[matching[["toMask"]]]]) != names(originalSpeciesLayers[[matching[["original"]]]]))
        stop("The original species raster and the succession one don't match. Please debug it.") # data check sanity
      originalSpeciesLayers[[matching[["original"]]]]
      speciesStack[[matching[["toMask"]]]]
      valsOriginal <- raster::getValues(originalSpeciesLayers[[matching[["original"]]]])
      valsToMask <- raster::getValues(speciesStack[[matching[["toMask"]]]])
      valsToMask[is.na(valsToMask)] <- valsOriginal[is.na(valsToMask)]
      speciesStack[[matching[["toMask"]]]] <- raster::setValues(x = speciesStack[[matching[["toMask"]]]], 
                                                               values = valsToMask)
      return(speciesStack[[matching[["toMask"]]]])
    })
    )
    gc()
    names(speciesStack) <- nameStack
  }
  
  # Make sure that species that were not modeled by LandR still have a raster 
  # IS THIS THE CORRECT BEHAVIOR? 
  # ZERO? OR SHOULD BE MEAN?
  layersAvailable <- c(names(staticLayers), names(speciesStack))
  missingLayersNames <- setdiff(predictors, layersAvailable)
  if (length(missingLayersNames) ==0) message(crayon::green("No layers missing, proceeding to prediction."))
  if (length(missingLayersNames) !=0){
    message(crayon::yellow(paste0("There are missing layers. Completing prediction stack with zeroed layers for: ")))
    message(crayon::yellow(paste(missingLayersNames, sep = "\n")))
    missingLayers <- lapply(X = missingLayersNames, FUN = function(miss){
      zeroedMap <- pixelGroupMap
      vals <- getValues(x = zeroedMap)
      vals[!is.na(vals)] <- 0
      zeroedMap <- setValues(x = zeroedMap, values = vals)
      names(zeroedMap) <- miss
      return(zeroedMap)
    })
  } else {
    missingLayers <- NULL 
  }
  
  if (!is.null(missingLayers)){
    finalStk <- raster::stack(missingLayers) %>%
      raster::stack(speciesStack)
  } else {
    finalStk <- raster::stack(speciesStack)
  }
gc()
  return(finalStk)
}
