createSpeciesStackLayer <- function(modelList,
                                    simulatedBiomassMap,
                                    cohortData, # Should also have age
                                    sppEquiv,
                                    sppEquivCol,
                                    staticLayers,
                                    pixelGroupMap,
                                    pathData,
                                    forestOnly,
                                    uplandsRaster,
                                    rasterToMatch){
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
  if (!all(unlist(lapply(list(uplandsRaster, forestOnly, rasterToMatch), FUN = is, class2 = "RasterLayer"))))
    stop("At least one of your layers (sim$uplandRaster, sim$forestOnly, sim$rasterToMatch) is NULL. Please debug.")
  forestUplandRTM <- uplandsRaster * forestOnly * rasterToMatch
  if (!names(table(forestUplandRTM, useNA = TRUE)) %in% c("0", "1", NA))
    forestUplandRTM[forestUplandRTM > 0 | forestUplandRTM < 0] <- 1
  if (!names(table(forestUplandRTM, useNA = TRUE)) %in% c("0", "1", NA))
    stop("One or more of your rasters (sim$uplandRaster, sim$forestOnly, sim$rasterToMatch)",
         " is not binary even after converting. Please debug.")
    
  pixelGroupMap <- postProcess(x = pixelGroupMap, rasterToMatch = forestUplandRTM, destinationPath = tempdir(),
                               filename2 = NULL, maskWithRTM = TRUE)
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
  
  # Make sure that species that were not modeled by LandR still have a raster
  layersAvailable <- c(names(staticLayers), names(speciesStack))
  missingLayersNames <- setdiff(predictors, layersAvailable)
  if (length(missingLayersNames) !=0){
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

  return(finalStk)
}
