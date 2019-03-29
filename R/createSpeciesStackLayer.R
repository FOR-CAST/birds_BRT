createSpeciesStackLayer <- function(modelList,
                                    simulatedBiomassMap,
                                    cohortData, # Should also have age
                                    sppEquiv,
                                    sppEquivCol,
                                    staticLayers,
                                    pixelGroupMap,
                                    pathData,
                                    forestOnly){
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
  # Iterate through species and for each species, plot the B in the `pixelGroupMap`
  names(speciesNames) <- speciesLayerNames$modelLayer[
    match(speciesLayerNames$speciesName, speciesNames)]
  
  speciesRasters <- lapply(X = speciesNames, FUN = function(sp){
    subsCohort <- cohortData[speciesCode == sp, ]
    zeroedMap <- raster(pixelGroupMap)
      
    if (NROW(subsCohort) == 0){
      #zeroedMap <- pixelGroupMap
      zeroedMap[] <- getValues(pixelGroupMap)
      vals <- getValues(x = zeroedMap)
      vals[!is.na(vals)] <- 0
      zeroedMap <- setValues(x = zeroedMap, values = vals)
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
      spMapMaskedToForestOnly <- postProcess(x = zeroedMap, rasterToMatch = forestOnly, maskWithRTM = TRUE,
                                             destinationPath = tempdir(), filename2 = NULL)
      return(spMapMaskedToForestOnly)
    }
  })
      
  # Rename biomass
  biomassLayerName <- predictors[grepl(x = predictors, pattern = "Biomass")]
  biomass <- simulatedBiomassMap
  names(biomass) <- biomassLayerName
  
  # Creat age map
  ageLayerName <- predictors[grepl(x = predictors, pattern = "Age")]
  ageMap <- raster(pixelGroupMap)
  valsAge <- data.table(pixelID = 1: ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- valsAge[cohortData[, list(age = max(age)), by = "pixelGroup"], on = "pixelGroup"]
  # newAgeVals <- plyr::join(x = valsAge, cohortData[, max(age), by = "pixelGroup"])
  #names(newAgeVals)[3] <- "age"
  # newAgeVals <- setDT(newAgeVals)[, .SD[1], by = .(pixelID)]
  ageMap[newAgeVals$pixelID] <- newAgeVals$age
  #newAgeMap <- setValues(x = ageMap, values = newAgeVals$age)
  # assign(x = ageLayerName, value = newAgeMap)
  names(ageMap) <- ageLayerName
  
  speciesStack <- raster::stack(speciesRasters) %>%
    raster::stack(biomass) %>%
    raster::stack(ageMap)
  
  # Make sure that species that were not modeled by LandR still have a raster
  layersAvailable <- c(names(staticLayers), names(speciesStack))
  missingLayersNames <- setdiff(predictors, layersAvailable)
  missingLayers <- lapply(X = missingLayersNames, FUN = function(miss){
    zeroedMap <- pixelGroupMap
    vals <- getValues(x = zeroedMap)
    vals[!is.na(vals)] <- 0
    zeroedMap <- setValues(x = zeroedMap, values = vals)
    names(zeroedMap) <- miss
    return(zeroedMap)
  })
  
  finalStk <- raster::stack(missingLayers) %>%
    raster::stack(speciesStack)

  return(finalStk)
}
