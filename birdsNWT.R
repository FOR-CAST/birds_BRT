defineModule(sim, list(
  name = "birdsNWT",
  description = paste0("This module loads a bird model from Stralberg (unpublished)", 
                       "for each species of interest",
                       " for the NWT, as well as static layers. Dynamic layers needed ", 
                       "for prediction come from LandR_Biomass"),
  keywords = c("NWT", "birds"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Diana", "Stralberg", email = "dstralberg@gmail.com", role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", birdsNWT = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdsNWT.Rmd"),
  reqdPkgs = list("googledrive", "data.table", "raster", "gbm", "crayon", "plyr", "dplyr", "tati-micheletti/usefun"),
  parameters = rbind(
    defineParameter("scenario", "character", NA, NA, NA, paste0("Are these predictions from a specific scenario?",
                                                                  " If not, leave it as NA")),
    defineParameter("rastersShowingNA", "logical", FALSE, NA, NA, paste0("Should the raster present NA where wetlands are?",
                                                                       " This is because LandR doesn't predict for wetlands")),
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, paste0("Should it schedule events for the last year",
                                                                       " of simulation if this is not a multiple of interval?")),
    defineParameter("useStaticPredictionsForNonForest", "logical", TRUE, NA, NA, paste0("If TRUE, it will use the original KNN data to fill up the NA's",
                                                                                        " back after if we don't want to leave NA pixels in the ",
                                                                                        "predictions, independently of having the pixelGroupMap ",
                                                                                        "being masked to uplands or not")),
    defineParameter("useOnlyUplandsForPrediction", "logical", TRUE, NA, NA, paste0("Should the bird layers be masked to forest uplands only? masks",
                                                                                   " pixelGroupMap with uplands as quality of DUCKS layer is better",
                                                                                   " than LCC05 to ID the wetlands. We currently have succession ",
                                                                                   "happening in some wetlands because of the low quality of LCC05.",
                                                                                   " This should not be happening. But as the layer is proprietary, ",
                                                                                   "we can't use it in LandR.")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching?"),
    defineParameter("version", "character", "2", NA, NA, "Number of the bird module version to be used"),
    defineParameter("useParallel", "logical", FALSE, NA, NA, "Should bird prediction be parallelized?"),
    defineParameter("useTestSpeciesLayers", "logical", TRUE, NA, NA, "Use testing layers if forest succesion is not available?"),
    defineParameter("predictionInterval", "numeric", 10, NA, NA, "Time between predictions"),
    defineParameter("nCores", "character|numeric", "auto", NA, NA, paste0("If parallelizing, how many cores to use?",
                                                                          " Use 'auto' (90% of available), or numeric")),
    defineParameter(name = "baseLayer", class = "numeric", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?"),
    defineParameter(name = "quickLoad", class = "logical", default = FALSE, min = NA, max = NA, 
                    desc = "Quickly load models?"),
    defineParameter(name = "overwritePredictions", class = "logical", default = FALSE, min = NA, max = NA, 
                    desc = "Should overwrite bird predictions thta might be available?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from final bird layers",
                 sourceURL = NA),
    expectsInput(objectName = "wetlandRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for creating upland raster",
                 sourceURL = NA),
    expectsInput(objectName = "uplandsRaster", objectClass = "RasterLayer",
                 desc = "Upland raster for excluding wetlands and water from bird's predictions. LandR has NOT been tested for wetlands",
                 sourceURL = NA),
    expectsInput(objectName = "birdsList", objectClass = "character", 
                 desc = "Bird species to be predicted", sourceURL = NA),
    expectsInput(objectName = "cloudFolderID", objectClass = "character", 
                 desc = "Folder ID for cloud caching", sourceURL = NA),
    expectsInput(objectName = "urlModels", objectClass = "character", 
                 desc = "Url for the GDrive folder that has all model objects",
                 sourceURL = "https://drive.google.com/open?id=1cpt-AKDbnlUEi6r70Oow2lEPrbzQfVpt"),
    expectsInput(objectName = "urlStaticLayers", objectClass = "RasterLayer", 
                 desc = "Static Layers (WAT, URBAG, lLED25, DEV25 and landform) url", 
                 sourceURL = "https://drive.google.com/open?id=1OzWUtBvVwBPfYiI_L_2S1kj8V6CzB92D"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Study area for the prediction. Currently only available for NWT", 
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it", 
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3"),
    expectsInput(objectName = "forestOnly", objectClass = "RasterLayer",
                 desc = "Raster to match but NA'ed for non-forest pixels", 
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdPrediction", objectClass = "list", 
                  desc = "List per year of the bird species predicted rasters"),
    createsOutput(objectName = "birdModels", objectClass = "list", 
                  desc = "List of the bird models for prediction"),
    createsOutput(objectName = "staticLayers", objectClass = "RasterStack", 
                  desc = paste0("Raster stack of all static layers (WAT, URBAG,", 
                                "lLED25, DEV25 and landform) for the bird models")),
    createsOutput(objectName = "successionLayers", objectClass = "RasterStack", 
                  desc = paste0("Raster stack of all succession layers (species)", 
                                " and total biomass for the bird models"))
  )
))

doEvent.birdsNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadModels")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadFixedLayers")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "predictBirds", eventPriority = .last())
      
    },
    loadModels = {
      sim$birdModels <- loadBirdModels(birdsList = sim$birdsList,
                              folderUrl = extractURL("urlModels"),
                              pathData = dataPath(sim),
                              version = P(sim)$version,
                              cloudFolderID = sim$cloudFolderID, 
                              quickLoad = P(sim)$quickLoad)
      message("Bird models loaded for: \n", paste(sim$birdsList, collapse = "\n"))
    },
    loadFixedLayers = {
      sim$staticLayers <- Cache(loadStaticLayers, fileURL = extractURL("urlStaticLayers"),
                                pathData = dataPath(sim), 
                                studyArea = sim$studyArea,
                                rasterToMatch = sim$rasterToMatch,
                                omitArgs = "pathData")
      message("The following static layers have been loaded: \n", 
              paste(names(sim$staticLayers), collapse = "\n"))
    },
    gettingData = {
      
      Require("magrittr")
      
      if (!is.null(sim$cohortData)){
      mod$cohortData <- sim$cohortData
      } else {
      mod$cohortData <- createModObject(data = "cohortData", sim = sim,
      pathInput = inputPath(sim), currentTime = time(sim))
      }
      
      if (!is.null(sim$pixelGroupMap)){
        mod$pixelGroupMap <- sim$pixelGroupMap
      } else {
        mod$pixelGroupMap <- createModObject(data = "pixelGroupMap", sim = sim, 
                                             pathInput = inputPath(sim), currentTime = time(sim))
      }
      
      if (!is.null(sim$simulatedBiomassMap)){
        mod$simulatedBiomassMap <- sim$simulatedBiomassMap
      } else {
        mod$simulatedBiomassMap <- createModObject(data = "simulatedBiomassMap", sim = sim, 
                                                   pathInput = inputPath(sim), currentTime = time(sim))
      }
      
      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData), is.null(mod$simulatedBiomassMap))) {
        params(sim)$useTestSpeciesLayers <- TRUE
      }

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "birdsNWT", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "birdsNWT", "gettingData")
      }
      
    },
    predictBirds = {
      if (P(sim)$useTestSpeciesLayers == TRUE){
        message("Using test layers for species. Predictions will be static and identical to original data.")
        sim$successionLayers <- Cache(prepInputStack,
                                      url = "https://drive.google.com/open?id=1QiwMJpbQYeBH5ifNZDEXe04bHmn-w4Oc",
                                      pathData = outputPath(sim),
                                      studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch)
      } else {
        if (any(!suppliedElsewhere("simulatedBiomassMap", sim),
                !suppliedElsewhere("cohortData", sim),
                !suppliedElsewhere("pixelGroupMap", sim)))
          if (any(is.null(mod$simulatedBiomassMap), 
                  is.null(mod$pixelGroupMap),
                  is.null(mod$cohortData)))
          stop("useTestSpeciesLayers is FALSE, but apparently no vegetation simulation was run. Check your inputs folder or simulation module.")
        
        sim$successionLayers <- createSpeciesStackLayer(modelList = sim$birdModels,
                                      simulatedBiomassMap = mod$simulatedBiomassMap,
                                      cohortData = mod$cohortData,
                                      staticLayers = sim$staticLayers,
                                      sppEquiv = sim$sppEquiv,
                                      sppEquivCol = sim$sppEquivCol,
                                      pixelGroupMap = mod$pixelGroupMap,
                                      pathData = dataPath(sim),
                                      forestOnly = sim$forestOnly,
                                      uplandsRaster = sim$uplandsRaster,
                                      rasterToMatch = sim$rasterToMatch,
                                      useOnlyUplandsForPrediction = P(sim)$useOnlyUplandsForPrediction,
                                      useStaticPredictionsForNonForest = P(sim)$useStaticPredictionsForNonForest)
      }

      sim$birdPrediction[[paste0("Year", time(sim))]] <- predictDensities(birdSpecies = sim$birdsList,
                                                               successionLayers = sim$successionLayers,
                                                               uplandsRaster = sim$uplandsRaster,
                                                               staticLayers = sim$staticLayers,
                                                               currentTime = time(sim),
                                                               modelList = sim$birdModels,
                                                               pathData = outputPath(sim),
                                                               overwritePredictions = P(sim)$overwritePredictions,
                                                               useParallel = P(sim)$useParallel,
                                                               nCores = P(sim)$nCores,
                                                               studyArea = sim$studyArea,
                                                               rasterToMatch = sim$rasterToMatch,
                                                               waterRaster = sim$waterRaster,
                                                               rastersShowingNA = P(sim)$rastersShowingNA,
                                                               scenario = P(sim)$scenario)

        sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "birdsNWT", "predictBirds")
        if (P(sim)$predictLastYear){
          if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
            sim <- scheduleEvent(sim, end(sim), "birdsNWT", "predictBirds")
        }
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere(object = "birdsList", sim = sim)){
      birdsAvailable <- googledrive::drive_ls(
        path = as_id("https://drive.google.com/open?id=1cpt-AKDbnlUEi6r70Oow2lEPrbzQfVpt"), 
        pattern = paste0("brt2.R"))
      sim$birdsList <- unlist(strsplit(x = birdsAvailable[["name"]], split = paste0("brt", P(sim)$version, ".R")))
      sim$birdsList <- sim$birdsList[-which(grepl(pattern = "CONW", x = sim$birdsList))] # CONW Model has some sort of problem...
      
      if (is.null(sim$birdsList))
        sim$birdsList <- c("REVI", "HETH", "RCKI", "HAFL", "WIWR", "GRCA", "RBNU", "WIWA", 
                         "GRAJ", "RBGR", "WEWP", "GCKI", "PUFI", "WETA", "FOSP", "PISI", 
                         "WCSP", "EVGR", "WBNU", "PIGR", "BTNW", "EAPH", "PHVI", "WAVI", 
                         "BRTH", "EAKI", "BRCR", "PAWA", "VESP", "DEJU", "BRBL", "OVEN", 
                         "VEER", "CSWA", "BOCH", "VATH", "OSFL", "BLPW", "COYE", "TRES")
  }
  if (!suppliedElsewhere("studyArea", sim = sim, where = "sim")){
    if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)
    
    message("No specific study area was provided. Croping to the Edehzhie Indigenous Protected Area (Southern NWT)")
    Edehzhie.url <- "https://drive.google.com/open?id=1klq0nhtFJZv47iZVG8_NwcVebbimP8yT"
    sim$studyArea <- Cache(prepInputs,
                               url = Edehzhie.url,
                               destinationPath = inputPath(sim),
                               omitArgs = c("destinationPath"))
  }
  
  if (!suppliedElsewhere("rasterToMatch", sim = sim, where = "sim")){
  sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df", 
                              studyArea = sim$studyArea,
                              targetFile = "RTM.tif", destinationPath = inputPath(sim),
                              filename2 = NULL,
                              omitArgs = c("destinationPath", "filename2"))
  }
  
  if (!suppliedElsewhere("LCC05", sim)){
    sim$LCC05 <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                      studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch)
  }
  
  if (!suppliedElsewhere("forestOnly", sim = sim, where = "sim")){
    
    forestClasses <- c(1:15, 34:35)
    sim$forestOnly <- sim$rasterToMatch
    sim$forestOnly[!sim$LCC05[] %in% forestClasses] <- NA
    
  }
  
  if (!suppliedElsewhere("uplandsRaster", sim = sim, where = "sim")){
    
    wetlandRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                               studyArea = sim$studyArea, 
                               userTags = "objectName:wetlandRaster")
    
    sim$uplandsRaster <- Cache(classifyWetlands, LCC = P(sim)$baseLayer,
                               wetLayerInput = wetlandRaster,
                               pathData = dataPath(sim),
                               studyArea = sim$studyArea,
                               userTags = c("objectName:wetLCC"))
    uplandVals <- raster::getValues(sim$uplandsRaster) # Uplands = 3, so we should convert 1 an 2 to NA
    uplandVals[uplandVals < 3] <- NA
    uplandVals[uplandVals == 3] <- 1
    sim$uplandsRaster <- raster::setValues(sim$uplandsRaster, uplandVals)
  }
  
  if (extent(sim$uplandsRaster) != extent(sim$studyArea)){
    sim$uplandsRaster <- postProcess(x = sim$uplandsRaster, studyArea = sim$studyArea,
                                     destinationFolder = dataPath(sim), filename2 = NULL)
  }
  if (!suppliedElsewhere("waterRaster", sim)){
    wetlandRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                           studyArea = sim$studyArea, 
                           userTags = "objectName:wetlandRaster")
    sim$waterRaster <- Cache(classifyWetlands, LCC = P(sim)$baseLayer,
                             wetLayerInput = wetlandRaster,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea,
                             userTags = c("objectName:wetLCC"))
    waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[waterVals == 1] <- NA
    waterVals[waterVals > 1] <- 1
    sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)
  }
  
  return(invisible(sim))
}
