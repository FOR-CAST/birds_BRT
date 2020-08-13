defineModule(sim, list(
  name = "birdsNWT",
  description = paste0("This module loads a bird model from Stralberg (unpublished)", 
                       "for each species of interest",
                       " for the NWT, as well as static layers. Dynamic layers needed ", 
                       "for prediction come from LandR_Biomass"),
  keywords = c("NWT", "birds"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", 
                     role = c("aut", "cre")),
              person("Diana", "Stralberg", email = "dstralberg@gmail.com", 
                     role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", birdsNWT = "0.1.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdsNWT.Rmd"),
  reqdPkgs = list("googledrive", "magrittr", "data.table", "raster", "gbm", 
                  "crayon", "plyr", "dplyr", "tati-micheletti/usefulFuns",
                  "future", "future.callr", "future.apply"),
  parameters = rbind(
    defineParameter("scenario", "character", NA, NA, NA, 
                    paste0("Are these predictions from a specific scenario?",
                           " If not, leave it as NA")),
    defineParameter("rastersShowingNA", "logical", FALSE, NA, NA, 
                    paste0(
                      "Should the raster present NA where wetlands are?",
                      " This is because LandR doesn't predict for wetlands"
                    )), 
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, 
                    paste0(
                      "Should it schedule events for the last year",
                      " of simulation if this is not a multiple of interval?"
                    )),
    defineParameter("useStaticPredictionsForNonForest", "logical", TRUE, NA, NA, 
                    paste0(
                      "If TRUE, it will use the original KNN data to fill up the NA's",
                      " back after if we don't want to leave NA pixels in the ",
                      "predictions, independently of having the pixelGroupMap ",
                      "being masked to uplands or not"
                    )),
    defineParameter("useOnlyUplandsForPrediction", "logical", TRUE, NA, NA, 
                    paste0(
                      "Should the bird layers be masked to forest uplands only? masks",
                      " pixelGroupMap with uplands as quality of DUCKS layer is better",
                      " than rstLCC to ID the wetlands. We currently have succession ",
                      "happening in some wetlands because of the low quality of LCC05.",
                      " This should not be happening. But as the layer is proprietary, ",
                      "we can't use it in LandR."
                    )), 
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be run with caching?")),
    defineParameter("version", "character", "6", NA, NA, 
                    paste0("Number of the bird module version to be used")),
    defineParameter("useParallel", "logical", FALSE, NA, NA, 
                    paste0("Should bird prediction be parallelized?")),
    defineParameter("useTestSpeciesLayers", "logical", TRUE, NA, NA, 
                    paste0("Use testing layers if forest succesion is not available?")),
    defineParameter("predictionInterval", "numeric", 10, NA, NA, 
                    paste0("Time between predictions")),
    defineParameter("nCores", "character|numeric", "auto", NA, NA, 
                    paste0("If parallelizing, how many cores to use?",
                           " Use 'auto' (90% of available), or numeric")),
    defineParameter(name = "baseLayer", class = "numeric", 
                    default = 2005, 
                    min = NA, max = NA, 
                    desc = paste0("Which layer should be used? LCC05 or LCC10?")),
    defineParameter(name = "quickLoad", class = "logical", 
                    default = FALSE, 
                    min = NA, max = NA, 
                    desc = paste0("Quickly load models?")),
    defineParameter(name = "overwritePredictions", class = "logical", 
                    default = FALSE, 
                    min = NA, max = NA, 
                    desc = paste0("Should overwrite bird predictions thta might be available?")),
    defineParameter(name = "lowMem", class = "logical", 
                    default = FALSE, 
                    min = NA, max = NA, 
                    desc = paste0("Should the bird predictions return the final ",
                                  "rasters (FALSE) or path to these (TRUE) ")),
    defineParameter(name = "vegetationStatic", class = "logical", 
                    default = FALSE, 
                    min = NA, max = NA, 
                    desc = paste0("Should the bird predictions keep vegetation ",
                                  "Static through time?")),
    defineParameter(name = "climateStatic", class = "logical", 
                    default = FALSE, 
                    min = NA, max = NA, 
                    desc = paste0("Should the bird predictions keep climate layers ",
                                  "Static through time?")),
    defineParameter(name = "RCP", class = "character", 
                    default = "85", 
                    min = NA, max = NA, 
                    desc = paste0("Which RCP should be used? Default to 85")),
    defineParameter(name = "climateModel", class = "character", 
                    default = "CCSM4", 
                    min = NA, max = NA, 
                    desc = paste0("Which climate model should be used? Default to CCSM4")),
    defineParameter(name = "ensemble", class = "character", 
                    default = "", 
                    min = NA, max = NA, 
                    desc = paste0("Which ensemble model should be used? Default to ''. ",
                                  "CCSM4 doesn't have ensemble, just CanESM2 (r11i1p1)")),
    defineParameter(name = "climateResolution", class = "character", 
                    default = "3ArcMin", 
                    min = NA, max = NA, 
                    desc = paste0("Which DEM resolution was used for generating the ",
                                  "climate layers? Default to '3ArcMin'.")),
    defineParameter(name = "climateFilePath", class = "character", 
                    default = "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                    min = NA, max = NA, 
                    desc = paste0("URL to zipped climate file coming from ClimateNA, ",
                                  "containing all climate variables for all years of simulation"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "usrEmail", objectClass = "character",
                 desc = "User's e.mail to automatic authentication of GoogleDrive",
                 sourceURL = NA),
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from final bird layers. Water == 1",
                 sourceURL = NA),
    expectsInput(objectName = "wetlandRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for creating upland raster. wetlands == 1",
                 sourceURL = NA),
    expectsInput(objectName = "uplandsRaster", objectClass = "RasterLayer",
                 desc = paste0("Upland raster for excluding wetlands and water from bird's ",
                               "predictions. LandR has NOT been tested for wetlands. Uplands == 1"),
                 sourceURL = NA),
    expectsInput(objectName = "birdsList", objectClass = "character", 
                 desc = "Bird species to be predicted", sourceURL = NA),
    expectsInput(objectName = "cloudFolderID", objectClass = "character", 
                 desc = "Folder ID for cloud caching", sourceURL = NA),
    expectsInput(objectName = "urlModels", objectClass = "character", 
                 desc = "Url for the GDrive folder that has all model objects",
                 sourceURL = "https://drive.google.com/open?id=1DD2lfSsVEOfHoob3fKaTvqOjwVG0ZByQ"),
    # V2 Bird Models (old): "https://drive.google.com/open?id=1cpt-AKDbnlUEi6r70Oow2lEPrbzQfVpt"
    # V3 Bird Models (old): "https://drive.google.com/open?id=19Ys5vHj6L_jyfrZdbUb6qpKyEfDfosQ9"
    # V4 Bird Models (Veg+Terrain): "https://drive.google.com/open?id=17RhA0KkmAJPpf4qss65I0F1wC77XmhzE"
    # V5 Bird Models: (Clim+Terrain)"https://drive.google.com/open?id=1HLcPg2SCtembYvKFTAXl1M2cj7hYPshg"
    # V6 Bird Models (Veg+Clim+Terrain): "https://drive.google.com/open?id=1DD2lfSsVEOfHoob3fKaTvqOjwVG0ZByQ"
    # V8 Bird Models (Veg+Clim+Terrain+Landscape): "https://drive.google.com/drive/folders/1AoScxKtKrVbStk9LldXGGjna9f9iBbfd?usp=sharing"
    expectsInput(objectName = "urlStaticLayers", objectClass = "RasterLayer", 
                 desc = "Static Layers (WET, VRUG, WAT, URBAG, lLED25, DEV25 and landform) url", 
                 sourceURL = "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"),
    # V2 Static layers: "https://drive.google.com/open?id=1OzWUtBvVwBPfYiI_L_2S1kj8V6CzB92D"
         # "Static Layers (WAT, URBAG, lLED25, DEV25 and landform) url"
    # V3 Static layers: "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"
    # V4 Static layers: "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"
    # V5 Static layers: "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"
    # V6 Static layers: "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Study area for the prediction. Currently only available for NWT", 
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it", 
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3"),
    expectsInput(objectName = "forestOnly", objectClass = "RasterLayer",
                 desc = "Raster to match but NA'ed for non-forest pixels", 
                 sourceURL = NA),
    expectsInput(objectName = "climateLayersBirds", objectClass = "list",  
                 desc = paste0("List of raster stacks of climate variables for birds such as: 
                 AHM, bFFP, CMD, DD_0, DD_18, DD18, DD5, eFFP,
                        EMT, EXT, FFP, MAP, MAT, MCMT, MSP, MWMT, NFFD,
                        PAS, PPT_sm, PPT_wt, SHM, Tave_sm, Tave_wt, TD"),
                 sourceURL = NA),
    expectsInput(objectName = "climateDataFolder", objectClass = "character",
                 desc = paste0("Folder where to look for the climate data. ",
                               "If not provided, set as inputPath(sim)"), 
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
                                " and total biomass for the bird models")),
    createsOutput(objectName = "unavailableModels", objectClass = "character", 
                  desc = "Character vector with all missing models")
  )
))

doEvent.birdsNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      #Make sure we only have one bird model for each species. Data sanity check
      sim$birdsList <- unique(sim$birdsList)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadModels")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadFixedLayers")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "predictBirds", eventPriority = 9)
      
    },
    loadModels = {
      sim$birdModels <- loadBirdModels(birdsList = sim$birdsList,
                              folderUrl = sim$urlModels,
                              pathData = dataPath(sim),
                              version = P(sim)$version,
                              cloudFolderID = sim$cloudFolderID, 
                              quickLoad = P(sim)$quickLoad)
      missingBirds <- setdiff(sim$birdsList, names(sim$birdModels))
      if (length(missingBirds) != 0)
        message(crayon::yellow("Models for the following are not available: ", 
                               paste(missingBirds, collapse = ", ")))
      sim$birdsList <- names(sim$birdModels)
      message("Bird models loaded for: ", paste(sim$birdsList, collapse = ", "))
      sim$unavailableModels <- c(sim$unavailableModels, missingBirds)
    },
    loadFixedLayers = {
      sim$staticLayers <- Cache(loadStaticLayers, fileURL = sim$urlStaticLayers,
                                pathData = dataPath(sim),
                                studyArea = sim$studyArea,
                                rasterToMatch = sim$rasterToMatch,
                                omitArgs = c("pathData", "useCache"))

      message("The following static layers have been loaded: \n", 
              paste(names(sim$staticLayers), collapse = "\n"))
    },
    gettingData = {

        if (P(sim)$vegetationStatic){
          timeVegetation <- start(sim)
          message(crayon::red("vegetationStatic is TRUE. Vegetation layers will be kept Static"))
        } else {
          timeVegetation <- time(sim)
        }
        
      if (!is.null(sim$cohortData)){
      mod$cohortData <- sim$cohortData
      } else {
      mod$cohortData <- createModObject(data = "cohortData", sim = sim,
      pathInput = inputPath(sim), currentTime = timeVegetation)
      }
      
      if (!is.null(sim$pixelGroupMap)){
        mod$pixelGroupMap <- sim$pixelGroupMap
      } else {
        mod$pixelGroupMap <- createModObject(data = "pixelGroupMap", sim = sim, 
                                             pathInput = inputPath(sim), currentTime = timeVegetation)
      }
      
      if (!is.null(sim$simulatedBiomassMap)){
        mod$simulatedBiomassMap <- sim$simulatedBiomassMap
      } else {
        mod$simulatedBiomassMap <- createModObject(data = "simulatedBiomassMap", sim = sim, 
                                                   pathInput = inputPath(sim), currentTime = timeVegetation)
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
                                                        pixelsWithDataAtInitialization = sim$pixelsWithDataAtInitialization,
                                                        urlStaticLayer = sim$urlStaticLayers,
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
      if (P(sim)$version %in% c("5", "6")) {
        if (P(sim)$climateStatic){
          timeClimate <- start(sim)
          message(crayon::red("climateStatic is TRUE. Climate layers will be kept Static"))
        } else {
          timeClimate <- time(sim)
        }

        sim$climateLayersBirds <- usefulFuns::prepareClimateLayers(authEmail = usrEmail,
                                                               pathInputs = sim$climateDataFolder, 
                                                               studyArea = sim$studyArea,
                                                               rasterToMatch = sim$rasterToMatch, years = timeClimate,
                                                               RCP = P(sim)$RCP,
                                                               climateModel = P(sim)$climateModel,
                                                               ensemble = P(sim)$ensemble, 
                                                               climateFilePath = P(sim)$climateFilePath,
                                                               fileResolution = P(sim)$climateResolution,
                                                               variables = "birdsModel", model = "birds")
        if (P(sim)$climateStatic)
          names(sim$climateLayersBirds) <- paste0("year", time(sim))
        tryCatch({ 
          #TODO THIS NEEDS TO BE IMPLEMENTED INSIDE THE  prepareClimateLayers function [ FIX ]
          sim$successionLayers <- raster::stack(sim$successionLayers, sim$climateLayersBirds[[paste0("year", time(sim))]])
        }, error = function(e){
          message(red(paste0("sim$successionLayers and sim$climateLayersBirds do not align for year ", time(sim),
                             ". Trying a postProcessing...")))
          climateLayersBirds <- raster::stack(lapply(names(sim$climateLayersBirds[[paste0("year", time(sim))]]), function(lay){
            print(paste0("Layer: ", lay))
            ras <- sim$climateLayersBirds[[paste0("year", time(sim))]][[lay]]
            ras <- postProcess(x = ras, 
                               destinationPath = sim$climateDataFolder,
                               rasterToMatch = sim$rasterToMatch, 
                               filename2 = NULL)
            return(ras)
          }))
          sim$climateLayersBirds[[paste0("year", time(sim))]] <- climateLayersBirds
          sim$successionLayers <- raster::stack(sim$successionLayers, sim$climateLayersBirds[[paste0("year", time(sim))]])
          message(green(paste0("postProcessing was successful!")))
        })

      }
      t1 <- Sys.time()
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
                                                               scenario = P(sim)$scenario,
                                                               # memUsedByEachProcess = ifelse(
                                                               #   P(sim)$version %in% c("5", "6"),
                                                               #   150000,
                                                               #    31000),
                                                               lowMem = P(sim)$lowMem)
      print(Sys.time()-t1)
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
  if (!suppliedElsewhere("urlModels", sim)){
    if (P(sim)$version == "2"){
      sim$urlModels <- "https://drive.google.com/open?id=1cpt-AKDbnlUEi6r70Oow2lEPrbzQfVpt"
    } else {
      if (P(sim)$version == "3"){
        sim$urlModels <- "https://drive.google.com/open?id=19Ys5vHj6L_jyfrZdbUb6qpKyEfDfosQ9"
      } else {
        if (P(sim)$version == "4"){
          sim$urlModels <- "https://drive.google.com/open?id=17RhA0KkmAJPpf4qss65I0F1wC77XmhzE"
        } else {
          if (P(sim)$version == "5"){
            sim$urlModels <- "https://drive.google.com/open?id=1HLcPg2SCtembYvKFTAXl1M2cj7hYPshg"
          } else {
            if (P(sim)$version == "6") {
              sim$urlModels <- "https://drive.google.com/open?id=1DD2lfSsVEOfHoob3fKaTvqOjwVG0ZByQ"
            }
          }
        }
      }
    }
  }
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere(object = "birdsList", sim = sim)){
      birdsAvailable <- googledrive::drive_ls(
        path = as_id(sim$urlModels), 
        pattern = paste0("brt", P(sim)$version, ".R"))
      sim$birdsList <- usefulFuns::substrBoth(strng = birdsAvailable[["name"]], howManyCharacters = 4, fromEnd = FALSE)
      # sim$birdsList <- sim$birdsList[-which(grepl(pattern = "CONW", x = sim$birdsList))] # CONW Model has some sort of problem in V3; Check V6!
      if (all(is.null(sim$birdsList)))
        stop("There are no bird models in the google drive link folder. 
             This is ok if you are passing the models, but in this case, 
             you also need to provide a species list for the provided models (birdsList)")
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
  
  if (!suppliedElsewhere("rstLCC", sim)){
    sim$rstLCC <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                      studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch)
  }
  
  if (!suppliedElsewhere("forestOnly", sim = sim, where = "sim")){
    
    forestClasses <- c(1:15, 34:35)
    sim$forestOnly <- sim$rasterToMatch
    sim$forestOnly[!sim$rstLCC[] %in% forestClasses] <- NA
    
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
    sim$waterRaster <- Cache(usefulFuns::classifyWetlands, LCC = P(sim)$baseLayer,
                             wetLayerInput = wetlandRaster,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea,
                             rasterToMatch = sim$rasterToMatch,
                             userTags = c("objectName:wetLCC"))
    waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[!is.na(waterVals) & waterVals != 1] <- 0
    sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)
  }
  if (!suppliedElsewhere("urlStaticLayers", sim)){
    if (P(sim)$version == "2"){ # Static Layers: WAT, URBAG, lLED25, DEV25 and landform
      sim$urlStaticLayers <- "https://drive.google.com/open?id=1OzWUtBvVwBPfYiI_L_2S1kj8V6CzB92D"
    } else {
      if (P(sim)$version %in% c("3", "4", "5", "6")){ # Static Layers: WET, VRUG, WAT, URBAG, lLED25, DEV25 and landform
        sim$urlStaticLayers <- "https://drive.google.com/open?id=1U3ygGav1vrqaynkP6hD_hd0Wk7LtZt4T"
      }
    }
  }
    if (!P(sim)$version %in% c("5", "6")){
      sim$climateLayersBirds <-  NULL # Layers not needed for models 2-4
    }
  if (!suppliedElsewhere("usrEmail", sim)){
    sim$usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
  }

  sim$unavailableModels <- NULL # For potentially missing modules in comparison to birds list
  
  if (!suppliedElsewhere("pixelsWithDataAtInitialization", sim)){
    sim$pixelsWithDataAtInitialization <- NULL
    message(crayon::red("pixelsWithDataAtInitialization was not provided. Predictions will be missing pixels where total biomass is 0, these will be NA"))
  }
  
  if (!suppliedElsewhere("climateDataFolder", sim)){
    sim$climateDataFolder <- inputPath(sim)
    message(crayon::red("climateDataFolder was not provided. Using inputPath"))
  }
  
  return(invisible(sim))
}
