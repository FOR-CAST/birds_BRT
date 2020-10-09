prepareBirdClimateLayers <- function(authEmail = NULL,
                                     pathToAnnualFolders, 
                                     year,
                                     fileResolution = NULL,
                                     RCP = NULL, # 85
                                     climateModel = NULL, # CCSM ~CanESM2~ ==> On 21stNOV19 changed
                                     # to CCSM due to the "squareness" of CanESM2
                                     ensemble = NULL, # r11i1p1
                                     rasterToMatch = NULL,
                                     studyArea = NULL,
                                     datasetSpan = c(2011, 2100),
                                     lengthEnsamble = 7){

  googledrive::drive_auth(email = authEmail)
  # 0. Make sure it has all defaults
  # 
  if (is.null(rasterToMatch)) {
    message("rasterToMatch is NULL, no post processing will happen")
  }
  if (is.null(ensemble)) {
    ensemble <- ""
  }
  if (is.null(RCP)) {
    RCP <- 85
    message("RCP is NULL, using default ", RCP)
  }
  if (is.null(climateModel)) {
    climateModel <- "CCSM4"
    message("climateModel is NULL, using default ", climateModel)
  }
  if (is.null(fileResolution)) {
  fileResolution <- "3ArcMin"
  message(paste0("fileResolution is NULL. Using the original ",fileResolution))
  }
  
  if (!year %in% min(datasetSpan):max(datasetSpan)) {
    message(red(paste0("The year", year,
                       "does not have climate projections. Returning NULL")))
    return(NULL)
  }

  # 1. Check if we have the layer
  fileName <- file.path(pathToAnnualFolders, paste0(paste(climateModel, RCP, ensemble,
                                                 fileResolution, "birds",
                                                 year, sep = "_"), ".grd"))
  if (file.exists(fileName)){
    message(paste0(fileName, " exists. Returning..."))
    return(raster::stack(fileName))
  }
  
  variables <- c("AHM", "bFFP", "CMD", "DD_0", "DD_18",
                 "DD18", "DD5", "eFFP", "EMT", "EXT", "FFP",
                 "MAP", "MAT", "MCMT", "MSP", "MWMT", "NFFD",
                 "PAS", "PPT_sm", "PPT_wt", "SHM", "Tave_sm",
                 "Tave_wt", "TD")

  folders <- grepMulti(list.dirs(pathToAnnualFolders), patterns = "MSY")
  # Identify all the closest 7 years
  if (year == min(datasetSpan)){
    yearsToAverage <- year:(year+(lengthEnsamble-1))
  } else if (year == max(datasetSpan)) {
    yearsToAverage <- (year-(lengthEnsamble-1)):year
  } else {
    yearsToAverage <- (year-((lengthEnsamble-1)/2)):(year+((lengthEnsamble-1)/2))
  }
    # NOTE: Even though we have data from before 2011, its not 
    # in the same format as the future ones, which means is not 
    # readly usable. Sigh. One day I might make it usable. Not now.
  
  ensembleStack <- lapply(yearsToAverage, function(Y){
    currentYearFolder <- grepMulti(x = folders, patterns = Y)
    allFiles <- paste0(tools::file_path_sans_ext(variables), "Year", Y, ".tif")
    allFilesPaths <- file.path(currentYearFolder, allFiles)
    if (!all(file.exists(allFiles))){
      filesToLoad <- file.path(currentYearFolder, paste0(variables, ".asc"))
      # future::plan("multiprocess", workers = length(filesToLoad))
      variablesStack <- stack(lapply(X = filesToLoad, FUN = function(variable){
        tic(paste0("Processing ", basename(tools::file_path_sans_ext(variable)), " for year ", Y))
        ras <- raster(variable)
        crs(ras) <- sp::CRS(paste0('+init=epsg:4326 +proj=longlat +ellps=WGS84 ",
                                  "+datum=WGS84 +no_defs +towgs84=0,0,0'))
        if (any(!is.null(rasterToMatch),
                !is.null(studyArea))){
          ras <- postProcess(x = ras,
                             studyArea = studyArea,
                             destinationPath = currentYearFolder,
                             rasterToMatch = rasterToMatch,
                             format = "GTiff",
                             filename2 = paste0(tools::file_path_sans_ext(variable), 
                                                "Year", Y))
        }
        toc()
        return(ras)
      }))
      # future::plan("sequential")
    } else {
      variablesStack <- stack(lapply(allFilesPaths, raster))
    }
    names(variablesStack) <- paste0(tools::file_path_sans_ext(allFiles))
    return(variablesStack)
  })
  
  # With the ensemble stack, I need to map over the stacks and calculate the average
  organizedEnsemble <- raster::stack(lapply(1:nlayers(ensembleStack[[1]]), function(index){
    lays <- stack(lapply(ensembleStack, `[[`, index))
    layName <- variables[index]
    tic(paste0("Calculating average ", layName, " for year ", year))
    ras <- calc(lays, fun = mean, na.rm = TRUE)
    names(ras) <- layName
    storage.mode(ras[]) <- "integer" # Reducing size of raster by converting it to a real binary
    toc()
    return(ras)
  }))
  
  # [08OCT20 ~ Checked that the model layers below were divided by 10 to fit the models
  #            We need then to divide the prediction layers as well]

  # Fixing the layers for the values that were multiplied by 10 in ClimateNA V6.11
  # The variables to potentially fix are:
  # •	Annual: MAT, MWMT, MCMT, TD, AHM, SHM, EMT, EXT and MAR;
  # •	Seasonal: Tmax, Tmin, Tave and Rad;
  # •	Monthly: Tmax, Tmin, Tave and Rad.'
  organizedEnsemble <- raster::stack(lapply(names(organizedEnsemble), function(lay) {
    if (lay %in% c("MAT", "MWMT", "MCMT", "TD", "AHM", "SHM", "EMT", "EXT", "MAR",
                   paste0("Tave_", c("wt", "sm", "at", "sp")))) {
      message(crayon::red(paste0("ClimateNA 6.11 multiplies ", lay, "by 10 for storage.",
                                 "Backtransforming the layer")))
      organizedEnsemble[[lay]] <- organizedEnsemble[[lay]]/10
      return(organizedEnsemble[[lay]])
    } else {
      return(organizedEnsemble[[lay]])
    }
  }))
  names(organizedEnsemble) <- names(organizedEnsemble)
  message(paste0("Writing average climate layer stack for year ", year))
  writeRaster(organizedEnsemble, 
              filename = fileName)
  
  return(raster::stack(fileName))
}
