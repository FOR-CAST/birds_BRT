prepareBirdClimateLayers <- function(authEmail = NULL,
                                     pathToAnnualFolders,
                                     pathToZipClimateFiles = NULL,
                                     year,
                                     modelVersion = "8",
                                     fileResolution = NULL,
                                     RCP = NULL, # 85
                                     climateModel = NULL, # CCSM ~CanESM2~ ==> On 21stNOV19 changed
                                     # to CCSM due to the "squareness" of CanESM2
                                     ensemble = NULL, # r11i1p1
                                     rasterToMatch = NULL,
                                     studyArea = NULL,
                                     datasetSpan = c(2011, 2100),
                                     lengthEnsamble = 7,
                                     studyAreaLongName) {

  googledrive::drive_auth(email = authEmail)
  # 0. Make sure it has all defaults
  #
  if (is.null(rasterToMatch)) {
    message("rasterToMatch is NULL, no post processing will happen")
  }
  if (is.null(ensemble)) {
    message(paste0("ensemble is NULL"))
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
  message(paste0("fileResolution is NULL"))
  }

  if (!year %in% min(datasetSpan):max(datasetSpan)) {
    message(red(paste0("The year", year,
                       "does not have climate projections. Returning NULL")))
    return(NULL)
  }

  # 1. Check if we have the layer
# In this file we save all climate covariates that could have been used by a
# given model. Therefore, if we have the stack, we have all of them, even if in
# the end they were dropped and will not be used. They will be subset for each
# individual bird species.
  fileName <- file.path(pathToAnnualFolders, paste0(paste(studyAreaLongName, climateModel,
                                                          tolower(RCP), sep = "_"),
                                                    ifelse(!is.null(ensemble),
                                                           paste0("_", ensemble, "_"), ""),
                                                    ifelse(!is.null(fileResolution),
                                                           paste0("_", fileResolution, "_"), ""),
                                                          "_birds_",
                                                          year, ".grd"))

    if (modelVersion == "reducedBAM") {
      variables <- c("AHM", "bFFP", "CMD", "DD_0", "DD18",
                     "DD5", "eFFP", "EMT", "EXT", "FFP",
                     "MAP", "MAT", "MCMT", "MSP", "MWMT", "NFFD",
                     "PPT_sm", "PPT_wt", "SHM", "Tave_sm",
                     "Tave_wt", "TD")

      folders <- list.dirs(pathToAnnualFolders)
    } else {
      if (modelVersion != "8") {
        variables <- c("AHM", "bFFP", "CMD", "DD_0", "DD_18",
                       "DD18", "DD5", "eFFP", "EMT", "EXT", "FFP",
                       "MAP", "MAT", "MCMT", "MSP", "MWMT", "NFFD",
                       "PAS", "PPT_sm", "PPT_wt", "SHM", "Tave_sm",
                       "Tave_wt", "TD")
      } else {
        variables <- c("AHM", "CMD", "EMT", "MAT", "NFFD",
                       "PPT_wt", "SHM", "Tave_sm",
                       "TD", "MAP")
      }
      folders <- list.dirs(pathToAnnualFolders, pattern = "MSY")
    }

    # Identify all the closest 7 years
    if (year == min(datasetSpan)) {
      yearsToAverage <- year:(year+(lengthEnsamble-1))
    } else if (year == max(datasetSpan)) {
      yearsToAverage <- (year-(lengthEnsamble-1)):year
    } else {
      yearsToAverage <- (year-((lengthEnsamble-1)/2)):(year+((lengthEnsamble-1)/2))
    }

    if (file.exists(fileName)) {
      message(paste0(fileName, " exists. Returning..."))
      return(raster::stack(fileName))
    }

    # NOTE: Even though we have data from before 2011, its not
    # in the same format as the future ones, which means is not
    # readly usable. Sigh. One day I might make it usable. Not now.


  ensembleStack <- lapply(yearsToAverage, function(Y) {

    allFolders <- list.dirs(file.path(folders,
                                      studyAreaLongName))
    if (length(allFolders) == 0) {
      # Means we don't have the MSY folders in the pathToAnnualFolders
      # we might have the .zip though
      message(paste0("Directories with monthly, seasonal and yearly data not found for ",
                     studyAreaLongName, ". ",
                     "Trying to extract from zip file"))

      climFls <- list.files(pathToZipClimateFiles, full.names = TRUE,
                            pattern = ".zip")
      if (length(climFls) == 0) stop("Neither extracted climate data nor zipfiles found.
                                     Please double check paths and file names.
                                     You might have to modify filenames in R/prepareBirdClimateLayers.R")
      zipFile <- grepMulti(climFls,
                           patterns = c(climateModel, tolower(RCP),
                                        unlist(strsplit(studyAreaLongName, split = " ")),
                                        "MSY", ".zip"))

      if (length(zipFile) == 0) stop("Neither extracted climate data nor zipfile found.
                                     Please double check paths and file names.
                                     You might have to modify filenames in R/prepareBirdClimateLayers.R")
# Need to make a system call of unzip because the darn R doesn't unzip files properly
# unzip filename.zip -d /path/to/directory
      toUnzip <- system(paste0("unzip ", file.path(getwd(), zipFile), " -d ", pathToAnnualFolders))
      if (toUnzip != 0) warning(paste0("Unzipping seems to have errored. If ",
                                       "simulations stop, please check integrity",
                                       " of climate files"), immediate. = TRUE)
      folders <- list.dirs(pathToAnnualFolders)
      allFolders <- list.dirs(file.path(folders,
                                        studyAreaLongName))
    }
    currentYearFolder <- grepMulti(x = allFolders,
                                   patterns = paste0(Y, "MSY"))

    if (length(currentYearFolder) == 0) stop(paste0("Monthly, seasonal, and ",
                                                    "yearly climate data not ",
                                                    "found for ", studyAreaLongName,
                                                    " for ", Y, ". Please make ",
                                                    "sure you manually extract ",
                                                    "MSY folders to ", pathToAnnualFolders))

    allFiles <- paste0(tools::file_path_sans_ext(variables), "_Year", Y, ".tif")
    allFilesPaths <- file.path(dirname(currentYearFolder), paste(climateModel, tolower(RCP),
                                                                  allFiles, sep = "_"))
    whichDontExist <- which(!file.exists(allFilesPaths))
    # Identify which of the needed variables don't exist yet and make them
    if (length(whichDontExist) != 0) {

      filesToLoad <- file.path(currentYearFolder, paste0(variables, ".asc"))[whichDontExist]

      cors <- if (length(filesToLoad) < parallel::detectCores()/2)
        length(filesToLoad) else
          parallel::detectCores()/2
      if (Sys.getenv("RSTUDIO") != 1)
        plan("multicore", workers = cors) else
          plan("sequential")

      future_lapply(X = filesToLoad, FUN = function(variable) {
        tic(paste0("Processing ", basename(tools::file_path_sans_ext(variable)), " for year ", Y))
        ras <- raster(variable)
        crs(ras) <- sp::CRS(paste0('+init=epsg:4326 +proj=longlat +ellps=WGS84 ",
                                  "+datum=WGS84 +no_defs +towgs84=0,0,0'))
        if (any(!is.null(rasterToMatch),
                !is.null(studyArea))) {
          ras <- postProcess(x = ras,
                             studyArea = studyArea,
                             destinationPath = currentYearFolder,
                             rasterToMatch = rasterToMatch,
                             format = "GTiff",
                             filename2 = file.path(dirname(currentYearFolder),
                                                   paste(climateModel, tolower(RCP),
                                                         paste0(tools::file_path_sans_ext(basename(variable)),
                                                                "_Year", Y, ".tif"),
                                                         sep = "_")))
        }
        toc()
        return(paste0("Variable ",
                      tools::file_path_sans_ext(basename(variable)),
                      " post-processed!"))
      }, future.seed = TRUE)
      plan("sequential", workers = 1)
      variablesStack <- stack(lapply(allFilesPaths, raster))
    } else {
      variablesStack <- stack(lapply(allFilesPaths, raster))
    }
    return(variablesStack)
  })

  # With the ensemble stack, I need to map over the stacks and calculate the average
  cors <- if (nlayers(ensembleStack[[1]]) < parallel::detectCores()/2)
    nlayers(ensembleStack[[1]]) else
      parallel::detectCores()/2
  if (Sys.getenv("RSTUDIO") != 1)
    plan("multicore", workers = cors) else
      plan("sequential", workers = 1)

  organizedEnsemble <- raster::stack(future_lapply(1:nlayers(ensembleStack[[1]]), function(index) {
    lays <- stack(lapply(ensembleStack, `[[`, index))
    layName <- variables[index]
    tic(paste0("Calculating average ", layName, " for year ", year))
    ras <- calc(lays, fun = mean, na.rm = TRUE)
    names(ras) <- layName
    storage.mode(ras[]) <- "integer" # Reducing size of raster by converting it to a real binary
    toc()
    return(ras)
  }, future.seed = TRUE))
  plan("sequential")

  # [08OCT20 ~ Checked that the model layers below were divided by 10 to fit the models
  #            V6a and V8. We need then to divide the prediction layers as well]
  # Fixing the layers for the values that were multiplied by 10 in ClimateNA V6.11
  # The variables to potentially fix are:
  # •	Annual: MAT, MWMT, MCMT, TD, AHM, SHM, EMT, EXT and MAR;
  # •	Seasonal: Tmax, Tmin, Tave and Rad;
  # •	Monthly: Tmax, Tmin, Tave and Rad.'

# [02MAR22 ~ Asked if the model layers below were divided by 10 to fit the models
#            If so, we need then to divide the prediction layers as well]
# [UPDATE 04MAR22] I compared what Diana sent me to the original ClimateNA layers
# I got from this latest version of the software, and the ones I am calculating
# (i.e., dividing by 10), and it seems like the layers used for bird model fitting
# are in the same ballpark as the ones I am calculating. I also checked how we
# did in the Pathways paper, and I saw these were also divided.
# Maybe the ClimateNA version you got the layers from was not yet multiplying
# the variables to save space. CONCLUSION: We need to divide the ClimateNA layers
# by 10 when we are using BAM's models to predict birds.

  # Fixing the layers for the values that were multiplied by 10 in ClimateNA V7.11
  # Data format: the following variables were multiplied by 10
  # • Annual: MAT, MWMT, MCMT, TD, AHM, SHM, EMT, EXT and MAR;
  # • Seasonal: Tmax, Tmin, Tave and Rad;
  # • Monthly: Tmax, Tmin, Tave and Rad.
  # REFERENCE: https://s3-us-west-2.amazonaws.com/www.cacpd.org/documents/ClimateNAv7_manual.pdf
  # Page 9.

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
