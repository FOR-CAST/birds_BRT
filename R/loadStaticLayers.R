loadStaticLayers <- function(fileURL,
                             pathData,
                             studyArea,
                             rasterToMatch,
                             useCache = NULL,
                             useCacheInternals = NULL,
                             staticLayersNames,
                             ...){
  dots <- list(...)
  modVersion <- dots[["version"]]

  if (modVersion == "reducedBAM"){
    province <- dots[["Province"]]
    if (!province %in% c("AB", "BC", "NT", "NU", "SK", "MB", "YT"))
      stop("Province ", province, " not compatible with module")
    if (province == "NU") province <- "NT"

    bcrs <- switch(EXPR = province,
                   BC = paste0("bcr", c(4, 10, 60), "all_1km"),
                   AB = paste0("bcr", c(60, 70, 80), "all_1km"),
                   NT = paste0("bcr", c(4, 61, 70), "all_1km"),
                   SK = paste0("bcr", c(60, 70, 80), "all_1km"),
                   MB = paste0("bcr", c(60, 70, 80), "all_1km"),
                   YT = paste0("bcr", 4, "all_1km"))

    allFiles <- googledrive::drive_ls(path = as_id(fileURL),
                                      pattern = paste(bcrs, collapse = "|"))

    down <- lapply(X = as.data.table(allFiles)[["name"]], function(layNames){
      if (!file.exists(file.path(pathData, layNames))){
        googledrive::drive_download(file = as_id(allFiles[allFiles$name %in% layNames, ]$id), #modelFile,
                                    path = file.path(pathData, layNames),
                                    overwrite = TRUE)
      }
    })
    filesToLoad <- allFiles$name[grepl(allFiles$name, pattern = "grd")]
  allLays <- lapply(filesToLoad, function(lay){
    allVariables <- dots[["allVariables"]]
    # Reduce to exclusive static layers available
    allVariables <- staticLayersNames[staticLayersNames %in% allVariables]

    # Select the static layers available
    ras <- stack(file.path(pathData, lay))
    laysToKeep <- which(names(ras) %in% allVariables)
    rasRed <- raster::subset(x = ras, subset = laysToKeep, drop = TRUE)
    stkPre <- Cache(reproducible::postProcess, x = rasRed,
                                       studyArea = studyArea,
                                       rasterToMatch = rasterToMatch,
                                       destinationPath = pathData)
  })
  # Rearrange the layers so that each variable becomes a raster stack
  # Subset matching tiles
  lengthVector <- 1:nlayers(allLays[[1]])
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(allLays, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  names(orderedRasterList) <- names(allLays[[1]])

  # Once I have all stacks, I need to mosaic, then postProcess
  postProcessedLays <- lapply(names(allLays[[1]]), function(nameLay){
    stk <- orderedRasterList[[nameLay]]
    if (length(stk) == 3){ # TODO: Make this horrible code better. Couldn't make it work with do.call()
      rasCombined <- raster::mosaic(stk[[1]], stk[[2]], stk[[3]], fun = "max")
    } else rasCombined <- stk[[1]]
    return(rasCombined)
  })

  names(postProcessedLays) <- names(allLays[[1]])
  staticLayers <- raster::stack(postProcessedLays)

  # Browse[1]> postProcessedLays
  # $Structure_Biomass_TotalLiveAboveGround_v1
  # $Structure_Biomass_TotalLiveAboveGround_v1[[1]]
  # class      : RasterLayer
  # dimensions : 4900, 2891, 14165900  (nrow, ncol, ncell)
  # resolution : 250, 250  (x, y)
  # extent     : -2222000, -1499250, 8136500, 9361500  (xmin, xmax, ymin, ymax)
  # crs        : +proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs
  # source     : memory
  # names      : Structure_Biomass_TotalLiveAboveGround_v1
  # values     : 0, 159.8483  (min, max)
  #
  # Browse[1]>   staticLayers <- raster::stack(postProcessedLays)
  # Error in .local(x, ...) : list has no "x"
  # Browse[1]>

  } else {

  stkPre <- reproducible::preProcess(url = fileURL,
                       alsoExtract = "similar",
                       destinationPath = pathData,
                       omitArgs = c("useCache", "purge"))
  stk <- raster::stack(stkPre$targetFilePath) # This file has all species too. Exclude those.
  stkNames <- unlist(lapply(X = 1:length(stk@layers), FUN = function(layers){
    lay <- stk@layers[[layers]]@data@names
    return(lay)
  }))
  spLayers <- c("Species", "Structure")
  fixedLayers <- stkNames[!grepl(pattern = paste(spLayers, collapse = "|"),
                                 x = stkNames)]

  subStaticLayers <- raster::subset(x = stk, subset = fixedLayers)
  staticLayers <- lapply(X = seq_len(nlayers(subStaticLayers)), FUN = function(layer){
    lay <- postProcess(subStaticLayers[[layer]], studyArea = studyArea,
                       rasterToMatch = rasterToMatch, destinationPath = pathData,
                       useCache = useCacheInternals, omitArgs = c("purge", "useCache"),
                       filename2 = NULL)
    return(lay)
  })
  staticLayers <- raster::stack(staticLayers)
  names(staticLayers) <- fixedLayers # Maybe just passing the names to the stack would do the trick
  }
  return(staticLayers)
}
