loadStaticLayers <- function(fileURL = extractURL("urlStaticLayers"),
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea,
                             rasterToMatch = sim$rasterToMatch){
  require("raster")
  stkPre <- preProcess(url = fileURL, targetFile = "bcr6_2011rasters250.grd",
                       alsoExtract = "bcr6_2011rasters250.gri",
                    destinationPath = pathData)
  stk <- raster::stack(stkPre$targetFilePath)
  stkNames <- unlist(lapply(X = 1:length(stk@layers), FUN = function(layers){
    lay <- stk@layers[[layers]]@data@names
    return(lay)
  }))
  spLayers <- c("Species", "Structure")
  fixedLayers <- stkNames[!grepl(pattern = paste(spLayers, collapse = "|"), 
                                 x = stkNames)]
  subStaticLayers <- raster::subset(x = stk, subset = fixedLayers)
  staticLayers <- lapply(X = seq_len(nlayers(subStaticLayers)), FUN = function(layer){
    lay <- postProcess(subStaticLayers[[layer]], studyArea = studyArea, rasterToMatch = rasterToMatch)
    return(lay)
  })
  staticLayers <- raster::stack(staticLayers)
  return(staticLayers)
}