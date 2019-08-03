loadStaticLayers <- function(fileURL,
                             pathData,
                             studyArea,
                             rasterToMatch){
  require("raster")
  stkPre <- preProcess(url = fileURL, 
                       alsoExtract = "similar",
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
    lay <- postProcess(subStaticLayers[[layer]], studyArea = studyArea, 
                       rasterToMatch = rasterToMatch, destinationPath = pathData, 
                       filename2 = NULL)
    return(lay)
  })
  # names(staticLayers) <- fixedLayers # Might not be necessary
  # staticLayers <- lapply(X = seq_len(length(staticLayers)), FUN = function(layNumber){
  #   names(staticLayers[[layNumber]]) <- fixedLayers[layNumber]
  #   return(staticLayers[[layNumber]])
  # }) # Might not be necessary
  staticLayers <- raster::stack(staticLayers)
  names(staticLayers) <- fixedLayers # Maybe just passing the names to the stack would do the trick
  return(staticLayers)
}