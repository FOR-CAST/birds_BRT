prepInputStack <- function(...){
  
  dots <- list(...)
  message("prepInput a raster stack...")
  stackLayers <- reproducible::prepInputs(archive = dots$archive,
                                           url = dots$url,
                                          targetFile = dots$targetFile,
                                          alsoExtract = dots$alsoExtract,
                                           destinationPath = dots$destinationPath, 
                                           fun = "raster::stack")
  postProcessedLayers <- lapply(X = seq_len(nlayers(stackLayers)), FUN = function(layer){
    lay <- postProcess(stackLayers[[layer]], studyArea = dots$studyArea, 
                       rasterToMatch = dots$rasterToMatch, destinationPath = dots$destinationPath,
                       filename2 = dots$filename2)
    names(lay) <- names(stackLayers[[layer]])
    return(lay)
  })
  postProcessedLayers <- raster::stack(postProcessedLayers)
  names(postProcessedLayers) <- names(stackLayers) # Added this one for computers that can't process in memory and have to write a temporary file

    return(postProcessedLayers)
}
