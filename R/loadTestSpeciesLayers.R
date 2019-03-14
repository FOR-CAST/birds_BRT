loadTestSpeciesLayers <- function(successionTables = sim$successionTables,
                                           modelList = sim$birdModels,
                                           pathData = dataPath(sim),
                                  studyArea = sim$studyArea,
                                  rasterToMatch = sim$rasterToMatch){
    message("Biomass data not simulated, using original dataset as test")
  specieLayers <- reproducible::prepInputs(targetFile = "StaticSpeciesLayers.grd",
                                           archive = "StaticSpeciesLayers.zip",
                                           alsoExtract = "similar",
                               url = "https://drive.google.com/open?id=1QiwMJpbQYeBH5ifNZDEXe04bHmn-w4Oc",
                               destinationPath = pathData, fun = "raster::stack")
  staticLayers <- lapply(X = seq_len(nlayers(specieLayers)), FUN = function(layer){
    lay <- postProcess(specieLayers[[layer]], studyArea = studyArea, 
                       rasterToMatch = rasterToMatch, destinationPath = pathData,
                       filename2 = NULL)
    names(lay) <- names(specieLayers[[layer]])
    return(lay)
  })
  staticLayers <- raster::stack(staticLayers)

    return(staticLayers)
}