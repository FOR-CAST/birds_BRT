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
  browser() # PROBLEMS WITH DOUBLING LAYER (Substituting... Check)
  staticLayers <- lapply(X = seq_len(nlayers(specieLayers)), FUN = function(layer){
    lay <- postProcess(specieLayers[[layer]], studyArea = studyArea, rasterToMatch = rasterToMatch)
    return(lay)
  })
  staticLayers <- raster::stack(staticLayers)
  
    return(staticLayers)
}

Species_Acer_Neg_v1