makeWaterLayer <- function(pathData, rasterToMatch, studyArea){
  waterRaster <- Cache(prepInputs, url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM", 
                       destinationPath = pathData,
                       targetFile = "waterRasterNWT.tif", studyArea = studyArea,
                       rasterToMatch = rasterToMatch,
                       filename2 = NULL)
  waterVals <- raster::getValues(waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
  waterVals[waterVals == 1] <- NA
  waterVals[waterVals > 1] <- 1
  waterRaster <- raster::setValues(waterRaster, waterVals)
  return(waterRaster)
}