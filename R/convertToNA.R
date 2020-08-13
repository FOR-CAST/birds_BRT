convertToNA <- function(ras, 
                        valsToConvert = 1){
  # Any values in valsToConvert will be converted to NA
  # NA will be converted to 1
  # Any values NOT in valsToConvert will be converted to 1
  rasVec <- raster::getValues(ras)
  rasVec[rasVec %in% valsToConvert] <- -666
  rasVec[is.na(rasVec) | rasVec != -666] <- 1
  rasVec[rasVec == -666] <- NA
  ras <- raster::setValues(ras, rasVec)
  return(ras)
}