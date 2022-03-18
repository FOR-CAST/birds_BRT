createModObjectBirds <- function (data, sim = NULL, pathInput, currentTime,
                                  fun = readRDS, ignore = NULL){

  if (all(is.null(sim), is.null(pathInput)))
    stop("Either a simList or a folder containing the data need to be supplied")
  dt <- NULL
  if (!is.null(sim) & !is.null(sim[[data]])) {
    dt <- sim[[data]]
  }
  else {
    message(crayon::yellow(paste0(data, " not supplied by another module.",
                                  " Will try using files in inputPath(sim)")))
    if (length(list.files(pathInput, recursive = TRUE)) ==
        0)
      stop(paste0("Please place the data in the input folder ",
                  pathInput))
    if (class(currentTime) != "numeric")
      stop("Current time needs to be numeric!")
    dataName <- grepMulti(x = list.files(pathInput, recursive = TRUE),
                          patterns = c(data, SpaDES.core::paddedFloatToChar(currentTime,
                                                                            padL = 3)))
    if (length(dataName) == 0) {
      dt <- NULL
    }
    else {
      if (!is.null(ignore)){
        toRemove <- grepl(x = dataName, pattern = paste(ignore, collapse = "|"))
        dataName <- dataName[!toRemove]
      }
      dt <- do.call(what = fun, args = list(file.path(pathInput,
                                                      dataName)))
    }
    if (!is.null(dt))
      message(paste0(data, " loaded from ", crayon::magenta(file.path(pathInput,
                                                                      dataName)), " for year ", SpaDES.core::paddedFloatToChar(currentTime,
                                                                                                                               padL = 3)))
    else message(crayon::red(paste0("No file found for ",
                                    currentTime, ". Returning NULL")))
  }
  return(dt)
}
