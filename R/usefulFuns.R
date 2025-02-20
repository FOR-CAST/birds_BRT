#' `grepl` for multiple patterns and return the object
#'
#' from <https://github.com/tati-micheletti/usefulFuns/blob/development/R/grepMulti.R>
#'
#' @param x object where to look for patterns.
#'
#' @param patterns Character vector of patterns to look for objects.
#'
#' @param unwanted Character vector of patterns to exclude from search.
#'
#' @return The objects with specified patterns combined
#'
#' @author Tati Micheletti
#' @export
#' @rdname grepMulti
grepMulti <- function(x, patterns, unwanted = NULL) {
  rescued <- sapply(x, function(fun) all(sapply(X = patterns, FUN = grepl, fun)))
  recovered <- x[rescued]
  if (!is.null(unwanted)) {
    discard <- sapply(recovered, function(fun) all(sapply(X = unwanted, FUN = grepl, fun)))
    afterFiltering <- recovered[!discard]
    return(afterFiltering)
  } else {
    return(recovered)
  }
}

#' Get a sub-string based on the number of characters and the side to start
#'
#' from <https://github.com/tati-micheletti/usefulFuns/blob/development/R/substrBoth.R>
#'
#' @param strng String from which to grab a subset
#' @param howManyCharacters numeric. How many characters should be returned in the sub-string?
#' @param fromEnd logical. Default is TRUE. Should the subset start in the end of the string?
#'
#' @return character string of the subset.
#'
#' @author Tati Micheletti
#' @export
#'
#' @rdname substrBoth
substrBoth <- function(strng, howManyCharacters, fromEnd = TRUE) {
  if (fromEnd) {
    substr(x = strng, start = nchar(strng) - howManyCharacters + 1, nchar(strng))
  } else {
    substr(x = strng, start = 1, stop = howManyCharacters)
  }
}
