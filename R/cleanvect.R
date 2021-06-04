#' Remove spaces and special characters from a vectoy
#'
#' This function special characters from a vector and converts spaces to underscore if desired
#' @param vect filename No default (required)
#' @param interiorSpaces2Underscore Relative (or absolute) path to data directory
#' @keywords data
#' @export
#' @examples
#' cleanvector(c(1, 2, " aja alk", "% s "), interiorSpaces2Underscore = TRUE)
#'
cleanvector<-function(vect, interiorSpaces2Underscore = FALSE) {
  x <- as.character(vect)
  x <- tolower(x)
  x <-  gsub(' +$', '', x)  ## trailing spaces only
  x <-  gsub("([0-9])([a-zA-Z])","\\1_\\2",x) #backreferencing (see: http://stackoverflow.com/questions/11605564/r-regex-gsub-separate-letters-and-numbers)
  x <-  gsub(' +$', '', x)  ## trailing spaces only
  x <-  gsub(' +$', '', x)  ## trailing spaces only

  x <- gsub("[[:punct:]]", "", x)
  if (interiorSpaces2Underscore) x <-  gsub(' +', '_', x)  ## interior spaces
  x <-  gsub('_+', '', x)  ## leading spaces

  x <-  gsub('\\.', '_', x)  ## interior spaces
  if (interiorSpaces2Underscore) x <-  gsub('__', '_', x)  ## interior spaces
  if (interiorSpaces2Underscore) x <-  gsub('&+', 'and', x)
  x <-  gsub('\\(', '', x)
  x <-  gsub('\\)', '', x)
  if (interiorSpaces2Underscore) x <-  gsub('__', '_', x)  ## interior spaces
  if (interiorSpaces2Underscore) x <-  gsub('__', '_', x)  ## interior spaces
  return(x)}



