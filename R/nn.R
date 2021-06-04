#' Vectorize and numericize a string of numbers.
#'
#' This function vectorizes and numericizes a string of numbers. Character elements converted to NA.
#' @param x string or vector of numbers
#' @keywords data
#' @export
#' @examples
#' nn(c("a", 1:3, "b", " 1"))
nn <- function(x){
  vect <- suppressWarnings(  as.vector(as.numeric(as.vector(x))))
  cat("These elements were not numeric (converted to NA): ", which(is.na(vect)),"\n\n" )
  return(vect)
  }
