#' Source from Dropbox file URL
#'
#' This function loads an .R file from a dropbox link
#' @param dropbox.url url for shared dropbox link
#'
#' @keywords dropbox source
#' @examples
#' source(sourceDropbox("https://www.dropbox.com/s/gidwa8g23df6uw4/functionlistSample.r?dl=0"))
#' @export
sourceDropbox <- function(dropbox.url) {

  # Purpose: source an R script from a Dropbox file

  # Input: the URL that you get when you share a Dropbox file

  # Output: the input to the source() function

  # Example of use:
  # source(sourceDropbox('https://www.dropbox.com/s/9m4139zbnee59jh/grfpairs.R'))

  # Author: George Fisher george@georgefisher.com

  # Date : October 30, 2013

  # thanks to
  # http://thebiobucket.blogspot.com/2012/05/source-r-script-from-dropbox.html

  # note: this goes out over the network so you might want to package several
  # functions into one file
  # modified by Jeff Garnas, 2021.08.03

  require(RCurl)
  #setwd(tempdir())

  tmpdr <- tempdir()

  destfile = "filetosource.txt"

  # use regex to get the piece of the Dropbox URL we need
  matches <- regexpr("(/s/.*)", dropbox.url, perl = TRUE, ignore.case = TRUE)
  result <- attr(matches, "capture.start")[, 1]
  attr(result, "match.length") <- attr(matches, "capture.length")[, 1]
  dropbox.tail = regmatches(dropbox.url, result)
  # (my eternal thanks to the RegexBuddy program)

  # create the request URL
  dburl = paste("https://dl.dropbox.com", dropbox.tail, sep = "")

  x = getBinaryURL(dburl, followlocation = TRUE, ssl.verifypeer = FALSE)
  writeBin(x, paste0(tmpdr, "\\", destfile), useBytes = TRUE)


  paste0(tmpdr, "\\", destfile)
}
