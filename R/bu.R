#' open a folder, file or URL with exception handling
#'
#' This function is a wrapper for browseURL. Simplifies opening files, folders, or URLs externally
#' @param fileOrUrl A string enclosed in quotes corresponding to a relative or absolute path to a file, folder, or URL. Spaces okay.
#' @keywords browseURL
#' @export
#' @examples
#' bu()
#' bu("https://mypages.unh.edu/garnaslab/home")

bu<-function(fileOrUrl=getwd()){ browseURL(eval(gsub("[[:space:]]", "%20", fileOrUrl))) }
