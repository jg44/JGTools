% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bu.R
\name{bu}
\alias{bu}
\title{open a folder, file or URL with exception handling}
\usage{
bu(fileOrUrl = getwd())
}
\arguments{
\item{fileOrUrl}{A string enclosed in quotes corresponding to a relative or absolute path to a file, folder, or URL. Spaces okay.}
}
\description{
This function is a wrapper for browseURL. Simplifies opening files, folders, or URLs externally
}
\examples{
bu()
bu("https://mypages.unh.edu/garnaslab/home")
}
\keyword{browseURL}
