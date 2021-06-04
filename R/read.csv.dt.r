#' open .csv as data.table object
#'
#' This function imports a csv file into a data.table object
#' @param file filename No default (required)
#' @param datadir Relative (or absolute) path to data directory
#' @param sep Use comma for csv
#' @param header Defaults to TRUE
#' @keywords csv
#' @export
#' @examples
#' imported.data.table <- read.csv.dt("./csvs/test.csv")

read.csv.dt<-function(file, datadir= "./data/", sep=",", header=TRUE){
  require(data.table)

  if (!(is.null(datadir) || datadir=="")){
    if (!grepl("*/$",datadir)) file <- paste0(datadir, "/", file)
    if (length(grep("/", file))==0) file <- paste0(datadir, file)
    }
  if (!grepl("*.csv$", file)) file <- paste0(file, ".csv")
  tmp <- data.table(read.csv(file, sep=sep, header=header, fileEncoding="UTF-8-BOM"))
  return(tmp)
}


