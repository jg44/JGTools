#' remove spaces and special characters in header of a data.frame or data.table
#'
#' This function imports a csv file into a data.table object
#' @param df data.frame or data.table (required)
#' @keywords dataManagement
#' @export
#' @examples
#' df <- data.frame("f f"=c(1,1), "G&G"=c(5,2))
#' df_cleaned <- cleannames(df)
cleannames=function(df) {
  x <- as.character(names(df))
  x <- tolower(x)
  x <-  gsub(' +$', '', x)  ## trailing spaces only
  x <-  gsub(' +', '_', x)  ## interior spaces
  x <-  gsub('_+', '_', x)  ## interior spaces
  x <-  gsub('\\.', '_', x)  ## interior spaces
  x <-  gsub('__', '_', x)  ## interior spaces
  x <-  gsub('&+', 'and', x)
  x <-  gsub('_+$', '', x)  ## trailing spaces only

  names(df)=x
  return(df)}




