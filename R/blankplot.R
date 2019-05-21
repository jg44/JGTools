#' prepare plotting area with a blank plot
#'
#' This function prepares the plotting area with a blank plot
#' @param x X values. Defaults to 0:1
#' @param y Y values. Defaults to 0:1
#' @keywords plot
#' @export
#' @examples
#' blankplot(1:10, 1:10)
#'

blankplot<-function(x=0:1, y=0:1, ...) {
  plot(x,y,..., xlab="", ylab="", axes=FALSE, type="n")
}
