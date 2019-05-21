#' Adds axis in preferred default style
#'
#' This function prepares the plotting area with a blank plot
#' @param x Add the x-axis. Defaults to TRUE
#' @param y Add the y-axis. Defaults to TRUE
#' @param box Add axis lines. Defaults to TRUE
#' @param cex.axis Sets axis font size. Defaults to 1.3
#' @keywords plot
#' @export
#' @examples
#' axx()
#' axx(X=FALSE) # Adds y-axis only.

axx<-function(x=T, y=T, box=T, cex.axis=1.3){
  if (x) axis(1, cex.axis=cex.axis)
  if (y) axis(2, las=2,cex.axis=cex.axis)
  if (box) box(bty="L", lwd=2)
}
