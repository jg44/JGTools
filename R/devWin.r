#' Create a plotting window across platforms
#'
#' This function prepares the plotting area with a blank plot
#' @param height Plot window height
#' @param width Plot window width
#' @param rows Row count of plots.
#' @param cols Column count of plots.
#' @param oma outer margins (bottom, left, top, right)
#' @keywords plot
#' @export
#' @examples
#' devWin(6,14, 1, 2, oma=c(2,2,1,1))
#' plot(1,1, main="left plot")
#' plot(1,1, main="right plot")
#' mtext("both plots x", outer=TRUE, side=1, cex=1.8)
#' mtext("both plots y", outer=TRUE, side=2, cex=1.8)

devWin <- function(height=7, width=7, rows=1, cols=1, oma=c(1,1,1,1)){
  if (Sys.info()[1]=="Windows") x11(height=height, width=width) else
    quartz(height=height, width=width)
  par(mfrow=c(rows, cols), oma=oma)
}


