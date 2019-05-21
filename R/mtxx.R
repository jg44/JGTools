#' Adds axis labels in preferred style
#'
#' This function prepares the plotting area with a blank plot
#' @param x Text string in quotes for x-axis
#' @param y Text string in quotes for x-axis
#' @param line1 Line count distance from x-axis. Defaults to 3. Can be negative.
#' @param line2 Line count distance from y-axis. Defaults to 3. Can be negative.
#' @param cex Font size. Defaults to 1.4.
#' @keywords plot
#' @export
#' @examples
#' mtxx(x = expression(bar(x) == sum(frac(x[i], n), i==1, alpha) ~ (non-sensical ~ axes ~ demo ~ only)),
#'     y = expression(Log[10] ~ italic(Daphnia ~ pulex) ~ count ~ cm^-3), line1=4)

mtxx<-function(x,y, line1=3, line2=3, cex=1.4){
  #opar<-par()
  par(las=0)
  mtext(side=1, x, line=line1, cex=cex)
  mtext(side=2, y, line=line2, cex=cex)
  #par(opar)
}

