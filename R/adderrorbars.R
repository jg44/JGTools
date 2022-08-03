#' prepare plotting area with a blank plot
#'
#' This function prepares the plotting area with a blank plot
#' @param x X values for error bar addition.
#' @param y Y values for error bar addition (typically mean Y values)
#' @param errorbar Length of lower and upper error bars as a vector of length 2 (or 1 if symmetrical)
#' @param length Sets default length of error bar "whisker" to 0.07
#' @param col Sets default error bar color to black. Use before "points" command.
#' @param horiz=FALSE Makes horizontal error bars when set to TRUE (Default = FALSE).
#' @keywords plot
#' @export
#' @examples
#' library(data.table)
#' library(JGTools)
#' data(plantGrowth)
#' plantGrowth <- as.data.table(plantGrowth)
#' # use data.table to aggregate data by treatment and calculate useful descriptive stats.
#' agg.plantGrowth <- plantGrowth[, list(mean.drymass=mean(drymass), sd=sd(drymass), N=.N,
#'     SE.drymass=sd(drymass)/sqrt(.N),
#'     plusminusCI95=abs(qt(.025, .N-1))*sd(drymass)/sqrt(.N) ),
#'     by=list(N, P)]
#' # x11() #or quartz() (windows and mac, respectively)
#' par(cex.axis=1.5, mar=c(6,6,1,1))
#' agg.plantGrowth[, blankplot(1:length(mean.drymass)+c(-.2, .2),
#'                             seq(0, max(mean.drymass+SE.drymass), length.out=length(mean.drymass))  )]
#' agg.plantGrowth[, adderrorbars(1:length(mean.drymass), y = mean.drymass, errorbar = SE.drymass)]
#' agg.plantGrowth[, points(1:length(mean.drymass), mean.drymass, pch=19, col=4, cex=2)]
#' agg.plantGrowth[, points(1:length(mean.drymass), mean.drymass, pch=19, col=1, cex=1.15)]
#' axx(x=FALSE)
#' agg.plantGrowth[, mtext(cex=1.4, at=1:length(mean.drymass), c(.1, 1, .1, 1), side=1, line=1, col="dark blue")]
#' agg.plantGrowth[, mtext(cex=1.4, at=c(1.5, 3.5), c(.04, .4), side=1, line=2.5, col="dark red")]
#' mtext(at=0.05, side=1, line=1, col="dark blue", "Nitrogen:", adj=0, cex=1.4)
#' mtext(at=0.05, side=1, line=2.5, col="dark red", "Phosphorus:", adj=0, cex=1.4)
#' mtxx(expression(paste("Nutrient levels (g·L"^{-1}," of watering solution)")) ,
#'      expression("Aboveground dry mass (g)  " %+-% "SE"), line1=4.5)
#' # Now plot with 95% CI's
#' par(cex.axis=1.5, mar=c(6,6,1,1))
#' agg.plantGrowth[, blankplot(1:length(mean.drymass)+c(-.2, .2),
#'                             seq(0, max(mean.drymass+plusminusCI95), length.out=length(mean.drymass))  )]
#' agg.plantGrowth[, adderrorbars(1:length(mean.drymass), y = mean.drymass, errorbar = plusminusCI95)]
#' agg.plantGrowth[, points(1:length(mean.drymass), mean.drymass, pch=19, col=4)]
#' axx(x=FALSE)
#' agg.plantGrowth[, mtext(cex=1.4, at=1:length(mean.drymass), c(.1, 1, .1, 1), side=1, line=1, col="dark blue")]
#' agg.plantGrowth[, mtext(cex=1.4, at=c(1.5, 3.5), c(.04, .4), side=1, line=2.5, col="dark red")]
#' mtext(at=0.05, side=1, line=1, col="dark blue", "Nitrogen:", adj=0, cex=1.4)
#' mtext(at=0.05, side=1, line=2.5, col="dark red", "Phosphorus:", adj=0, cex=1.4)
#' mtxx(expression(paste("Nutrient levels (g·L"^{-1}," of solution)")) ,
#'      expression("Aboveground dry mass (g) with 95% CI"), line1=4.5)

adderrorbars<-function(x, y, errorbar, length=.07, col=1, horiz=FALSE){
  errorcount <- ncol(as.data.frame(errorbar))
  if (errorcount==1) errorUse <- data.frame(LL=errorbar, UL=errorbar)
  if (errorcount==2) errorUse <- data.frame(LL=errorbar[,1], UL=errorbar[,2])
  if (!(errorcount %in% 1:2)) break
  if (horiz) {arrows(x-errorUse[,1], y, x+errorUse[,2], y, code=3, angle=90, length=length, col=col)} else {
    arrows(x, y-errorUse[,1], x, y+errorUse[,2], code=3, angle=90, length=length, col=col)
  }
}
