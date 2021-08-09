pm <- function (response, factor1, factor2, error.bars = c("se", "sd",
                                                           "conf.int", "none"), level = 0.95, xlab = deparse(substitute(factor1)),
                ylab = paste("mean of", deparse(substitute(response))),
                legend.lab = deparse(substitute(factor2)), legend.pos = c("farright",
                                                                          "bottomright", "bottom", "bottomleft", "left", "topleft",
                                                                          "top", "topright", "right", "center"), main = "Plot of Means",
                pch = 1:n.levs.2, lty = 1:n.levs.2, col = palette(), connect = TRUE,
                ...)
{
  if (!is.numeric(response))
    stop("Argument response must be numeric.")
  xlab
  ylab
  legend.lab
  legend.pos <- match.arg(legend.pos)
  error.bars <- match.arg(error.bars)
  if (!is.factor(factor1)) {
    if (!(is.character(factor1) || is.logical(factor1)))
      stop("Argument factor1 must be a factor, character, or logical.")
    factor1 <- as.factor(factor1)
  }
  if (missing(factor2)) {
    valid <- complete.cases(factor1, response)
    factor1 <- factor1[valid]
    response <- response[valid]
    means <- tapply(response, factor1, mean)
    sds <- tapply(response, factor1, sd)
    ns <- tapply(response, factor1, length)
    if (error.bars == "se")
      sds <- sds/sqrt(ns)
    if (error.bars == "conf.int")
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
      sds/sqrt(ns)
    sds[is.na(sds)] <- 0
    yrange <- if (error.bars != "none")
      c(min(means - sds, na.rm = TRUE), max(means + sds,
                                            na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs <- levels(factor1)
    n.levs <- length(levs)
    plot(c(1, n.levs), yrange, type = "n", xlab = xlab,
         ylab = ylab, axes = FALSE, main = main, ...)
    points(1:n.levs, means, type = if (connect)
      "b"
      else "p", pch = 16, cex = 2)
    box()
    axis(2)
    axis(1, at = 1:n.levs, labels = levs)
    if (error.bars != "none")
      arrows(1:n.levs, means - sds, 1:n.levs, means +
               sds, angle = 90, lty = 2, code = 3, length = 0.125)
  }
  else {
    if (!is.factor(factor2)) {
      if (!(is.character(factor2) || is.logical(factor2)))
        stop("Argument factor2 must be a factor, charcter, or logical.")
      factor2 <- as.factor(factor2)
    }
    valid <- complete.cases(factor1, factor2, response)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    response <- response[valid]
    means <- tapply(response, list(factor1, factor2), mean)
    sds <- tapply(response, list(factor1, factor2), sd)
    ns <- tapply(response, list(factor1, factor2), length)
    if (error.bars == "se")
      sds <- sds/sqrt(ns)
    if (error.bars == "conf.int")
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
      sds/sqrt(ns)
    sds[is.na(sds)] <- 0
    yrange <- if (error.bars != "none")
      c(min(means - sds, na.rm = TRUE), max(means + sds,
                                            na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs.1 <- levels(factor1)
    levs.2 <- levels(factor2)
    n.levs.1 <- length(levs.1)
    n.levs.2 <- length(levs.2)
    if (length(pch) == 1)
      pch <- rep(pch, n.levs.2)
    if (length(col) == 1)
      col <- rep(col, n.levs.2)
    if (length(lty) == 1)
      lty <- rep(lty, n.levs.2)
    expand.x.range <- if (legend.pos == "farright")
      1.4
    else 1
    if (n.levs.2 > length(col))
      stop(sprintf("Number of groups for factor2, %d, exceeds number of distinct colours, %d."),
           n.levs.2, length(col))
    plot(c(1, n.levs.1 * expand.x.range), yrange, type = "n",
         xlab = xlab, ylab = ylab, axes = FALSE, main = main,
         ...)
    box()
    axis(2)
    axis(1, at = 1:n.levs.1, labels = levs.1)
    for (i in 1:n.levs.2) {
      points(1:n.levs.1, means[, i], type = if (connect)
        "b"
        else "p", pch = pch[i], cex = 2, col = col[i], lty = lty[i])
      if (error.bars != "none")
        arrows(1:n.levs.1, means[, i] - sds[, i], 1:n.levs.1,
               means[, i] + sds[, i], angle = 90, code = 3,
               col = col[i], lty = lty[i], length = 0.125)
    }
    if (legend.pos == "farright") {
      x.posn <- n.levs.1 * 1.1
      y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
      legend(x.posn, y.posn, levs.2, pch = pch, col = col,
             lty = lty, title = legend.lab)
    }
    else legend(legend.pos, levs.2, pch = pch, col = col,
                lty = lty, title = legend.lab, inset = 0.02)
  }
  invisible(NULL)
}

source("/Users/jrg1035/Dropbox/R/myfunctions/functionlist.r")


library(car)

Moore

xfactor <-  Moore$fcategory
factor2 <- Moore$partner.status
response <- Moore$fscore

getxaxis <- function(response, xfactor, factor2, between=0, border=.5){

  dt1 <- data.table(response, xfactor, factor2)

  meanz <- dt1[,  list(mean(response), sd(response), length(response)), by=list(xfactor, factor2)]
  names(meanz)[-(1:2)] <- c("mean", "sd", "N")
  meanz[, se := sd/sqrt(N), by=list(xfactor, factor2)]
  print(meanz)

    xfactorName <- deparse(substitute(xfactor))
    factor2Name <- deparse(substitute(factor2))
    responseName <- deparse(substitute(response))

  if (between > 1 || between < 0) {print("Between value can only range from 0 to 1. Reverting to 0.25"); between <- .25}
  totPts <- nrow(expand.grid(levels(xfactor), levels(factor2)))
  fcat <- length(levels(factor2))
  xcat <- length(levels(xfactor))
  offset <- ifelse(fcat %% 2 == 0, .5, 1)
  #xvals <- seq(1-offset/fcat, max(xcat)+offset/fcat, 1/fcat)

  fcatMultiplier <- seq(-(fcat-fcat %% 2)/2, (fcat-fcat %% 2)/2, length.out=fcat)#*ifelse(fcat %% 2 == 0, (fcat-1)/fcat, 1)
  fcatOffset <- offset/fcat-between/fcat*offset #*offset/fcat*(xcat-1)

  xvals <- rep(1:xcat, each=fcat)+fcatMultiplier*fcatOffset

  betweencats <- 1:(xcat-1)+diff(1:xcat)/2

  xax <- c(min(xvals)-border/fcat, max(xvals)+border/fcat)


  # plot(xvals, 1:totPts, ylim=c(0, max(totPts)*1.1), xlim=xax, col=1:fcat)
  #
  # .addse(xvals, 1:totPts, .8 )
  #
  return(list(xvals=xvals, xax=xax, catlabels=1:xcat, catbreaks=betweencats,
              xlabels=levels(xfactor), legendlabels=levels(factor2),
              xfactorName=xfactorName, factor2Name=factor2Name,
              responseName=responseName, meanvalues=meanz))
}





with(Moore, getxaxis(fscore, fcategory, partner.status, between=.25, border=.7))

par(pch=19)

Moore$fcategory <- factor(Moore$fcategory, levels = c("low", "medium", "high"))
Moore$partner.status <- factor(Moore$partner.status, levels = c("low", "high"))

blankplot(1,1)
plotparams <- with(Moore, getxaxis(fscore, partner.status, fcategory, between=.25, border=.7))


library(JGTools)

par(pch=19, cex=1.1, mar=c(6,6,1,5))
with(plotparams, {
  yl <- c(min(meanvalue$mean-0.1*(meanvalue$mean+meanvalue$se)), max(meanvalue$mean+0.1*(meanvalue$mean+meanvalue$se)))
  blankplot(1,1, xlim=xax, ylim=yl)
  .addse(xvals, meanvalue$mean, .9)
  points(xvals, meanvalue$mean, col=(2:4)[as.factor(legendlabels)], pch=c(15,4,19)[as.factor(legendlabels)])
  axx(x=FALSE)
  axis(1, cex.axis = 1.3, at=catlabels, xlabels)
  abline(v=catbreaks, lty=3)
  mtxx(xfactorName, responseName)
  par(xpd=TRUE)
  legend("topright", title = factor2Name, col=(2:4)[as.factor(legendlabels)],
         legend=legendlabels, pch=c(15,4,19)[as.factor(legendlabels)], inset=c(-.2,.03), bty="n", cex=1.2)
  par(xpd=FALSE)
}
)

