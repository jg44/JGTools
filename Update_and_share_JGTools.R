# install.packages("devtools")
#install.packages("devtools", lib = Sys.getenv("R_LIBS_USER"))

library("devtools")
 #devtools::install_github("klutometis/roxygen", lib = Sys.getenv("R_LIBS_USER"))
 #devtools::install_github
library(roxygen2)
# roxygen2::d

library("usethis")

# create_package("JGTools")
setwd("C:/Users/jrg1035/GitProjects/JGTools/")

document()
usethis::use_data(plantGrowth)
#
# devtools::install("JGTools", upgrade="always")
#
# install.packages("JGTools",dependencies=TRUE,repos= NULL, type="source")
# library(JGTools)
# update_packages("JGTools")

# browseURL("/Users/jrg1035/Dropbox/R/myfunctions/functionlist.r")
# Share -------------------------------------------------------------------

#install.packages("devtools")
library(devtools)
install_github("jg44/JGTools", upgrade = TRUE)
library(JGTools)

?adderrorbars
X11()
library(data.table)
library(JGTools)
data(plantGrowth)
plantGrowth <- as.data.table(plantGrowth)
# use data.table to aggregate data by treatment and calculate useful descriptive stats.
agg.plantGrowth <- plantGrowth[, list(mean.drymass=mean(drymass), sd=sd(drymass), N=.N,
                                      SE.drymass=sd(drymass)/sqrt(.N),
                                      plusminusCI95=abs(qt(.025, .N-1))*sd(drymass)/sqrt(.N) ),
                               by=list(N, P)]

# x11() #or quartz() (windows and mac, respectively)
par(cex.axis=1.5, mar=c(6,6,1,1))
agg.plantGrowth[, blankplot(1:length(mean.drymass)+c(-.2, .2),
                            seq(0, max(mean.drymass+SE.drymass), length.out=length(mean.drymass))  )]

example(adderrorbars)
