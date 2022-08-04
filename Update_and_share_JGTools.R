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
detach("package:JGTools", unload = TRUE)

install_github("jg44/JGTools", upgrade = TRUE)
library(JGTools)

?adderrorbars
example("adderrorbars")
detach("package:stringr", unload = TRUE)

devWin <- function(height=8, width=9){
  if (Sys.info()[1]=="Windows") x11(height=height, width=width) else
    quartz(height=height, width=width)
}
devWin(8,15)
par(mfrow=c(1,2))
library(data.table)
library(JGTools)
data(plantGrowth)
example("adderrorbars")
plantGrowth <- as.data.table(plantGrowth)
# use data.table to aggregate data by treatment and calculate useful descriptive stats.
agg.plantGrowth <- plantGrowth[, list(mean.drymass=mean(drymass), sd=sd(drymass), N=.N,
                                      SE.drymass=sd(drymass)/sqrt(.N),
                                      plusminusCI95=abs(qt(.025, .N-1))*sd(drymass)/sqrt(.N) ),
                               by=list(N, P)]

# x11() #or quartz() (windows and mac, respectively)
