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
document()
detach("package:JGTools", unload = TRUE)

install_github("jg44/JGTools", upgrade = TRUE, force=TRUE)
library(JGTools)

# cntl-shift-F10 to restart R
?adderrorbars
example("adderrorbars")


