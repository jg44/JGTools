# install.packages("devtools")
#install.packages("devtools", lib = Sys.getenv("R_LIBS_USER"))

library("devtools")
 #devtools::install_github("klutometis/roxygen", lib = Sys.getenv("R_LIBS_USER"))
 #devtools::install_github
library(roxygen2)


# create_package("JGTools")
setwd("C:/Users/jrg1035/GitProjects/JGTools/")

document()

#
# devtools::install("JGTools", upgrade="always")
#
# install.packages("JGTools",dependencies=TRUE,repos= NULL, type="source")
# library(JGTools)
# update_packages("JGTools")

