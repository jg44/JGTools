# load common libraries and source specific functions

# REPLACES local loadPackagesFunctions.r
#Rather run source("https://raw.githubusercontent.com/jg44/JGTools/master/lib/loadPackagesFunctions_main.R")


# install devtools and JGTools
#install.packages("devtools", update=TRUE)
library(devtools)
#R.version

# install_github("jg44/JGTools", upgrade = TRUE)

.libraryInstallorLoad <- function(x) {
    if (!any(dimnames(installed.packages())[[1]]==eval(x))) install.packages(eval(x))
    library(eval(x), character.only=TRUE)
}

.libraryInstallorLoad("devtools")
#.libraryInstallorLoad("JGTools")
#.libraryInstallorLoad("ProjectTemplate")
.libraryInstallorLoad("data.table")
.libraryInstallorLoad("vegan")
.libraryInstallorLoad("iNEXT")
.libraryInstallorLoad("dplyr")
.libraryInstallorLoad("ggplot2")
.libraryInstallorLoad("clipr")
.libraryInstallorLoad("knitr")
.libraryInstallorLoad("RCurl")
.libraryInstallorLoad("igraph")
.libraryInstallorLoad("data.table")


# bre additional
.libraryInstallorLoad("ggplot2")
.libraryInstallorLoad("car")
.libraryInstallorLoad("agricolae")
.libraryInstallorLoad("multcomp")
.libraryInstallorLoad("lme4")
.libraryInstallorLoad("lmerTest")
.libraryInstallorLoad("emmeans")
.libraryInstallorLoad("knitr")
.libraryInstallorLoad("kableExtra")

options(knitr.table.format = "pipe")

# load other functions from Dropbox
#source(sourceDropbox("https://www.dropbox.com/s/xgbv7suucwf57xq/functionlist.r?dl=0"))
#source(sourceDropbox("https://www.dropbox.com/s/x85by9r4ho91mh3/markdown_html_functions.R?dl=0"))


source("https://raw.githubusercontent.com/jg44/JGTools/master/manyFunctions/functionlist.r")
source("https://raw.githubusercontent.com/jg44/JGTools/master/manyFunctions/devpdf_active.r")
source("https://raw.githubusercontent.com/jg44/JGTools/master/manyFunctions/markdown_html_functions.r")




libs <- sort(loaded_packages()$package)
vers <- c()
for (i in libs){ vers <- c(vers, as.character(packageVersion(eval(i)))) }


cat(paste0("## R version, latest run: ", R.version.string, "\n\n## Today:  ", date(),
           "\n\nLoaded packages:\n\n"), file="R.version.latestused.md")
cat(kable(data.frame(libs, vers)), file="R.version.latestused.md", append=TRUE, sep="\n")


print(data.frame(libs, vers))
print(paste0("R version, latest run: ", R.version.string))
print(paste0("Today: ", date()))

.bu("README.md")

print("See editable copy here: ./lib/loadPackagesFunction_editable.R")
url <- "https://raw.githubusercontent.com/jg44/JGTools/master/lib/loadPackagesFunctions_main.R"
download.file(url, "./lib/loadPackagesFunction_editable.R")
