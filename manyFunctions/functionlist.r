# updated 2022.12.30 # C:/Users/jrg1035/GitProjects/JGTools/manyFunctions/functionlist.r

# ggplot2 functions
#browseURL(getwd())
# .wk function (knitr, adds content wrapped in kable to file mdFil --------

.wk <- function(content, mdFilename = NULL, kab = TRUE) {

  require("knitr")

  if (is.null(mdFilename)) {
    if (exists("mdFilename", envir = .GlobalEnv)) {
      mdFilename <- get("mdFilename", envir = .GlobalEnv)
    } else {
      mdFilename <- "keep"
    }
  }

  filename <- file.path("html", paste0(mdFilename, ".Notes.md"))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  # Prepare content to write
  out <- if (kab) as.character(kable(content)) else as.character(content)
  full_out <- c("", "", out, "", "")

  # Write to file
  con <- file(filename, open = "a", encoding = "UTF-8")
  on.exit(close(con))  # Ensure connection is closed even if error occurs
  writeLines(full_out, con = con, sep = "\n", useBytes = TRUE)

  # Also write to console
  cat(paste(full_out, collapse = "\n"), "\n")
  cat("Saved to:", filename, "\n")
}

# .wc function (knitr, adds content text file mdFilename --------

.wc <- function(content, mdFilename = NULL, kab = FALSE) {

  require("knitr")

  if (is.null(mdFilename)) {
    if (exists("mdFilename", envir = .GlobalEnv)) {
      mdFilename <- get("mdFilename", envir = .GlobalEnv)
    } else {
      mdFilename <- "keep"
    }
  }

  filename <- file.path("html", paste0(mdFilename, ".Notes.md"))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  # Prepare content to write
  out <- if (kab) as.character(kable(content)) else as.character(content)
  full_out <- c("", "", out, "", "")

  # Write to file
  con <- file(filename, open = "a", encoding = "UTF-8")
  on.exit(close(con))  # Ensure connection is closed even if error occurs
  writeLines(full_out, con = con, sep = "\n", useBytes = TRUE)

  # Also write to console
  cat(paste(full_out, collapse = "\n"), "\n")
  cat("Saved to:", filename, "\n")
}




.wf <- function(content, fullpath=FALSE, mdFilename = NULL) {
  require("knitr")

  if (is.null(mdFilename)) {
    if (exists("mdFilename", envir = .GlobalEnv)) {
      mdFilename <- get("mdFilename", envir = .GlobalEnv)
    } else {
      mdFilename <- "keep"
    }
  }

  filename <- file.path("html", paste0(mdFilename, ".Notes.md"))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)


  # Open file connection in append mode
  con <- file(filename, open = "a", encoding = "UTF-8")

  if (!fullpath) figpath <- paste0("![](images/", content, ".png)") else figpath <-
    paste0("![](", content, ".png)")

  figpath <- gsub("\\.png\\.png", ".png", figpath)

  # Write with proper spacing
  writeLines(c("", "", as.character(figpath), "", ""),
             con = con, sep = "\n", useBytes = TRUE)
  close(con)

  # Also write to console
  cat(paste0(figpath, collapse = "\n"))
  cat("\nSaved to: ", filename)
}



themeJG <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.text.y = element_text(margin = margin(r = 10)),  # Adjust space for y-axis text
      axis.title.y = element_text(margin = margin(r = 20, l = 20)),  # Increase space for y-axis label
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.line.x.bottom = element_line(color = "black"),  # Bottom axis line
      axis.line.y.left = element_line(color = "black"),  # Left axis line
      axis.ticks = element_line(color = "black"),  # Make tick marks visible
      axis.ticks.length = unit(0.25, "cm"),  # Lengthen axis ticks
      axis.line.x.top = element_blank(),  # Remove top line
      axis.line.y.right = element_blank(),  # Remove right line
      panel.border = element_blank()  # Remove default border
    )
}

.getmd <- function(whichfile=NULL, path="./html", pattern = "^[0-9][0-9].+.md") {
  # list files starting with ## and ending in .md
  allfiles <- list.files(path=path, pattern=pattern)
  if (!is.null(whichfile)) return(paste(path, allfiles[whichfile], sep="/")) else return(allfiles)
}


.renumberFiles <- function(whichfile=NULL, path="./html", pattern = "^[0-9][0-9].+.md"){
  # renumber .md files in the html folder
  allfiles <- .getmd(path=path, pattern=pattern)
  for (i in 1:length(allfiles)){
    new <- paste0(path, "/", formatC(i, width = 2, format = "d", flag = "0"), "_", substring(allfiles[i], 4,100))
    file.rename(paste0(path, "/", allfiles[i]), new)
  }

}

#require(JGTools)
require(data.table)

R.version
#.gitbash()

.getProjName <- function(){
  require(rstudioapi)
  tmp <- strsplit(rstudioapi::getActiveProject(),"/")[[1]][length(strsplit(rstudioapi::getActiveProject(),"/")[[1]])]
  return(tmp)
}

.graphwindow <- function(height=6, width=6, mar=c(6,6,1,1), xpos=500, ypos=150){
    if (.Platform$OS.type == "windows") x11(height=height, width=width, xpos=xpos, ypos=ypos) else
        quartz(height=height, width=width)
    par(mar=mar)
}

# .devpdf function --------------------------------------------------------

# .devpdf <- function(file, pdfPath="./graphs", openfile=FALSE, overwrite=FALSE, caption=NULL, powerpoint=TRUE, slidetitle=NULL,
#                     png=TRUE, tf=NULL, meta=TRUE, NotesInput=FALSE, Notes="", history=TRUE,
#                     pathtoMDeditor="C:/Users/jrg1035/AppData/Local/Markdown Monster/MarkdownMonster.exe"){
#   require("rstudioapi")
#   tf <- rstudioapi::getActiveDocumentContext()$path
#   projName <- strsplit(rstudioapi::getActiveProject(),"/")[[1]][length(strsplit(rstudioapi::getActiveProject(),"/")[[1]])]
#   projNamePath <- paste0("./",substring(tf, nchar(rstudioapi::getActiveProject())-nchar(projName)+1, nchar(tf)))
#
#   grep("/", rstudioapi::getActiveProject())
#   if (!(dir.exists(pdfPath))) dir.create(pdfPath)
#   if (is.null(pdfPath)) pdfPath=getwd()
#   #if ((meta) && (is.null(tf))) tf<-.getfile()
#   file <- .clean(file)
#   ff<-paste(file,".pdf",sep="")
#   ff<-gsub('//', '/', ff)  ## interior spaces
#   ff<-gsub('.pdf.pdf', '.pdf', ff)
#   if (!any(list.dirs()=="./notesGraphAnnot")) dir.create("./notesGraphAnnot") #else print("")
#   #overwrite <- FALSE
#   annotationsPath <- "./html/"
#   if ((overwrite) || (!(file.exists(paste0(pdfPath, "/", ff))))) dev.copy2pdf(file=paste0(pdfPath, "/",ff)) else
#     if (!overwrite) {
#       print("File already exists. Opening existing file. Or choose overwrite=TRUE")
#       #ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#       #dev.copy2pdf(file=ff)}
#       browseURL(paste0(pdfPath, "/", ff))
#     }
#   if (openfile) browseURL(paste0(pdfPath, "/", ff))
#   if (png) {
#     #ff <- "test7.pdf"
#     pngfile <- .pdf2png(paste0(pdfPath, "/", ff))
#   }
#   if ((meta) && (tf!="n")) {
#     sink(paste0(annotationsPath, ff, ".annot.md"))
#     cat("\n### Script file: ")
#     cat(paste0("\n", projNamePath))
#     cat("\n")
#
#     if (NotesInput) {
#       cat("Approximate line: ", readline("Approx. line? "))
#       cat("\nNotes: ", readline("Notes? "))
#       cat("\nFigure #: ", readline("Figure #: "))
#     }
#
#     bbname <- paste0("images/",strsplit(pngfile, "\\\\")[[1]][length(strsplit(pngfile, "\\\\")[[1]])])
#     cat("\nTimestamp: ", date(), "\n")
#     cat("\n### ", .clean(file))
#     cat(paste0("\nCaption: ", caption,'\n'))
#     cat("\n![", bbname, "](", bbname,")", sep="")
#     cat("\n\n###### ", gsub("\\./", paste0(getwd(), "/"), ff), sep="")
#     #cat("\n\n[", .clean(file), "_png]:", sep="")
#     #cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     #cat("\n\n[", .clean(file), "]:", sep="")
#     #cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat(paste0("\n", "Notes: ", Notes))
#     cat("\n------\n")
#     .sinkall()
#     browseURL(paste0(annotationsPath, ff, ".annot.md"))
#   }
#   if (history) {
#     sink(paste0(getwd(), "/ProjectHistoryFile.md"), append = TRUE)
#     cat(paste0("\n### Script file: ", tf))
#     cat("\n")
#     cat("\nTimestamp: ", date(), "\n")
#
#     cat("\n###", .clean(file))
#     cat(paste0("\nCaption: ", caption,'\n'))
#     cat("\n![", .clean(file), "](/images/",.clean(file), ".png)", sep="")
#     cat("\n\n###### ", gsub("\\./", paste0(getwd(), "/"), ff), sep="")
#
#     # cat("\n## ", .clean(file))
#     # cat("\n[![", .clean(file), "][", .clean(file),"_png]][",.clean(file), "]", sep="")
#     # cat("\n", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     # cat("\n\n[", .clean(file), "_png]:", sep="")
#     # cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     # cat("\n\n[", .clean(file), "]:", sep="")
#     # cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat(paste0("\n", "Notes: ", Notes))
#     cat("\n------\n")
#     .sinkall()
#   }
#
#   #make Powerpoint
#   if (powerpoint) {
#     if (is.null(caption)) caption <- .clean(file)
#     if (is.null(slidetitle)) slidetitle <- .clean(file)
#     sink(paste0(getwd(), "/powerpoint.md"), append = TRUE)
#     #cat(paste0("\n### Script file: ", tf))
#     cat("\n\n# ", slidetitle, sep="")
#     cat("\n![", caption, "](/images/",.clean(file), ".png)", sep="")
#     cat("\n")
#     .sinkall()
#   }
# }

# .gitbash function -------------------------------------------------------

.gitbash <- function(wd=getwd(), gitpath="C:/Program Files/Git/git-bash.exe"){
    if (wd=="rt") wd <- "C:/Users/jrg1035/GitProjects/JGTools/"
    print(wd)
    wd1 <- list.dirs()[grep('.git+$', list.dirs())]
    (wduse <- gsub("/.git", "", wd1))
    print(wd1)
    wduse <- wd
    system(wait=FALSE, eval( paste0("\"", gitpath, "\" --cd=", wduse)))
    #system('"C:/Program Files/Git/git-bash.exe" --cd="C:/Users/jrg1035/GitProjects/r_ascc.insects.cz"', wait=FALSE)

}

# .gitbash()
.lastfile <- c("MASTERFILE.R")
#getwd()

.open <- function(file="MASTERFILE.R") {
    list.files(recursive=TRUE)
    if (is.logical(file)) {
        if (file)  print(list.files(pattern=".r$", ignore.case = TRUE, recursive = TRUE))} else browseURL(file)
        require("rstudioapi")
        tf <- rstudioapi::getActiveDocumentContext()$path
        assign(".lastfile", tf, envir = .GlobalEnv)
    }

.openlast <- function(){
    if (exists(".lastfile")) browseURL(.lastfile) else print(".lastfile not defined.")
    require("rstudioapi")
    tf <- rstudioapi::getActiveDocumentContext()$path
    assign(".lastfile", tf, envir = .GlobalEnv)
}

.updateEditFileDesc <- function(df, columnmatch, open=TRUE, sortby=c("modified", "filename", "author")[1]){
  if (!dir.exists("./fileMetadata/")) dir.create("fileMetadata")
  if  (!file.exists("./fileMetadata/scriptDescriptions.csv")) write.csv(row.names = FALSE, file="./fileMetadata/scriptDescriptions.csv",
    structure(list(filename____________________________________________ = character(0),
    author________ = character(0), Desc______ = character(0), Modified______ = character(0)), row.names = integer(0), class = "data.frame"))
    tmp <- read.csv(file="./fileMetadata/scriptDescriptions.csv", na.strings = "abc")
    tmp
    tmpLf <- list.files(pattern="\\.r$|\\.rmd$", recursive=TRUE, ignore.case = TRUE)
    addLf <- tmpLf[!(tmpLf %in% tmp$filename)]
    if (length(addLf)!=0) {
        tmpAdd <- data.frame(filename=addLf, author=rep(".", length(addLf)), Desc=rep(".", length(addLf)))
        names(tmpAdd) <- names(tmp)[1:ncol(tmpAdd)]
            tmp <- rbind(tmp[,-grep("modified", tolower(names(tmp)))], tmpAdd)
    }
    tmpModified <- data.frame(filename=tmpLf, Modified=file.mtime(list.files(pattern="\\.r$|\\.rmd$", recursive=TRUE, ignore.case = TRUE)))
    tmp_new <- merge(tmp[,1:3], tmpModified, by.x=names(tmp)[grep("filename", names(tmp))],
          by.y=names(tmpModified)[grep("filename", names(tmpModified))], all=TRUE)

    if (sortby=="modified") ord <- order(tmp_new$Modified, decreasing=TRUE)
    if (sortby=="filename") ord <- order(tmp_new[,1], decreasing=FALSE)
    if (sortby=="author") ord <- order(tmp_new[,2], decreasing=FALSE)

    tmp_ordered <- tmp_new[ord,]
    write.csv(file="./fileMetadata/scriptDescriptions.csv", tmp_ordered, row.names = FALSE)
    if (open) browseURL("./fileMetadata/scriptDescriptions.csv")
}

.updateEditFileDescData <- function(df, columnmatch, open=TRUE, sortby=c("modified", "filename", "author")[1]){
    if (!dir.exists("./fileMetadata/")) dir.create("fileMetadata")
    if  (!file.exists("./fileMetadata/dataDescriptions.csv")) write.csv(row.names = FALSE, file="./fileMetadata/dataDescriptions.csv",
                                                                   structure(list(filename____________________________________________ = character(0),
                                                                                  author________ = character(0), Desc______ = character(0), Modified______ = character(0)), row.names = integer(0), class = "data.frame"))
    tmp <- read.csv(file="./fileMetadata/dataDescriptions.csv", na.strings = "abc")
    tmpLf <- list.files(pattern="\\.csv$|\\.xls$\\.xlsx$", recursive=TRUE, ignore.case = TRUE)
    addLf <- tmpLf[!(tmpLf %in% tmp$filename)]
    if (length(addLf)!=0) {
        tmpAdd <- data.frame(filename=addLf, author=rep(".", length(addLf)), Desc=rep(".", length(addLf)))
        names(tmpAdd) <- names(tmp)[1:ncol(tmpAdd)]
        tmp <- rbind(tmp[,-grep("modified", tolower(names(tmp)))], tmpAdd)
    }
    tmpModified <- data.frame(filename=tmpLf, Modified=file.mtime(list.files(pattern="\\.csv$|\\.xls$\\.xlsx$", recursive=TRUE, ignore.case = TRUE)))
    tmp_new <- merge(tmp[,1:3], tmpModified, by.x=names(tmp)[grep("filename", names(tmp))],
                     by.y=names(tmpModified)[grep("filename", names(tmpModified))], all=TRUE)

    if (sortby=="modified") ord <- order(tmp_new$Modified, decreasing=TRUE)
    if (sortby=="filename") ord <- order(tmp_new[,1], decreasing=FALSE)
    if (sortby=="author") ord <- order(tmp_new[,2], decreasing=FALSE)

    tmp_ordered <- tmp_new[ord,]
    write.csv(file="./fileMetadata/dataDescriptions.csv", tmp_ordered, row.names = FALSE)
    if (open) browseURL("./fileMetadata/dataDescriptions.csv")
}

# file.mtime(list.files(pattern="\\.csv$|\\.xls$\\.xlsx$", recursive=TRUE, ignore.case = TRUE))


.readFileDescriptions <- function(sortby=c("modified", "filename", "author")[1]){
    .updateEditFileDesc(open=FALSE, sortby=sortby)
    tmp <- read.csv(file="./fileMetadata/scriptDescriptions.csv")
    sink("./fileMetadata/FileDescriptions.md")
    cat("## If descriptions are missing, run the .updateEditFileDesc() function in R.\n")
    cat("#### Make sure to save and close scriptDescriptions.csv\n\n")
    cat(paste0("(Sorted by ", sortby, " column).\n"))
    cat(knitr::kable(tmp, format="html"))
    #?kable
    cat("\n\n\n")
    .sinkall()
    browseURL("./fileMetadata/FileDescriptions.md")
}

.readFileDescriptionsData <- function(sortby=c("modified", "filename", "author")[1]){
    .updateEditFileDescData(open=FALSE, sortby=sortby)
    tmp <- read.csv(file="./fileMetadata/dataDescriptions.csv")
    sink("./fileMetadata/dataDescriptions.md")
    cat("## If descriptions are missing, run the .updateEditFileDescData() function in R.\n")
    cat("#### Make sure to save and close scriptDescriptions.csv\n\n")
    cat(paste0("(Sorted by ", sortby, " column).\n"))
    cat(knitr::kable(tmp, format="html"))
    #?kable
    cat("\n\n\n")
    .sinkall()
    browseURL("./fileMetadata/dataDescriptions.md")
}

.whichquantile <- function(test, dist, decr=FALSE){
  q <- c()
  for (i in 1:length(test)){
    q <- c(q, mean(which(sort(c(test[i], dist), decreasing = decr)==test[i]) ))
    qP <- q/length(dist)
  }
  return(qP)
}

.clip <- function(x=1, Rtoclipboard=TRUE, header=FALSE){
  if (Rtoclipboard) {
    write.table (x, "clipboard-128", sep="\t")
    #print("Written to clipboard.")
  }
  if (!Rtoclipboard) {
    read.table ("clipboard", sep="\t", header=header)
    #print("Read from clipboard.")
  }
}


.glu <- function(x){
  lowuse <- c(c(93,90,87), c(93,90,87)-10, c(93,90,87)-20, c(93,90)-30,0 )
  lge <- data.frame(  mark = c(rep(LETTERS[c(1:4, 6)], each=3))[-c(1,14:15)],
                      low = lowuse,
                      high = c(100, lowuse[1:11]-.0001))
  lge$mark <- paste0(lge$mark, c("", "-", "+", "", "-","+", "", "-","+", "", "-" ))

  for (i in 1:12){
    if (between(x, lge$low[i], lge$high[i])) return(lge$mark[i])
  }}

.gradelookup <- function(x){
  marks <- c()
  for (j in 1:length(x)){
    marks <- c(marks, ifelse(is.null(.glu(x[j])), "OOB", .glu(x[j])) )
  }
  return(marks)
}

.lettergrades <- .gradelookup
#.lettergrades(54)

.fileinc <- function(x, path="./graphs/", ft='.png', digs=2, inc=1){
  x <- gsub(".png", "", x)
  use=NULL
  lf <- list.files(path)
  if (length(lf)>0){
    tmpnames <- unlist(lapply(strsplit(list.files(path), "_"), "[[",1))
    use <- grep(x, tmpnames)
    }
  if (length(use)>0) {
    tmpnum <- gsub(".*?([0-9]+).*", "\\1", lapply(strsplit(lf[use], "_"), "[[", 2))
    digs <- nchar(tmpnum)
    inc <- max(as.numeric(tmpnum))+1
  }
  incname <- paste0(x, "_", sprintf(paste0("%0", digs, "d"), inc), ft)
  return(incname)

}





# .fileinc(x="BCI_logrankAbundance")

.pngincrement <- function(x, path="./graphs/", ft='.png', digs=2, inc=1){
  if (!dir.exists(path)) dir.create(path)
  tmp <- paste0(path, .fileinc(x, path, ft, digs, inc))
  cat(paste0("![](",tmp,")\n"))
  dev.print(png, file = tmp, width = 1024/1.6, height = 768/1.6)
}
.pnginc <- .pngincrement


.addsubplotletter <- function(lett, separator=")", location="topleft", inset=c(-.05, 0)){
  par(new=TRUE); .plot(1,1); legend(legend=paste0(lett, separator), x=location, bty="n", cex=2, inset=inset)
}

# .pdf2png(ff)
# list.dirs("./html")
# (!dir.exists("html/images"))


.pdf2png <- function(pdffile){

  if (!dir.exists("html")) dir.create("html")
    if (!dir.exists("html/images")) dir.create("html/images")
  pngfile <- gsub("graphs", "html/images", pdffile)
  pdffile <- gsub("/", "\\\\", pdffile)
  pngfile <- gsub("/", "\\\\", pngfile)

  pngfile <- gsub(".pdf", ".png", pngfile)
  x <- paste0('"C:\\Program Files\\ImageMagick-7.1.1-Q16-HDRI\\magick.exe" -density 300 "', pdffile, '" -resize 40% "', pngfile)
  system(x)
  print(pdffile)
  print(pngfile)
  return(pngfile)
}

#
# .pdf2png("C:/Users/jrg1035/Dropbox/R/Projects2019Win/Proj_SirexKatieFinalPush/R_SirexPaperKT2017/graphs/tunnel_length_by_position_and_larval_stage.pdf")

.getCaption <- function(fileName, tag="Caption:"){

  breakFun <- function(x){
    #function to replace empty lines with newline.
    if(nchar(x) == 0){
      return("\n\n") #double newline to give same space as in the .md-file
    } else {
      return(x)
    }
  }

  storeLines <- readLines(fileName)
  # cat(paste0(lapply(storeLines, FUN=function(x) breakFun(x)), collapse=""))
  caption <- gsub(tag, "", storeLines[substring(storeLines, 1,nchar(tag))==tag])
  if (tag == "Caption:" & length(caption)==0) caption <- paste("No caption found in", fileName)
  if (tag != "Caption:" & length(caption)==0) caption <- "\n"

  return(trimws(caption))
}



.listSorted <- function(whichpngs=NULL, dir=paste0(getwd(), "/html/images/"), pat="*.png"){
  tmpfl <- list.files(dir, pattern = pat)
  tmpflDate <- c(); captions <- c(); notes <- c()
  for (i in 1:length(tmpfl)){ #i=5
    tmpflDate <- c(tmpflDate, file.mtime(paste0(dir, tmpfl[i])))
    #print(file.mtime(paste0(dir, tmpfl[i])))

    capFile <- gsub(".png", ".pdf.metadata.md", gsub("html/images/", "", paste0(dir, tmpfl[i])))
    captions <- c(captions, .getCaption(fileName = capFile))
    notes <- c(notes, .getCaption(fileName = capFile, tag = "Notes:"))

  }

  fileList <- data.frame(filename=tmpfl, Last_Modified=as.POSIXct(tmpflDate, origin = "1970-01-01"), FigNum=0, captions, Notes=notes)
  fileList <- fileList[order(fileList$Last_Modified),]

  if (is.null(whichpngs)) whichpngs <- 1:nrow(fileList)
  fileList[whichpngs,]$FigNum <- 1:length(whichpngs)

  fileList_out <- fileList[whichpngs, ]


  return(fileList_out)
}

# .listSorted()
# .listSorted(c(4,2))

# pngfilename <- .listSorted(3)
.addCaption <- function(pngfilename){
  if (is.data.frame(pngfilename)) pngfilename <- as.character(pngfilename$filename)
  for (i in 1:length(pngfilename)){
    browseURL(paste0(getwd(),"/", gsub(".png", ".pdf.metadata.md", gsub("html/images", "", pngfilename[i]))))
    Sys.sleep(.75)
  }
}

# .addCaption(.listSorted(3))
# .listSorted(c(2,0,0,1))
# .listSorted()
#
# str(file.info(paste0(dir, "deliocourt_ln_beechnonbeechall.png")))


#
# .addCaption <- function(caption, file, dir=paste0(getwd(), "/html/images/"))){
#     tmpfile <- paste0(dir, file)
#     if file.exists(tmpfile) {
#         captionFile <- gsub(".png", ".md", tmpfile )
#     sink(, append = TRUE)
#     }
#









# .getCaption(paste0(getwd(), "/deliocourt_ln_beechnonbeechall.pdf.metadata.md"), "Notes:")



.makeMD <- function(listPickSort=NULL, filename, title=NULL, notes=""){
  if (is.null(listPickSort)) tmpList <- .listSorted() else tmpList <- .listSorted(listPickSort)
  if (is.null(title)) title <- filename
  sink(paste0("./html/", filename, ".Notes.md"))
  cat("<link rel=\"stylesheet\" href=\"RNotes.css\">\n")
  cat("\n## ", title, "\n", format(Sys.Date(), "%Y %b %d"))
  cat("\n\n### ", notes, "\n-------\n")
  for (i in 1:nrow(tmpList)){
    cat(paste0("\n### Figure ", tmpList[i,]$FigNum, ". ", tmpList[i,]$captions, "\n<br />\n"))
    cat(paste0("\n\n![", tmpList[i,]$filename,"](./images/", tmpList[i,]$filename,")"))
    cat(paste0("\n\n### Notes: ", tmpList[i,]$Notes), "\n------\n\n")
  }
  sink()
  browseURL(paste0("./html/", filename, ".Notes.md"))
  return(tmpList)
}

# .listSorted(c(3,5, 4, 1:2))
# .makeMD(c(3,5, 4, 1:2), "BBD FIA, round 2", "BBD next round looking")
#








.ProjHist <- function(x, title = NULL, date=FALSE){
  sink(paste0(getwd(), "/ProjectHistoryFile.md"), append = TRUE)
  if (date && !is.null(title)) cat(paste0("\n### ", title, ", ", format(Sys.time(), "%Y %M %d"))) else if (date)
    cat(paste0("\n###", " ", format(Sys.time(), "%Y %M %d")))
  cat(paste0("\n", x, "\n"))
  sink()
}



.ph <- .ProjHist

.tf <- function() { rstudioapi::getActiveDocumentContext()$path}
.addFigHTML<-function(pdffilename, targethtml=target, Caption="no caption",...){
  require(R2HTML)
  require(readtext)
  graphs <- paste0(getwd(), "/graphs/")
  basename <- gsub(".pdf", "", pdffilename)
  HTMLhr(file = HTMLGetFile(), Width = "100%", Size = "6",
         CSSclass=NULL, append=TRUE)
  HTML(paste0("<h1>",paste0(basename, ".pdf"),"</h1>"), file=target)
  HTML(paste0("<h3>",paste0("file:///", graphs,basename, ".pdf"),"</h3>"), file=target)
  HTMLInsertGraph(GraphFileName=paste0(graphs,basename, ".png"),
                  Caption=Caption, GraphBorder=1,
                  Align="center", WidthHTML=600, HeightHTML=NULL,
                  file=HTMLGetFile(), append=TRUE)
  metadat <- paste0(graphs,basename, ".pdf.metadata.txt")
  if (file.exists(metadat)){
    textadd <- readtext(metadat)
    txxt <- gsub("\n", "<br>", textadd$text)
    HTML(txxt)
  }
}

.hist<-function(x=rnorm(100, 10, 1), marg=TRUE, breaks="Sturges", ...) {
  if (marg) {par(mar=c(6,7.5,1,1))}
  hist(x, xlab="", ylab="", axes=F, col="skyblue3", main="", breaks=breaks)
}

.pointRaster <- function(x, y, pchIcons=NULL, whichones,  cexIcons=1, offset=.05){
  library(png)
  require(grid)
  treeIcons <- list(
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage0standing.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage1standing.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage2standing.png", TRUE) ,
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage3standing.png", TRUE) ,

    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage0felled1.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage1felled1.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/stage2felled1.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/OAK.png", TRUE),
    readPNG("C:/Users/jrg1035/Dropbox/GrantsAndFunding/2019/EM_SPB_2019Maybe/TreeIcons/gap.png", TRUE))
  if (is.null(pchIcons)) pchIcons <- treeIcons
  os  <-  cexIcons*offset
  for (i in 1:length(x)){
    rasterImage(pchIcons[[whichones[i]]], x[i]-os, y[i]-os, x[i]+os, y[i]+os)
  }
}





.anyshape <- function(sx=0, sy=0, sides, rotation, radius=1, makeplot=FALSE, col="light blue"){
    xx=seq(0,2*pi, .01)
    if (makeplot) .plot(radius*cos(xx), radius*sin(xx))
    x <- seq(pi/2,2*pi+pi/2, 2*pi/sides)

    if (sides %%2 == 0) { x <- seq(pi/2 - pi/sides, 2*pi+pi/2 - pi/sides,  2*pi/sides)}
    # sides=3

    polygon(sx+radius*cos(x), sy+radius*sin(x), col=col)
    #points(radius*cos(x), radius*sin(x), col=1:sides, pch=19, cex=2)
}



.bellcurve <- function(mean=0, sd=1, w=3.5){
  x <- seq(mean-w*sd, mean+w*sd, length.out = 1000 )
  y <- dnorm(x, mean=mean, sd=sd)
  return(data.frame(x,y))
}

.plotbell <- function(mean=0, sd=1, w=3.5, col=4, lwd=2){
  points(.bellcurve(mean, sd, w), col=col, lwd=lwd, type="l")
}

.bL <- function() {box(bty="L", lwd=2)}

.tolength <- function(xx, l){
    tmp <- rep(xx, l%/%length(xx))
    if (length(tmp)<l) tmp <- c(tmp, xx[1:(l%%length(xx))])
    #print(tmp)
    return(tmp)
}



.shapes <- function(x, y, sides=1000, size=10, rot=0, lwd=1, lty=1, fill=0, border=1){
    l<-length(x)
    rot <- .tolength(rot, l)
    sides <- .tolength(sides, l)
    sides[-which(sides %in% 1:15)]<-1001
    rot[sides==1] <- 180
    rot[sides==2] <- 45
    sides[sides==1] <- 3
    sides[sides==2] <-4
    sides[sides==0] <- 1000
    half <- rep(2, l)
    half[sides %in% 16:19] <- 1
    rot[sides==16] <- 45
    rot[sides==17] <- 90
    rot[sides==18] <- 180
    rot[sides==19]  <- 270
    sides[sides %in% 16:999]<-1000
    #print(sides)
    print(rot)
    for (i in 1:l) {
        shape(x[i],y[i], sides[i],.tolength(size,l)[i],  rot[i],  .tolength(lwd,l)[i],
              .tolength(lty,l)[i], .tolength(fill, l)[i], .tolength(border,l)[i], half[i])
    }

}


.shape <- function(x, y, sides=1000, size=10, rot=0, lwd=1, lty=1, fill=0, border=1, half=2){
    tur <- sides/2 + sides*rot/180
    print(rot)
    if (sides %in% 16:19) tur <- 2*rot/180

    x1 <- cos(seq(tur*pi/sides,half*pi+tur*pi/sides, length.out = sides+1)[-(sides+1)])*size/10
    y1 <- sin(seq(tur*pi/sides, half*pi+tur*pi/sides, length.out = sides+1)[-(sides+1)])*size/10
    # data.frame(x1,y1)[1,]
    # points(x+x1, y+y1)
    polygon(x+x1, y+y1, lwd=lwd, lty=lty, col=fill, border=border)
    #points(x+x1, y+y1, col=1:6)
}

.characters <- function(characters)
.rotateNodes <-function(tree,nodes,polytom=c(1,2),...){
    n<-length(tree$tip.label)
    if(nodes[1]=="all") nodes<-1:tree$Nnode+n
    for(i in 1:length(nodes))
        tree<-rotate(tree,nodes[i],polytom)
    if(hasArg(reversible)) reversible<-list(...)$reversible
    else reversible<-TRUE
    if(reversible){
        ii<-which(tree$edge[,2]<=n)
        jj<-tree$edge[ii,2]
        tree$edge[ii,2]<-1:n
        tree$tip.label<-tree$tip.label[jj]
    }
    return(tree)
}

# install.packages("dplyr")
.pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}



.openRprofile <- function() {
    browseURL("C:/Program Files/R/R-4.2.1/etc/")
    print("Open as admistrator in notepad...")

}

.add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
    mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}


.setwdFileLoc <- function(tf) {
  tmp<- strsplit(tf, "/")[[1]]
  setwd(paste(tmp[-length(tmp)], sep="/", collapse="/"))
  return(getwd())
}



.read.csv.dt<-function(file, datadir= "./data/", sep=",", header=TRUE){
    require(data.table)
    ff<- "ajf/sgs/"
    if (length(grep("/", ff))==0) file <- paste0(datadir, file)
    tmp <- data.table(read.csv(file, sep=sep, header=header, fileEncoding="UTF-8-BOM"))
}

.ci95 <- function(x){
   error <- qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))
   return(c(mean(x)-error, mean(x)+error))
}

.trimSpaces <- function (x) gsub("^\\s+|\\s+$", "", x)


.fileNotes <- function(dirCode="./Code", drop=FALSE){  ## use this file to store metadata about all the project files.  Use .fileNav to navigate and open them.
require(data.table)
   tmp <- list.files(dirCode, pattern ='.R$|.Rmd$', ignore.case = TRUE)
   if (!exists("fileNotes")) fileNotes <- data.table(filenames=tmp, notes=gsub(".r$|.rmd$", "", tmp, ignore.case = TRUE), path=dirCode, notes2="")
   #update fileNotes
   tmp <-    list.files(dirCode, pattern ='.R$|.Rmd$', ignore.case = TRUE)[is.na(match( list.files(dirCode, pattern ='.R$|.Rmd$', ignore.case = TRUE), fileNotes$filenames))]
   if (length(tmp)>0) fileNotes <-
      rbind(fileNotes, data.table(filenames=tmp, notes=gsub(".r$|.rmd$", "", tmp, ignore.case = TRUE), path=dirCode, notes2="") )
   print(fileNotes)
}

.fileNav <- function(x="MASTERFILE", fileN=fileNotes){
    # function to open files listed in masterfile (must be named MF)
    if (!exists("fileN")) print("No fileNotes variable");
    wh <- grep(x, fileN$notes, ignore.case = TRUE)
    if (length(wh)==0) print("No match") else if
        (length(wh)>1) {
            cat("More than one matching file:\n\n " );
            print(fileN[wh,])} else {
               fn <- fileN[wh, paste0(path,"/",filenames)]
               browseURL(fn)
               }
}


# for (i in 1:nrow(fileNotes)){
#       if (!(fileNotes$filenames[i] %in% list.files(fileNotes$path[i]))) fileNotes$notes2<-
#             "missing or moved"
#          }
#
# assign("fileNotes", fileNotes[order(filenames),], envir = .GlobalEnv)
# # return()
# }
#
# .fileNav("1log")
#


.mf<-function(x="MASTERFILE", tmpMF=MF){
    # function to open files listed in masterfile (must be named MF)
    if (!exists("tmpMF")) print("Invalid masterfile")
    wh <- grep(x, names(tmpMF), ignore.case = TRUE)
    if (length(wh)==0) print("No match") else if
        (length(wh)>1) {
            cat("More than one matching file:\n\n " );
            print(tmpMF[grep(x, names(MF), ignore.case = TRUE)])} else
        .bu(MF[grep(x, names(tmpMF), ignore.case = TRUE)])
}

.get_user <- function(){
  env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
  unname(Sys.getenv(env))
}


.assign_with_metadata <- function(x, value, ..., pos = parent.frame(), inherits = FALSE)
{
  # x (MUST be in quotes) is the name of the variable to be saved as important
  # value is the variable or file that serves as the source
  # pos is the environment

  attr(value, "creator") <- get_user()
  attr(value, "time_created") <- Sys.time()
  more_attr <- list(...)
  attr_names <- names(more_attr)
  for(i in seq_along(more_attr))
  {
    attr(value, attr_names[i]) <- more_attr[[i]]
  }
  assign(x, value, pos = pos, inherits = inherits)
}


.rd.csv <- function(varname, src, makefile=NULL, notes=NULL){
    require(data.table)
    .assign_with_metadata(eval(varname), data.table(.cleannames(read.csv(src))), source=src, makefile=makefile, notes=notes, pos=globalenv())
}



.saveImportant <- function(){
  for (i in 1:length(.imptVars())) {saveRDS(eval(parse(text=.imptVars()[i])), eval(paste0("./data/", eval(.imptVars()[i]), ".rds" ) ))} }


#

#
# attributes(xx)
# .rd.csv("xx, "/Users/jeff/Dropbox/katie_data/MasterFile_larvaeall.csv", makefile = thisfile, notes = "larv17 is the authoritative larvae variable but may contain unusable records")
.imptVars<-function(printattr=FALSE, lss=ls(globalenv()), notesOnly=TRUE ) {
    impVar<-c()
    impattr<-list(); j=0
    for (i in 1:length(lss)) {
        tmp <- attributes( eval(parse(text=lss[i])) )
        if (!is.null(tmp$creator)) {
            j=1+j
            impattr[[j]] <- tmp
            impVar <- c(impVar, lss[i])
            }
    }
    if (printattr) {
        if (notesOnly) {
          #return(data.table(impVar, notes=sapply(impattr, "[[", "notes")))
          tmpImpt <- data.table(impVar, notes=sapply(impattr, "[[", "notes"))
          # spacer <- c()
          # for (i in 1:nrow(tt)){    spacer <- c(spacer, paste0(rep("=",60-nchar(tt$impVar[i])), collapse=""))}
          x <- paste0(tmpImpt$impVar, "::::",tmpImpt$notes)
          x
          ## Split into paragraphs and rem\nove the first three ones
          x <- unlist(strsplit(x, "\n[ \t\n]*\n"))
          ## Join the rest
          x <- paste(x, collapse = "\n\n")

          writeLines(strwrap(x, width = 80, exdent = 5,prefix = "imptvars   "))


        } else return(impattr) } else
                    return(impVar)
}


#.imptVars(T)

 # sapply(ls(), FUN=function(x) attributes(eval(parse(text=x))))
.libraryInstallorLoad("rstudioapi")
rstudioapi::getActiveDocumentContext()$path

#dirname(sys.frame(1)$ofile)

.write.csv<-function(df, wd=getwd(), file){
    write.csv(df, paste(wd, file, sep="/"))
}
.read.csv<-function(wd=getwd(), file){
    read.csv(paste(wd, file, sep="/"))
}

##

.roundup<-function(x, increm=1) {
    return(
    trunc(x / increm)*increm + ifelse((x %% increm)>0, increm, 0)
    )
}

# .bin(x, breakby=10)
.bin<-function(x, minx=NULL, maxx=NULL, breakby=NULL, breaks=NULL, breakcount=10)   {
        if (is.null(minx)) minx<-floor(min(x))
    if (is.null(maxx)) maxx<-ceiling(max(x))
      if (is.null(breaks)) {
        if (!is.null(breakby)) {
            breaks <- seq(minx, .roundup(maxx, increm=breakby) + ifelse(minx>0, minx,0), breakby)
            }
        if (is.null(breakby)) {
            breakby<-(maxx - minx )/breakcount
            breaks <- seq(minx, (.roundup(maxx,
                breakby)+minx)*breakby, breakby)
        }
      }
   index<-.bincode(x, breaks = breaks, include.lowest = TRUE, right=TRUE)
   range <- paste(breaks[-length(breaks)], breaks[-1], sep="-")
   midpoint <- breaks[-length(breaks)]+diff(breaks)/2
   return( list(data.frame(x, index, midpoint=midpoint[index], range=range[index]),
       data.frame(index=1:length(midpoint), midpoint, range) ) )
}

# .bin(x=1:13, breakby=3)
.readImportant <- function(datadir="./data/", overwrite=FALSE) {
  lf <- list.files(datadir, pattern=".rds")
  fn <- gsub(".rds", "", lf)
  for (i in 1:length(lf)){
    print(i)
    Sys.sleep(.1)
    if (!exists(fn[i])) assign(fn[i], readRDS(paste0(datadir, lf)[i]), envir = .GlobalEnv) else if (overwrite) assign(fn[i], readRDS(paste0(datadir, lf)[i]),envir = .GlobalEnv)
    if (exists(fn[i]) & !overwrite) print(paste0(fn[i], " already exists. Use overwrite = TRUE to overwrite."))
  }
}







.groups <- function(groups=3, N, letterslength=26, countchars=2) {
    reps<-trunc(N/groups) + N %% groups
    df<-data.frame(LETTERS[1:letterslength], LETTERS[1:letterslength])
    trts<-apply(expand.grid(df), 1, paste, collapse="")[1:groups]
    return(
        sample(c(rep(trts, each=N/groups),
    sample(trts, N %% groups, replace = TRUE)), N)
        )
}



.rbind0<-function(x, y){
    df<-data.frame(x=names(x), y=names(y))
    namez<-with(df, paste(x,y, sep="."))
    names(x)<-namez; names(y)<-namez
    return(rbind(x,y))
}


.ordercols<-function(namesord,x){
    m<-match(namesord,names(x))
    return(x[,c(m, (1:ncol(x))[-m])])
}

.capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

x <- "The Response Patterns of Arbuscular Mycorrhizal and Ectomycorrhizal Symbionts Under Elevated CO2: A Meta-Analysis"

# x=sitedeets$site1 caps=c(14)
.title<-function(x, caps=NULL, allcaps=NULL){
    x <- gsub("[\r\n]", " ", x)
    x <- gsub("^\\s+|\\s+$", "", x)
    x1 <- strsplit(x, " ")[[1]]
   if (is.null(caps)) print(as.data.frame(x1)) else {
    x1 <- tolower(x1)
    for (i in c(1, caps)) {x1[i] <- .capwords(x1[i], strict = TRUE)}
    if (!is.null(allcaps)) { for (i in allcaps){ x1[i]<-strsplit(x, " ")[[1]][i]}
    }
    x2 <- (paste(x1, collapse = " "))
    sink("outfile1.txt")
    cat(x2)
    sink()
    file.show("outfile1.txt")
    }}

.leadingzeros <- function(ct=NULL, prefix="", suffix="", addnum="0", sep="_", leading=2, start=1){
   #ct=1:14
   l <- max(length(prefix), length(suffix))

   if (l>1) ct <- start:(l+start)
   nums<-as.numeric(regmatches(ct, gregexpr("[0-9]+", ct)))
   m<-max(nchar(nums), leading)
   numschar <- as.character(nums)
   while (any(nchar(numschar) < m))
      numschar[nchar(numschar) < m] <-
      paste0(addnum, numschar[nchar(numschar) < m])

   if (all(nchar(prefix)>0)) prefix <- paste0(prefix, sep)
   if (all(nchar(suffix)>0)) suffix <- paste0(sep, suffix)

   if (length(prefix)!=1 & length(suffix)!=1 & length(prefix)!=length(suffix))
      warning("Prefix and suffix lengths differ. Shorter vector will be recycled.\n")

   if ((length(prefix)!=1 | length(suffix)!=1) & length(ct)>1)
      warning("ct is ignorned when either prefix or suffix vectors has more than one element.\n")

   return(paste(prefix, numschar, suffix, sep=""))
   # .leadingzeros(1:11, "tree")
   # .leadingzeros(1:11, suffix="tree")
   # .leadingzeros(1:11, suffix="fakedata", prefix="tree")
   # .leadingzeros(1:11, prefix=c('f', "fff"))
}



.leadingzerosOLD<-function(x, addchar="X", addnum="0"){
    nums<-as.numeric(regmatches(x, gregexpr("[0-9]+", x)))
    m<-max(nchar(nums))
    numschar <- as.character(nums)
    while (any(nchar(numschar) < m))
        numschar[nchar(numschar) < m] <-
            paste0(addnum, numschar[nchar(numschar) < m])
    sub("[A-Z]", "", x)
    charvec <- unlist(
        regmatches(tolower(x),
            gregexpr("[A-Z]+", x)
            )
        )
    m1 <- max(nchar(charvec))
    if (!is.null(addchar)) {
    while (any(nchar(charvec) < max(nchar(charvec))))
        charvec[nchar(charvec) < m1] <- paste0(charvec[nchar(charvec) < m1], addchar)
    }
    return(paste0(toupper(charvec), numschar))
}


.as.data.frame.summary.aovlist <- function(x) {
   if(length(x) == 1) {
     as.data.frame(x[[1]])
   } else {
     lapply(unlist(x, FALSE), as.data.frame)
   }
}

.opendir<-function(path=getwd()){
print(data.frame(dir=list.dirs()[-grep(".Rproj", list.dirs())]))
op<-as.numeric(readline("Directory #:  "))
pathtofile<-paste0(path, "/",list.dirs(full.names = FALSE)[-grep(".Rproj", list.dirs())][op], "/")
print(pathtofile)
.bu(pathtofile)
}

.rangeoutput<-function(range, suffix=NULL){
    if (is.null(suffix)) suffix <- "" else suffix <- paste0(" ", suffix)
    paste0(range[-length(range)], "-", range[-1], suffix)
}


.MakeNewProject<-function(projname, path="~/Documents/R/projects2017/", openproject=FALSE){
   path1<-paste0(path, projname, "/")
      if (!dir.exists(path1)) dir.create(path1)
  ask <- readline(paste0("Copy GarnasModifiedProjectTemplate-master to ", path, "? (y/n)  "))
   if (tolower(ask)=="y") {

      system(paste("cp -r", '/Users/jeff/Dropbox/R/GarnasModifiedProjectTemplate-master/*',  path1))
      system(paste("mv", paste0(path1, "InsertProjectNameHere.Rproj"), paste0(path1, projname,".Rproj")))
      #path1<-"/Users/jeff/Documents/R/projects2017/testdelete/"
    list.dirs()
      if (openproject) browseURL(paste0(path1, projname,".Rproj")) else {
         browseURL(paste0(path1))
         print("\nProject created. Save current project and open, or switch over. \n")
      }

#    Sys.sleep(13)
#       browseURL(paste0(path1, "README.md"))
#     Sys.sleep(1)
#       browseURL(paste0(path1, "MASTERFILE.R"))
         } else print("Exiting -- change path")
}



.signif<-function(x) {
   ret<-rep("", length(x))
   ret[x<=0.1]<-"."
   ret[x<=0.05]<-"*"
   ret[x<=0.01]<-"**"
   ret[x<=0.001]<-"***"
   ret[x<=0.0001]<-"****"
   return(ret)
}

.getfile<-function(uniq=T, changepath=FALSE, diruse=getwd(), filter=".r"){
      lets<-c('couldnotpossiblybehere')
      if (changepath) diruse<-getwd()
      lets<-readline("Type some letters from the filename (n to break):  ")
      if (lets!="n") {
      if (length(list.files(ignore.case = T, pattern="\\.r$")[grep(ignore.case=T,lets, list.files(ignore.case = T, pattern="\\.r$"))])==1) ret<-
         paste(sep="/",getwd(), list.files(ignore.case = T, pattern="\\.r$")[grep(ignore.case=T,as.character(lets), list.files(ignore.case = T, pattern="\\.r$"))])
      if (length(list.files(ignore.case = T, pattern="\\.r$")[grep(ignore.case=T,lets, list.files(ignore.case = T, pattern="\\.r$"))])>1) {
         print(data.frame(filenames=list.files(ignore.case = T, pattern="\\.r$")[grep(ignore.case=T,lets, list.files(ignore.case = T, pattern="\\.r$"))]))
         ll<-as.numeric(readline("Type the # of the file:  "))
         ret<-paste(sep="/",getwd(), list.files(ignore.case = T, pattern="\\.r$")[grep(ignore.case=T,lets, list.files(ignore.case = T, pattern="\\.r$"))][ll] )
}
      if (length(list.files(ignore.case = T,
         pattern="\\.r$")[grep(ignore.case=T,lets,
            list.files(ignore.case = T, pattern="\\.r$"))])==0) ret<-
            file.choose()
       return(ret)
      }
}

# pdffile<- "./graphs/BW_VS_Adj Morphospecies by family host.pdf"



# .devpdfoldJune2020 <- function(file, wd="./graphs", open=TRUE, overwrite=FALSE, png=FALSE, tf=NULL, meta=FALSE){
#   require("rstudioapi")
#   tf <- rstudioapi::getActiveDocumentContext()$path
#   if (is.null(wd)) wd=getwd()
#   if ((meta) && (is.null(tf))) tf<-.getfile()
#   file <- .clean(file)
#   ff<-paste(wd,"/",file,".pdf",sep="")
#   ff<-gsub('//', '/', ff)  ## interior spaces
#   ff<-gsub('.pdf.pdf', '.pdf', ff)
#   if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
#     if (!overwrite) {
#       print("File already exists.  Opening existing file. Or choose overwrite=TRUE")
#       #ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#       #dev.copy2pdf(file=ff)}
#       browseURL(ff)
#     }
#   if (open) browseURL(ff)
#   if (png) {
#     .pdf2png(ff)
#     # system(paste("mkdir '",wd, "/png_version/'", sep=""))
#     # ff <- gsub("/", "\\\\", ff)
#     # fng <- gsub(".pdf", ".png", ff)
#     # fn<-strsplit(ff, "/")[[1]][length(strsplit(ff, "/")[[1]])]
#     # #pngpath<-paste(wd, "/png_version/", sep="")
#     # #x<-paste("sips -s format png '", ff, "' --out '", pngpath, substring(fn, 1, nchar(fn)-4), ".png'", sep="")
#     # x <- paste0('"C:/Program Files/ImageMagick-7.0.8-Q16/magick.exe" convert -density 288 "', ff, '" -resize 25% "', fng)
#     # system(x)
#   }
#   if ((meta) && (tf!="n") && (overwrite)) {
#     sink(paste0(ff, ".metadata.txt"))
#     cat("Script file:  ")
#     cat(paste0("\n", tf))
#     cat("\n")
#     cat("Approximate line: ", readline("Approx. line? "))
#     cat("\nNotes: ", readline("Notes? "))
#     cat("\nFigure #: ", readline("Figure #: "))
#     cat("\nTimestamp: ", date())
#     .sinkall()
#   }
#   print(ff)
# }
#
# #.graphwindow(5,16)
#
# .ggsave <- function(file, dpi=500){
#     require("rstudioapi")
#     require("ggplot2")
#     tf <- rstudioapi::getActiveDocumentContext()$path
#     ggsave(file, dpi=dpi, width=dev.size()[1], height = dev.size()[2],
#            path=paste0(getwd(),"/html/images/"))
# }


# .graphwindow(5,17)
# .ggsave("tilf.png")



# .devpdf_2022.12.11 <- function(file, wd="./graphs", open=FALSE, overwrite=FALSE, caption=NULL, powerpoint=TRUE, slidetitle=NULL,
#                     png=TRUE, tf=NULL, meta=TRUE, NotesInput=FALSE, Notes="", history=TRUE){
#   require("rstudioapi")
#   tf <- rstudioapi::getActiveDocumentContext()$path
#   if (is.null(wd)) wd=getwd()
#   #if ((meta) && (is.null(tf))) tf<-.getfile()
#   file <- .clean(file)
#   ff<-paste(wd,"/",file,".pdf",sep="")
#   ff<-gsub('//', '/', ff)  ## interior spaces
#   ff<-gsub('.pdf.pdf', '.pdf', ff)
#   if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
#     if (!overwrite) {
#       print("File already exists.  Opening existing file. Or choose overwrite=TRUE")
#       #ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#       #dev.copy2pdf(file=ff)}
#       browseURL(ff)
#     }
#   if (open) browseURL(ff)
#   if (png) {
#     .pdf2png(ff)
#   }
#   if ((meta) && (tf!="n")) {
#     sink(paste0(gsub("graphs/", "", ff), ".metadata.md"))
#     cat("\n### Script file: ")
#     cat(paste0("\n", tf))
#     cat("\n")
#
#     if (NotesInput) {
#       cat("Approximate line: ", readline("Approx. line? "))
#       cat("\nNotes: ", readline("Notes? "))
#       cat("\nFigure #: ", readline("Figure #: "))
#     }
#     cat("\nTimestamp: ", date(), "\n")
#     cat("\n### ", .clean(file))
#     cat(paste0("\nCaption: ", caption,'\n'))
#     cat("\n![", .clean(file), "](./html/images/",.clean(file), ".png)", sep="")
#     cat("\n\n###### ", gsub("\\./", paste0(getwd(), "/"), ff), sep="")
#     #cat("\n\n[", .clean(file), "_png]:", sep="")
#     #cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     #cat("\n\n[", .clean(file), "]:", sep="")
#     #cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat(paste0("\n", "Notes: ", Notes))
#     cat("\n------\n")
#     .sinkall()
#   }
#   if (history) {
#     sink(paste0(getwd(), "/ProjectHistoryFile.md"), append = TRUE)
#     cat(paste0("\n### Script file: ", tf))
#     cat("\n")
#     cat("\nTimestamp: ", date(), "\n")
#
#     cat("\n###", .clean(file))
#     cat(paste0("\nCaption: ", caption,'\n'))
#     cat("\n![", .clean(file), "](./html/images/",.clean(file), ".png)", sep="")
#     cat("\n\n###### ", gsub("\\./", paste0(getwd(), "/"), ff), sep="")
#
#     # cat("\n## ", .clean(file))
#     # cat("\n[![", .clean(file), "][", .clean(file),"_png]][",.clean(file), "]", sep="")
#     # cat("\n", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     # cat("\n\n[", .clean(file), "_png]:", sep="")
#     # cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     # cat("\n\n[", .clean(file), "]:", sep="")
#     # cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat(paste0("\n", "Notes: ", Notes))
#     cat("\n------\n")
#     .sinkall()
#   }
#
#   #make Powerpoint
#   if (powerpoint) {
#     if (is.null(caption)) caption <- .clean(file)
#     if (is.null(slidetitle)) slidetitle <- .clean(file)
#     sink(paste0(getwd(), "/powerpoint.md"), append = TRUE)
#     #cat(paste0("\n### Script file: ", tf))
#     cat("\n\n# ", slidetitle, sep="")
#     cat("\n![", caption, "](./html/images/",.clean(file), ".png)", sep="")
#     cat("\n")
#     .sinkall()
#   }
#
#
#   print(ff)
#   Sys.sleep(1.3)
#   print(paste0(gsub("graphs/", "", ff), ".metadata.md"))
#   # browseURL(paste0(gsub("graphs/", "", ff), ".metadata.md"), browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#   if (open) browseURL(paste0(gsub("graphs/", "", ff), ".metadata.md"), browser = "C:/Users/jrg1035/AppData/Local/Markdown Monster/MarkdownMonster.exe")
# }
#
#
# .devpdf.2020.06.07 <- function(file, wd="./graphs", open=FALSE, overwrite=FALSE,
#                     png=FALSE, tf=NULL, meta=TRUE, Notes=FALSE, history=TRUE){
#   require("rstudioapi")
#   tf <- rstudioapi::getActiveDocumentContext()$path
#   if (is.null(wd)) wd=getwd()
#   if ((meta) && (is.null(tf))) tf<-.getfile()
#   file <- .clean(file)
#   ff<-paste(wd,"/",file,".pdf",sep="")
#   ff<-gsub('//', '/', ff)  ## interior spaces
#   ff<-gsub('.pdf.pdf', '.pdf', ff)
#   if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
#     if (!overwrite) {
#       print("File already exists.  Opening existing file. Or choose overwrite=TRUE")
#       #ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#       #dev.copy2pdf(file=ff)}
#       browseURL(ff)
#     }
#   if (open) browseURL(ff)
#   if (png) {
#     .pdf2png(ff)
#   }
#   if ( (meta) && (tf!="n") ) {
#     sink(paste0(ff, ".metadata.md"))
#     cat("Script file:  ")
#     cat(paste0("\n", tf))
#     cat("\n")
#
#     if (Notes) {
#       cat("Approximate line: ", readline("Approx. line? "))
#       cat("\nNotes: ", readline("Notes? "))
#       cat("\nFigure #: ", readline("Figure #: "))
#     }
#     cat("\nTimestamp: ", date(), "\n")
#     cat("\n##", .clean(file))
#     cat("\n[![", .clean(file), "][", .clean(file),"_png]][",.clean(file), "]", sep="")
#     cat("\n", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n\n[", .clean(file), "_png]:", sep="")
#     cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n\n[", .clean(file), "]:", sep="")
#     cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n------")
#     .sinkall()
#   }
#   if (history) {
#     sink(paste0(getwd(), "/ProjectHistoryFile.md"), append = TRUE)
#     cat(paste0("\nScript file: ", tf))
#     cat("\n")
#     cat("\nTimestamp: ", date(), "\n")
#     cat("\n##", .clean(file))
#     cat("\n[![", .clean(file), "][", .clean(file),"_png]][",.clean(file), "]", sep="")
#     cat("\n", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n\n[", .clean(file), "_png]:", sep="")
#     cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n\n[", .clean(file), "]:", sep="")
#     cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
#     cat("\n------")
#     .sinkall()
#   }
#   print(ff)
#   if (open)  browseURL(paste0(gsub("graphs/", "", ff), ".metadata.md"))
# }
#

# old version for mac
# .devpdf<-function(file, wd="./graphs", open=TRUE, overwrite=FALSE, png=FALSE, tf=NULL, meta=TRUE){
#     require("rstudioapi")
#     tf <- rstudioapi::getActiveDocumentContext()$path
#       if (is.null(wd)) wd=getwd()
#       if ((meta) && (is.null(tf))) tf<-.getfile()
#          ff<-paste(wd,"/",file,".pdf",sep="")
#          ff<-gsub('//', '/', ff)  ## interior spaces
#          ff<-gsub('.pdf.pdf', '.pdf', ff)
#          if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
#             if (overwrite) { print("File already exists.  Opening existing file.")
#                 ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#                 dev.copy2pdf(file=ff)}
#          if (open) browseURL(ff)
#          if (png) {
#                system(paste("mkdir '",wd, "png_version/'", sep=""))
#                fn<-strsplit(ff, "/")[[1]][length(strsplit(ff, "/")[[1]])]
#                pngpath<-paste(wd, "png_version/", sep="")
#                x<-paste("sips -s format png '", ff, "' --out '", pngpath, substring(fn, 1, nchar(fn)-4), ".png'", sep="")
#                system(x) }
#          if ((meta) && (tf!="n")) {
#             sink(paste0(ff, ".metadata.txt"))
#             cat("Script file:  ")
#             cat(paste0("\nMade by: ", tf))
#             cat("\n")
#             cat("Approximate line: ", readline("Approx. line? "))
#             cat("\nNotes: ", readline("Notes? "))
#             cat("\nFigure #: ", readline("Figure #: "))
#             cat("\nTimestamp: ", date())
#             .sinkall()
#          }
#          print(ff)
# }


.devtext<-function(file, wd="./graphs", open=TRUE, overwrite=TRUE, tf=NULL, meta=TRUE, header=""){
    require("rstudioapi")
    #file="kk"
    tf <- rstudioapi::getActiveDocumentContext()$path
      if (is.null(wd)) wd=getwd()
      # if ((meta) && (is.null(tf))) tf<-.getfile()
         ff<-paste(wd,"/",file,".txt",sep="")
         ff<-gsub('//', '/', ff)  ## interior spaces
         ff<-gsub('.txt.txt', '.txt', ff)

        if (!(overwrite)) { print("File already exists."); break } else  sink(paste0(ff, ".metadata.txt"))
         # if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
                    # ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
         # if (open) browseURL(ff)
         # if (meta)  .sinktext(addMeta = meta)
cat(header,"\n__________________\n\n")
cat("Source script: ", tf,"\n")
return(paste0(ff, ".metadata.txt"))

         }


.sinktext<-function(file=NULL, addMeta=TRUE, notes, open=TRUE){
        cat("\n", notes,"\n_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_\n")
        # cat("Source script: ", tf,"\n")
        if (addMeta){
            cat("Approximate line: ", readline("Approx. line? "))
            cat("\nNotes: ", readline("Notes? "))
            cat("\nFigure #: ", readline("Figure #: "))
            cat("\nVersion: ", readline("Version: "))
            cat("\nFinal (y/n): ", readline("Final: ")) }

            .sinkall()
        browseURL(file)
}

.sinkaddtxt <- function(txt="", level=0){
  header <- ifelse(level>0, paste(rep("#", level), collapse = ""), "")
  cat("\n", header, ifelse(level>0, " ", ""), txt, "\n\n")
}



.annot <- function(txt, lev=2, charret=2){
    if (!is.null(lev)) levuse <- paste0(c(rep("#", lev), " "), collapse = "") else levuse <- ""
    charret <- paste0(c(rep("\n", charret)), collapse = "")
    cat(paste0(levuse, txt, charret))
}
.a <- .annot

sinklist <- "./html/basic structure of data collection.md"
.sink <- function(txt=NULL, file=NULL, whichsink=99, append=TRUE){  #, sinklist=sinklist) {
    if (!exists("sinklist") & is.null(file)) {
        print("Need a filename or a sinklist variable")} else {
            if (!is.null(file)) fl <- file else {
                if (whichsink==99 && exists("sinklist")) fl <- sinklist[length(sinklist)]
                if (whichsink!=99 && exists("sinklist")) {
                    if (whichsink <= length(sinklist)) fl <- sinklist[whichsink] else
                        print ("whichsink does not match any files.")
                }
            }
            if (!exists("sinklist")) sinklist <<- c()
            if (any(sinklist==fl)) sinklist <- sinklist[-which(sinklist==fl)]

            sinklist <<- c(sinklist, fl)

            sink(file=fl, append=append)

            if (!is.null(txt)){
                .annot(txt)
                .sinkall()
            }
        }
}

.sinkall <- function() while (sink.number()>0) { sink() }
.sa <- .sinkall

.sink_echo <- function(command){
  commandtxt <- as.character(substitute(paste0("(",command,")")))
  output <- capture.output({
    # Print the command to the console
    cto <- gsub("paste0", "", commandtxt)
    cat("\n\n```\n> ", cto, "\n````\n")

    #cat("\n\n```\n", commandtxt, "\n````\n")

    # Evaluate the command
    eval(command)
  })
  cat(output, sep = "\n")
}

# sinklist <- "./html/testh.md"
# .sink()
#   .stadd("Add a hedear", 1)
#   .sink_echo(kable(table(rep(1:3,4))))
#   cat(singleString)
# .sa()
# .bu(sinklist)

.addpng <- function(pathtoAddition){
  parsename <- strsplit(pathtoAddition, "\\.")[[1]]
   cat("______\n")
  if (parsename[length(parsename)]=="png") {
    pngPath <- gsub("/html/" ,"/", pathtoAddition)
    cat(pngPath)
    cat(
      paste0("![",pngPath,"]","(",pngPath,")", collapse="")
    )
  } else {
    singleString <- paste(readLines(pathtoAddition), collapse="\n")
    cat(singleString)
  }

}
# .sink()
# .stadd("askdjf laksjfd")
# .addpng(pathtoAddition)
# .sa()
# .bu(sinklist)
# pathtoAddition <- "./html/images/01_larval_establishment_by_dbh_and_treatment.png"

.stadd <- function(txt="", level=0){
  header <- ifelse(level>0, rep("#", level), "")
  cat("\n", header, ifelse(level>0, " ", ""), txt, "\n\n")
}


.meansd<-function(x, rnd=1, se=F, nospace=F){
   med<-paste0("median = ",fivenum(x)[3],"; range = {",range(x)[1], ",",range(x)[2],"}" )
   if (se) ret<-paste(round(mean(x),rnd), round(sd(x)/sqrt(length(x)),rnd), sep=" ± ") else
      ret<-paste(round(mean(x),rnd), round(sd(x),rnd), sep=" ± ")
   stem(x)
   if (se) ret1<-(c(paste0("mean ± se = ", ret), med)) else ret1<-(c(paste0("mean ± sd = ", ret), med))
   if (nospace) return(gsub(' ','', ret1)) else return(ret1)
}


.clip<-function(df, cp="copy", header=FALSE){
   if (cp=="copy")
{   clip <- pipe("pbcopy", "w")
    write.table(file = pipe("pbcopy"), df, row.names=FALSE, sep = "\t")}
   if (cp=="paste"){
      return(read.table(pipe("pbpaste"), sep="\t", header=header))
   }
}


 .plot<-function(marg=TRUE, ...) {
    if (marg) {par(mar=c(6,7.5,1,1))}
    plot(..., xlab="", ylab="", axes=F, type="n")
 }

 .barplot<-function(x, marg=TRUE, ...) {
   if (marg) {par(mar=c(8,8.5,1,1))}
  bp <- barplot(x, ..., xlab="", ylab="", axes=F, beside=TRUE)
   .axx(x=FALSE)
   return(bp)
 }




# .addse function ---------------------------------------------------------

 .addse<-function(x, y, se, length=.07, col=1, horiz=FALSE, upperonly=FALSE){

   if (horiz) {minbar <- x-se; maxbar <- x+se; loc <- y} else {minbar <- y-se; maxbar <- y+se; loc <- x}
   if (upperonly) minbar <- minbar+se

   arrows(loc, minbar, loc, maxbar, code=3, angle=90, length=length, col=col)
 }

  #
   # if (horiz) {arrows(x-se, y, x+se, y, code=3, angle=90, length=length, col=col)} else {
   #   arrows(x, y-se, x, y+se, code=3, angle=90, length=length, col=col)
   # }
   # if (upperonly)  { arrows(x, y-0, x, y+se, code=2, angle=90, length=length, col=col)
   # }
 # }




 .plusmin<-function(x,rnd=1) {paste(round(x[,2],rnd), round(x[,3],rnd), sep=" ± ")}


.dpng<-function(file, wd="graphs", open=TRUE, overwrite=FALSE, png=TRUE){
      if (is.null(wd)) wd=getwd()
         ff<-paste(wd,"/",file,".pdf",sep="")
         ff<-gsub('//', '/', ff)  ## interior spaces
         ff<-gsub('.pdf.pdf', '.pdf', ff)
         if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
            if (overwrite) { print("File already exists.  Opening existing file.")
                ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
                dev.copy2pdf(file=ff)}
         if (open) browseURL(ff)
         if (png) {
               system(paste("mkdir '",wd, "png_version/'", sep=""))
               fn<-strsplit(ff, "/")[[1]][length(strsplit(ff, "/")[[1]])]
               pngpath<-paste(wd, "png_version/", sep="")
               x<-paste("sips -s format png '", ff, "' --out '", pngpath, substring(fn, 1, nchar(fn)-4), ".png'", sep="")
               system(x) }
}


.html_graph<-function(graphfilepng, folder=NULL,caption=NULL, showfilename=TRUE){
   if (!is.null(folder)) graphfilepng <- paste0(folder, graphfilepng)
   #if (!is.null(caption) & showfilename) caption<-paste(caption, graphfilepng, sep="\n\n")
   if (is.null(caption) & showfilename) caption<-graphfilepng
   if (!is.null(caption) & !showfilename) caption<-"graph"
   HTMLInsertGraph(GraphFileName=graphfilepng,
   Caption=.wordwrap( paste("<h1>",caption, "</h1>", sep=""), 150), GraphBorder=0, Align="center", WidthHTML=800, HeightHTML=NULL, append=TRUE)
   #HTML(paste0("<h2>",caption, "</h2>"))
   if (showfilename) HTML(paste0("<h2>",graphfilepng, "</h2>"))
}


.table <- function (..., useNA = 'ifany') base::table(..., useNA = useNA)



.findanywhere<-function(x, df){
   dfc<-ncol(df)
   tmke<-data.frame()
   for (i in 1:dfc){
      tmp<-grep(x, as.character(df[,i]), ignore.case = TRUE)
      if (length(tmp)>0) {tmk<-data.frame(col=rep(i, length(tmp)), wh=tmp);
         tmke<-rbind(tmke, tmk )}
         }
   return(list(tmke[,2],unique(tmke[,1]),tmke, table(tmke$col), names(df)[unique(tmke$col)]))
}

.relevel<-function(x) as.factor(as.character(x))

.getrefs<-function(filename){
   require(readr)
   mystring <- read_file(filename)
   bparen<-regmatches(mystring, gregexpr("(?<=\\().*?(?=\\))", mystring, perl=T))[[1]]
   # regmatches(mystring, gregexpr("(?<=\\().*?(?=\\))", mystring, perl=T))[[1]]
   mtch<-gregexpr("(?<=\\().*?(?=\\))", mystring, perl=T)
   #attr(mtch[[1]], "match.length")
   # regmatches(mystring, gregexpr("(?<=[A-Z]).*?(?=\\())", mystring, perl=T))
   bparen1<-bparen[grep("[0-9][0-9][0-9][0-9]",bparen)]
   bparen2<-unlist(strsplit(bparen1, "; "))
   bparen3<-bparen2[grep("[0-9][0-9][0-9][0-9]",bparen2)]
   extras<-substring(mystring, mtch[[1]][which(nchar(bparen)==4)]-30, mtch[[1]][which(nchar(bparen)==4)]+4)
   extras_noparen<-gsub("\\)", "", gsub("\\(", "",extras))
   extras_noparen_firstcap<-substring(extras_noparen, sapply(gregexpr("[A-Z]", extras_noparen, perl=T), "[[", 1),100)
   all<-c(bparen3, extras_noparen_firstcap)
   all1<-   gsub("e.g., ", "", all)
   all1<-   gsub("e.g. ", "", all1)
   all1<-   gsub("but see ", "", all1)
   multiyears<-grep("[0-9][0-9][0-9][0-9], [0-9][0-9][0-9][0-9]", all1)

   all2<-all1[-multiyears]

   addmulti<-c(substring(all1[multiyears],1, sapply(gregexpr(",", all1[multiyears], perl=T), "[[", 1)-1),
      paste(substring(all1[multiyears],1, sapply(gregexpr(",", all1[multiyears], perl=T), "[[", 1)-6),
      substring(all1[multiyears], sapply(gregexpr(",", all1[multiyears], perl=T), "[[", 1)+2, 100)))

   return(sort(unique(c(all2, addmulti))))
}



.openwd<-function(){browseURL(getwd())}

.bu<-function(x=getwd()){
  require(rstudioapi)
    tmp <- x # gsub("[[:space:]]", "%20", x)
    if (substring(tolower(x), (nchar(tmp)-1), nchar(tmp)) == ".r") rstudioapi::navigateToFile(eval(tmp)) else browseURL(eval(tmp))
    }

.bmaster<-function(file, open=TRUE){  #assumes a global "master" df
   str<-paste(getwd(), master[file,1], sep="/")
   if (open) browseURL(eval(gsub("[[:space:]]", "%20", str))) else master[file,1]
#    print(str)
}

#source("/Users/jeff/Documents/R_drawing/hajek biocontrol figure 1 apparent competition/arrdiag_function.r")

library(igraph)
library(dplyr)
.curvedarrow<-function(x, y, ang=NULL, angoff=20, ce=c(20,20), col=4, wi=2, cu=-.75, circo=1, dir=TRUE, lwd=3, sh.lty=1, h.lty=1){
   require(igraph)
   iArrows <- igraph:::igraph.Arrows
   if (length(ce)==1) ce<-c(ce,ce)
   if (length(circo==1)) circo<-c(circo, circo)
   rad=ce/12
   allang<-seq(0,2*pi,.001)
   for (i in 1:2) {points( x[i]+(cos(allang)*rad[i]), y[i]+(sin(allang)*rad[i]), type="l",
         col=circo[i], lwd=lwd)}
   angle<-(atan((y[2]-y[1])/(x[2]-x[1])))/(pi/180)
   if (is.null(ang))
      if (cu<0) ang<-c(angle-angoff, angle+180+angoff) else ang<-c(angle+angoff, angle+180-angoff)
   (x1<-x+(cos(ang*(pi/180))*rad))
   (y1<-y+(sin(ang*(pi/180))*rad))
   if (dir) use<-c(x1[1], y1[1], x1[2], y1[2]) else {use<-c(x1[2], y1[2], x1[1], y1[1]); cu<-(-cu)}
   iArrows(use[1],use[2],use[3],use[4],
          h.lwd=wi, sh.lwd=wi, sh.col=col,
          curve=cu, width=1.5, size=1.2,  sh.lty=sh.lty, h.lty=h.lty)
}

.clean<-function(vect) {
   x <- as.character(vect)
   x <- tolower(x)
   x <-  gsub(' +$', '', x)  ## trailing spaces only
   x <-  gsub("([0-9])([a-zA-Z])","\\1_\\2",x) #backreferencing (see: http://stackoverflow.com/questions/11605564/r-regex-gsub-separate-letters-and-numbers)
   x <-  gsub(' +$', '', x)  ## trailing spaces only
   x <- gsub("[[:punct:]]", " ", x)
   x <-  gsub(' +', '_', x)  ## interior spaces
   x <-  gsub('_+', '_', x)  ## interior spaces
   x <-  gsub('\\.', '_', x)  ## interior spaces
   x <-  gsub('__', '_', x)  ## interior spaces
   x <-  gsub('&+', 'and', x)
   x <-  gsub('\\(', '', x)
   x <-  gsub('\\)', '', x)
  return(x)}

.auths<-function(x){ #author order for endnote
#x<-"Matthew M. McConnachie1*, Brian W. van Wilgen1, David M. Richardson1, Paul J. Ferraro2 and Aurelia T. Forsyth"
# .clean
   x<-gsub(", and ", ",", x, ignore.case = TRUE)
   x<-gsub(" and, ", ",", x, ignore.case = TRUE)
   x<-gsub(" and ", ",", x, ignore.case = TRUE)
   x<-gsub("&", ",", x)
   x <-  gsub(' +$', '', x)  ## trailing spaces only
   x <-  gsub(' ,', ',', x)  ## trailing spaces only
   x <-  gsub(', ', ',', x)  ## trailing spaces only
   x <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
   x <-gsub("[0-9],[0-9]", "", x, perl=T)
   x <-gsub("[[:digit:]]", "", x, perl=T)
   x <- gsub("([0-9]+)", "", x)
   x <- gsub("[^[:alnum:][:blank:],.;+?&/\\-]", "", x)
   while (length(grep(",,", x))>0) x <- gsub(",,", ",", x)
   x <- data.frame(full=unlist(strsplit(x, ",")), stringsAsFactors=F)
   x$full<-as.character(x$full)
   xx<-strsplit(x$full, " ")

   l<-unlist(lapply(xx, function(x) length(x)))
sink("outfile.txt")
   for (i in 1:length(xx)){
      xx<-lapply(xx, function(x) paste(toupper(substring(x,1,1)), tolower(substring(x,2,100)), sep=""))
      if (any(xx[[i]] %in% c("van", "vander"))) lst<-which(xx[[i]] %in% c("van", "vander")) else lst<-l[i]
      last<-paste(xx[[i]][lst:l[i]], collapse=" ")
      last <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", last)
      firstmiddle<-paste(xx[[i]][1:(lst-1)], collapse=" ")
      firstmiddle <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", firstmiddle)
      hyp<-(gregexpr(pattern ='-',last)[[1]][1])
      last<-paste(substring(last, 1,hyp),
      toupper(substring(last, hyp+1,hyp+1 )),
      substring(last, hyp+2,100), sep="" )
      tmp<-paste(last, ", ", firstmiddle, sep="")
      cat(tmp)
      cat("\n")
   }
while (sink.number()>0) sink()
file.show("outfile.txt")
   }

.polygon<-function(xleft, xright, bottom, top, col="gray70", border=1, lwd=1){
   x1<-c(xleft, xright, xright, xleft, xleft)
   y1<-c(rep(bottom,2), rep(top, 2), bottom)
   polygon(x1,y1, col=col, border=border, lwd=lwd)
}

.png<-function(file, wd=NULL, open=TRUE, overwrite=TRUE){
      if (is.null(wd)) wd=getwd()
         ff<-paste(wd,"/",file,".png",sep="")
         ff<-gsub('//', '/', ff)  ## interior spaces
         ff<-gsub('.png.png', '.png', ff)
         if (overwrite) png(file=ff, bg="transparent") else
            if (file.exists(ff)) print("File already exists.  Opening existing file.")
         if (open) browseURL(ff)
}

#oldversion
# .devpdf<-function(file, wd=NULL, open=TRUE, overwrite=FALSE, png=FALSE){
#       if (is.null(wd)) wd=getwd()
#          ff<-paste(wd,"/",file,".pdf",sep="")
#          ff<-gsub('//', '/', ff)  ## interior spaces
#          ff<-gsub('.pdf.pdf', '.pdf', ff)
#          if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
#             if (overwrite) { print("File already exists.  Opening existing file.")
#                 ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
#                 dev.copy2pdf(file=ff)}
#          if (open) browseURL(ff)
#          if (png) {
#                system(paste("mkdir '",wd, "png_version/'", sep=""))
#                fn<-strsplit(ff, "/")[[1]][length(strsplit(ff, "/")[[1]])]
#                pngpath<-paste(wd, "png_version/", sep="")
#                x<-paste("sips -s format png '", ff, "' --out '", pngpath, substring(fn, 1, nchar(fn)-4), ".png'", sep="")
#                system(x) }
# }


.rankabundance<-function(x, proportion=FALSE, orderby=NULL){
   par(mar=c(8,5,1,1))
   if (proportion) {
         if (is.null(orderby)) tmp_x<-sort(colSums(.pa(x))/nrow(x),decreasing=TRUE)
         if (!is.null(orderby)) tmp_x<-(colSums(.pa(x))/nrow(x))[orderby]
         yla=paste("Proportion of samples (n=",nrow(x),")", sep="")
            } else {
         if (is.null(orderby)) tmp_x<-sort(colSums(.pa(x)),decreasing=TRUE)
         if (!is.null(orderby)) tmp_x<-(colSums(.pa(x)))[orderby]
         yla=paste("# of samples where present (n=",nrow(x),")", sep="")
      }
   bp<-barplot(tmp_x, xlab="", ylab="", axes=F, axisnames=F)
   axis(1, las=2, label=names(tmp_x), at=bp, cex.axis=.6)
   axis(2, las=2)
   #mtext(side=2, line=2.6, paste("", sep=""), cex=1.4)
   mtext(side=2, line=2.6, yla, cex=1.4)
   box(bty="l")
}


.pool<-function(df, bulk){
   prov<-as.character(bulk[1]); from<-as.numeric(bulk[2]); to=as.numeric(bulk[3])
   fr=which(df$code==paste(prov, from, sep=""))
   t0=which(df$code==paste(prov, to, sep=""))
   tmp<-df[fr:t0,sapply(df, is.character)][0,]
  if  (sum(sapply(df, is.character))>0){
   for (j in 1:ncol(tmp)) { # j =1
      tmp[1,j]<-paste(sort(unique(df[fr:t0,sapply(df, is.character)][,j])), collapse="/")
   }
  }
     if (nrow(tmp)>0)  ret<-cbind(t(.adf(colMeans(df[fr:t0,sapply(df, is.numeric)]))), tmp)
     if (nrow(tmp)==0)  ret<-t(.adf(colMeans(df[fr:t0,sapply(df, is.numeric)])))
   return(ret)
}


.qb<-function(r=1,c=1, baseht=7, pardefault=TRUE){
   if(c>r) maxwidth<-12.5 else maxwidth<-7
   (w<-c/r*baseht)
   if (w>12.5) {w<-w*12.5/w; baseht<-baseht*12.5/w}
   h<-w*r/c*1.1
   if (h>baseht) {h<-h*baseht/h; w<-w*baseht/h}
   .qt(h, w)
   if (pardefault) par(ann=F, las=1, cex = 1.2, cex.axis = 1.25, cex.lab = 1.25, pch=20)
   .panes(r,c)
   return(c(h, w))
}



.ggplotreg <- function (fit) {

require(ggplot2)

ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle(paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "; Int. =",signif(fit$coef[[1]],5 ),
                     "; Slope =",signif(fit$coef[[2]], 5),
                     "; P =",signif(summary(fit)$coef[2,4], 5)))
}



.plotreg<-function(x, y, degree=1, lw=1.8,...){
   plot(x, y, pch=20)
   abline(t1<-lm(y~x), col=4, lw)
   nd<-data.frame(x=seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=200))
   if (degree==1)  ret=t1
   if (degree>1) { t2 <- lm(y ~ x + I(x^2))
            points(nd$x, predict(t2, nd), col = 2, type="l", lwd=lw)
            ret=list(t1, t2)
   }
   if (degree>2) { t3 <- lm(y ~ poly(x, 3, raw=TRUE))
                    points(nd$x, predict(t3, nd), col = 3, type="l", lwd=lw)
            ret=list(t1, t2, t3)
   }
   return(ret)
}

.pa<-function(x) {.columnnum(x, pa=T)}

# returns columns from a data frame/table that are numeric.  If pa=T, converts all non-zeros to 1
.columnnum<-function(x, pa=FALSE){
  x<-x[sapply(x, is.numeric) ]
  if (pa) x[x>0]<-1
   return(x)
}
.toordinal<-function(x, char=TRUE){
   xx<-as.character(x)
   while (sum(nchar(xx)<max(nchar(xx)))>0)  xx[nchar(xx)<max(nchar(xx))]<-paste("0", xx[nchar(xx)<max(nchar(xx))], sep="")
   xxnum<-x/10^max(nchar(x))
   if (char) return(xx) else return(xxnum)
}


.l<-function(x){
   if (is.data.frame(x) | is.matrix(x)) ret<-nrow(x)
   if (is.vector(x)) ret<-length(x)
   return(ret)
}

.plot<-function(...) plot(..., xlab="", ylab="", axes=FALSE, type="n") ## imported

#Function to add a png
.drawpng <- function(label, image, x, y, ht, wd, ...) {
  require(png)
  require(grid)
   lab <- textGrob(label = label,
    x = unit(x, "npc"), y = unit(y, "npc"),
    just = c("left", "centre"), gp = gpar(...))
  logo <- rasterGrob(image = image,
    x = unit(x, "npc") + unit(1, "grobwidth", lab), y = unit(y, "npc"),
    width = unit(wd, "cm"), height = unit(ht, "cm"),
    just = c("left", "centre"), gp = gpar(...))
  grid.draw(lab)
  grid.draw(logo)
}


.reshape2wide<-function(dframe, newrows, newcolumns, valuecolumn){
    df=as.data.frame(dframe[,c(which(names(dframe) %in% newrows),
                               which(names(dframe) %in% newcolumns),
                               which(names(dframe) == valuecolumn))])
    dfwide<-reshape(df, timevar=newcolumns, idvar=newrows, direction="wide")
    return(dfwide)
}

.emplot<-function(x=1, y=1, yl=NULL) plot(x,y, type="n", axes=F, xlab="", ylab="", ylim=yl)

.ax <- function(bty="L", cex.axis=1.3, sides=c(1,2)){  ## JGTools
  ls <- ifelse(i %in% c(2,4), 2,1)
  for (i in sides) { axis(i, cex.axis=cex.axis, las=ifelse(i %in% c(2,4), 2,1)) }
  box(bty=bty, lwd=2)
}

.mtx<-function(x="", y="", ln=c(3,3), cex=1.4, sides=c(1,2), cl=c(1,1)){
  #is.null(x)
  txt <- c(x, y)
  #if (is.null(x)) {ln <- ln[1]; sides <- sides[1]; }
  #opar<-par()
  par(las=0)
  for (i in 1:length(sides)){ mtext(txt[i], side=sides[i], line=ln[i], cex=cex, col=cl[i]) }
  #mtext(side=2, y, line=line2, cex=cex)
  #par(opar)
}



.mtxx<-function(x,y, line1=3, line2=3, cex=1.4){
 #opar<-par()
 par(las=0)
 mtext(side=1, x, line=line1, cex=cex)
 mtext(side=2, y, line=line2, cex=cex)
 #par(opar)
}

.axx<-function(x=T, y=T, box=T, cex.axis=1.3){  ## JGTools
 if (x) axis(1, cex.axis=cex.axis)
 if (y) axis(2, las=2,cex.axis=cex.axis)
 if (box) box(bty="L", lwd=2)
}


.codelevels<-function(dataset, colourfac=NULL, symbolfac=NULL, defsymbols=c(19,18,15, 17, 4, 1, 2, 6)){

  if (is.null(colourfac)){ colourfac1<-rep(1, length(colourfac)) } else {
      t_fac1=as.factor(colourfac)
      colourfac1<-rep(0, length(colourfac))
      lc<-length(levels(t_fac1))
      cols=rainbow(lc)
      for (i in 1:lc){colourfac1[colourfac==levels(t_fac1)[i]]<-cols[i]}
      }
  if (is.null(symbolfac)){ symbolfac1<-rep(1, length(symbolfac)) } else {
      t_fac2=as.factor(symbolfac)
      symbolfac1<-rep(0, length(symbolfac))
      lc2<-length(levels(t_fac2))
      symbols=defsymbols[1:lc2]
      for (j in 1:lc2){symbolfac1[symbolfac==levels(t_fac2)[j]]<-symbols[j]}
      }
  colsymb<-data.frame(cols=colourfac1, symb=symbolfac1)
  return(colsymb)

  .legend()



#   USAGE:
#    cls<-.codelevels(tmp, tmp$site, tmp$position)
#    .qt()
#    plot(tmp$mass~tmp$log_diam, pch=cls[,2], col=cls[,1])
}




.hsdorder<-function(hsdoutput, srt=NULL){
 #srt is not used -- currently sorting is alphabetical.  need to change
 #hsdoutput comes form package agricolae, function HSD.test (i.e. HSD.test(m_rgr, c("spp"), group=T))
 require(agricolae)
  h<-hsdoutput
  h$means$trt<-row.names(h$means)
  mergetable<- merge(h$means,h$groups, by=intersect(names(h$means), names(h$groups)))
 #current codes (cc)
 cc<-unlist(lapply(strsplit(as.character(mergetable$M), ""), "[[", 1))
  mergetable$sig.codes<-as.character(mergetable$trt)
  codes<-data.frame(old=unique(cc), new=sort(unique(cc)))
for (i in 1:length(mergetable$M)){
   change<-unlist(strsplit(as.character(mergetable$M[i]), ""))
   tmp<-vector()
   for (j in 1:length(change)){ #j=1
     tmp<-c(tmp, as.character(codes$new[which(change[j]==codes$old)]))
   }
  mergetable$sig.codes[i]<-paste(sort(tmp), sep="", collapse="")
}
return(mergetable)
}


.histo<-function(histobj, col="light grey", inverse=F, xlim=NULL, ylim=NULL, add=F, axes=T,
                xlab="", ylab="", freq=T, ysym=F)  {
  bre<-histobj$breaks; if (freq) den<-histobj$density else den<-histobj$counts
  if (inverse) den = -den
  if (is.null(xlim)) xlim=c(min(bre), max(bre))
  if (is.null(ylim)) ylim=c(min(den), max(den))
  if (ysym) ylim=1.9*c(-max(abs(den)), max(abs(den)))
  if (!(add)) plot(1, type="n", xlim=xlim, ylim=ylim, axes=axes, xlab=xlab, ylab=ylab)
  for (i in 1:(-1+length(histobj$breaks))){
  polygon(x=c(bre[i], bre[i], bre[i+1], bre[i+1]), y=c(0,den[i],den[i],0), col=col) }
  }


.caic <- function(model) {

  sigma <- attr(VarCorr(model), 'sc')
	observed <- attr(model, 'y')
	predicted <- fitted(model)
	cond.loglik <- sum(dnorm(observed, predicted, sigma, log=TRUE))

	rho <- hatTrace(model)
	p <- length(fixef(model))
	N <- nrow(attr(model, 'X'))
	K.corr <- N*(N-p-1)*(rho+1)/((N-p)*(N-p-2)) + N*(p+1)/((N-p)*(N-p-2))

	CAIC <- -2*cond.loglik + 2*K.corr

	return(CAIC)

	}

.pd <- function(header=FALSE, aschar=T) {
  x <- read.table(pipe("pbpaste"), header=header)
  if (aschar) x <- as.data.frame(rapply(x, as.character, classes="factor", how="replace"), stringsAsFactors=F)
  if (ncol(x)==1) x <- as.vector(x[,1])
  return(x)}

.examplefile<-function(path2x) {
  library(png)
  x<-readPNG(path2x)
  .qt(8,8); plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes=F)
  rasterImage(x, 0, 0, 1, 1, interpolate = TRUE)
}


.fac2char <- function(x) {
  x <- as.data.frame(rapply(x, as.character, classes="factor", how="replace"), stringsAsFactors=F)
  if (ncol(x)==1) x <- as.vector(x[,1])
  return(x)}

## setwd("/Users/Jeff/Documents/Rworkfile/")
## options(device='quartz')
.qt<-function(x,y){quartz(height=x, width=y); par(mar=c(5,6,1,1))}

.nn=function(x){as.numeric(as.vector(x))}

.adf=function(x){as.data.frame(x)}


.RSS=function(x){RSiteSearch(x)}
.panes=function(x,y){ par(mfrow=c(x,y))}
.nameslook <- function(x) .adf(names(x))

.fixednumber_old=function(prefix, tobeordered, seps="_"){
  ## Ex:  fixednumber("a", c("one","two", "three"))
  ## "a" will precede same-length numbering, "1,2,3" is the vector to be numbered
  l=length(tobeordered)
  numstr=1:l
  for (i in 1:(nchar(l)-1)){
    numstr[nchar(numstr)<nchar(l)]=paste("0", numstr[nchar(numstr)<nchar(l)], sep="")
  }
  paste(rep(prefix, l), numstr, seps, tobeordered, sep="")
}

# .fixednumber<-function(prefix, suffix, digits=NULL, suffappend=TRUE){
#    l=length(suffix)
#    numstr=1:l
#    numstr[nchar(numstr)==1]=paste(rep('0', length(numstr[nchar(numstr)==1])),numstr[nchar(numstr)==1], sep="")
#    numstr[nchar(numstr)==2]=paste(rep('', length(numstr[nchar(numstr)==2])),numstr[nchar(numstr)==2], sep="")
#    ret<-paste(rep(prefix, l), numstr, sep="")
#    if (!is.null(digits)) return(substring(nchar(ret[1])-digits+1,nchar(ret[1]), ret) ) else return(ret)
# }
#
.fixednumber<-function(prefix, suffix, digits=NULL, suffappend=TRUE){
   mx<-max(nchar(suffix))
   while (any(nchar(suffix)<mx)) {
       suffix[nchar(suffix)<mx] <- paste0("0", suffix[nchar(suffix)<mx])
   }
    return(paste0(prefix, suffix))
      }
#.fixednumber("IS", 1:51)










.ipg=function(x, repos=1) {
  if (repos==1) repo="http://cran.ru.ac.za/"
  if (repos==2) repo="http://cran.us.r-project.org"
  if (repos==3) repo=NULL
  if (!(x %in% installed.packages())) install.packages(x, repos=repo)
  else print("Package installed already")}

.fn=function(filename, dir=getwd(), ftype="pdf", date=FALSE){
   if (date) paste(dir, paste(filename, "_", format(Sys.time(), "%Y.%m.%d"),".", ftype, sep=""), sep="/")
   else paste(dir, paste(filename, ".", ftype, sep=""), sep="/")}

.fixednumber=function(prefix, suffix){
   l=length(suffix)
   numstr=1:l
   numstr[nchar(numstr)==1]=paste(rep('00', length(numstr[nchar(numstr)==1])),numstr[nchar(numstr)==1], sep="")
   numstr[nchar(numstr)==2]=paste(rep('0', length(numstr[nchar(numstr)==2])),numstr[nchar(numstr)==2], sep="")
   paste(rep(prefix, l), numstr,"_", suffix, sep="")
}


.cleannames=function(df) {
   x <- as.character(names(df))
   x <- tolower(x)
   x <-  gsub(' +$', '', x)  ## trailing spaces only
   x <-  gsub(' +', '_', x)  ## interior spaces
   x <-  gsub('_+', '_', x)  ## interior spaces
   x <-  gsub('\\.', '_', x)  ## interior spaces
   x <-  gsub('__', '_', x)  ## interior spaces
   x <-  gsub('&+', 'and', x)
   x <-  gsub('_+$', '', x)  ## trailing spaces only
   x <-  gsub('\\.\\.', '\\.', x)  ## interior spaces
   names(df)=x
  return(df)}

.wordwrap<-function(x,len) {
    ret<-c()
    for (i in 1:length(x)) { ret<-c(ret, paste(strwrap(x[i],width=len),collapse="\n"))}
    return(ret)
    }

.Caption <- function(x, wd=66) { .wordwrap(paste(caption,"Created: ",date(), "  Path: ", paste(getwd(),filename, sep="/"), "  By: ", thisfile), wd) }

#browseURL("/Users/Jeff/Documents/R/myfunctions/editR_function.R")


#     if (length(ls())>0)  myfuncs=ls(all = TRUE)[-which(ls(all=T) %in% ls())]
#     if (length(ls())==0) myfuncs=ls(all=T)
#   print(myfuncs)



.resetfac=function(x){
  for (i in 1:ncol(x)){
    if (is.factor(x[,i]))  x[,i]=x[,i][, drop=T]}
    return(x)
  }


.normalarea<-function(area=1, mean=0, sd=2, x=NULL){ #returns a data frame with normal curve x and y that integrates to area
   sdinput=sd
    i=1/area^.5
    sd=sd/i
   if (is.null(x)) x=seq(mean-4*sd, mean+4*sd, .01)
   normout<-data.frame(x, (1/(sd*sqrt(2*pi)))*exp(-(x-mean)^2/(2*sd^2))/i^2)
   return(normout)
}


.addImg <- function(
  # pasted from https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing.
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj,
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2),
              interpolate = interpolate)
}



.sigmoid <- function(xrange=c(0,1), yrange=c(0,1), inflection=.5, decreasing=FALSE){
  xmax <- ifelse (length(xrange)==1, xrange, xrange[2])
  ymax <- ifelse (length(yrange)==1, yrange, yrange[2])
  xmin <- ifelse (length(xrange)==1, 0, xrange[1])
  ymin <- ifelse (length(yrange)==1, 0, yrange[1])

  x2 <- seq(.01,10, .01)

  # xout <- (x2/max(x2))*xmax
  yy <- (1 / (1 + exp(-(x2-inflection*10))) )

  # rescale y
  yout <- (ymax-ymin)*((yy-min(yy))/(max(yy)-min(yy)))+ymin
  # rescale y
  xout <- (xmax-xmin)*((x2-min(x2))/(max(x2)-min(x2)))+ xmin

  if (decreasing) {yout <- ymin-yout; print("doesn't work")}
  xout <- seq(xmin, xmax, length.out=length(yout))
  return(data.frame(xout,yout))
}

.knitrFunctions <- function(){
  return(data.frame(mainFunctions=c(".wc", ".wk", ".wf", ".mkMD", ".s1", ".k1", ".write_md"),
             Desc=c("write phrase or number","writes kabler wrap","writes figure","writes mdFilename", "", "", "write to an existing .md and close sink")))

}
# .knitrFunctions()



# annotation tools
.mkMD <- function(filename=NULL, title=NULL, notes=NULL,open=FALSE, createnew=FALSE){
  print(filename)
  if (!is.null(filename) & !is.null(mdFilename) & createnew) {
    cat(paste0("By default, you can only write to one markdown capture file at a time. The current file is ",
                 mdFilename,". If you want to set a new target, use createnew=TRUE.\n\n\n",
                 "Set mdFilename to the output of this function to make this change durable."))
    return()
  }
  if (!exists("mdFilename")) { mdFilename <- "keep";
    print("mdFilename not in local environment. Using 'keep'.") }
  #    if (is.null(listPickSort)) tmpList <- .listSorted() else tmpList <- .listSorted(listPickSort)
  if (is.null(title)) title <- mdFilename
  #file.create()
  fnUse <- paste0("./html/", mdFilename, ".Notes.md")
  if (!(file.exists(fnUse))) {
    sink(fnUse)
    #cat("<link rel=\"stylesheet\" href=\"RNotes.css\">\n")
    cat("\n## ", title, "\n", format(Sys.Date(), "%Y %b %d"))}
  else  sink(fnUse, append = TRUE)
  if (!is.null(notes)) cat("\n\n### ", notes, "\n-------\n") else cat("\n-------\n")
  # for (i in 1:nrow(tmpList)){
  #     cat(paste0("\n### Figure ", tmpList[i,]$FigNum, ". ", tmpList[i,]$captions, "\n<br />\n"))
  #     cat(paste0("\n\n![", tmpList[i,]$filename,"](./images/", tmpList[i,]$filename,")"))
  #     cat(paste0("\n\n### Notes: ", tmpList[i,]$Notes), "\n------\n\n")
  # }
  # sink()
  if (open) browseURL(fnUse)
  #mk
  return(mdFilename)
}



# use .mkMD WITH KABLE OR .sinkaddtxt(), followed by .sa()

.s1 <- function(txt){
  .mkMD()
  .sinkaddtxt(txt)
  .sa()
}

.k1 <- function(txt){
  require("knitr")
  .mkMD()
  kable(txt)
  .sa()
}

.write_md <- function(content, filename=paste0("./html/", mdFilename,".Notes.md")){
  # Open the sink connection to the specified file
  sink(filename, append = TRUE)

  # Write the content to the file
  cat("\n______\n",  content, "\n")

  # Close the sink connection
  sink()
}

