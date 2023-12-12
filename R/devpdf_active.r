# source("https://github.com/jg44/JGTools/blob/master/R/devpdf_active.r")

.devpdf <- function(file, pdfPath="graphs", openfile=FALSE, overwrite=FALSE, caption=NULL, powerpoint=TRUE, slidetitle=NULL,
                    png=TRUE, tf=NULL, meta=TRUE, NotesInput=FALSE, Notes="", history=TRUE, annotationsPath="./html/",
                    pathtoMDeditor="C:/Users/jrg1035/AppData/Local/Markdown Monster/MarkdownMonster.exe"){
  suppressWarnings(largestPdf <- as.numeric(gsub("\\D", "", substring(list.files(pdfPath), 1,2))))
  mxCur <- ifelse(((all(is.na(largestPdf))) || length(largestPdf)==0), 0, max(largestPdf, na.rm=TRUE))

  require("rstudioapi")
  # edited 2023.12.07
  tf <- rstudioapi::getActiveDocumentContext()$path
  projName <- .getProjName() #strsplit(rstudioapi::getActiveProject(),"/")[[1]][length(strsplit(rstudioapi::getActiveProject(),"/")[[1]])]
  projNamePath <- paste0("./",substring(tf, nchar(rstudioapi::getActiveProject())-nchar(projName), nchar(tf)))

  grep("/", rstudioapi::getActiveProject())
  if (!(dir.exists(pdfPath))) dir.create(pdfPath)
  if (is.null(pdfPath)) pdfPath=getwd()
  #if ((meta) && (is.null(tf))) tf<-.getfile()
  file <- .clean(file)
  ff<-paste(file,".pdf",sep="")
  ff<-gsub('//', '/', ff)  ## interior spaces
  ff<-gsub('.pdf.pdf', '.pdf', ff)
  fileExists <- ifelse(any(ff==substring(list.files(pdfPath),4,100)), TRUE, FALSE)
  if (overwrite && fileExists) {
    ff <- list.files(pdfPath)[which(ff==substring(list.files(pdfPath),4,100))]
  } else {
    inc <- ifelse(((!overwrite) && fileExists),0,1)
    nextNum <- formatC(mxCur+inc, width = 2, format = "d", flag = "0")
    ff <- paste0(nextNum, "_", ff)
  }
  # ff
  #
  # if (!overwrite){
  #     mxCur <- max(is.na(as.numeric((gsub("\\D", "", substring(list.files(pdfPath), 1,2))))), na.rm=TRUE)
  #     nextNum <- formatC(mxCur, width = 2, format = "d", flag = "0")
  #     ff <- paste0(nextNum, "_", ff)
  #
  # }
  # tmpMaxannotNum <- as.numeric(gsub("\\D", "", substring(list.files(annotationsPath, pattern = "*.annot.md"), 1,2)))
  #     if (all(is.na(tmpMaxannotNum))) mxCur <- 0 else mxCur <- max(tmpMaxannotNum, na.rm=TRUE)
  #     if (overwrite & mxCur>0) mxCur <- mxCur-1
  #     nextNum <- formatC(mxCur, width = 2, format = "d", flag = "0")
  #     annotFilenameCheck <- paste0(nextNum, "_", ff, ".annot.md")
  #     ff <- paste0(nextNum, "_", ff)
  if (!any(list.dirs()=="./notesGraphAnnot")) dir.create("./notesGraphAnnot") #else print("")
  #overwrite <- FALSE

  if (overwrite || (!fileExists)) {
    # print(overwrite)
    # print(fileExists)
    dev.copy2pdf(file=paste0(pdfPath, "/",ff))
    if (png) pngfile <- .pdf2png(paste0(pdfPath, "/", ff))
  } else
    if (!overwrite & fileExists) {
      print("File already exists. Opening existing file. Or choose overwrite=TRUE")
      browseURL(paste0(pdfPath, "/", ff))
    }
  if (openfile) browseURL(paste0(pdfPath, "/", ff))

  if ((meta) && (tf!="n")) {

    tmpMaxannotNum <- as.numeric(gsub("\\D", "", substring(list.files(annotationsPath, pattern = "*.annot.md"), 1,2)))
    if (all(is.na(tmpMaxannotNum))) mxCur <- 0 else mxCur <- max(tmpMaxannotNum, na.rm=TRUE)

    nextNum <- formatC(mxCur, width = 2, format = "d", flag = "0")
    annotFilenameCheck <- paste0(nextNum, "_", ff, ".annot.md")

    if (file.exists(paste0(annotationsPath, annotFilenameCheck)) &
        (overwrite))  annotFilename <- annotFilenameCheck  else     annotFilename <-
      paste0(ff, ".annot.md")
    # paste0(formatC(mxCur+1, width = 2, format = "d", flag = "0"), "_", ff, ".annot.md")


    sink(paste0(annotationsPath, annotFilename))
    if (NotesInput) {
      cat("Approximate line: ", readline("Approx. line? "))
      cat("\nNotes: ", readline("Notes? "))
      cat("\nFigure #: ", readline("Figure #: "))
    }

    bbname <- paste0("images/",strsplit(pngfile, "\\\\")[[1]][length(strsplit(pngfile, "\\\\")[[1]])])
    cat("\n# ", .clean(file))
    cat(paste0("\n**Caption:** ", caption,'\n'))
    cat("\n![", bbname, "](", bbname,")", sep="")
    cat("\n\n###### ", gsub("\\./", paste0(pdfPath, "/"), ff), sep="")

    cat("\n### Script file: ")
    cat(paste0("\n", projNamePath))
    #cat("\n")
    cat("\nTimestamp: ", date(), "\n")

    #cat("\n\n[", .clean(file), "_png]:", sep="")
    #cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
    #cat("\n\n[", .clean(file), "]:", sep="")
    #cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
    cat(paste0("\n", "Notes: ", Notes))
    cat("\n------------------------------\n")
    .sinkall()
    browseURL(paste0(annotationsPath, annotFilename))
  }
  if (history) {
    sink(paste0(annotationsPath, "ProjectHistoryFile.md"), append = TRUE)
    cat(paste0("\n### Script file: ", tf))
    cat("\n")
    cat("\nTimestamp: ", date(), "\n")

    cat("\n###", .clean(file))
    cat(paste0("\nCaption: ", caption,'\n'))
    cat("\n![", .clean(file), "](/images/",.clean(file), ".png)", sep="")
    cat("\n\n###### ", gsub("\\./", paste0(getwd(), "/"), ff), sep="")

    # cat("\n## ", .clean(file))
    # cat("\n[![", .clean(file), "][", .clean(file),"_png]][",.clean(file), "]", sep="")
    # cat("\n", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
    # cat("\n\n[", .clean(file), "_png]:", sep="")
    # cat("\nfile:///", gsub(".pdf", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
    # cat("\n\n[", .clean(file), "]:", sep="")
    # cat("\nfile:///", gsub(".dummy1", ".png", gsub("\\./", paste0(getwd(), "/"), ff)), sep="")
    cat(paste0("\n", "Notes: ", Notes))
    cat("\n------\n")
    .sinkall()
  }

  #make Powerpoint
  if (powerpoint) {
    if (is.null(caption)) caption <- .clean(file)
    if (is.null(slidetitle)) slidetitle <- .clean(file)
    sink(paste0(annotationsPath, "/powerpoint.md"), append = TRUE)
    #cat(paste0("\n### Script file: ", tf))
    cat("\n\n# ", slidetitle, sep="")
    cat("\n![", caption, "](/images/",.clean(file), ".png)", sep="")
    cat("\n")
    .sinkall()
  }
}

#
# file="log_of_y_issuses_for_bre"
# .devpdf(file, overwrite = TRUE)
