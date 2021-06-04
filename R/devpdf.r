#' Adds axis labels in preferred style
#'
#' This function outputs active plot to a pdf file and (optionally) a png, including (optionally) metadata
#' @param file Text string in quotes pdf (and png) filename
#' @param wd Text string in quotes -- path to graph directory (defaults to "./graphs")
#' @param open Logical. Open resulting pdf file? Default = TRUE
#' @param overwrite Logical. Overwrite pdf file? Default = FALSE
#' @param png Logical. Make png file for inclusion in other applications (i.e., html)
#' @param tf Collects current .r file and path for output (Requires rstudioapi)
#' @param meta Logical. Prompts for figure number, notes and line #'s for output to text metafile
#' @keywords plot
#' @keywords pdf
#' @keywords png
#' @export
#' @examples
#' devpdf("plotoutput", wd="", open=TRUE, overwrite=TRUE, png=TRUE, meta=FALSE)

devpdf <- function(file, wd="./graphs", open=TRUE, overwrite=FALSE, png=FALSE, tf=NULL, meta=FALSE){
  require("rstudioapi")
  tf <- rstudioapi::getActiveDocumentContext()$path
  if (is.null(wd)) wd=getwd()
  if ((meta) && (is.null(tf))) tf<-.getfile()
  ff<-paste(wd,"/",file,".pdf",sep="")
  ff<-gsub('//', '/', ff)  ## interior spaces
  ff<-gsub('.pdf.pdf', '.pdf', ff)
  if ((overwrite) || (!(file.exists(ff)))) dev.copy2pdf(file=ff) else
    if (overwrite) { print("File already exists.  Opening existing file.")
      ff<-paste(wd,"/",file,format(Sys.time(), "%Y%m%dhr%H"),".pdf",sep="_")
      dev.copy2pdf(file=ff)}
  if (open) browseURL(ff)
  if (png) {
    .pdf2png(ff)
    # system(paste("mkdir '",wd, "/png_version/'", sep=""))
    # ff <- gsub("/", "\\\\", ff)
    # fng <- gsub(".pdf", ".png", ff)
    # fn<-strsplit(ff, "/")[[1]][length(strsplit(ff, "/")[[1]])]
    # #pngpath<-paste(wd, "/png_version/", sep="")
    # #x<-paste("sips -s format png '", ff, "' --out '", pngpath, substring(fn, 1, nchar(fn)-4), ".png'", sep="")
    # x <- paste0('"C:/Program Files/ImageMagick-7.0.8-Q16/magick.exe" convert -density 288 "', ff, '" -resize 25% "', fng)
    # system(x)
  }
  if ((meta) && (tf!="n")) {
    sink(paste0(ff, ".metadata.txt"))
    cat("Script file:  ")
    cat(paste0("\n", tf))
    cat("\n")
    cat("Approximate line: ", readline("Approx. line? "))
    cat("\nNotes: ", readline("Notes? "))
    cat("\nFigure #: ", readline("Figure #: "))
    cat("\nTimestamp: ", date())
    .sinkall()
  }
  print(ff)
}
