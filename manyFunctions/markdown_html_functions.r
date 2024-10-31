.html_graph<-function(graphfilepng, caption=NULL, showfilename=TRUE){
   if (!is.null(caption) & showfilename) caption<-paste(caption, graphfilepng, sep="\n")
   if (is.null(caption) & showfilename) caption<-graphfilepng
   if (!is.null(caption) & !showfilename) caption<-"graph"
   HTMLInsertGraph(GraphFileName=graphfilepng,
   Caption=.wordwrap( paste("<h2>",caption, "</h2>", sep=""), 150), GraphBorder=0, Align="center", WidthHTML=1200, HeightHTML=NULL, append=TRUE)
}



.qhtml<-function(x=NULL, txt="output from r", GraphFileName=NULL, filename=NULL, outdir = paste0(getwd(),"/notes_html")){
   require(R2HTML)
   if (is.null(filename)) if (txt!="output from r") filename=txt else filename<-paste(sample(letters[1:26], 10, replace = T), collapse="")

   filename<-paste0(format(Sys.Date(), "%Y.%m.%d"), filename)

   target<-HTMLInitFile(outdir = outdir, filename=filename, extension="html",
   HTMLframe=FALSE, BackGroundColor = "FFFFFF", BackGroundImg = "",
   Title = filename, CSSFile="/Users/jeff/Documents/RWork/mystyle.css", useLaTeX=TRUE, useGrid=TRUE)

   HTML(paste("<h1>", txt,"</h1>"),file=target)

   if (!is.null(GraphFileName)) {
   HTMLInsertGraph(GraphFileName=GraphFileName, GraphBorder=0, Align="center", WidthHTML=700, HeightHTML=NULL, append=TRUE)
      }

   if (!is.null(x)) {   HTML(x,file=target) }
   HTMLEndFile()

   .bu(target)
}


.qmd<-function(x=NULL, txt="output from r", GraphFileName=NULL, filename=NULL, outdir = paste0(getwd(),"/notes_html")){
   require(R2HTML)
   if (is.null(filename)) if (txt!="output from r") filename=txt else filename<-paste(sample(letters[1:26], 10, replace = T), collapse="")
   filename<-paste0(format(Sys.Date(), "%Y.%m.%d"),"_", filename)

   target<-HTMLInitFile(outdir = outdir, filename=filename, extension="html",
   HTMLframe=FALSE, BackGroundColor = "FFFFFF", BackGroundImg = "",
   Title = filename, CSSFile="/Users/jeff/Documents/RWork/mystyle.css", useLaTeX=TRUE, useGrid=TRUE)

   HTML(paste("<h1>", txt,"</h1>"),file=target)

   if (!is.null(GraphFileName)) {
       if (length(grep(".png", GraphFileName)==1)==0) grf<-paste0(GraphFileName, ".png")
   HTMLInsertGraph(GraphFileName=paste0("../graphspng_version/", grf), GraphBorder=0, Align="center", WidthHTML=700, HeightHTML=NULL, append=TRUE)
      }

   if (!is.null(x)) {   HTML(x,file=target) }
   HTMLEndFile()

   .bu(target)
}

.htm<-function(txt,lev=1){ HTML(paste0("<h",lev,">", txt,"</h",lev,">"),file=target)}



