# replacement for .devpdf.
# technically:
# x11()
# plot(1,1)
# .fig_save("title") # for base graphics
# or p <- ggplot(...)
# x11(); print(p)
# .fig_save("title", plot_object=p) # for ggplot2 plotting

.fig_save <- function(
    name,
    plot_obj = NULL,
    root = ".",
    pdf_dir = "graphs",
    img_dir = file.path("html", "images"),
    annot_dir = file.path("html", "annotations"),
    gallery_file = file.path("html", "figures.md"),
    width = 9.75,
    height = 8.42,
    units = "in",
    dpi = 300,
    make_pdf = TRUE,
    make_png = TRUE
) {

  # ---------------- helpers ----------------

  fig_slug <- function(x) {
    x <- trimws(x)
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("[^A-Za-z0-9_\\-]+", "", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    if (nchar(x) == 0) "figure" else x
  }

  next_prefix <- function(dir, width = 2) {
    files <- list.files(dir, pattern = "^[0-9]+")
    if (length(files) == 0) return(sprintf(paste0("%0", width, "d"), 1))
    nums <- as.numeric(sub("^([0-9]+).*", "\\1", files))
    nums <- nums[!is.na(nums)]
    sprintf(paste0("%0", width, "d"), max(nums) + 1L)
  }

  # ---------------- setup ----------------

  dirs <- c(pdf_dir, img_dir, annot_dir, dirname(gallery_file))
  for (d in dirs) dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)

  slug <- fig_slug(name)
  prefix <- next_prefix(file.path(root, pdf_dir))
  base_name <- paste0(prefix, "_", slug)

  pdf_file   <- file.path(root, pdf_dir,  paste0(base_name, ".pdf"))
  png_file   <- file.path(root, img_dir,  paste0(base_name, ".png"))
  annot_file <- file.path(root, annot_dir, paste0(base_name, ".md"))


  # ---------- auto-capture base plot if none supplied ----------

  if (is.null(plot_obj)) {
    if (grDevices::dev.cur() == 1) {
      stop("No active plot to capture.")
    }
    plot_obj <- grDevices::recordPlot()
  }

  # ---------------- validate plot ----------------

  is_gg <- inherits(plot_obj, "ggplot")
  is_base_recorded <- inherits(plot_obj, "recordedplot")

  if (!is_gg && !is_base_recorded) {
    stop("plot_obj must be a ggplot or recordedplot.")
  }

  is_gg <- inherits(plot_obj, "ggplot")
  is_base_recorded <- inherits(plot_obj, "recordedplot")

  if (!is_gg && !is_base_recorded) {
    stop("plot_obj must be a ggplot or recordedplot.")
  }

  draw_plot <- function() {
    if (is_gg) {
      print(plot_obj)
    } else {
      grDevices::replayPlot(plot_obj)
    }
  }

  # ---------------- save PDF ----------------

  if (make_pdf) {
    grDevices::cairo_pdf(pdf_file, width = width, height = height)
    draw_plot()
    grDevices::dev.off()

    if (!file.exists(pdf_file)) {
      stop("PDF not created: ", pdf_file)
    }
  }

  # ---------------- save PNG ----------------

  if (make_png) {
    grDevices::png(
      filename = png_file,
      width = width,
      height = height,
      units = units,
      res = dpi,
      type = "cairo"
    )
    draw_plot()
    grDevices::dev.off()

    if (!file.exists(png_file)) {
      stop("PNG not created: ", png_file)
    }
  }

  # ---------------- annotation ----------------

  if (!file.exists(annot_file)) {
    writeLines(
      c(
        paste0("# ", base_name),
        "",
        if (make_png) paste0("![](../images/", basename(png_file), ")") else "",
        "",
        if (make_pdf) paste0("PDF: ../", pdf_dir, "/", basename(pdf_file)) else "",
        "",
        paste0("Saved: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        "## Notes",
        ""
      ),
      annot_file
    )
  }

  # ---------------- gallery ----------------

  cat(
    paste0(
      "\n\n---\n\n",
      "## ", base_name, "\n\n",
      if (make_png) paste0("![](images/", basename(png_file), ")\n\n") else "",
      if (make_pdf) paste0("[PDF](", pdf_dir, "/", basename(pdf_file), ")\n\n") else "",
      "- Saved: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
    ),
    file = file.path(root, gallery_file),
    append = TRUE
  )

  invisible(list(
    pdf = if (make_pdf) pdf_file else NULL,
    png = if (make_png) png_file else NULL,
    annot = annot_file,
    base_name = base_name
  ))
}
