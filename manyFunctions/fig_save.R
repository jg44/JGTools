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
    make_pdf = TRUE,
    make_png = TRUE,
    open_pdf = FALSE,
    png_res = 300
) {

  # ---------- helpers ----------

  get_script_relpath <- function(root) {
    if (!requireNamespace("rstudioapi", quietly = TRUE))
      return(NULL)
    if (!rstudioapi::isAvailable())
      return(NULL)

    ctx <- rstudioapi::getSourceEditorContext()
    if (is.null(ctx$path) || ctx$path == "")
      return(NULL)

    full <- normalizePath(ctx$path, winslash = "/")
    root <- normalizePath(root, winslash = "/")

    if (startsWith(full, root)) {
      sub(paste0("^", root, "/?"), "", full)
    } else {
      basename(full)
    }
  }

  fig_slug <- function(x) {
    x <- trimws(x)
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("[^A-Za-z0-9_\\-]+", "", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    if (nchar(x) == 0) "figure" else x
  }

  next_integer_prefix <- function(pdf_dir, width = 2) {
    files <- list.files(pdf_dir, pattern = "^[0-9]+")
    if (length(files) == 0)
      return(sprintf(paste0("%0", width, "d"), 1))

    nums <- as.numeric(sub("^([0-9]+).*", "\\1", files))
    nums <- nums[!is.na(nums)]
    sprintf(paste0("%0", width, "d"), max(nums) + 1L)
  }

  resolve_name <- function(slug, pdf_dir, int_width = 2, rev_width = 2) {
    pattern <- paste0("^[0-9]+(\\.[0-9]+)?_", slug, "\\.(pdf|png)$")
    matches <- list.files(pdf_dir, pattern = pattern)

    if (length(matches) == 0) {
      prefix <- next_integer_prefix(pdf_dir, int_width)
      return(paste0(prefix, "_", slug))
    }

    base_prefix <- sub("^([0-9]+).*", "\\1", matches[1])

    revs <- sub(paste0("^", base_prefix, "(\\.[0-9]+)?_.*$"), "\\1", matches)
    revs <- gsub("^\\.", "", revs)
    revs[revs == ""] <- NA
    revs <- as.numeric(revs)

    next_rev <- if (all(is.na(revs))) 1 else max(revs, na.rm = TRUE) + 1

    paste0(
      base_prefix,
      ".",
      sprintf(paste0("%0", rev_width, "d"), next_rev),
      "_",
      slug
    )
  }

  # ---------- determine what we are saving ----------

  if (grDevices::dev.cur() == 1)
    stop("No active graphics device.")

  # Capture size from the CURRENT device (your on-screen plot window)
  size_in <- grDevices::dev.size("in")

  is_gg <- inherits(plot_obj, "ggplot")

  # If plot_obj is NULL: assume base graphics and record what is on screen
  # If plot_obj is ggplot: redraw into each device
  if (is.null(plot_obj)) {
    rp <- grDevices::recordPlot()
  } else if (!is_gg) {
    stop("plot_obj supplied, but it is not a ggplot object. For base graphics, call fig_save(name) with plot_obj = NULL.")
  }

  # ---------- ensure directories ----------

  dirs <- c(pdf_dir, img_dir, annot_dir, dirname(gallery_file))
  for (d in dirs)
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)

  script_rel <- get_script_relpath(root)
  slug <- fig_slug(name)

  pdf_path <- file.path(root, pdf_dir)
  base_name <- resolve_name(slug, pdf_path)

  pdf_file   <- file.path(root, pdf_dir,  paste0(base_name, ".pdf"))
  png_file   <- file.path(root, img_dir,  paste0(base_name, ".png"))
  annot_file <- file.path(root, annot_dir, paste0(base_name, ".md"))

  # ---------- writer (single place so we don't accidentally mix replay/print) ----------

  draw_into_device <- function() {
    if (is.null(plot_obj)) {
      grDevices::replayPlot(rp)
    } else {
      print(plot_obj)
    }
  }

  # ---------- write PDF ----------

  if (make_pdf) {
    grDevices::cairo_pdf(
      filename = pdf_file,
      width  = size_in[1],
      height = size_in[2]
    )
    draw_into_device()
    grDevices::dev.off()

    if (open_pdf)
      utils::browseURL(pdf_file)
  }

  # ---------- write PNG (Windows-safe Cairo backend) ----------
  if (make_png) {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      stop("Package 'ragg' is required for PNG output. Install it with install.packages('ragg').")
    }

    ragg::agg_png(
      filename = png_file,
      width  = size_in[1],
      height = size_in[2],
      units  = "in",
      res    = png_res
    )

    draw_into_device()

    grDevices::dev.off()
  }

  # ---------- annotation stub ----------

  if (!file.exists(annot_file)) {
    writeLines(
      c(
        paste0("# ", base_name),
        "",
        if (make_png) paste0("![](../images/", basename(png_file), ")") else "",
        "",
        if (make_pdf) paste0("PDF: ../", pdf_dir, "/", basename(pdf_file)) else "",
        "",
        if (!is.null(script_rel)) paste0("Script: ", script_rel) else "",
        "",
        paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        "## Notes",
        ""
      ),
      con = annot_file
    )
  }

  # ---------- append to gallery ----------

  cat(
    paste0(
      "\n\n---\n\n",
      "## ", base_name, "\n\n",
      if (make_png) paste0("![](images/", basename(png_file), ")\n\n") else "",
      if (make_pdf) paste0("[PDF](", pdf_dir, "/", basename(pdf_file), ") | ") else "",
      "[Notes](annotations/", basename(annot_file), ")\n\n",
      if (!is.null(script_rel)) paste0("- Script: ", script_rel, "\n") else "",
      "- Saved: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
    ),
    file = file.path(root, gallery_file),
    append = TRUE
  )

  invisible(list(
    pdf = if (make_pdf) pdf_file else NULL,
    png = if (make_png) png_file else NULL,
    annot = annot_file,
    base_name = base_name,
    size_in = size_in
  ))
}
