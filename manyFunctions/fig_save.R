# .graphwindow()
# plot(1, 1)
# fig_save("example_plot")
# new fig_save function to replace .devpdf

fig_save <- function(
        name,
        root = ".",
        pdf_dir = "graphs",
        img_dir = file.path("html", "images"),
        annot_dir = file.path("html", "annotations"),
        gallery_file = file.path("html", "figures.md"),
        make_pdf = TRUE,
        make_png = TRUE,
        open_pdf = FALSE
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
    
    script_rel <- get_script_relpath(root)
    
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
    
    resolve_name <- function(slug, pdf_dir,
                             int_width = 2,
                             rev_width = 2) {
        
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
    
    # ---------- ensure directories ----------
    
    dirs <- c(pdf_dir, img_dir, annot_dir, dirname(gallery_file))
    for (d in dirs)
        dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
    
    slug <- fig_slug(name)
    pdf_path <- file.path(root, pdf_dir)
    
    base_name <- resolve_name(slug, pdf_path)
    
    pdf_file  <- file.path(root, pdf_dir, paste0(base_name, ".pdf"))
    png_file  <- file.path(root, img_dir, paste0(base_name, ".png"))
    annot_file <- file.path(root, annot_dir, paste0(base_name, ".md"))
    
    # ---------- capture current plot ----------
    
    if (grDevices::dev.cur() == 1)
        stop("No active graphics device.")
    
    rp <- grDevices::recordPlot()
    
    # ---------- write PDF ----------
    
    if (make_pdf) {
        size_in <- grDevices::dev.size("in")
        
        grDevices::cairo_pdf(
            filename = pdf_file,
            width  = size_in[1],
            height = size_in[2]
        )
        grDevices::replayPlot(rp)
        grDevices::dev.off()
        
        if (open_pdf)
            utils::browseURL(pdf_file)
    }
    
    # ---------- write PNG ----------
    
    if (make_png) {
        
        size_in <- grDevices::dev.size("in")
        size_px <- grDevices::dev.size("px")
        
        fallback_res <- 300
        
        if (!any(is.na(size_px))) {
            width_px  <- size_px[1]
            height_px <- size_px[2]
            res_use   <- width_px / size_in[1]
        } else {
            width_px  <- size_in[1] * fallback_res
            height_px <- size_in[2] * fallback_res
            res_use   <- fallback_res
        }
        
        grDevices::png(
            filename = png_file,
            width  = width_px,
            height = height_px,
            res    = res_use
        )
        grDevices::replayPlot(rp)
        grDevices::dev.off()
    }
    
    # ---------- annotation stub ----------
    
    if (!file.exists(annot_file)) {
        writeLines(
            c(
                paste0("# ", base_name),
                "",
                if (make_png)
                    paste0("![](../images/", basename(png_file), ")")
                else "",
                "",
                if (make_pdf)
                    paste0("PDF: ../", pdf_dir, "/", basename(pdf_file))
                else "",
                "",
                if (!is.null(script_rel))
                    paste0("Script: ", script_rel)
                else "",
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
            if (make_png)
                paste0("![](images/", basename(png_file), ")\n\n")
            else "",
            if (make_pdf)
                paste0("[PDF](", pdf_dir, "/", basename(pdf_file), ") | ")
            else "",
            "[Notes](annotations/", basename(annot_file), ")\n\n",
            if (!is.null(script_rel))
                paste0("- Script: ", script_rel, "\n")
            else "",
            "- Saved: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
        ),
        file = file.path(root, gallery_file),
        append = TRUE
    )
    
    invisible(list(
        pdf = if (make_pdf) pdf_file else NULL,
        png = if (make_png) png_file else NULL,
        annot = annot_file
    ))
}


