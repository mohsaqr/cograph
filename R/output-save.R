#' @title Output and Saving
#' @description Functions for saving network visualizations to files.
#' @name output-save
NULL

#' Save Network Visualization
#'
#' Save a Cograph network visualization to a file.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param filename Output filename. Format is detected from extension.
#' @param width Width in inches (default 7).
#' @param height Height in inches (default 7).
#' @param dpi Resolution for raster formats (default 300).
#' @param title Optional plot title.
#' @param ... Additional arguments passed to the graphics device.
#'
#' @return Invisible filename.
#' @export
#'
#' @examples
#' \dontrun{
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' # With cograph()
#' net <- cograph(adj)
#' sn_save(net, "network.pdf")
#'
#' # Direct matrix input
#' sn_save(adj, "network.png", dpi = 300)
#' }
sn_save <- function(network, filename, width = 7, height = 7, dpi = 300,
                    title = NULL, ...) {

  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  # Detect format from extension
  ext <- tolower(tools::file_ext(filename))

  if (ext == "") {
    stop("Filename must have an extension (e.g., .pdf, .png, .svg)",
         call. = FALSE)
  }

  # Open device
  device_opened <- FALSE
  on.exit({
    if (device_opened) grDevices::dev.off()
  })

  switch(ext,
    pdf = {
      grDevices::pdf(filename, width = width, height = height, ...)
      device_opened <- TRUE
    },
    png = {
      grDevices::png(filename, width = width, height = height,
                     units = "in", res = dpi, ...)
      device_opened <- TRUE
    },
    svg = {
      grDevices::svg(filename, width = width, height = height, ...)
      device_opened <- TRUE
    },
    jpeg = ,
    jpg = {
      grDevices::jpeg(filename, width = width, height = height,
                      units = "in", res = dpi, quality = 95, ...)
      device_opened <- TRUE
    },
    tiff = {
      grDevices::tiff(filename, width = width, height = height,
                      units = "in", res = dpi, compression = "lzw", ...)
      device_opened <- TRUE
    },
    eps = ,
    ps = {
      grDevices::postscript(filename, width = width, height = height,
                            paper = "special", horizontal = FALSE, ...)
      device_opened <- TRUE
    },
    {
      stop("Unsupported format: ", ext,
           ". Supported: pdf, png, svg, jpeg, tiff, eps", call. = FALSE)
    }
  )

  # Render
  sn_render(network, title = title)

  message("Saved to: ", filename)
  invisible(filename)
}

#' Save as ggplot2
#'
#' Save network as a ggplot2 object to file using ggsave.
#'
#' @param network A cograph_network object.
#' @param filename Output filename.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution for raster formats.
#' @param title Optional plot title.
#' @param ... Additional arguments passed to ggsave.
#'
#' @return Invisible filename.
#' @export
#'
#' @examples
#' \dontrun{
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- cograph(adj)
#' sn_save_ggplot(net, "network.pdf")
#' }
sn_save_ggplot <- function(network, filename, width = 7, height = 7,
                           dpi = 300, title = NULL, ...) {

  p <- sn_ggplot(network, title = title)
  ggplot2::ggsave(filename, plot = p, width = width, height = height,
                  dpi = dpi, ...)
  message("Saved to: ", filename)
  invisible(filename)
}
