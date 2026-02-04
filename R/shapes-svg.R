#' @title Custom SVG Node Shapes
#' @description Functions for rendering custom SVG shapes as nodes.
#' @name shapes-svg
#' @keywords internal
NULL

# Global registry for custom SVG shapes
svg_shape_registry <- new.env(parent = emptyenv())

#' Register Custom SVG Shape
#'
#' Register an SVG file or string as a custom node shape.
#'
#' @param name Character: unique name for this shape (used in node_shape parameter).
#' @param svg_source Character: path to SVG file OR inline SVG string.
#' @return Invisible NULL. The shape is registered for use with sn_nodes().
#' @export
#'
#' @examples
#' \dontrun{
#' # Register from file
#' register_svg_shape("custom_icon", "path/to/icon.svg")
#'
#' # Register from inline SVG
#' register_svg_shape("simple_star",
#'   '<svg viewBox="0 0 100 100">
#'     <polygon points="50,5 20,99 95,39 5,39 80,99" fill="currentColor"/>
#'   </svg>')
#'
#' # Use in network
#' cograph(adj) |> sn_nodes(shape = "custom_icon")
#' }
register_svg_shape <- function(name, svg_source) {
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string", call. = FALSE)
  }

  if (!is.character(svg_source) || length(svg_source) != 1) {
    stop("svg_source must be a single character string (file path or SVG content)",
         call. = FALSE)
  }

  # Check if it's a file path or inline SVG
  is_file <- file.exists(svg_source)

  # Store the SVG data
  svg_data <- list(
    source = svg_source,
    is_file = is_file,
    parsed = NULL  # Will be populated on first use
  )

  # Register in the SVG shape registry
  svg_shape_registry[[name]] <- svg_data

  # Also register as a shape for the main registry
  register_shape(name, function(x, y, size, fill, border_color, border_width,
                                 alpha = 1, svg_preserve_aspect = TRUE, ...) {
    draw_svg_shape(x, y, size, svg_data, fill, border_color, border_width,
                   alpha, svg_preserve_aspect)
  })

  invisible(NULL)
}

#' Get Registered SVG Shape
#'
#' Retrieve SVG shape data by name.
#'
#' @param name Shape name.
#' @return SVG data list or NULL if not found.
#' @keywords internal
get_svg_shape <- function(name) {
  if (exists(name, envir = svg_shape_registry)) {
    svg_shape_registry[[name]]
  } else {
    NULL
  }
}

#' Parse SVG Content
#'
#' Parse SVG from string or file.
#'
#' @param svg_data SVG data list from registry.
#' @return Parsed SVG object (grImport2 Picture or NULL).
#' @keywords internal
parse_svg <- function(svg_data) {
  if (!is.null(svg_data$parsed)) {
    return(svg_data$parsed)
  }

  # Check for grImport2 package
  if (!requireNamespace("grImport2", quietly = TRUE)) {
    warning("Package 'grImport2' is required for SVG shapes. ",
            "Install with: install.packages('grImport2')",
            call. = FALSE)
    return(NULL)
  }

  tryCatch({
    if (svg_data$is_file) {
      # Read from file
      parsed <- grImport2::readPicture(svg_data$source)
    } else {
      # Parse from string - write to temp file first
      temp_file <- tempfile(fileext = ".svg")
      on.exit(unlink(temp_file), add = TRUE)
      writeLines(svg_data$source, temp_file)
      parsed <- grImport2::readPicture(temp_file)
    }

    # Cache the parsed result
    svg_data$parsed <- parsed
    parsed
  }, error = function(e) {
    warning("Failed to parse SVG: ", e$message, call. = FALSE)
    NULL
  })
}

#' Draw SVG Shape (Grid)
#'
#' Render an SVG as a node shape using grid graphics.
#'
#' @param x,y Node center coordinates (NPC units).
#' @param size Node size (NPC units).
#' @param svg_data SVG data list from registry.
#' @param fill Fill color (replaces SVG fill colors).
#' @param border_color Border color.
#' @param border_width Border width.
#' @param alpha Transparency.
#' @param preserve_aspect Maintain SVG aspect ratio.
#' @return Grid grob or nullGrob if SVG unavailable.
#' @keywords internal
draw_svg_shape <- function(x, y, size, svg_data, fill, border_color, border_width,
                           alpha = 1, preserve_aspect = TRUE) {

  parsed <- parse_svg(svg_data)

  if (is.null(parsed)) {
    # Fallback to circle if SVG parsing fails
    fill_col <- adjust_alpha(fill, alpha)
    border_col <- adjust_alpha(border_color, alpha)

    return(grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
    ))
  }

  # Check for grImport2
  if (!requireNamespace("grImport2", quietly = TRUE)) {
    fill_col <- adjust_alpha(fill, alpha)
    border_col <- adjust_alpha(border_color, alpha)

    return(grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
    ))
  }

  # Create viewport for the SVG
  vp <- grid::viewport(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    width = grid::unit(size * 2, "npc"),
    height = grid::unit(size * 2, "npc")
  )

  # Draw the picture in the viewport
  # Note: grImport2::pictureGrob handles the rendering
  tryCatch({
    grob <- grImport2::pictureGrob(
      parsed,
      x = 0.5, y = 0.5,
      width = 1, height = 1,
      just = "center",
      default.units = "npc",
      expansion = 0,
      clip = "off"
    )

    # Wrap in a gTree with the viewport
    grid::gTree(
      children = grid::gList(grob),
      vp = vp
    )
  }, error = function(e) {
    warning("Failed to render SVG: ", e$message, call. = FALSE)

    # Fallback to circle
    fill_col <- adjust_alpha(fill, alpha)
    border_col <- adjust_alpha(border_color, alpha)

    grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
    )
  })
}

#' Draw SVG Shape (Base R)
#'
#' Render an SVG as a node shape using base R graphics.
#' Falls back to circle if rasterization fails.
#'
#' @param x,y Node center coordinates.
#' @param size Node size.
#' @param svg_data SVG data list from registry.
#' @param fill Fill color.
#' @param border_color Border color.
#' @param border_width Border width.
#' @keywords internal
draw_svg_shape_base <- function(x, y, size, svg_data, fill, border_color, border_width) {
  # For Base R, we attempt to use rsvg to rasterize and rasterImage to draw
  # This requires the 'rsvg' package

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    # Fallback to circle
    graphics::symbols(
      x = x, y = y,
      circles = size,
      inches = FALSE, add = TRUE,
      fg = border_color, bg = fill, lwd = border_width
    )
    return(invisible())
  }

  tryCatch({
    # Get SVG content
    svg_content <- if (svg_data$is_file) {
      readLines(svg_data$source, warn = FALSE)
    } else {
      svg_data$source
    }
    svg_content <- paste(svg_content, collapse = "\n")

    # Rasterize to bitmap
    bitmap <- rsvg::rsvg(charToRaw(svg_content), width = 100, height = 100)

    # Draw as raster image
    graphics::rasterImage(
      bitmap,
      xleft = x - size,
      ybottom = y - size,
      xright = x + size,
      ytop = y + size
    )
  }, error = function(e) {
    # Fallback to circle
    graphics::symbols(
      x = x, y = y,
      circles = size,
      inches = FALSE, add = TRUE,
      fg = border_color, bg = fill, lwd = border_width
    )
  })

  invisible()
}

#' List Registered SVG Shapes
#'
#' Get names of all registered custom SVG shapes.
#'
#' @return Character vector of registered shape names.
#' @export
#'
#' @examples
#' list_svg_shapes()
list_svg_shapes <- function() {
  ls(envir = svg_shape_registry)
}

#' Unregister SVG Shape
#'
#' Remove a custom SVG shape from the registry.
#'
#' @param name Shape name to remove.
#' @return Invisible TRUE if removed, FALSE if not found.
#' @export
#' @examples
#' # Attempt to unregister a non-existent shape (returns FALSE)
#' unregister_svg_shape("nonexistent")
unregister_svg_shape <- function(name) {
  if (exists(name, envir = svg_shape_registry)) {
    rm(list = name, envir = svg_shape_registry)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}
