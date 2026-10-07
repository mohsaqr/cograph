#' @title CographLayout R6 Class
#'
#' @description
#' Class for managing layout algorithms and computing node positions.
#'
#' @return A \code{CographLayout} R6 object.
#' @export
#' @examples
#' layout <- CographLayout$new("circle")
#' layout$compute(CographNetwork$new(regulation_net))
CographLayout <- R6::R6Class(

  "CographLayout",
  public = list(
    #' @description Create a new CographLayout object.
    #' @param type Layout name. One of the names returned by
    #'   [list_layouts()], or `"custom"` together with a `coords` argument.
    #' @param ... Additional parameters stored and passed to the layout function.
    #' @return A new CographLayout object.
    initialize = function(type = "circle", ...) {
      private$.type <- type
      private$.params <- list(...)
      invisible(self)
    },

    #' @description Compute layout coordinates for a network.
    #' @param network A CographNetwork or cograph_network object.
    #' @param ... Additional parameters passed to the layout function. They
    #'   override parameters given to `$new()`.
    #' @return A data frame with columns `x` and `y`, one row per node, rescaled
    #'   by `$normalize_coords()`.
    compute = function(network, ...) {
      if (!is_cograph_network(network) && !inherits(network, "CographNetwork")) {
        stop("network must be a CographNetwork object", call. = FALSE)
      }

      # Handle custom coordinates
      if (private$.type == "custom") {
        coords <- private$.params$coords
        if (is.null(coords)) {
          stop("Custom layout requires 'coords' parameter", call. = FALSE)
        }
        return(self$normalize_coords(coords))
      }

      # Get layout function from registry
      layout_fn <- get_layout(private$.type)
      if (is.null(layout_fn)) {
        stop("Unknown layout type: ", private$.type, call. = FALSE)
      }

      # Merge parameters
      params <- utils::modifyList(private$.params, list(...))

      # Compute coordinates
      coords <- do.call(layout_fn, c(list(network = network), params))

      # Normalize to 0-1 range
      self$normalize_coords(coords)
    },

    #' @description Rescale coordinates into the unit square. Both axes are
    #'   scaled by the same factor, so the larger spread spans
    #'   `[padding, 1 - padding]` and the layout is centered at 0.5.
    #' @param coords Matrix or data frame. Columns `x` and `y` are used, or the
    #'   first two columns when these names are absent.
    #' @param padding Numeric. Margin left on each side of the larger spread.
    #' @return A data frame with rescaled `x` and `y` columns.
    normalize_coords = function(coords, padding = 0.1) {
      if (is.matrix(coords)) {
        coords <- as.data.frame(coords)
      }
      if (!all(c("x", "y") %in% names(coords))) {
        names(coords)[1:2] <- c("x", "y")
      }

      # Normalize to [padding, 1-padding] using uniform scaling to preserve aspect ratio
      x_range <- range(coords$x, na.rm = TRUE)
      y_range <- range(coords$y, na.rm = TRUE)

      max_spread <- max(diff(x_range), diff(y_range))

      if (max_spread > 0) {
        scale <- (1 - 2 * padding) / max_spread
        x_center <- mean(x_range)
        y_center <- mean(y_range)
        coords$x <- 0.5 + (coords$x - x_center) * scale
        coords$y <- 0.5 + (coords$y - y_center) * scale
      } else {
        coords$x <- 0.5
        coords$y <- 0.5
      }

      coords
    },

    #' @description Get layout type.
    #' @return A character string.
    get_type = function() {
      private$.type
    },

    #' @description Get layout parameters.
    #' @return A list of the parameters given to `$new()`.
    get_params = function() {
      private$.params
    },

    #' @description Print layout summary.
    #' @return The object itself, invisibly.
    print = function() {
      cat("CographLayout\n")
      cat("  Type:", private$.type, "\n")
      if (length(private$.params) > 0) {
        cat("  Parameters:\n")
        for (nm in names(private$.params)) {
          val <- private$.params[[nm]]
          if (length(val) > 3) {
            val <- paste0(paste(val[1:3], collapse = ", "), ", ...")
          }
          cat("    ", nm, ":", val, "\n")
        }
      }
      invisible(self)
    }
  ),

  private = list(
    .type = NULL,
    .params = NULL
  )
)
