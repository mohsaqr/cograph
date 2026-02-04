#' @title CographLayout R6 Class
#'
#' @description
#' Class for managing layout algorithms and computing node positions.
#'
#' @export
#' @examples
#' # Create a circular layout
#' layout <- CographLayout$new("circle")
#'
#' # Apply to network
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- CographNetwork$new(adj)
#' coords <- layout$compute(net)
CographLayout <- R6::R6Class(

  "CographLayout",
  public = list(
    #' @description Create a new CographLayout object.
    #' @param type Layout type (e.g., "circle", "spring", "groups").
    #' @param ... Additional parameters for the layout algorithm.
    #' @return A new CographLayout object.
    initialize = function(type = "circle", ...) {
      private$.type <- type
      private$.params <- list(...)
      invisible(self)
    },

    #' @description Compute layout coordinates for a network.
    #' @param network A CographNetwork object.
    #' @param ... Additional parameters passed to the layout function.
    #' @return Data frame with x, y coordinates.
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

    #' @description Normalize coordinates to 0-1 range with padding.
    #' @param coords Matrix or data frame with x, y columns.
    #' @param padding Numeric. Padding around edges (default 0.1).
    #' @return Normalized coordinates.
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
    #' @return Character string.
    get_type = function() {
      private$.type
    },

    #' @description Get layout parameters.
    #' @return List of parameters.
    get_params = function() {
      private$.params
    },

    #' @description Print layout summary.
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
