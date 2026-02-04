#' @title CographTheme R6 Class
#'
#' @description
#' Class for managing visual themes for network plots.
#'
#' @export
#' @examples
#' # Create a custom theme
#' theme <- CographTheme$new(
#'   background = "white",
#'   node_fill = "steelblue",
#'   edge_color = "gray60"
#' )
CographTheme <- R6::R6Class(
  "CographTheme",
  public = list(
    #' @description Create a new CographTheme object.
    #' @param name Theme name (optional).
    #' @param background Background color.
    #' @param node_fill Default node fill color.
    #' @param node_border Default node border color.
    #' @param node_border_width Default node border width.
    #' @param edge_color Default edge color.
    #' @param edge_positive_color Color for positive edge weights.
    #' @param edge_negative_color Color for negative edge weights.
    #' @param edge_width Default edge width.
    #' @param label_color Default label color.
    #' @param label_size Default label size.
    #' @param title_color Title color.
    #' @param title_size Title size.
    #' @param legend_background Legend background color.
    #' @return A new CographTheme object.
    initialize = function(
      name = "custom",
      background = "white",
      node_fill = "#4A90D9",
      node_border = "#2C5AA0",
      node_border_width = 1,
      edge_color = "gray50",
      edge_positive_color = "#2E7D32",
      edge_negative_color = "#C62828",
      edge_width = 1,
      label_color = "black",
      label_size = 10,
      title_color = "black",
      title_size = 14,
      legend_background = "white"
    ) {
      private$.name <- name
      private$.params <- list(
        background = background,
        node_fill = node_fill,
        node_border = node_border,
        node_border_width = node_border_width,
        edge_color = edge_color,
        edge_positive_color = edge_positive_color,
        edge_negative_color = edge_negative_color,
        edge_width = edge_width,
        label_color = label_color,
        label_size = label_size,
        title_color = title_color,
        title_size = title_size,
        legend_background = legend_background
      )
      invisible(self)
    },

    #' @description Get a theme parameter.
    #' @param name Parameter name.
    #' @return Parameter value.
    get = function(name) {
      private$.params[[name]]
    },

    #' @description Set a theme parameter.
    #' @param name Parameter name.
    #' @param value Parameter value.
    set = function(name, value) {
      private$.params[[name]] <- value
      invisible(self)
    },

    #' @description Get all theme parameters.
    #' @return List of parameters.
    get_all = function() {
      private$.params
    },

    #' @description Merge with another theme.
    #' @param other Another CographTheme or list of parameters.
    #' @return A new merged CographTheme.
    merge = function(other) {
      if (inherits(other, "CographTheme")) {
        other_params <- other$get_all()
      } else {
        other_params <- other
      }

      new_params <- utils::modifyList(private$.params, other_params)

      do.call(CographTheme$new, c(list(name = "merged"), new_params))
    },

    #' @description Clone the theme.
    #' @return A new CographTheme.
    clone_theme = function() {
      do.call(CographTheme$new, c(list(name = private$.name), private$.params))
    },

    #' @description Print theme summary.
    print = function() {
      cat("CographTheme:", private$.name, "\n")
      cat("  Background:", private$.params$background, "\n")
      cat("  Node fill:", private$.params$node_fill, "\n")
      cat("  Node border:", private$.params$node_border, "\n")
      cat("  Edge color:", private$.params$edge_color, "\n")
      cat("  Edge positive:", private$.params$edge_positive_color, "\n")
      cat("  Edge negative:", private$.params$edge_negative_color, "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field name Theme name.
    name = function() {
      private$.name
    }
  ),

  private = list(
    .name = NULL,
    .params = NULL
  )
)

#' @title Check if object is a CographTheme
#' @param x Object to check.
#' @return Logical.
#' @keywords internal
is_cograph_theme <- function(x) {
  inherits(x, "CographTheme")
}
