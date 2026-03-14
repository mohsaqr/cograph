#' Simplicial Complex Visualization
#'
#' Visualize higher-order pathways as smooth blobs overlaid on a
#' network layout. Source nodes are blue, target nodes are red.
#'
#' @param x A network object: \code{tna}, matrix, \code{igraph}, or
#'   \code{cograph_network}. When \code{NULL}, states are inferred
#'   from \code{pathways}.
#' @param pathways Character vector of pathway strings. Supports
#'   separators: \code{"A B -> C"}, \code{"A, B, C"},
#'   \code{"A - B - C"}, \code{"A B C"}. Last state is the target.
#'   Also accepts a list of character vectors.
#' @param layout \code{"circle"} (default) or a coordinate matrix.
#' @param labels Display labels. \code{NULL} uses state names.
#' @param node_color Source node fill color.
#' @param target_color Target node fill color.
#' @param ring_color Donut ring color.
#' @param node_size Node point size.
#' @param label_size Label text size.
#' @param blob_alpha Blob fill transparency.
#' @param blob_colors Blob fill colors (recycled).
#' @param blob_linetype Blob border line styles (recycled).
#' @param blob_linewidth Blob border line width.
#' @param blob_line_alpha Blob border line transparency.
#' @param shadow Draw soft drop shadows?
#' @param title Plot title.
#' @param dismantled If \code{TRUE}, one panel per pathway.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{ggplot} object (or list if dismantled), invisibly.
#'
#' @examples
#' \donttest{
#' mat <- matrix(runif(16), 4, 4,
#'               dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' diag(mat) <- 0
#' plot_simplicial(mat, c("A B -> C", "B C -> D"))
#' }
#'
#' @import ggplot2
#' @export
plot_simplicial <- function(x = NULL,
                            pathways,
                            layout = "circle",
                            labels = NULL,
                            node_color = "#4A7FB5",
                            target_color = "#E8734A",
                            ring_color = "#F5A623",
                            node_size = 22,
                            label_size = 5,
                            blob_alpha = 0.25,
                            blob_colors = NULL,
                            blob_linetype = NULL,
                            blob_linewidth = 0.7,
                            blob_line_alpha = 0.8,
                            shadow = TRUE,
                            title = NULL,
                            dismantled = FALSE,
                            ...) {

  states <- .extract_blob_states(x)
  pw_list <- .parse_pathways(pathways, states)
  if (length(pw_list) == 0L) {
    message("No pathways to plot.")
    return(invisible(NULL))
  }

  if (is.null(states)) {
    states <- sort(unique(unlist(lapply(pw_list, function(pw) {
      c(pw$source, pw$target)
    }))))
  }

  n <- length(states)
  if (is.null(labels)) labels <- states
  label_map <- setNames(labels, states)
  pos <- .blob_layout(states, labels, layout, n)

  blob_colors <- rep_len(blob_colors %||% .blob_default_colors(),
                         length(pw_list))
  blob_borders <- .darken_colors(blob_colors, 0.20)
  blob_linetype <- rep_len(blob_linetype %||% .blob_default_linetypes(),
                           length(pw_list))
  ring_border <- .darken_colors(ring_color, 0.15)

  if (dismantled) {
    plots <- lapply(seq_along(pw_list), function(k) {
      .plot_single_pathway(
        pw_list[[k]], pos, states, label_map,
        node_color, target_color, ring_color, ring_border,
        blob_colors[k], blob_borders[k], blob_linetype[k], blob_alpha,
        blob_linewidth, blob_line_alpha, shadow, node_size, label_size
      )
    })
    lapply(plots, print)
    return(invisible(plots))
  }

  p <- .plot_combined_pathways(
    pw_list, pos, states, label_map,
    node_color, target_color, ring_color, ring_border,
    blob_colors, blob_borders, blob_linetype, blob_alpha,
    blob_linewidth, blob_line_alpha, shadow, node_size, label_size, title
  )
  print(p)
  invisible(p)
}

# =========================================================================
# Pathway parsing
# =========================================================================

#' @noRd
.parse_pathways <- function(pathways, states) {
  if (is.character(pathways)) {
    lapply(pathways, .parse_pathway_string, states = states)
  } else if (is.list(pathways)) {
    lapply(pathways, function(pw) {
      pw <- as.character(pw)
      stopifnot(length(pw) >= 2L)
      list(source = pw[-length(pw)], target = pw[length(pw)])
    })
  } else {
    stop("pathways must be a character vector or a list of character vectors.")
  }
}

#' @noRd
.parse_pathway_string <- function(s, states = NULL) {
  s <- trimws(s)
  arrow_pat <- c("->", "\u2192")
  for (ap in arrow_pat) {
    if (grepl(ap, s, fixed = TRUE)) {
      parts <- trimws(strsplit(s, ap, fixed = TRUE)[[1]])
      src <- .split_state_tokens(
        paste(parts[-length(parts)], collapse = " "), states
      )
      tgt <- .split_state_tokens(parts[length(parts)], states)
      return(list(source = src, target = tgt[length(tgt)]))
    }
  }
  tokens <- .split_state_tokens(s, states)
  if (length(tokens) < 2L) {
    stop(sprintf("Cannot parse pathway (need at least 2 states): '%s'", s))
  }
  list(source = tokens[-length(tokens)], target = tokens[length(tokens)])
}

#' @noRd
.split_state_tokens <- function(s, states = NULL) {
  s <- trimws(s)
  if (!nzchar(s)) return(character(0))
  seps <- c(",", " - ", "-", " ")
  if (!is.null(states)) {
    lc_states <- tolower(states)
    for (sep in seps) {
      tokens <- trimws(strsplit(s, sep, fixed = TRUE)[[1]])
      tokens <- tokens[nzchar(tokens)]
      if (length(tokens) >= 1L && all(tolower(tokens) %in% lc_states)) {
        return(vapply(tokens, function(t) {
          states[lc_states == tolower(t)][1L]
        }, character(1), USE.NAMES = FALSE))
      }
    }
  }
  tokens <- trimws(strsplit(s, "\\s+")[[1]])
  tokens[nzchar(tokens)]
}

# =========================================================================
# Plot assembly
# =========================================================================

#' @noRd
.plot_single_pathway <- function(pw, pos, states, label_map,
                                  node_color, target_color,
                                  ring_color, ring_border,
                                  blob_color, blob_border, blob_lty, blob_alpha,
                                  blob_linewidth, blob_line_alpha,
                                  shadow, node_size, label_size) {
  name_to_idx <- setNames(seq_along(states), states)
  all_st <- unique(c(pw$source, pw$target))
  ndf <- pos[unname(name_to_idx[all_st]), , drop = FALSE]
  is_target <- ndf$state == pw$target
  blob <- .smooth_blob(ndf$x, ndf$y)

  cx <- mean(ndf$x); cy <- mean(ndf$y)
  half <- max(max(ndf$x) - min(ndf$x), max(ndf$y) - min(ndf$y)) / 2 + 3.5

  src_lab <- vapply(pw$source, function(s) label_map[s], character(1),
                     USE.NAMES = FALSE)
  title_str <- sprintf("%s  \u2192  %s",
                        paste(src_lab, collapse = " | "), label_map[pw$target])

  p <- .blob_base_plot(c(cx - half, cx + half), c(cy - half, cy + half))
  if (shadow) p <- .add_shadow(p, blob)
  border_col <- adjustcolor(blob_border, alpha.f = blob_line_alpha)
  p <- p + geom_polygon(data = blob, aes(x = x, y = y),
                         fill = blob_color, color = border_col,
                         linetype = blob_lty, linewidth = blob_linewidth,
                         alpha = blob_alpha)
  p <- .add_pathway_nodes(p, ndf, is_target, node_color, target_color,
                           ring_color, ring_border, node_size, label_size)
  p + labs(title = title_str)
}

#' @noRd
.plot_combined_pathways <- function(pw_list, pos, states, label_map,
                                     node_color, target_color,
                                     ring_color, ring_border,
                                     blob_colors, blob_borders,
                                     blob_linetypes, blob_alpha,
                                     blob_linewidth, blob_line_alpha,
                                     shadow, node_size, label_size, title) {
  name_to_idx <- setNames(seq_along(states), states)
  p <- .blob_base_plot()

  n_nodes <- vapply(pw_list, function(pw) {
    length(unique(c(pw$source, pw$target)))
  }, integer(1))

  for (k in order(n_nodes, decreasing = TRUE)) {
    pw <- pw_list[[k]]
    ndf <- pos[unname(name_to_idx[unique(c(pw$source, pw$target))]), ,
               drop = FALSE]
    blob <- .smooth_blob(ndf$x, ndf$y)
    if (shadow) p <- .add_shadow(p, blob)
    border_col <- adjustcolor(blob_borders[k], alpha.f = blob_line_alpha)
    p <- p + geom_polygon(data = blob, aes(x = x, y = y),
                           fill = blob_colors[k], color = border_col,
                           linetype = blob_linetypes[k],
                           linewidth = blob_linewidth, alpha = blob_alpha)
  }

  all_targets <- unique(vapply(pw_list, `[[`, character(1), "target"))
  is_target <- pos$state %in% all_targets
  p <- .add_pathway_nodes(p, pos, is_target, node_color, target_color,
                           ring_color, ring_border, node_size, label_size)

  p + labs(
    title = title %||% "Higher-Order Pathways (Simplicial Complex)",
    subtitle = "Blue = source  |  Red = target"
  )
}
