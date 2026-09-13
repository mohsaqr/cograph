#' Simplicial Complex Visualization
#'
#' Visualize higher-order pathways as smooth blobs overlaid on a
#' network layout. Source nodes are blue, target nodes are red.
#'
#' Supports direct use with \code{tna} and \code{netobject} models:
#' when \code{x} has sequence data, HON or HYPA pathways are built
#' automatically (requires the \pkg{Nestimate} package). Pathways can
#' also be passed as \code{net_hon} or \code{net_hypa} objects, with
#' labels auto-translated when \code{x} is a \code{tna}/\code{netobject}.
#'
#' @param x A network object: \code{tna}, \code{netobject}, matrix,
#'   \code{igraph}, \code{cograph_network}, \code{net_hon},
#'   \code{net_hypa}, or \code{simplicial_complex} (an unordered
#'   complex — see \code{ordered}). When \code{x} is a \code{tna} or
#'   \code{netobject} with sequence data and \code{pathways} is
#'   \code{NULL}, higher-order pathways are built automatically
#'   using the \code{method} parameter.
#' @param pathways Character vector of pathway strings, a list of
#'   character vectors, a \code{net_hon} / \code{net_hypa} object, or
#'   any data.frame with a \code{path} column (e.g., the output of
#'   \code{Nestimate::mogen_transitions()}). If a data.frame with a
#'   \code{path} column is passed as \code{x} and \code{pathways} is
#'   \code{NULL}, it is auto-promoted to \code{pathways} and the state
#'   set is derived from the path strings — \code{plot_simplicial(mgt)}
#'   works directly. String separators:
#'   \code{"A B -> C"}, \code{"A -> B -> C"}, \code{"A, B, C"},
#'   \code{"A - B - C"}, \code{"A B C"}. Last state is the target.
#'   When a data.frame is passed and a \code{count} column is present,
#'   rows are sorted by count descending before \code{max_pathways} is
#'   applied. When \code{NULL} and \code{x} is a model with sequence
#'   data, pathways are built automatically.
#' @param method Pathway source when auto-building from a
#'   \code{tna}/\code{netobject}: \code{"hon"} (default, higher-order
#'   network), \code{"hypa"} (anomalous paths via hypergeometric null),
#'   or \code{"rules"} (association-rule itemsets via
#'   \code{Nestimate::association_rules}; rules are rendered as
#'   single-colored blobs because itemsets are undirected).
#' @param max_pathways Maximum number of pathways to display. HON
#'   pathways are ranked by count, HYPA by anomaly ratio.
#'   \code{NULL} shows all. Default \code{10}.
#' @param pathway_index Optional positive integer vector selecting
#'   ranked pathways after extraction and ranking, before
#'   \code{max_pathways} is applied. For example, \code{2} plots the
#'   second-ranked pathway and \code{2:4} plots pathways ranked second
#'   through fourth.
#' @param anomaly HYPA anomaly type to display when plotting a
#'   \code{net_hypa} object or auto-building HYPA pathways via
#'   \code{method = "hypa"}. One of \code{"all"}, \code{"over"}, or
#'   \code{"under"}. Default \code{"all"}. Ignored (with a warning) for
#'   non-HYPA inputs such as \code{net_hon}, \code{net_association_rules},
#'   \code{net_link_prediction}, character pathway vectors, or
#'   \code{method = "hon"} / \code{"rules"}, which have no anomaly concept.
#' @param layout \code{"circle"} (default) or a coordinate matrix.
#' @param labels Display labels. \code{NULL} uses state names.
#' @param node_color Source node fill color.
#' @param target_color Target node fill color.
#' @param ring_color Donut ring color.
#' @param node_size Node point size.
#' @param label_size Label text size.
#' @param label_color Label text color (default \code{"#e8e8e8"},
#'   very light grey). Light grey reads on both white and dark fills
#'   when the auto-contrast halo is enabled (it is by default).
#'   Applied to both source and target labels unless
#'   \code{target_label_color} overrides for targets.
#' @param target_label_color Target-node label color. \code{NULL}
#'   (default) reuses \code{label_color}.
#' @param label_halo Logical. Draw a contrasting halo behind each
#'   label so it stays readable on any fill — node disc, blob, or
#'   the white canvas. Default \code{TRUE}. The halo is the only
#'   reliable way to keep, e.g., white labels legible when
#'   \code{node_color} is also light.
#' @param label_halo_color Halo color. \code{NULL} (default)
#'   auto-picks black or white based on the luminance of
#'   \code{label_color}, so a white label gets a dark halo and vice
#'   versa.
#' @param label_halo_width Halo thickness in plot units. Default
#'   \code{0.035}; raise for chunkier outlines, lower for subtler
#'   ones, or set to \code{0} to disable without touching
#'   \code{label_halo}.
#' @param label_halo_alpha Halo opacity (0–1). Default \code{0.6}
#'   reads as a soft glow rather than a hard outline; raise toward
#'   \code{1} for sharper contrast on very busy backgrounds.
#' @param blob_alpha Blob fill transparency.
#' @param blob_colors Blob fill colors (recycled).
#' @param blob_linetype Blob border line styles (recycled).
#' @param blob_linewidth Blob border line width.
#' @param blob_line_alpha Blob border line transparency.
#' @param shadow Draw soft drop shadows?
#' @param title Plot title.
#' @param dismantled If \code{TRUE}, one panel per pathway arranged
#'   in a grid layout.
#' @param ncol Number of columns in the grid when \code{dismantled = TRUE}.
#'   Default \code{NULL} auto-selects based on the number of pathways.
#' @param ordered Is each higher-order structure a PATH or a SET?
#'   \code{TRUE} treats the last state of every pathway as its target
#'   (HON / HYPA / MOGen). \code{FALSE} treats every member as co-equal:
#'   there is no target, so no node is painted with \code{target_color},
#'   no direction cue is drawn, and the panel title is a member list
#'   rather than an arrow. \code{NULL} (default) reads it off the input
#'   — \code{net_association_rules} and \code{simplicial_complex} are
#'   sets, everything else is a path.
#' @param direction Draw the traversal inside each per-pathway panel:
#'   a light-to-dark core ramp along the path, a ring whose gold peaks
#'   on the side facing the next state, and an arrowhead just outside
#'   each node aimed at its successor. \code{NULL} (default) enables
#'   them exactly when \code{dismantled = TRUE}. A simplex is a set of
#'   vertices, so the combined overlay — where blobs overlap and a state
#'   can sit in several pathways at once — cannot express direction;
#'   \code{direction = TRUE} with \code{dismantled = FALSE} is an
#'   error rather than a silent no-op. Also forced off when the caller
#'   has collapsed the source/target two-tone (undirected input such as
#'   \code{net_association_rules}).
#' @param direction_cues Which cues to draw, any of \code{"shade"},
#'   \code{"ring"}, \code{"arrows"}. Default all three.
#' @param node_radius Node core radius in data units, used only on the
#'   directed path (rings and cores become polygons there so the ring
#'   gradient and the arrow offset are expressible; \code{geom_point()}
#'   sizes are device millimeters and cannot answer either).
#'   \code{NULL} (default) scales it to the panel extent so the nodes
#'   keep the size they have today.
#' @param legend Draw the in-figure legend strip beneath a dismantled
#'   grid. Default \code{TRUE} when \code{direction} is on.
#' @param ... Additional arguments passed to
#'   \code{Nestimate::build_hon()} or \code{Nestimate::build_hypa()}
#'   when auto-building.
#'
#' @return Invisibly, a \code{ggplot} object for the combined overlay. With
#'   \code{dismantled = TRUE} the arranged grid is returned instead: a
#'   \code{gtable} when \pkg{gridExtra} is available, otherwise a plain list
#'   of the per-pathway \code{ggplot} objects. \code{NULL} is returned when
#'   there is nothing to draw (no pathways could be extracted). Called for the
#'   side effect of drawing.
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(runif(16), 4, 4,
#'               dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' diag(mat) <- 0
#' plot_simplicial(mat, c("A B -> C", "B C -> D"))
#'
#' @import ggplot2
#' @export
plot_simplicial <- function(x = NULL,
                            pathways = NULL,
                            method = "hon",
                            max_pathways = 10L,
                            pathway_index = NULL,
                            anomaly = c("all", "over", "under"),
                            layout = "circle",
                            labels = NULL,
                            node_color = "#4A7FB5",
                            target_color = "#E8734A",
                            ring_color = "#F5A623",
                            node_size = 22,
                            label_size = 5,
                            label_color = "#e8e8e8",
                            target_label_color = NULL,
                            label_halo = TRUE,
                            label_halo_color = NULL,
                            label_halo_width = 0.035,
                            label_halo_alpha = 0.6,
                            blob_alpha = 0.25,
                            blob_colors = NULL,
                            blob_linetype = NULL,
                            blob_linewidth = 0.7,
                            blob_line_alpha = 0.8,
                            shadow = TRUE,
                            title = NULL,
                            dismantled = FALSE,
                            ncol = NULL,
                            ordered = NULL,
                            direction = NULL,
                            direction_cues = c("shade", "ring", "arrows"),
                            node_radius = NULL,
                            legend = NULL,
                            ...) {
  anomaly_explicit <- !missing(anomaly)
  anomaly <- match.arg(anomaly)
  direction_cues <- match.arg(direction_cues, several.ok = TRUE)
  # `direction = TRUE` on the combined overlay is a contract the figure
  # cannot keep: blobs overlap and one state can belong to several
  # pathways, so a single node has no one successor to point at.
  if (isTRUE(direction) && !isTRUE(dismantled)) {
    stop(errorCondition(
      paste0("`direction = TRUE` needs `dismantled = TRUE`: the combined ",
             "overlay draws every pathway on one circle, so a node shared ",
             "by two pathways has no single successor to point at."),
      class = "cograph_direction_needs_panels", call = NULL
    ))
  }
  direction <- isTRUE(direction %||% dismantled) && isTRUE(dismantled)
  hypa_used <- FALSE

  # If x is a pathways data.frame (e.g. Nestimate::mogen_transitions() output),
  # promote it to `pathways`. The path strings carry every state we need, so x
  # is not required for layout — states are derived from the parsed pathways.
  if (is.null(pathways) && is.data.frame(x) && "path" %in% names(x)) {
    pathways <- x
    x <- NULL
  }

  # Build label map for numeric ID -> label translation
  label_map <- .build_hon_label_map(x)

  # --- Resolve pathways ---
  # 1. pathways is a net_hon / net_hypa object
  if (inherits(pathways, "net_hon")) {
    pathways <- .extract_hon_pathways(pathways, label_map = label_map)
    if (length(pathways) == 0L) {
      message("No higher-order pathways found in HON object.")
      return(invisible(NULL))
    }
  } else if (inherits(pathways, "net_hypa")) {
    hypa_used <- TRUE
    pathways <- .extract_hypa_pathways(pathways, type = anomaly,
                                       label_map = label_map)
    if (length(pathways) == 0L) {
      message("No anomalous pathways found in HYPA object.")
      return(invisible(NULL))
    }
  } else if (inherits(pathways, "net_association_rules")) {
    pathways <- .extract_association_pathways(pathways)
    if (length(pathways) == 0L) {
      message("No association rules to plot.")
      return(invisible(NULL))
    }
    # Association rules are undirected itemsets — every node in a blob is
    # co-equal. Say so once; the absent target is what collapses the
    # two-tone, rather than a color assignment standing in for it.
    ordered <- ordered %||% FALSE
  } else if (inherits(pathways, "simplicial_complex")) {
    pathways <- .extract_simplicial_pathways(pathways, max_pathways)
    if (length(pathways) == 0L) {
      message("No simplices of dimension >= 1 to plot.")
      return(invisible(NULL))
    }
    ordered <- ordered %||% FALSE
  } else if (inherits(pathways, "net_link_prediction")) {
    pathways <- .extract_link_prediction_pathways(pathways)
    if (length(pathways) == 0L) {
      message("No link predictions to plot.")
      return(invisible(NULL))
    }
  } else if (is.data.frame(pathways) && "path" %in% names(pathways)) {
    pathways <- .extract_mogen_transitions_pathways(pathways,
                                                    label_map = label_map)
    if (length(pathways) == 0L) {
      message("No pathways to plot.")
      return(invisible(NULL))
    }
  }

  # 2. pathways still NULL — auto-extract or auto-build
  if (is.null(pathways)) {
    if (inherits(x, "net_hon")) {
      pathways <- .extract_hon_pathways(x, label_map = label_map)
      if (length(pathways) == 0L) {
        message("No higher-order pathways found in HON object.")
        return(invisible(NULL))
      }
      x <- NULL
    } else if (inherits(x, "net_hypa")) {
      hypa_used <- TRUE
      pathways <- .extract_hypa_pathways(x, type = anomaly,
                                         label_map = label_map)
      if (length(pathways) == 0L) {
        message("No anomalous pathways found in HYPA object.")
        return(invisible(NULL))
      }
      x <- NULL
    } else if (inherits(x, "net_association_rules")) {
      pathways <- .extract_association_pathways(x)
      if (length(pathways) == 0L) {
        message("No association rules to plot.")
        return(invisible(NULL))
      }
      # Association rules are undirected itemsets (see above).
      ordered <- ordered %||% FALSE
      x <- NULL
    } else if (inherits(x, "simplicial_complex")) {
      pathways <- .extract_simplicial_pathways(x, max_pathways)
      if (length(pathways) == 0L) {
        message("No simplices of dimension >= 1 to plot.")
        return(invisible(NULL))
      }
      ordered <- ordered %||% FALSE
      x <- NULL
    } else if (inherits(x, "net_link_prediction")) {
      pathways <- .extract_link_prediction_pathways(x)
      if (length(pathways) == 0L) {
        message("No link predictions to plot.")
        return(invisible(NULL))
      }
      x <- NULL
    } else if (inherits(x, c("tna", "netobject"))) {
      # Auto-build pathways from the model's sequence data
      ho_obj <- .build_higher_order(x, method = method, ...)
      if (method == "hon") {
        pathways <- .extract_hon_pathways(ho_obj)
        if (length(pathways) == 0L) {
          message("No higher-order pathways found.")
          return(invisible(NULL))
        }
      } else if (method == "hypa") {
        hypa_used <- TRUE
        pathways <- .extract_hypa_pathways(ho_obj, type = anomaly)
        if (length(pathways) == 0L) {
          message("No anomalous pathways found.")
          return(invisible(NULL))
        }
      } else if (method == "rules") {
        pathways <- .extract_association_pathways(ho_obj)
        if (length(pathways) == 0L) {
          message("No association rules to plot.")
          return(invisible(NULL))
        }
        # Association rules are undirected itemsets (see above).
        ordered <- ordered %||% FALSE
      }
    } else {
      stop("'pathways' must be provided unless 'x' is a tna, netobject, ",
           "net_hon, net_hypa, net_association_rules, simplicial_complex, ",
           "or net_link_prediction object.", call. = FALSE)
    }
  }

  if (anomaly_explicit && !hypa_used) {
    warning("'anomaly' only applies to HYPA pathways; ignored for this input.",
            call. = FALSE)
  }

  if (!is.null(pathway_index) && is.character(pathways)) {
    if (!is.numeric(pathway_index) || anyNA(pathway_index) ||
        any(pathway_index < 1L) ||
        any(pathway_index != as.integer(pathway_index))) {
      stop("'pathway_index' must be a positive integer vector.", call. = FALSE)
    }
    if (max(pathway_index) > length(pathways)) {
      stop(sprintf(
        "'pathway_index' requested rank %d, but only %d pathway%s available.",
        max(pathway_index), length(pathways),
        if (length(pathways) == 1L) "" else "s"
      ), call. = FALSE)
    }
    pathways <- pathways[as.integer(pathway_index)]
  }

  # Limit number of pathways
  if (!is.null(max_pathways) && length(pathways) > max_pathways &&
      (is.character(pathways) || is.list(pathways))) {
    pathways <- pathways[seq_len(max_pathways)]
  }

  ordered <- isTRUE(ordered %||% TRUE)
  # A set has no traversal to draw. This is now a fact about the input, not
  # an inference from `target_color` happening to equal `node_color`.
  if (!ordered) direction <- FALSE

  states <- .extract_blob_states(x)
  pw_list <- .parse_pathways(pathways, states, ordered = ordered)
  if (length(pw_list) == 0L) {
    message("No pathways to plot.")
    return(invisible(NULL))
  }

  if (is.null(states)) {
    states <- sort(unique(unlist(lapply(pw_list, .pw_members))))
  }

  # Expand repeated nodes: states appearing multiple times in a pathway
  # get duplicate positions so each occurrence is visually distinct
  orig_states <- states
  expanded <- .expand_repeated_nodes(pw_list, states)
  states <- expanded$states
  pw_list <- expanded$pw_list

  n <- length(states)
  if (is.null(labels)) {
    labels <- expanded$display_labels
  } else {
    # User-provided labels for original states; extend for duplicates
    orig_map <- setNames(labels, orig_states)
    dup_labels <- vapply(setdiff(states, orig_states), function(s) { # nocov start
      orig <- sub("\x02.*", "", s)
      if (orig %in% names(orig_map)) unname(orig_map[orig]) else s
    }, character(1), USE.NAMES = FALSE) # nocov end
    labels <- c(labels, dup_labels)
  }
  label_map <- setNames(labels, states)
  pos <- .blob_layout(states, labels, layout, n)

  blob_colors <- rep_len(blob_colors %||% .blob_default_colors(),
                         length(pw_list))
  blob_borders <- .darken_colors(blob_colors, 0.20)
  blob_linetype <- rep_len(blob_linetype %||% .blob_default_linetypes(),
                           length(pw_list))
  ring_border <- .darken_colors(ring_color, 0.15)

  # Belt and braces: a caller that collapsed the two-tone by hand also has
  # no order to draw.
  if (identical(target_color, node_color)) direction <- FALSE
  legend <- isTRUE(legend %||% direction)

  if (dismantled) {
    # Scale down for grid panels
    grid_node_size <- node_size * 0.6
    grid_label_size <- label_size * 0.7
    plots <- lapply(seq_along(pw_list), function(k) {
      p <- .plot_single_pathway(
        pw_list[[k]], pos, states, label_map,
        node_color, target_color, ring_color, ring_border,
        blob_colors[k], blob_borders[k], blob_linetype[k], blob_alpha,
        blob_linewidth, blob_line_alpha, shadow,
        grid_node_size, grid_label_size,
        label_color = label_color,
        target_label_color = target_label_color,
        label_halo = label_halo,
        label_halo_color = label_halo_color,
        label_halo_width = label_halo_width,
        label_halo_alpha = label_halo_alpha,
        panel_pad = 1.5,
        show_title = FALSE,
        direction = direction,
        direction_cues = direction_cues,
        node_radius = node_radius
      )
      p + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
    })
    nc <- ncol %||% ceiling(sqrt(length(plots)))
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      combined <- do.call(gridExtra::arrangeGrob,
                          c(plots, list(ncol = nc,
                                        padding = grid::unit(0, "line"),
                                        respect = TRUE)))
      if (legend) {
        # Explicit heights: a ggplotGrob handed to `bottom =` claims a
        # null-unit share and would swallow the grid.
        combined <- gridExtra::arrangeGrob(
          combined,
          .simplicial_legend_grob(node_color, target_color, ring_color,
                                  ring_border, direction = direction,
                                  ordered = ordered),
          ncol = 1L,
          heights = grid::unit.c(grid::unit(1, "null"), grid::unit(10, "mm"))
        )
      }
      grid::grid.newpage()
      grid::grid.draw(combined)
      return(invisible(combined))
    }
    lapply(plots, print) # nocov
    return(invisible(plots)) # nocov
  }

  p <- .plot_combined_pathways(
    pw_list, pos, states, label_map,
    node_color, target_color, ring_color, ring_border,
    blob_colors, blob_borders, blob_linetype, blob_alpha,
    blob_linewidth, blob_line_alpha, shadow, node_size, label_size, title,
    label_color = label_color,
    target_label_color = target_label_color,
    label_halo = label_halo,
    label_halo_color = label_halo_color,
    label_halo_width = label_halo_width,
    label_halo_alpha = label_halo_alpha
  )
  print(p)
  invisible(p)
}

# =========================================================================
# Pathway parsing
# =========================================================================

#' @noRd
.parse_pathways <- function(pathways, states, ordered = TRUE) {
  if (is.character(pathways)) {
    lapply(pathways, .parse_pathway_string, states = states,
           ordered = ordered)
  } else if (is.list(pathways)) {
    lapply(pathways, function(pw) {
      pw <- as.character(pw)
      stopifnot(length(pw) >= 2L)
      if (!isTRUE(ordered)) {
        return(list(source = unique(pw), target = NULL, ordered = FALSE))
      }
      list(source = pw[-length(pw)], target = pw[length(pw)], ordered = TRUE)
    })
  } else {
    stop("pathways must be a character vector or a list of character vectors.")
  }
}

#' @noRd
.parse_pathway_string <- function(s, states = NULL, ordered = TRUE) {
  s <- trimws(s)
  arrow_pat <- c("->", "\u2192")
  if (!isTRUE(ordered)) {
    # A set: flatten any arrow the caller's encoding happens to carry and
    # keep every member co-equal. Promoting the last token to a target would
    # make {A, B, C} depend on the order it was typed in.
    flat <- s
    for (ap in arrow_pat) flat <- gsub(ap, " ", flat, fixed = TRUE)
    tokens <- unique(.split_state_tokens(flat, states))
    if (length(tokens) < 2L) {
      stop(sprintf("Cannot parse itemset (need at least 2 states): '%s'", s))
    }
    return(list(source = tokens, target = NULL, ordered = FALSE))
  }
  for (ap in arrow_pat) {
    if (grepl(ap, s, fixed = TRUE)) {
      parts <- trimws(strsplit(s, ap, fixed = TRUE)[[1]])
      src <- .split_state_tokens(
        paste(parts[-length(parts)], collapse = " "), states
      )
      tgt <- .split_state_tokens(parts[length(parts)], states)
      return(list(source = src, target = tgt[length(tgt)], ordered = TRUE))
    }
  }
  tokens <- .split_state_tokens(s, states)
  if (length(tokens) < 2L) {
    stop(sprintf("Cannot parse pathway (need at least 2 states): '%s'", s))
  }
  list(source = tokens[-length(tokens)], target = tokens[length(tokens)],
       ordered = TRUE)
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
                                  shadow, node_size, label_size,
                                  label_color = "#e8e8e8",
                                  target_label_color = NULL,
                                  label_halo = TRUE,
                                  label_halo_color = NULL,
                                  label_halo_width = 0.035,
                                  label_halo_alpha = 0.6,
                                  panel_pad = 3.5,
                                  show_title = TRUE,
                                  direction = FALSE,
                                  direction_cues = c("shade", "ring", "arrows"),
                                  node_radius = NULL) {
  name_to_idx <- setNames(seq_along(states), states)
  # `.expand_repeated_nodes()` has already given each visit its own state id,
  # so this stays in path order: source states first, target last.
  all_st <- unique(.pw_members(pw))
  ndf <- pos[unname(name_to_idx[all_st]), , drop = FALSE]
  # A set has no target, so no node is singled out.
  is_target <- if (.pw_is_ordered(pw)) {
    ndf$state == pw$target
  } else {
    rep(FALSE, nrow(ndf))
  }
  blob <- .smooth_blob(ndf$x, ndf$y)

  # Range midpoint, not the mean: the mean is pulled toward clustered nodes,
  # so an outlying node can sit farther than `half` from it and get clipped
  # at the panel edge (a 3-node clique with two nodes close together did).
  cx <- (min(ndf$x) + max(ndf$x)) / 2
  cy <- (min(ndf$y) + max(ndf$y)) / 2
  half <- max(max(ndf$x) - min(ndf$x), max(ndf$y) - min(ndf$y)) / 2 + panel_pad

  p <- .blob_base_plot(c(cx - half, cx + half), c(cy - half, cy + half))
  if (shadow) p <- .add_shadow(p, blob)
  border_col <- adjustcolor(blob_border, alpha.f = blob_line_alpha)
  p <- p + geom_polygon(data = blob, aes(x = x, y = y),
                         fill = blob_color, color = border_col,
                         linetype = blob_lty, linewidth = blob_linewidth,
                         alpha = blob_alpha)
  if (isTRUE(direction) && .pw_is_ordered(pw)) {
    # Data units, not device millimeters — see R/blob-direction.R. The
    # default tracks the panel extent so the nodes keep the size the
    # geom_point() rendering gives them today.
    # 0.1174 was MEASURED off a rendered panel, not derived: the ring in the
    # geom_point() rendering came out 37.3 px against 44.4 px per data unit,
    # i.e. 0.1491 * half, and the core is that over 1.27. Deriving it from
    # `size` in millimeters gets it wrong in both directions, because
    # `respect = TRUE` squares the panel inside its grid cell. The engine's
    # own proportion is tighter at 18/205 = 0.088 * half; pass
    # `node_radius = 0.088 * half` for figures that must match it.
    node_r <- node_radius %||% (0.1174 * half)
    p <- .add_directed_pathway_nodes(
      p, ndf, node_color, target_color, ring_color, ring_border,
      node_radius = node_r, ring_radius = node_r * 1.27,
      label_size = label_size,
      label_color = label_color,
      target_label_color = target_label_color,
      label_halo = label_halo,
      label_halo_color = label_halo_color,
      label_halo_width = label_halo_width,
      label_halo_alpha = label_halo_alpha,
      arrows = "arrows" %in% direction_cues,
      step_shade = "shade" %in% direction_cues,
      ring_gradient = "ring" %in% direction_cues
    )
  } else {
  p <- .add_pathway_nodes(p, ndf, is_target, node_color, target_color,
                           ring_color, ring_border, node_size, label_size,
                           label_color = label_color,
                           target_label_color = target_label_color,
                           label_halo = label_halo,
                           label_halo_color = label_halo_color,
                           label_halo_width = label_halo_width,
                           label_halo_alpha = label_halo_alpha)
  }
  if (show_title) {
    lab <- vapply(.pw_members(pw), function(s) label_map[s], character(1),
                   USE.NAMES = FALSE)
    title_str <- if (.pw_is_ordered(pw)) {
      sprintf("%s  \u2192  %s",
              paste(utils::head(lab, -1L), collapse = " | "),
              lab[length(lab)])
    } else {
      # No arrow: an itemset is a membership statement, not a transition.
      paste(lab, collapse = ", ")
    }
    p <- p + labs(title = title_str)
  }
  p
}

#' @noRd
.plot_combined_pathways <- function(pw_list, pos, states, label_map,
                                     node_color, target_color,
                                     ring_color, ring_border,
                                     blob_colors, blob_borders,
                                     blob_linetypes, blob_alpha,
                                     blob_linewidth, blob_line_alpha,
                                     shadow, node_size, label_size, title,
                                     label_color = "#e8e8e8",
                                     target_label_color = NULL,
                                     label_halo = TRUE,
                                     label_halo_color = NULL,
                                     label_halo_width = 0.035,
                                     label_halo_alpha = 0.6) {
  name_to_idx <- setNames(seq_along(states), states)
  p <- .blob_base_plot()

  n_nodes <- vapply(pw_list, function(pw) {
    length(unique(.pw_members(pw)))
  }, integer(1))

  for (k in order(n_nodes, decreasing = TRUE)) {
    pw <- pw_list[[k]]
    ndf <- pos[unname(name_to_idx[unique(.pw_members(pw))]), , drop = FALSE]
    blob <- .smooth_blob(ndf$x, ndf$y)
    if (shadow) p <- .add_shadow(p, blob)
    border_col <- adjustcolor(blob_borders[k], alpha.f = blob_line_alpha)
    p <- p + geom_polygon(data = blob, aes(x = x, y = y),
                           fill = blob_colors[k], color = border_col,
                           linetype = blob_linetypes[k],
                           linewidth = blob_linewidth, alpha = blob_alpha)
  }

  # An unordered pathway contributes no target, so `all_targets` is empty
  # and every node keeps `node_color`.
  all_targets <- unique(unlist(lapply(pw_list, .pw_target)))
  is_target <- pos$state %in% all_targets
  p <- .add_pathway_nodes(p, pos, is_target, node_color, target_color,
                           ring_color, ring_border, node_size, label_size,
                           label_color = label_color,
                           target_label_color = target_label_color,
                           label_halo = label_halo,
                           label_halo_color = label_halo_color,
                           label_halo_width = label_halo_width,
                           label_halo_alpha = label_halo_alpha)

  # Suppress the source/target legend when the caller has collapsed the
  # two-tone (e.g., net_association_rules, which has no source/target).
  any_ordered <- any(vapply(pw_list, .pw_is_ordered, logical(1)))
  two_tone <- !identical(target_color, node_color) && any_ordered
  p + labs(
    title = title %||% "Higher-Order Pathways (Simplicial Complex)",
    subtitle = if (two_tone) "Blue = source  |  Red = target" else NULL
  )
}
