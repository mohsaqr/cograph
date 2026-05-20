# Shared helpers for plot_simplicial() and overlay_communities()

# =========================================================================
# Repeated-node expansion
# =========================================================================

#' Expand repeated nodes in pathways
#'
#' When a state appears multiple times in a pathway's sequence
#' (e.g., "A B -> B" where B is both source and target), creates
#' duplicate node IDs so each occurrence gets its own layout position.
#' Display labels for duplicates map back to the original state name.
#'
#' @param pw_list Parsed pathway list (each element has source/target).
#' @param states Character vector of unique state names.
#' @return List with \code{states} (expanded), \code{pw_list} (updated),
#'   and \code{display_labels} (original names for all states).
#' @noRd
.expand_repeated_nodes <- function(pw_list, states) {
  new_states <- states

  pw_list <- lapply(pw_list, function(pw) {
    full_seq <- c(pw$source, pw$target)
    n <- length(full_seq)
    new_ids <- character(n)
    seen <- integer(0)
    names(seen) <- character(0)

    for (i in seq_len(n)) {
      s <- full_seq[i]
      if (is.na(seen[s])) {
        seen[s] <- 1L
        new_ids[i] <- s
      } else {
        seen[s] <- seen[s] + 1L
        dup_id <- paste0(s, "\x02", seen[s])
        new_ids[i] <- dup_id
        if (!(dup_id %in% new_states)) {
          new_states <<- c(new_states, dup_id)
        }
      }
    }

    n_src <- length(pw$source)
    list(source = new_ids[seq_len(n_src)], target = new_ids[n])
  })

  display_labels <- vapply(new_states, function(s) {
    sub("\x02.*", "", s)
  }, character(1), USE.NAMES = FALSE)

  list(states = new_states, pw_list = pw_list, display_labels = display_labels)
}

# =========================================================================
# State extraction
# =========================================================================

#' Extract state names from a network object
#' @noRd
.extract_blob_states <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "tna")) return(x$labels)
  if (inherits(x, "igraph")) {
    if (requireNamespace("igraph", quietly = TRUE)) {
      return(igraph::V(x)$name %||% paste0("S", seq_len(igraph::vcount(x))))
    }
  }
  if (inherits(x, "cograph_network")) return(x$nodes$label)
  if (is.matrix(x)) {
    states <- rownames(x)
    return(if (is.null(states)) paste0("S", seq_len(nrow(x))) else states)
  }
  if (inherits(x, "net_hon")) return(x$first_order_states)
  if (inherits(x, "net_hypa")) {
    parts <- strsplit(
      gsub("\x01", " -> ", x$nodes, fixed = TRUE), " -> ", fixed = TRUE
    )
    return(sort(unique(unlist(parts))))
  }
  stop("x must be a tna object, matrix, igraph, or cograph_network.")
}

# =========================================================================
# Layout
# =========================================================================

#' Compute circle or custom layout for blob plots
#' @noRd
.blob_layout <- function(states, labels, layout, n) {
  if (is.character(layout) && layout == "circle") {
    angles <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1)[seq_len(n)]
    R <- 5.5
    data.frame(
      x = R * cos(angles),
      y = R * sin(angles),
      label = labels,
      state = states,
      stringsAsFactors = FALSE
    )
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    layout <- as.data.frame(layout)
    stopifnot(nrow(layout) == n, ncol(layout) >= 2)
    data.frame(
      x = as.numeric(layout[, 1]),
      y = as.numeric(layout[, 2]),
      label = labels,
      state = states,
      stringsAsFactors = FALSE
    )
  } else {
    stop("layout must be 'circle' or a matrix of coordinates.")
  }
}

# =========================================================================
# Geometry
# =========================================================================

#' Smooth blob polygon via padded convex hull + Laplacian smoothing
#'
#' Filters non-finite anchor points before calling `grDevices::chull()` —
#' callers that pass NA coordinates (e.g. when a pathway references a
#' state missing from the layout) would otherwise abort with "finite
#' coordinates are needed". Returns an empty polygon (zero-row data.frame
#' on the caller's expected shape) when no finite anchors remain, so the
#' caller can skip the geom without erroring.
#' @noRd
.smooth_blob <- function(px, py, pad = 1.0, n_circle = 60L,
                         n_upsample = 800L, n_smooth_iter = 80L) {
  ok <- is.finite(px) & is.finite(py)
  px <- px[ok]; py <- py[ok]
  if (length(px) == 0L) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  all_x <- all_y <- numeric(0)
  for (i in seq_along(px)) {
    a <- seq(0, 2 * pi, length.out = n_circle + 1L)[-(n_circle + 1L)]
    all_x <- c(all_x, px[i] + pad * cos(a))
    all_y <- c(all_y, py[i] + pad * sin(a))
  }
  hi <- grDevices::chull(all_x, all_y)
  hx <- all_x[hi]; hy <- all_y[hi]
  n_hull <- length(hx)
  hx <- c(hx, hx[1]); hy <- c(hy, hy[1])
  ux <- uy <- numeric(0)
  for (i in seq_len(n_hull)) {
    seg_n <- max(2L, round(n_upsample / n_hull))
    t_seq <- seq(0, 1, length.out = seg_n + 1L)[-(seg_n + 1L)]
    ux <- c(ux, hx[i] + t_seq * (hx[i + 1] - hx[i]))
    uy <- c(uy, hy[i] + t_seq * (hy[i + 1] - hy[i]))
  }
  n_pts <- length(ux)
  for (iter in seq_len(n_smooth_iter)) {
    nx <- ny <- numeric(n_pts)
    for (j in seq_len(n_pts)) {
      jp <- if (j == 1L) n_pts else j - 1L
      jn <- if (j == n_pts) 1L else j + 1L
      nx[j] <- (ux[jp] + ux[j] + ux[jn]) / 3
      ny[j] <- (uy[jp] + uy[j] + uy[jn]) / 3
    }
    ux <- nx; uy <- ny
  }
  data.frame(x = c(ux, ux[1]), y = c(uy, uy[1]))
}

#' Darken hex colors by a fraction
#' @noRd
.darken_colors <- function(cols, amount = 0.2) {
  vapply(cols, function(col) {
    rgb <- grDevices::col2rgb(col)[, 1] / 255
    darkened <- pmax(rgb * (1 - amount), 0)
    grDevices::rgb(darkened[1], darkened[2], darkened[3])
  }, character(1), USE.NAMES = FALSE)
}

#' Pick black or white text for readability over a fill color
#' @noRd
.contrasting_text_color <- function(fill, light = "white", dark = "#1a1a1a",
                                     threshold = 0.6) {
  vapply(fill, function(col) {
    rgb <- grDevices::col2rgb(col)[, 1] / 255
    lum <- 0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
    if (lum > threshold) dark else light
  }, character(1), USE.NAMES = FALSE)
}

# =========================================================================
# Default palettes
# =========================================================================

#' Default blob fill colors
#' @noRd
.blob_default_colors <- function() {
  c("#B0D4F1", "#A8D8A8", "#F0C8A0", "#D4B0F0",
    "#F0DFA0", "#C8E8E0", "#F0D4B0", "#E0C8E8",
    "#D4F0B0", "#F0B0B0")
}

#' Default blob linetype cycle
#' @noRd
.blob_default_linetypes <- function() {
  c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
}

# =========================================================================
# ggplot layer helpers
# =========================================================================

#' Add shadow layers to a ggplot
#' @noRd
.add_shadow <- function(p, blob, n_layers = 3L, offset = 0.04,
                         alpha = 0.008) {
  for (s in seq(n_layers, 1L, by = -1L)) {
    shadow_df <- blob
    shadow_df$x <- shadow_df$x + s * offset
    shadow_df$y <- shadow_df$y - s * offset
    p <- p + ggplot2::geom_polygon(
      data = shadow_df, ggplot2::aes(x = x, y = y),
      fill = "black", color = NA, alpha = alpha
    )
  }
  p
}

#' Base ggplot with void theme for blob plots
#' @noRd
.blob_base_plot <- function(xlim = c(-9, 9), ylim = c(-8.5, 8.5)) {
  ggplot2::ggplot() +
    ggplot2::coord_equal(clip = "off", xlim = xlim, ylim = ylim) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = 18, face = "bold", color = "#2c3e50"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = 11, color = "#7f8c8d",
        margin = ggplot2::margin(b = 15)
      )
    )
}

#' Add source/target colored nodes to a ggplot
#' @noRd
.add_pathway_nodes <- function(p, ndf, is_target, node_color, target_color,
                                ring_color, ring_border, node_size,
                                label_size,
                                label_color = "#e8e8e8",
                                target_label_color = NULL,
                                label_halo = TRUE,
                                label_halo_color = NULL,
                                label_halo_width = 0.035,
                                label_halo_alpha = 0.6) {
  src_text_color <- label_color
  tgt_text_color <- target_label_color %||% label_color
  src_halo_color <- label_halo_color %||% .contrasting_text_color(src_text_color)
  tgt_halo_color <- label_halo_color %||% .contrasting_text_color(tgt_text_color)

  ring_size <- node_size * 1.27
  p <- p + ggplot2::geom_point(
    data = ndf, ggplot2::aes(x = x, y = y),
    fill = ring_color, color = ring_border,
    size = ring_size, shape = 21, stroke = 1
  )

  src_df <- ndf[!is_target, , drop = FALSE]
  if (nrow(src_df) > 0L) {
    p <- p + ggplot2::geom_point(
      data = src_df, ggplot2::aes(x = x, y = y),
      fill = node_color, color = node_color,
      size = node_size, shape = 21, stroke = 0.5
    )
    p <- .add_text_with_halo(p, src_df, src_text_color, src_halo_color,
                              label_size, label_halo, label_halo_width,
                              label_halo_alpha)
  }

  tgt_df <- ndf[is_target, , drop = FALSE]
  if (nrow(tgt_df) > 0L) {
    p <- p + ggplot2::geom_point(
      data = tgt_df, ggplot2::aes(x = x, y = y),
      fill = target_color, color = target_color,
      size = node_size, shape = 21, stroke = 0.5
    )
    p <- .add_text_with_halo(p, tgt_df, tgt_text_color, tgt_halo_color,
                              label_size, label_halo, label_halo_width,
                              label_halo_alpha)
  }
  p
}

#' Draw bold text with an optional contrasting halo for readability
#'
#' Stamps the text 8x at small offsets in \code{halo_color}, then the
#' real text once on top in \code{color}. Eight directions is the
#' minimum that reads as smooth at typical print sizes; four is
#' visibly blocky.
#' @noRd
.add_text_with_halo <- function(p, data, color, halo_color,
                                 size, halo = TRUE, halo_width = 0.035,
                                 halo_alpha = 0.6) {
  if (isTRUE(halo) && halo_width > 0 && halo_alpha > 0) {
    angles <- seq(0, 2 * pi, length.out = 9L)[-9L]
    halo_layers <- lapply(angles, function(a) {
      d <- data
      d$x <- d$x + halo_width * cos(a)
      d$y <- d$y + halo_width * sin(a)
      ggplot2::geom_text(
        data = d, ggplot2::aes(x = x, y = y, label = label),
        color = halo_color, fontface = "bold", size = size,
        alpha = halo_alpha
      )
    })
    p <- Reduce(`+`, halo_layers, init = p)
  }
  p + ggplot2::geom_text(
    data = data, ggplot2::aes(x = x, y = y, label = label),
    color = color, fontface = "bold", size = size
  )
}

# =========================================================================
# Nestimate higher-order pathway extraction
# =========================================================================

#' Extract higher-order pathways from a net_hon object
#'
#' Converts HON edge paths (format \code{"A -> B -> C"}) where
#' \code{from_order > 1} into simplicial pathway strings
#' (\code{"A B -> C"}). Sorted by count (descending).
#'
#' @param x A \code{net_hon} object from \code{nestimate::build_hon()}.
#' @param label_map Named character vector mapping numeric IDs to labels.
#' @return Character vector of pathway strings.
#' @noRd
.extract_hon_pathways <- function(x, label_map = NULL) {
  # Nestimate net_hon stores higher-order edges in $ho_edges with a
  # $from_order column; $edges is the flattened first-order projection.
  edges <- x$ho_edges %||% x$edges
  ho <- edges[edges$from_order > 1L, , drop = FALSE]
  if (nrow(ho) == 0L) return(character(0))
  ho <- ho[order(-ho$count), , drop = FALSE]
  vapply(ho$path, function(p) {
    parts <- trimws(strsplit(p, "->", fixed = TRUE)[[1]])
    if (!is.null(label_map)) {
      parts <- vapply(parts, function(s) {
        if (s %in% names(label_map)) unname(label_map[s]) else s
      }, character(1), USE.NAMES = FALSE)
    }
    src <- parts[-length(parts)]
    tgt <- parts[length(parts)]
    paste0(paste(src, collapse = " "), " -> ", tgt)
  }, character(1), USE.NAMES = FALSE)
}

#' Extract anomalous pathways from a net_hypa object
#'
#' Converts HYPA scored paths (format \code{"A -> B -> C"}) where
#' \code{anomaly != "normal"} into simplicial pathway strings.
#' Sorted by ratio (descending).
#'
#' @param x A \code{net_hypa} object from \code{nestimate::build_hypa()}.
#' @param type Which anomalies to include: \code{"all"} (default),
#'   \code{"over"}, or \code{"under"}.
#' @param label_map Named character vector mapping numeric IDs to labels.
#' @return Character vector of pathway strings.
#' @noRd
.extract_hypa_pathways <- function(x, type = "all", label_map = NULL) {
  type <- match.arg(type, c("all", "over", "under"))
  scores <- x$scores
  if (type == "all") {
    anom <- scores[scores$anomaly != "normal", , drop = FALSE]
  } else {
    anom <- scores[scores$anomaly == type, , drop = FALSE]
  }
  if (nrow(anom) == 0L) return(character(0))
  if ("ratio" %in% names(anom)) {
    if (type == "under") {
      anom <- anom[order(anom$ratio), , drop = FALSE]
    } else {
      anom <- anom[order(-anom$ratio), , drop = FALSE]
    }
  }
  vapply(anom$path, function(p) {
    parts <- trimws(strsplit(p, "->", fixed = TRUE)[[1]])
    if (!is.null(label_map)) {
      parts <- vapply(parts, function(s) {
        if (s %in% names(label_map)) unname(label_map[s]) else s
      }, character(1), USE.NAMES = FALSE)
    }
    src <- parts[-length(parts)]
    tgt <- parts[length(parts)]
    paste0(paste(src, collapse = " "), " -> ", tgt)
  }, character(1), USE.NAMES = FALSE)
}

#' Extract pathways from a data.frame with a \code{$path} column
#'
#' Converts rows of the form \code{"A -> B -> C"} into the simplicial
#' pathway string format (\code{"A B -> C"}). When a \code{$count}
#' column is present, rows are sorted by count descending. Designed to
#' accept \code{Nestimate::mogen_transitions()} output without depending
#' on its class.
#'
#' @param x A data.frame with a \code{path} column (and optionally a
#'   \code{count} column).
#' @param label_map Named character vector mapping numeric IDs to labels
#'   (accepted for signature consistency with the other extractors;
#'   ignored when paths are already in label space).
#' @return Character vector of pathway strings.
#' @noRd
.extract_mogen_transitions_pathways <- function(x, label_map = NULL) {
  if (nrow(x) == 0L) return(character(0))
  d <- if ("count" %in% names(x)) x[order(-x$count), , drop = FALSE] else x
  vapply(d$path, function(p) {
    parts <- trimws(strsplit(p, "->", fixed = TRUE)[[1]])
    if (length(parts) < 2L) return(p)
    if (!is.null(label_map)) {
      parts <- vapply(parts, function(s) {
        if (s %in% names(label_map)) unname(label_map[s]) else s
      }, character(1), USE.NAMES = FALSE)
    }
    src <- parts[-length(parts)]
    tgt <- parts[length(parts)]
    paste0(paste(src, collapse = " "), " -> ", tgt)
  }, character(1), USE.NAMES = FALSE)
}

#' Extract pathways from association rules (net_association_rules)
#'
#' Converts rules \code{{A, B} => {C}} into simplicial pathway strings
#' (\code{"A B -> C"}). Sorted by lift (descending).
#'
#' @param x A \code{net_association_rules} object from
#'   \code{Nestimate::association_rules()}.
#' @return Character vector of pathway strings.
#' @noRd
.extract_association_pathways <- function(x) {
  rules <- x$rules
  if (nrow(rules) == 0L) return(character(0))
  rules <- rules[order(-rules$lift, -rules$confidence), , drop = FALSE]
  # Normalise antecedent/consequent columns to space-separated itemset strings.
  # Works on both Nestimate shapes: list-column (character vectors per row) and
  # character-column ("A, B" per row). Vectorized per column — no per-row split.
  norm_col <- function(col) {
    if (is.list(col)) vapply(col, paste, character(1), collapse = " ")
    else              gsub(",\\s*", " ", col)
  }
  paste(norm_col(rules$antecedent), "->", norm_col(rules$consequent))
}

#' Extract pathways from link predictions (net_link_prediction)
#'
#' For each top predicted edge, includes common neighbor evidence as
#' source nodes: \code{"A cn1 cn2 -> B"}. Falls back to simple
#' \code{"A -> B"} when adjacency matrix is unavailable.
#'
#' @param x A \code{net_link_prediction} object from
#'   \code{Nestimate::predict_links()}.
#' @param method Character or NULL. Which method's predictions to use.
#' @param max_evidence Integer. Max evidence nodes per pathway.
#' @return Character vector of pathway strings.
#' @noRd
.extract_link_prediction_pathways <- function(x, method = NULL,
                                               max_evidence = 3L) {
  if (is.null(method)) method <- x$methods[1]
  df <- x$predictions[x$predictions$method == method, , drop = FALSE]
  df <- df[order(-df$score), , drop = FALSE]
  if (nrow(df) == 0L) return(character(0))

  A <- x$adjacency
  if (is.null(A)) {
    return(paste(df$from, "->", df$to))
  }

  nodes <- x$nodes
  vapply(seq_len(nrow(df)), function(i) {
    from_idx <- match(df$from[i], nodes)
    to_idx <- match(df$to[i], nodes)
    from_out <- A[from_idx, ] > 0
    to_in <- A[, to_idx] > 0
    cn_mask <- from_out & to_in
    cn_mask[from_idx] <- FALSE
    cn_mask[to_idx] <- FALSE
    cn_indices <- which(cn_mask)
    cn_nodes <- nodes[cn_indices]
    if (length(cn_nodes) > max_evidence) {
      cn_weights <- A[from_idx, cn_indices] + A[cn_indices, to_idx]
      cn_nodes <- cn_nodes[order(-cn_weights)][seq_len(max_evidence)]
    }
    sources <- c(df$from[i], cn_nodes)
    paste0(paste(sources, collapse = " "), " -> ", df$to[i])
  }, character(1), USE.NAMES = FALSE)
}

#' Build label map from a tna object for HON/HYPA numeric ID translation
#' @noRd
.build_hon_label_map <- function(x) {
  if (inherits(x, "tna") && !is.null(x$labels)) {
    return(setNames(x$labels, as.character(seq_along(x$labels))))
  }
  if (inherits(x, "netobject") && !is.null(rownames(x$weights))) {
    nms <- rownames(x$weights)
    if (all(grepl("^\\d+$", nms))) {
      return(setNames(nms, nms))
    }
  }
  NULL
}

#' Extract labeled sequence data from a tna or netobject
#'
#' Returns a data.frame with state labels (not numeric IDs) suitable
#' for passing to \code{Nestimate::build_hon()} / \code{build_hypa()}.
#'
#' @param x A \code{tna} or \code{netobject}.
#' @return A data.frame of sequence data with label names, or \code{NULL}.
#' @noRd
.extract_sequence_data <- function(x) {
  if (inherits(x, "tna") && !is.null(x$data)) {
    df <- as.data.frame(x$data)
    lbl <- attr(x$data, "labels") %||% x$labels
    if (!is.null(lbl) && all(vapply(df, is.numeric, logical(1)))) {
      df[] <- lapply(df, function(col) lbl[col])
    }
    return(df)
  }
  if (inherits(x, "netobject") && !is.null(x$data)) {
    df <- as.data.frame(x$data)
    lbl <- rownames(x$weights)
    if (!is.null(lbl) && all(vapply(df, is.numeric, logical(1)))) {
      df[] <- lapply(df, function(col) lbl[col])
    }
    return(df)
  }
  NULL
}

#' Build HON or HYPA from a tna/netobject (requires Nestimate)
#' @noRd
.build_higher_order <- function(x, method = "hon", ...) {
  if (!requireNamespace("Nestimate", quietly = TRUE)) {
    stop("Package 'Nestimate' is required for automatic higher-order ",
         "pathway extraction. Install it or pass pathways manually.",
         call. = FALSE)
  }
  seq_data <- .extract_sequence_data(x)
  if (is.null(seq_data)) {
    stop("Cannot extract sequence data from '", class(x)[1],
         "' object. Provide pathways manually or pass a tna/netobject ",
         "with sequence data.", call. = FALSE)
  }
  if (method == "hon") {
    Nestimate::build_hon(seq_data, ...)
  } else if (method == "hypa") {
    Nestimate::build_hypa(seq_data, ...)
  } else if (method == "rules") {
    Nestimate::association_rules(seq_data, ...)
  } else {
    stop("method must be 'hon', 'hypa', or 'rules'.", call. = FALSE)
  }
}
