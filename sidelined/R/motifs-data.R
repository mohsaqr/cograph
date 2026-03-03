# Shared constants and helpers for motif analysis
# Used across: motifs.R, motifs-plot.R, motifs-extract.R, motifs-temporal.R

# Cache environment for memoization
.cograph_cache <- new.env(parent = emptyenv())

#' Get triad patterns for visualization (column-major, no byrow)
#' Used by .plot_motifs_network, .plot_triad_networks, .plot_motif_patterns
#' @noRd
.get_triad_patterns_visual <- function() {
  list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "021D" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "021U" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "021C" = matrix(c(0L,1L,0L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "111D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "111U" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "120D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,1L,0L), 3, 3),
    "120U" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,1L,0L), 3, 3),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3),
    "210" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3)
  )
}

#' Get triad patterns for canonical lookup (row-major, byrow = TRUE)
#' Verified against igraph - used by .build_triad_lookup
#' @noRd
.get_triad_patterns_canonical <- function() {
  list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "021D" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "021U" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "021C" = matrix(c(0L,0L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "111D" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "111U" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "120D" = matrix(c(0L,0L,1L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "120U" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "210" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3, byrow = TRUE)
  )
}

#' Get MAN type descriptions
#' @noRd
.get_man_descriptions <- function() {
  c(
    "003" = "Empty",
    "012" = "Single edge",
    "102" = "Mutual pair",
    "021D" = "Out-star",
    "021U" = "In-star",
    "021C" = "Chain",
    "111D" = "Out-star + mutual",
    "111U" = "In-star + mutual",
    "030T" = "Feed-forward",
    "030C" = "Cycle",
    "201" = "Mutual + in-star",
    "120D" = "Two out-stars",
    "120U" = "Two in-stars",
    "120C" = "Mixed regulated",
    "210" = "Mutual + feed-forward",
    "300" = "Clique"
  )
}

#' Get pattern filter definitions for motif extraction
#' @return list with triangle_types, all_types, network_exclude, closed_exclude
#' @noRd
.get_pattern_filters <- function() {
  all_types <- c("003", "012", "102", "021D", "021U", "021C",
                 "111D", "111U", "030T", "030C", "201",
                 "120D", "120U", "120C", "210", "300")
  triangle_types <- c("030C", "030T", "120C", "120D", "120U", "210", "300")
  list(
    all_types = all_types,
    triangle_types = triangle_types,
    network_exclude = c("003", "012", "021C"),
    closed_exclude = c("003", "012", "021C", "120C")
  )
}

#' Get motif names based on size and directedness
#' @noRd
.get_motif_names <- function(size, directed) {
  if (size == 3 && directed) {
    c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
      "030T", "030C", "201", "120D", "120U", "120C", "210", "300")
  } else if (size == 3 && !directed) {
    c("empty", "edge", "triangle")
  } else if (size == 4) {
    paste0("M", seq_len(if (directed) 199 else 11))
  } else {
    paste0("motif_", seq_len(100))
  }
}

#' Helper to get tna internal function without using :::
#' Uses getFromNamespace which is CRAN-acceptable
#' @noRd
.get_tna_initialize_model <- function() {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' required for this operation", call. = FALSE)
  }
  get("initialize_model", envir = asNamespace("tna"))
}

#' Shared base ggplot2 theme for motif plots
#' @param base_size Base font size. Default 12.
#' @noRd
.motifs_ggplot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )
}
